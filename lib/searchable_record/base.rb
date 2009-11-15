module SearchableRecord
  module Base
    def self.included(base)
      base.extend(MacroMethods)
    end
  
    module MacroMethods
      def searches_on(*args)
        unless self.is_a? SearchableRecord::Base::ClassMethods
          extend SearchableRecord::Base::ClassMethods
        end

        self.class_searchable_attributes.concat(args.map(&:to_s)) if args.any? && args.first != :all
      end
    end
  
    module ClassMethods
      def class_searchable_attributes
        attributes = read_inheritable_attribute(:class_searchable_attributes)
        write_inheritable_attribute(:class_searchable_attributes, attributes = []) if attributes.nil?
        return attributes
      end

      def searchable_attributes(tables = nil)
        attributes = read_inheritable_attribute(:class_searchable_attributes)
        if attributes.blank?
          attributes = []
          string_columns = self.columns.select { |c| c.type == :text or c.type == :string }
          attributes = string_columns.collect { |c| c.name }
        end
        attributes = attributes.collect { |attribute| self.table_name + "." + attribute }
      
        tables = tables ? [ tables ].flatten : []
        attributes += searchable_attributes_in_associations(tables)

        return attributes
      end

      def searchable_attributes_in_associations(tables)
        attributes = []

        unless tables.empty?
          tables.each do |table, assocs|
            if table.kind_of? Hash
              attributes += searchable_attributes_in_associations(table)
            else
              klass = eval table.to_s.classify
              attributes += klass.searchable_attributes(assocs)
            end
          end
        end

        return attributes
      end

      def search(text = nil, options = {})
        search_includes = options[:search_include] ? [ options[:search_include] ].flatten : []

        attributes = searchable_attributes(search_includes)
        attributes &= options[:only].map(&:to_s) if options[:only]
        attributes -= options[:except].map(&:to_s) if options[:except]

        unless options[:case_sensitive]
          text.downcase!
          attributes.map! { |attribute| "lower(#{attribute})" }
        end

        text_conditions = text.blank? || attributes.blank? ? nil : build_text_condition(attributes, text)
        if text_conditions
          conditions = case options[:conditions]
          when String
            "#{text_conditions} AND #{options[:conditions]}"
          when Array
            options[:conditions][ 1, options[:conditions].length ].unshift("#{text_conditions} AND #{options[:conditions].first}")
          when Hash
            options[:conditions].values.unshift(([ text_conditions ] + options[:conditions].keys.map { |key| "#{key} = ?" }).join(" AND "))
          else
            text_conditions
          end
        else
          conditions = options[:conditions]
        end

        includes = search_includes.dup
        includes << options[:include] if options[:include]
        includes.flatten!.uniq!

        if self.respond_to?(:paginate)
          search_options = { :include => includes.any? ? includes : nil,
                             :conditions => conditions,
                             :page => options[:page],
                             :per_page => options[:per_page],
                             :order => options[:order] }

          paginate(search_options)
        else
          search_options = { :include => includes.any? ? includes : nil,
                             :conditions => conditions,
                             :offset => options[:offset],
                             :limit => options[:limit],
                             :order => options[:order] }
    
          find(search_options)
        end
      end


      private

      # A chunk is a string of non-whitespace,
      # except that anything inside double quotes
      # is a chunk, including whitespace
      def make_chunks(s)
        chunks = []
        while s.length > 0
          next_interesting_index = (s =~ /\s|\"/)
          if next_interesting_index
            if next_interesting_index > 0
              chunks << s[0...next_interesting_index]
              s = s[next_interesting_index..-1]
            else
              if s =~ /^\"/
                s = s[1..-1]
                next_interesting_index = (s =~ /[\"]/)
                if next_interesting_index
                  chunks << s[0...next_interesting_index]
                  s = s[next_interesting_index+1..-1]
                elsif s.length > 0
                  chunks << s
                  s = ''
                end
              else
                next_interesting_index = (s =~ /\S/)
                s = s[next_interesting_index..-1]
              end
            end
          else
            chunks << s
            s = ''
          end
        end

        chunks
      end

      def process_chunk(chunk)
        case chunk
        when /^-/
          if chunk.length  1
            [:not]
          else
            [:not, *process_chunk(chunk[1..-1])]
          end
        when /^\(.*\)$/
          if chunk.length  2
            [:left_paren, :right_paren]
         else
            [:left_paren].concat(process_chunk(chunk[1..-2])) << :right_paren
          end
        when /^\(/
          if chunk.length  1
            [:left_paren]
          else
            [:left_paren].concat(process_chunk(chunk[1..-1]))
          end
        when /\)$/
          if chunk.length  1
            [:right_paren]
          else
            process_chunk(chunk[0..-2]) << :right_paren
          end
        when 'and'
          [:and]
        when 'or'
          [:or]
        when 'not'
          [:not]
        else
          [chunk]
        end
      end

      def lex(s)
        tokens = []

        make_chunks(s).each { |chunk|
          tokens.concat(process_chunk(chunk))
        }

        tokens
      end

      def parse_paren_expr(tokens)
        expr_tokens = []
        while !tokens.empty? && tokens[0] != :right_paren
          expr_tokens << tokens.shift
        end

        if !tokens.empty?
          tokens.shift
        end

        parse_expr(expr_tokens)
      end

      def parse_term(tokens)
        if tokens.empty?
          return ''
        end

        token = tokens.shift
        case token
        when :not
            [:not, parse_term(tokens)]
        when :left_paren
          parse_paren_expr(tokens)
        when :right_paren
          '' # skip bogus token
        when :and
            '' # skip bogus token
        when :or
            '' # skip bogus token
        else
          token
        end
      end

      def parse_and_expr(tokens, operand)
        if (tokens[0]  == :and)
          tokens.shift
        end
        # Even if :and is missing, :and is implicit
        [:and, operand, parse_term(tokens)]
      end

      def parse_or_expr(tokens, operand)
        if (tokens[0]  == :or)
          tokens.shift
          [:or, operand, parse_expr(tokens)]
        else
          parse_and_expr(tokens, operand)
        end
      end

      def parse_expr(tokens)
        if tokens.empty?
          return ''
        end

        expr = parse_term(tokens)
        while !tokens.empty?
          expr = parse_or_expr(tokens, expr)
        end

        expr
      end

      def parse_tokens(tokens)
        tree = parse_expr(tokens)
        tree.kind_of?(Array)? tree : [tree]
      end

      def parse(text)
        parse_tokens(lex(text))
      end

      def apply_demorgans(tree)
        if tree == []
          return []
        end

        token = tree.kind_of?(Array)? tree[0] : tree
        case token
        when :not
            if (tree[1].kind_of?(Array))
              subtree = tree[1]
              if subtree[0] == :and
                  [:or,
                   apply_demorgans([:not, subtree[1]]),
                   apply_demorgans([:not, subtree[2]])]
              elsif tree[1][0] == :or
                  [:and,
                   apply_demorgans([:not, subtree[1]]),
                   apply_demorgans([:not, subtree[2]])]
              else
                # assert tree[1][0] == :not
                apply_demorgans(subtree[1])
              end
            else
              tree
            end
        when :and
            [:and, apply_demorgans(tree[1]), apply_demorgans(tree[2])]
        when :or
            [:or, apply_demorgans(tree[1]), apply_demorgans(tree[2])]
        else
          tree
        end
      end

      def demorganize(tree)
        result = apply_demorgans(tree)
        result.kind_of?(Array)? result : [result]
      end

      def sql_escape(s)
        s.gsub('%', '\%').gsub('_', '\_')
      end

      def compound_tc(attributes, tree)
        '(' +
          build_tc_from_tree(attributes, tree[1]) +
          ' ' + tree[0].to_s + ' ' +
          build_tc_from_tree(attributes, tree[2]) +
          ')'
      end

      def build_tc_from_tree(attributes, tree)
        token = tree.kind_of?(Array)? tree[0] : tree
        case token
        when :and
            compound_tc(attributes, tree)
        when :or
            compound_tc(attributes, tree)
        when :not
            # assert tree[1].kind_of?(String)
          "(" +
          attributes.map { |f|
            "(#{f} is null or #{f} not like #{sanitize('%%'+sql_escape(tree[1])+'%%')})"
          }.join(" and ") +
            ")"
        else
          "(" +
          attributes.map { |f|
            "#{f} like #{sanitize('%%'+sql_escape(token)+'%%')}"
          }.join(" or ") +
            ")"
        end
      end

      def build_text_condition(attributes, text)
        build_tc_from_tree(attributes, demorganize(parse(text)))
      end
    end
  end
end

ActiveRecord::Base.send(:include, SearchableRecord::Base) if defined?(ActiveRecord::Base)