module SearchableRecord
  def self.included(base)
    base.extend(MacroMethods)              
  end
  
  module MacroMethods
    def searches_on(*args)
      write_inheritable_attribute(:local_searchable_fields, args.collect { |f| f.to_s }) if args.any? && args.first != :all

      unless self.is_a? SearchableRecord::ClassMethods
        extend SearchableRecord::ClassMethods
        class_eval do
          include SearchableRecord::InstanceMethods
        end
      end
    end
  end
  
  module ClassMethods
    def local_searchable_fields
      read_inheritable_attribute(:local_searchable_fields)
    end

    def searchable_fields(tables = nil)
      fields = read_inheritable_attribute(:local_searchable_fields)
      if fields.nil?
        fields = []
        string_columns = self.columns.select { |c| c.type == :text or c.type == :string }
        fields = string_columns.collect { |c| c.name }
      end
      fields = fields.collect { |field| self.table_name + "." + field }
      
      tables = tables ? [ tables ].flatten : []
      fields += searchable_fields_in_associations(tables)

      return fields
    end

    def searchable_fields_in_associations(tables)
      fields = []

      if not tables.empty?
        tables.each do |table, assocs|
          if table.kind_of? Hash
            fields += searchable_fields_in_associations(table)
          else
            klass = eval table.to_s.classify
            fields += klass.searchable_fields(assocs)
          end
        end
      end

      return fields
    end

    def search(text = nil, options = {})
      search_includes = options[:search_include] ? [ options[:search_include] ].flatten : []

      fields = searchable_fields(search_includes)
      fields &= options[:only] if options[:only]
      fields -= options[:except] if options[:except]

      unless options[:case_sensitive]
        text.downcase!
        fields.map! { |field| "lower(#{field})" }
      end

      condition_list = []
      condition_list << build_text_condition(fields, text) unless text.blank?
      conditions_list << options[:conditions] if options[:conditions] && options[:conditions].is_a?(String)
      conditions_list << options[:conditions].first if options[:conditions].is_a?(Array)
      conditions = condition_list.join(" AND ")
      conditions = options[:conditions][ 1, options[:conditions].length ].unshift(conditions) if options[:conditions].is_a?(Array)

      includes = search_includes.dup
      includes << options[:include] if options[:include]
      includes.flatten.uniq

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

    def compound_tc(fields, tree)
      '(' +
        build_tc_from_tree(fields, tree[1]) +
        ' ' + tree[0].to_s + ' ' +
        build_tc_from_tree(fields, tree[2]) +
        ')'
    end

    def build_tc_from_tree(fields, tree)
      token = tree.kind_of?(Array)? tree[0] : tree
      case token
      when :and
          compound_tc(fields, tree)
      when :or
          compound_tc(fields, tree)
      when :not
          # assert tree[1].kind_of?(String)
        "(" +
        fields.map { |f|
          "(#{f} is null or #{f} not like #{sanitize('%'+sql_escape(tree[1])+'%')})" 
        }.join(" and ") +
          ")" 
      else
        "(" +
        fields.map { |f|
          "#{f} like #{sanitize('%'+sql_escape(token)+'%')}" 
        }.join(" or ") +
          ")" 
      end
    end

    def build_text_condition(fields, text)
      build_tc_from_tree(fields, demorganize(parse(text)))
    end
  end

  module InstanceMethods
  end
end

ActiveRecord::Base.send(:include, SearchableRecord)