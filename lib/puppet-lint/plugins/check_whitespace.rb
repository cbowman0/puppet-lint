# Public: Check the raw manifest string for lines containing hard tab
# characters and record an error for each instance found.
PuppetLint.new_check(:hard_tabs) do
  WHITESPACE_TYPES = Set[:INDENT, :WHITESPACE]

  def check
    tokens.select { |r|
      WHITESPACE_TYPES.include?(r.type) && r.value.include?("\t")
    }.each do |token|
      notify :error, {
        :message => 'tab character found',
        :line    => token.line,
        :column  => token.column,
        :token   => token,
      }
    end
  end

  def fix(problem)
    problem[:token].value.gsub!("\t", '  ')
  end
end

# Public: Check the manifest tokens for lines ending with whitespace and record
# an error for each instance found.
PuppetLint.new_check(:trailing_whitespace) do
  def check
    tokens.select { |token|
      [:WHITESPACE, :INDENT].include?(token.type)
    }.select { |token|
      token.next_token.nil? || token.next_token.type == :NEWLINE
    }.each do |token|
      notify :error, {
        :message => 'trailing whitespace found',
        :line    => token.line,
        :column  => token.column,
        :token   => token,
      }
    end
  end

  def fix(problem)
    prev_token = problem[:token].prev_token
    next_token = problem[:token].next_token
    prev_token.next_token = next_token
    next_token.prev_token = prev_token unless next_token.nil?
    tokens.delete(problem[:token])
  end
end

# Public: Test the raw manifest string for lines containing more than 80
# characters and record a warning for each instance found.  The only exceptions
# to this rule are lines containing URLs and template() calls which would hurt
# readability if split.
PuppetLint.new_check(:'80chars') do
  def check
    manifest_lines.each_with_index do |line, idx|
      unless line =~ /:\/\// || line =~ /template\(/
        if line.scan(/./mu).size > 80
          notify :warning, {
            :message => 'line has more than 80 characters',
            :line    => idx + 1,
            :column  => 80,
          }
        end
      end
    end
  end
end

# Public: Check the manifest tokens for any indentation not using 2 space soft
# tabs and record an error for each instance found.
PuppetLint.new_check(:'2sp_soft_tabs') do
  def check
    tokens.select { |r|
      r.type == :INDENT
    }.reject { |r|
      r.value.length % 2 == 0
    }.each do |token|
      notify :error, {
        :message => 'two-space soft tabs not used',
        :line    => token.line,
        :column  => token.column,
      }
    end
  end
end

# Public: Check the manifest tokens for indentation level
PuppetLint.new_check(:'indent_level') do
  def check
    line_lex = []
    indent_levels = []
    indent_levels << Hash["indent_level" => 0, "row_type" => "START"]
    indent_size = 2
    indent_level = 0

    tokens.each do |token|
      line_lex << token
      if [:NEWLINE].include? token.type # if newline

        # Line only contains a NEWLINE.  Skip it.
        if line_lex.length == 1
          line_lex.clear
          next
        end

        # Identify current indent token and length
        # row_type is first non-indent token
        current_indent = 0
        indent_token_idx = line_lex.index { |t| t.type == :INDENT }
        row_type = line_lex[0].type.to_s
        if !indent_token_idx.nil?
          indent_token = line_lex[indent_token_idx]
          current_indent = indent_token.value.length
          row_type = line_lex[indent_token_idx+1].type.to_s
        end

        # Find the token before the newline(s)
        token_before_newline_idx = line_lex.rindex { |t| t.type != :NEWLINE }
        if !token_before_newline_idx.nil?
          token_before_newline = line_lex[token_before_newline_idx]
        end

        current_indent_level = indent_levels.last
        indent_level = current_indent_level["indent_level"]

        if ["CLASS", "NAME", "CASE"].include? row_type  
          if line_lex.index{|x| x.type == :FARROW}.nil?
            indent_levels << Hash["indent_level" => indent_level, "row_type" => row_type]
          end
        end

        # Sort out number of indents and push/pop appropriately
        #  May need to treat PARENs and BRACEs separately
        num_open = line_lex.count { |t| [:LBRACE, :LPAREN].include? t.type }
        num_close = line_lex.count { |t| [:RBRACE, :RPAREN].include? t.type }
        if num_open > num_close
          for i in 1..(num_open-num_close)
            last_indent_level = indent_levels.last["indent_level"]
            indent_levels << Hash["indent_level" => last_indent_level+indent_size, "row_type" => "LBRACE"]
          end
        elsif num_close > num_open
          num = (num_close-num_open)
          i = 1
          until i > num do
            l = indent_levels.pop
            num -= 1 if l["row_type"] == "LBRACE"
          end
          current_indent_level = indent_levels.last
          indent_level = current_indent_level["indent_level"]
        elsif ["RBRACE", "RPAREN"].include? row_type
          until ["LBRACE", "LPAREN"].include? indent_levels.last["row_type"]
            indent_levels.pop
          end
          current_indent_level = indent_levels.last(2)[0]
          indent_level = current_indent_level["indent_level"]
        end

        if [:COLON].include? token_before_newline.type
          last_indent_level = indent_levels.last["indent_level"]
          indent_levels << Hash["indent_level" => last_indent_level+indent_size, "row_type" => "COLON"]
        end
        if [:SEMIC].include? token_before_newline.type
            l = indent_levels.pop
            # Semicolon cannot end a {} block
            if l["row_type"] == "LBRACE"
              indent_levels << l
            end
        end

        # Error condition
        if current_indent != indent_level
          bad_token = line_lex[0]
          notify :warning, {
            :message => "indent level incorrect.  Expected " + indent_level.to_s,
            :line    => bad_token.line,
            :column  => bad_token.column,
            :token   => bad_token,
            :indent_depth => indent_level,
          }
        end

        # Done with this line
        line_lex.clear
      end
    end
  end

  def fix(problem)
    new_ws_len = problem[:indent_depth]
    new_ws = ' ' * new_ws_len
    if problem[:token].type == :INDENT
      problem[:token].value = new_ws
    else
      index = tokens.index(problem[:token].prev_token)
      tokens.insert(index + 1, PuppetLint::Lexer::Token.new(:INDENT, new_ws, 0, 0))
    end
  end
end

# Public: Check the manifest tokens for any arrows (=>) in a grouping ({}) that
# are not aligned with other arrows in that grouping.
PuppetLint.new_check(:arrow_alignment) do
  COMMENT_TYPES = Set[:COMMENT, :SLASH_COMMENT, :MLCOMMENT]

  def check
    resource_indexes.each do |res_idx|
      indent_depth = [0]
      indent_depth_idx = 0
      level_tokens = []
      resource_tokens = res_idx[:tokens]
      resource_tokens.reject! do |token|
        COMMENT_TYPES.include? token.type
      end

      # If this is a single line resource, skip it
      first_arrow = resource_tokens.index { |r| r.type == :FARROW }
      last_arrow = resource_tokens.rindex { |r| r.type == :FARROW }
      next if first_arrow.nil?
      next if last_arrow.nil?
      next unless resource_tokens[first_arrow..last_arrow].any? { |r| r.type == :NEWLINE }

      resource_tokens.each_with_index do |token, idx|
        if token.type == :FARROW
          (level_tokens[indent_depth_idx] ||= []) << token
          prev_indent_token = resource_tokens[0..idx].rindex { |t| t.type == :INDENT }
          indent_token_length = prev_indent_token.nil? ? 0 : resource_tokens[prev_indent_token].to_manifest.length
          indent_length = indent_token_length + token.prev_code_token.to_manifest.length + 2

          if indent_depth[indent_depth_idx] < indent_length
            indent_depth[indent_depth_idx] = indent_length
          end

        elsif token.type == :LBRACE
          indent_depth_idx += 1
          indent_depth << 0
          level_tokens[indent_depth_idx] ||= []
        elsif token.type == :RBRACE
          level_tokens[indent_depth_idx].each do |arrow_tok|
            unless arrow_tok.column == indent_depth[indent_depth_idx] || level_tokens[indent_depth_idx].size == 1
              arrows_on_line = level_tokens[indent_depth_idx].select { |t| t.line == arrow_tok.line }
              notify :warning, {
                :message        => 'indentation of => is not properly aligned',
                :line           => arrow_tok.line,
                :column         => arrow_tok.column,
                :token          => arrow_tok,
                :indent_depth   => indent_depth[indent_depth_idx],
                :newline        => !(arrows_on_line.index(arrow_tok) == 0),
                :newline_indent => arrows_on_line.first.prev_code_token.prev_token.value,
              }
            end
          end
          indent_depth[indent_depth_idx] = 0
          level_tokens[indent_depth_idx].clear
          indent_depth_idx -= 1
        end
      end
    end
  end

  def fix(problem)
    new_ws_len = (problem[:indent_depth] - (problem[:newline_indent].length + problem[:token].prev_code_token.to_manifest.length + 1))
    new_ws = ' ' * new_ws_len
    if problem[:newline]
      index = tokens.index(problem[:token].prev_code_token.prev_token)

      #insert newline
      tokens.insert(index, PuppetLint::Lexer::Token.new(:NEWLINE, "\n", 0, 0))

      # indent the parameter to the correct depth
      problem[:token].prev_code_token.prev_token.type = :INDENT
      problem[:token].prev_code_token.prev_token.value = problem[:newline_indent].dup
    end

    if problem[:token].prev_token.type == :WHITESPACE
      problem[:token].prev_token.value = new_ws
    else
      index = tokens.index(problem[:token].prev_token)
      tokens.insert(index + 1, PuppetLint::Lexer::Token.new(:WHITESPACE, new_ws, 0, 0))
    end
  end
end
