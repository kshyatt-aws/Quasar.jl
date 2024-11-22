parse_hw_qubit(token, qasm)   = QasmExpression(:hw_qubit, read_raw(token, qasm))
parse_identifier(token, qasm) = QasmExpression(:identifier, String(read_raw(token, qasm)))

function parse_block_body(expr, tokens, stack, start, qasm)
    is_scope = tokens[1][end] == lbrace
    if is_scope
        body       = parse_scope(tokens, stack, start, qasm)
        body_exprs = convert(Vector{QasmExpression}, collect(Iterators.reverse(body)))::Vector{QasmExpression}
        foreach(body_expr->push!(body_exprs[1], body_expr), body_exprs[2:end])
        push!(expr, body_exprs[1])
    else # one line
        eol  = findfirst(triplet->triplet[end] == semicolon, tokens)
        body = parse_expression(splice!(tokens, 1:eol), stack, start, qasm)
        push!(expr, body)
    end
    return expr
end

function parse_switch_block(tokens, stack, start, qasm)
    cond_tokens     = extract_expression(tokens, lparen, rparen, stack, start, qasm)
    expr            = QasmExpression(:switch, parse_expression(cond_tokens, stack, start, qasm))
    interior_tokens = extract_expression(tokens, lbrace, rbrace, stack, start, qasm)
    met_default     = false
    while !isempty(interior_tokens)
        next_token = popfirst!(interior_tokens)
        if next_token[end] == case
            !met_default || throw(QasmParseError("case statement cannot occur after default in switch block.", stack, start, qasm))
            brace_loc  = findfirst(triplet->triplet[end] == lbrace, interior_tokens)
            isnothing(brace_loc) && throw(QasmParseError("case statement missing opening {", stack, start, qasm))
            val_tokens = splice!(interior_tokens, 1:brace_loc-1)
            push!(val_tokens, (-1, Int32(-1), semicolon))
            case_expr  = QasmExpression(:case, parse_list_expression(val_tokens, stack, start, qasm))
            case_expr  = parse_block_body(case_expr, interior_tokens, stack, start, qasm)
            push!(expr, case_expr)
        elseif next_token[end] == default
            !met_default || throw(QasmParseError("only one default statement allowed in switch block.", stack, start, qasm))
            default_expr = parse_block_body(QasmExpression(:default), interior_tokens, stack, start, qasm)
            push!(expr, default_expr)
            met_default  = true
        elseif next_token[end] ∈ (newline, semicolon)
            continue
        else
            throw(QasmParseError("invalid switch-case statement.", stack, start, qasm))
        end
    end
    return expr
end

function parse_if_block(tokens, stack, start, qasm)
    condition_tokens = extract_expression(tokens, lparen, rparen, stack, start, qasm)
    condition_value  = parse_expression(condition_tokens, stack, start, qasm)
    if_expr  = QasmExpression(:if, condition_value)
    # handle condition
    if_expr  = parse_block_body(if_expr, tokens, stack, start, qasm)
    has_else = tokens[1][end] == else_block 
    if has_else
        popfirst!(tokens) # else
        else_expr = parse_block_body(QasmExpression(:else), tokens, stack, start, qasm)
        push!(if_expr, else_expr)
    end
    return if_expr
end
function parse_while_loop(tokens, stack, start, qasm)
    condition_tokens = extract_expression(tokens, lparen, rparen, stack, start, qasm)
    condition_value  = parse_expression(condition_tokens, stack, start, qasm)
    while_expr       = QasmExpression(:while, condition_value)
    return parse_block_body(while_expr, tokens, stack, start, qasm)
end
function parse_for_loop(tokens, loop_var_type, loop_var_name, loop_values, stack, start, qasm)
    for_expr = QasmExpression(:for, loop_var_type, loop_var_name, loop_values)
    return parse_block_body(for_expr, tokens, stack, start, qasm)
end
function parse_arguments_list(tokens, stack, start, qasm)
    first(tokens)[end] != lparen && return QasmExpression(:arguments)
    interior = extract_expression(tokens, lparen, rparen, stack, start, qasm)
    return QasmExpression(:arguments, parse_list_expression(interior, stack, start, qasm))
end
function parse_function_def(tokens, stack, start, qasm)
    function_name    = popfirst!(tokens)
    function_name[end] == identifier || throw(QasmParseError("function definition must have a valid identifier as a name", stack, start, qasm))
    function_name_id = parse_identifier(function_name, qasm) 
    arguments        = parse_arguments_list(tokens, stack, start, qasm)
    next_token_type  = first(tokens)[end]
    return_type      = QasmExpression(:void) # default
    has_return_type  = next_token_type == arrow_token
    if has_return_type
        arrow = popfirst!(tokens)
        next_token_type  = first(tokens)[end]
        if next_token_type == classical_type
            return_type = parse_classical_type(tokens, stack, start, qasm)
        elseif next_token_type == waveform_token
            popfirst!(tokens)
            return_type = QasmExpression(:waveform)
        else
            throw(QasmParseError("function return type must be a classical type or waveform", stack, start, qasm))
        end
    end
    expr = QasmExpression(:function_definition, function_name_id, arguments, return_type)
    if !isempty(tokens)
        expr = parse_block_body(expr, tokens, stack, start, qasm)
    else
        push!(expr, QasmExpression(:extern))
    end
    return expr
end
function parse_gate_or_cal_def(head::Symbol, tokens, stack, start, qasm)
    def_name    = popfirst!(tokens)
    def_name[end] == identifier || throw(QasmParseError("$head must have a valid identifier as a name", stack, start, qasm))
    def_name_id = parse_identifier(def_name, qasm)

    def_args     = parse_arguments_list(tokens, stack, start, qasm)
    qubit_tokens = splice!(tokens, 1:findfirst(triplet->triplet[end]==lbrace, tokens)-1)
    push!(qubit_tokens, (-1, Int32(-1), semicolon))
    target_expr  = QasmExpression(:qubit_targets, parse_list_expression(qubit_tokens, stack, start, qasm))
    expr         = QasmExpression(head, def_name_id, def_args, target_expr)
    return parse_block_body(expr, tokens, stack, start, qasm)
end

function parse_classical_type(tokens, stack, start, qasm)
    is_sized  = length(tokens) > 1 && tokens[2][end] == lbracket
    type_name = popfirst!(tokens)
    type_name[end] == classical_type || throw(QasmParseError("classical variable must have a classical type", stack, start, qasm))
    var_type  = read_raw(type_name, qasm)
    if var_type == "complex"
        complex_tokens = extract_expression(tokens, lbracket, rbracket, stack, start, qasm)
        eltype = parse_classical_type(complex_tokens, stack, start, qasm)
        size   = eltype.args[1].size
    elseif var_type == "array"
        array_tokens = extract_expression(tokens, lbracket, rbracket, stack, start, qasm)
        eltype = parse_classical_type(array_tokens, stack, start, qasm)
        first(array_tokens)[end] == comma && popfirst!(array_tokens)
        size   = parse_expression(array_tokens, stack, start, qasm)
        return QasmExpression(:classical_type, SizedArray(eltype, size))
    elseif var_type == "duration"
        @warn "duration expression encountered -- currently `duration` is a no-op"
        # TODO: add proper parsing of duration expressions, including
        # support for units and algebraic durations like 2*a.
        return QasmExpression(:classical_type, :duration)
    elseif var_type == "stretch"
        @warn "stretch expression encountered -- currently `stretch` is a no-op"
        # TODO: add proper parsing of stretch expressions
        return QasmExpression(:classical_type, :stretch)
    else
        !any(triplet->triplet[end] == semicolon, tokens) && push!(tokens, (-1, Int32(-1), semicolon))
        size = is_sized ? parse_expression(tokens, stack, start, qasm) : QasmExpression(:integer_literal, -1)
    end
    var_type == "bit"     && return QasmExpression(:classical_type, SizedBitVector(size))
    var_type == "int"     && return QasmExpression(:classical_type, SizedInt(size))
    var_type == "uint"    && return QasmExpression(:classical_type, SizedUInt(size))
    var_type == "float"   && return QasmExpression(:classical_type, SizedFloat(size))
    var_type == "angle"   && return QasmExpression(:classical_type, SizedAngle(size))
    var_type == "complex" && return QasmExpression(:classical_type, SizedComplex(size))
    var_type == "bool"    && return QasmExpression(:classical_type, Bool)
    throw(QasmParseError("could not parse classical type", stack, start, qasm))
end

function parse_classical_var(tokens, stack, start, qasm)
    # detect if we have a declared size
    tokens[end][end] == semicolon && pop!(tokens)
    name = pop!(tokens)
    name[end] == identifier || throw(QasmParseError("classical variable must have a valid name", stack, start, qasm))
    return parse_classical_type(tokens, stack, start, qasm), parse_identifier(name, qasm)
end

const binary_assignment_ops = Dict{String, Symbol}(
                                                   "="=>Symbol("="),
                                                   "-="=>Symbol("-"),
                                                   "+="=>Symbol("+"),
                                                   "*="=>Symbol("*"),
                                                   "/="=>Symbol("/"),
                                                   "^="=>Symbol("^"),
                                                   "&="=>Symbol("&"),
                                                   "|="=>Symbol("|"),
                                                   "<<="=>Symbol("<<"),
                                                   ">>="=>Symbol(">>"),
                                                  )

read_raw(token, qasm) = String(codeunits(qasm)[token[1]:token[1]+token[2]-1])
parse_assignment_op(op_token, qasm) = binary_assignment_ops[String(read_raw(op_token, qasm))]

parse_string_literal(token, qasm)  = QasmExpression(:string_literal,  read_raw(token, qasm))
parse_integer_literal(token, qasm) = QasmExpression(:integer_literal, tryparse(Int, read_raw(token, qasm)))
parse_hex_literal(token, qasm)     = QasmExpression(:integer_literal, tryparse(UInt, read_raw(token, qasm)))
parse_float_literal(token, qasm)   = QasmExpression(:float_literal,   tryparse(Float64, read_raw(token, qasm)))
parse_boolean_literal(token, qasm) = QasmExpression(:boolean_literal, tryparse(Bool, read_raw(token, qasm)))
function parse_duration_literal(token, qasm)
    str = String(read_raw(token, qasm))
    duration = if endswith(str, "ns")
            Nanosecond(tryparse(Int, chop(str, tail=2)))
        elseif endswith(str, "ms")
            Millisecond(tryparse(Int, chop(str, tail=2)))
        elseif endswith(str, "us") || endswith(str, "μs")
            Microsecond(tryparse(Int, chop(str, tail=2)))
        elseif endswith(str, "s")
            Second(tryparse(Int, chop(str, tail=1)))
        elseif endswith(str, "dt")
            dt_type[](tryparse(Int, chop(str, tail=2)))
        end
    QasmExpression(:duration_literal, duration)
end
function parse_irrational_literal(token, qasm)
    raw_string = String(read_raw(token, qasm))
    raw_string ∈ ("π", "pi")  && return QasmExpression(:irrational_literal, π)
    raw_string ∈ ("τ", "tau") && return QasmExpression(:irrational_literal, 2*π)
    raw_string ∈ ("ℯ", "ℇ", "euler") && return QasmExpression(:irrational_literal, ℯ)
end
function extract_expression(tokens::Vector{Tuple{Int64, Int32, Token}}, opener, closer, stack, start, qasm)
    opening = findfirst(triplet->triplet[end] == opener, tokens)
    isnothing(opening) && return tokens
    popat!(tokens, opening)
    openers_met  = 1
    closers_met  = 0
    extracted_tokens = Tuple{Int64, Int32, Token}[]
    while closers_met < openers_met && !isempty(tokens)
        next_token      = popfirst!(tokens)
        next_token[end] == opener && (openers_met += 1)
        next_token[end] == closer && (closers_met += 1)
        push!(extracted_tokens, next_token)
    end
    pop!(extracted_tokens) # final closer
    push!(extracted_tokens, (-1, Int32(-1), semicolon))
    return extracted_tokens
end

function parse_scope(tokens, stack, start, qasm)
    scope_tokens = extract_expression(tokens, lbrace, rbrace, stack, start, qasm)
    return parse_qasm(scope_tokens, qasm, QasmExpression(:scope))
end

function parse_list_expression(tokens::Vector{Tuple{Int64, Int32, Token}}, stack, start, qasm)
    expr_list = QasmExpression[]
    while !isempty(tokens) && first(tokens)[end] != semicolon
        tokens[1][end] == comma && popfirst!(tokens)
        next_expr = parse_expression(tokens, stack, start, qasm)
        push!(expr_list, next_expr)
    end
    return length(expr_list) == 1 ? only(expr_list) : QasmExpression(:array_literal, expr_list)
end

function parse_literal(tokens::Vector{Tuple{Int64, Int32, Token}}, stack, start, qasm)
    tokens[1][end] == duration_literal && return parse_duration_literal(popfirst!(tokens), qasm)
    tokens[1][end] == string_token     && return parse_string_literal(popfirst!(tokens), qasm)
    tokens[1][end] == hex              && return parse_hex_literal(popfirst!(tokens), qasm)
    tokens[1][end] == oct              && return parse_integer_literal(popfirst!(tokens), qasm)
    tokens[1][end] == bin              && return parse_integer_literal(popfirst!(tokens), qasm)
    tokens[1][end] == irrational       && return parse_irrational_literal(popfirst!(tokens), qasm)
    tokens[1][end] == boolean          && return parse_boolean_literal(popfirst!(tokens), qasm)
    tokens[1][end] == integer_token    && length(tokens) == 1 && return parse_integer_literal(popfirst!(tokens), qasm)
    tokens[1][end] == float_token      && length(tokens) == 1 && return parse_float_literal(popfirst!(tokens), qasm)
    
    is_float     = tokens[1][end] == float_token
    is_complex   = false
    is_operator  = tokens[2][end] == operator
    is_plusminus = is_operator && parse_identifier(tokens[2], qasm).args[1] ∈ ("+","-")
    is_terminal  = (tokens[2][end] == semicolon || tokens[2][end] == comma || (is_operator && !is_plusminus))
    tokens[1][end] == integer_token && is_terminal && return parse_integer_literal(popfirst!(tokens), qasm)
    tokens[1][end] == float_token   && is_terminal && return parse_float_literal(popfirst!(tokens), qasm)
    splice_end = 1
    if tokens[2][end] == im_token
        is_complex = true
        splice_end = 2
    elseif is_plusminus && tokens[3][end] ∈ (integer_token, float_token) && tokens[4][end] == im_token
        is_complex = true
        is_float |= tokens[3][end] == float_token
        splice_end = 4
    elseif tokens[2][end] ∈ (integer_token, float_token) && tokens[3][end] == im_token # may have absorbed +/- sign
        is_complex = true
        is_float |= tokens[2][end] == float_token
        splice_end = 3
    end
    literal_tokens = splice!(tokens, 1:splice_end)
    raw_literal_string = qasm[literal_tokens[1][1]:literal_tokens[end][1]+literal_tokens[end][2]-1]
    raw_literal = if is_float
            parse(ComplexF64, raw_literal_string)
        elseif is_complex
            parse(Complex{Int}, raw_literal_string)
        else
            parse(Int, raw_literal_string)
        end
    if is_complex # complex float
        return QasmExpression(:complex_literal, raw_literal)
    elseif is_float
        return QasmExpression(:float_literal, raw_literal)
    else
        return QasmExpression(:integer_literal, raw_literal)
    end
    throw(QasmParseError("unable to parse literal", stack, start, qasm)) 
end

function parse_qubit_declaration(tokens::Vector{Tuple{Int64, Int32, Token}}, stack, start, qasm)
    size = QasmExpression(:integer_literal, 1)
    next_token = tokens[1]
    if next_token[end] == lbracket
        size_tokens = extract_expression(tokens, lbracket, rbracket, stack, start, qasm)
        size        = parse_expression(size_tokens, stack, start, qasm)
    end
    qubit_name = parse_identifier(popfirst!(tokens), qasm)
    size.args[1] == -1 && (size.args[1] = 1)
    return QasmExpression(:qubit_declaration, qubit_name, size)
end

function parse_gate_mods(tokens::Vector{Tuple{Int64, Int32, Token}}, stack, start, qasm)
    mod_type = popfirst!(tokens)
    expr = if mod_type[end] == control_mod
               QasmExpression(:control_mod)
           elseif mod_type[end] == negctrl_mod
               QasmExpression(:negctrl_mod)
           elseif mod_type[end] == inverse_mod
               QasmExpression(:inverse_mod)
           elseif mod_type[end] == power_mod
               QasmExpression(:power_mod)
           else
               throw(QasmParseError("cannot parse token of type $(mod_type[end]) as a gate modifier", stack, start, qasm))
           end
    next_token = first(tokens)
    if next_token[end] == lparen
        interior_tokens = extract_expression(tokens, lparen, rparen, stack, start, qasm)
        arg             = parse_list_expression(interior_tokens, stack, start, qasm)
        push!(expr, arg)
        next_token      = first(tokens)
    end
    if next_token[end] == at
        popfirst!(tokens)
        next_token = first(tokens)
        if next_token[end] == identifier || next_token[end] == builtin_gate
            push!(expr, parse_expression(tokens, stack, start, qasm))
        else
            push!(expr, parse_gate_mods(tokens, stack, start, qasm))
        end
        return expr
    end
end

function _op_precedence(op::Symbol)::Int
    op == Symbol("**") && return 0
    op ∈ (:!, :~)      && return 1
    op ∈ (:*, :/, :%)  && return 2
    op ∈ (:+, :-)      && return 3
    op ∈ (Symbol("<<"), Symbol(">>")) && return 4
    op ∈ (:<, Symbol("<="), :>, Symbol(">=")) && return 5
    op ∈ (Symbol("!="), Symbol("==")) && return 6
    op == :& && return 7
    op == :^ && return 8
    op == :| && return 9
    op == Symbol("&&") && return 10
    op == Symbol("||") && return 11
end
has_precedence(op_a::Symbol, op_b::Symbol) = _op_precedence(op_a) <= _op_precedence(op_b)

function expression_start(tokens, stack, start, qasm)
    start_token = popfirst!(tokens)
    next_token  = first(tokens)
    expr_head   = QasmExpression(:empty)
    if start_token[end] != classical_type && next_token[end] == lbracket
        name      = parse_identifier(start_token, qasm)
        indices   = parse_expression(tokens, stack, start, qasm)
        expr_ixs  = length(indices) == 1 ? only(indices) : indices
        expr_head = QasmExpression(:indexed_identifier, name, expr_ixs)
    elseif start_token[end] == identifier || start_token[end] == builtin_gate
        expr_head = parse_identifier(start_token, qasm)
    elseif start_token[end] == hw_qubit
        expr_head = parse_hw_qubit(start_token, qasm)
    elseif start_token[end] == qubit
        expr_head = parse_qubit_declaration(tokens, stack, start, qasm)
    elseif start_token[end] == operator
        expr_head = parse_identifier(start_token, qasm)
    elseif start_token[end] == break_token
        expr_head = QasmExpression(:break)
    elseif start_token[end] == continue_token
        expr_head = QasmExpression(:continue)
    elseif start_token[end] ∈ (lparen, lbrace, lbracket) # could be some kind of expression
        interior_tokens = extract_expression(pushfirst!(tokens, start_token), start_token[end], closing_token(start_token[end]), stack, start, qasm)
        expr_head = parse_list_expression(interior_tokens, stack, start, qasm)
    elseif start_token[end] == classical_type 
        expr_head = parse_classical_type(pushfirst!(tokens, start_token), stack, start, qasm)
    elseif start_token[end] == waveform_token
        expr_head = QasmExpression(:waveform)
    elseif start_token[end] == frame_token
        expr_head = QasmExpression(:frame)
    elseif start_token[end] ∈ (string_token, integer_token, float_token, hex, oct, bin, irrational, dot, boolean, duration_literal)
        expr_head = parse_literal(pushfirst!(tokens, start_token), stack, start, qasm)
    elseif start_token[end] ∈ (mutable, readonly, const_token)
        expr_head = parse_identifier(start_token, qasm)
    elseif start_token[end] == dim_token
        raw_dim   = read_raw(start_token, qasm)
        dim       = replace(replace(raw_dim, " "=>""), "#dim="=>"")
        expr_head = QasmExpression(:n_dims, QasmExpression(:integer_literal, parse(Int, dim)))
    end
    return expr_head, start_token
end

function parse_range(expr_head, tokens, stack, start, qasm)
    popfirst!(tokens)
    second_colon = findfirst(triplet->triplet[end] == colon, tokens)
    if !isnothing(second_colon)
        step_tokens = push!(splice!(tokens, 1:second_colon-1), (-1, Int32(-1), semicolon))
        popfirst!(tokens) # colon
        step = parse_expression(step_tokens, stack, start, qasm)::QasmExpression
    else
        step = QasmExpression(:integer_literal, 1)
    end
    if isempty(tokens) || first(tokens)[end] == semicolon # missing stop
        stop = QasmExpression(:integer_literal, -1)
    else
        stop = parse_expression(tokens, stack, start, qasm)::QasmExpression
    end
    return QasmExpression(:range, QasmExpression[expr_head, step, stop])
end

function parse_classical_assignment(expr_head, tokens, stack, start, qasm)
    op_token   = popfirst!(tokens)
    next_token = first(tokens)
    if next_token[end] ∈ (lparen, lbracket, lbrace, string_token, integer_token, float_token, hex, oct, bin)
        right_hand_side       = parse_expression(tokens, stack, start, qasm)
    elseif next_token[end] == measure
        popfirst!(tokens)
        right_hand_side       = QasmExpression(:measure, parse_expression(tokens, stack, start, qasm))
    elseif next_token[end] == operator
        unary_op_token        = parse_identifier(popfirst!(tokens), qasm)
        next_token            = first(tokens)
        unary_right_hand_side = parse_expression(tokens, stack, start, qasm)
        unary_op              = Symbol(unary_op_token.args[1]::String)
        right_hand_side       = QasmExpression(:unary_op, unary_op, unary_right_hand_side)
    else
        right_hand_side       = parse_expression(tokens, stack, start, qasm)::QasmExpression
    end
    op_expr = QasmExpression(:binary_op, parse_assignment_op(op_token, qasm), expr_head, right_hand_side) 
    return QasmExpression(:classical_assignment, op_expr)
end

function parse_binary_op(left_hand_side, tokens, stack, start, qasm)
    op_token        = parse_identifier(popfirst!(tokens), qasm)
    right_hand_head = first(tokens)[end]
    right_hand_side = parse_expression(tokens, stack, start, qasm)::QasmExpression
    op_symbol       = Symbol(op_token.args[1])
    raw_expr        = QasmExpression(:binary_op, op_symbol, left_hand_side, right_hand_side)
    if head(right_hand_side) == :binary_op && has_precedence(op_symbol, right_hand_side.args[1]) && right_hand_head != lparen # parens pre-prempt precedence 
        rhs_op   = right_hand_side.args[1]
        new_lhs  = QasmExpression(:binary_op, op_symbol, left_hand_side, right_hand_side.args[2])
        new_rhs  = right_hand_side.args[end]
        raw_expr = QasmExpression(:binary_op, rhs_op, new_lhs, new_rhs)
    end
    if !isempty(tokens) && first(tokens)[end] == im_token
        popfirst!(tokens)
        return QasmExpression(:binary_op, :*, raw_expr, QasmExpression(:complex_literal, im))
    else
        return raw_expr
    end
end

function parse_unary_op(expr_head, tokens, stack, start, qasm)
    unary_op_symbol::Symbol = Symbol(expr_head.args[1]::String)
    unary_op_symbol ∈ (:~, :!, :-) || throw(QasmParseError("invalid unary operator $unary_op_symbol.", stack, start, qasm))
    next_expr = parse_expression(tokens, stack, start, qasm)
    # apply unary op to next_expr
    if head(next_expr) ∈ (:identifier, :indexed_identifier, :integer_literal, :float_literal, :string_literal, :irrational_literal, :boolean_literal, :complex_literal, :function_call, :cast, :duration_literal)
        expr = QasmExpression(:unary_op, unary_op_symbol, next_expr)
    elseif head(next_expr) == :binary_op
        # replace first argument
        left_hand_side     = next_expr.args[2]::QasmExpression
        new_left_hand_side = QasmExpression(:unary_op, unary_op_symbol, left_hand_side)
        next_expr.args[2]  = new_left_hand_side
        expr               = next_expr
    end
    return expr
end

function parse_function_expr(expr_head, arguments, tokens, stack, start, qasm)
    raw_expr   = QasmExpression(:function_call, expr_head, arguments)
    if !isempty(tokens) && first(tokens)[end] == operator
        next_op_token   = parse_identifier(popfirst!(tokens), qasm)
        right_hand_side = parse_expression(tokens, stack, start, qasm)::QasmExpression
        return QasmExpression(:binary_op, Symbol(next_op_token.args[1]), raw_expr, right_hand_side)
    else
        return raw_expr
    end
end

function parse_cast_expr(expr_head, tokens, stack, start, qasm)
    interior = extract_expression(tokens, lparen, rparen, stack, start, qasm)
    raw_expr = QasmExpression(:cast, expr_head, parse_expression(interior, stack, start, qasm))
    if !isempty(tokens) && first(tokens)[end] == operator
        next_op_token   = parse_identifier(popfirst!(tokens), qasm)
        right_hand_side = parse_expression(tokens, stack, start, qasm)::QasmExpression
        return QasmExpression(:binary_op, Symbol(next_op_token.args[1]), raw_expr, right_hand_side)
    else
        return raw_expr
    end
end

function parse_expression(tokens::Vector{Tuple{Int64, Int32, Token}}, stack, start, qasm)
    expr_head, start_token = expression_start(tokens, stack, start, qasm)
    start_token_type = start_token[end]
    head(expr_head) == :empty && throw(QasmParseError("unable to parse line with start token $(start_token_type)", stack, start, qasm))
    next_token = first(tokens)
    if next_token[end] ∈ (semicolon, comma) || start_token_type ∈ (lbracket, lbrace)
        expr = expr_head
    elseif start_token_type == integer_token && next_token[end] == irrational # this is banned! 2π is not supported, 2*π is.
        integer_lit    = parse_integer_literal(start_token, qasm).args[1]
        irrational_lit = parse_irrational_literal(next_token, qasm).args[1]
        throw(QasmParseError("expressions of form $(integer_lit)$(irrational_lit) are not supported -- you must separate the terms with a * operator.", stack, start, qasm))
    elseif start_token_type == operator
        expr       = parse_unary_op(expr_head, tokens, stack, start, qasm)
    elseif next_token[end] == colon
        expr       = parse_range(expr_head, tokens, stack, start, qasm)
    elseif next_token[end] == classical_type && start_token_type ∈ (mutable, readonly, const_token)
        type       = parse_classical_type(tokens, stack, start, qasm)
        is_mutable = (start_token_type == mutable)
        header     = is_mutable ? :classical_declaration : :const_declaration
        expr       = QasmExpression(header, type, parse_expression(tokens, stack, start, qasm))
    elseif start_token_type == classical_type && (next_token[end] ∈ (lbracket, identifier))
        expr      = QasmExpression(:classical_declaration, expr_head, parse_expression(tokens, stack, start, qasm))
    elseif start_token_type ∈ (frame_token, waveform_token) && (next_token[end] ∈ (lbracket, identifier))
        expr      = QasmExpression(:classical_declaration, expr_head, parse_expression(tokens, stack, start, qasm))
    elseif start_token_type == classical_type && next_token[end] == lparen
        expr      = parse_cast_expr(expr_head, tokens, stack, start, qasm)
    elseif next_token[end] == assignment
        expr      = parse_classical_assignment(expr_head, tokens, stack, start, qasm)
    elseif next_token[end] == operator
        expr      = parse_binary_op(expr_head, tokens, stack, start, qasm)
    else # either a gate call or function call
        arguments  = parse_arguments_list(tokens, stack, start, qasm)
        next_token = first(tokens)
        is_gphase::Bool = (head(expr_head) == :identifier && expr_head.args[1]::String == "gphase")::Bool
        # this is a gate call with qubit targets
        is_gate_call = next_token[end] == identifier || next_token[end] == hw_qubit || is_gphase
        if is_gate_call
            target_expr = QasmExpression(:qubit_targets, parse_list_expression(tokens, stack, start, qasm))
            expr        = QasmExpression(:gate_call, expr_head, arguments, target_expr)
        else # this is a function call
            expr        = parse_function_expr(expr_head, arguments, tokens, stack, start, qasm) 
        end
    end
    return expr
end

function parse_version(tokens, stack, start, qasm)
    closing = findfirst(triplet->triplet[end] == semicolon, tokens)
    isnothing(closing) && throw(QasmParseError("missing final semicolon for OPENQASM", stack, start, qasm))
    closing == 1 && throw(QasmParseError("missing version number", stack, start, qasm))
    version_val = parse_literal([popfirst!(tokens)], stack, start, qasm)
    isinteger(version_val.args[1]) || throw(QasmParseError("version number must be an integer", stack, version_start, qasm))
    return QasmExpression(:version, QasmExpression(:float_literal, version_val.args[1]))
end

function parse_include(tokens, stack, start, qasm)
    closing       = findfirst(triplet->triplet[end] == semicolon, tokens)
    isnothing(closing) && throw(QasmParseError("missing final semicolon for include", stack, start, qasm))
    file_name     = popfirst!(tokens)
    file_name[end] == string_token || throw(QasmParseError("included filename must be passed as a string", stack, start, qasm))
    file_name_str = replace(qasm[file_name[1]:file_name[1]+file_name[2]-1], "\""=>"", "'"=>"")
    file_contents = read(file_name_str, String)
    file_expr     = parse_qasm(file_contents)
    return file_expr.args
end

function parse_measure(tokens, stack, start, qasm)::QasmExpression
    eol = findfirst(triplet->triplet[end] == semicolon, tokens)
    measure_tokens = splice!(tokens, 1:eol)
    arrow_location = findfirst(triplet->triplet[end] == arrow_token, measure_tokens)
    if !isnothing(arrow_location) # assignment
        targets_tokens  = splice!(measure_tokens, 1:arrow_location - 1)
        popfirst!(measure_tokens) # arrow
        push!(targets_tokens, (-1, Int32(-1), semicolon))
        targets         = parse_expression(targets_tokens, stack, start, qasm)
        left_hand_side  = parse_expression(measure_tokens, stack, start, qasm)
        right_hand_side = QasmExpression(:measure, targets)
        op_expression   = QasmExpression(:binary_op, Symbol("="), left_hand_side, right_hand_side)
        return QasmExpression(:classical_assignment, op_expression)
    else
        targets = parse_expression(measure_tokens, stack, start, qasm)
        return QasmExpression(:measure, targets)
    end
end

function parse_qasm(clean_tokens::Vector{Tuple{Int64, Int32, Token}}, qasm::String, root=QasmExpression(:program))
    stack = Stack{QasmExpression}()
    push!(stack, root)
    while !isempty(clean_tokens)
        start, len, token = popfirst!(clean_tokens)
        if token == newline
            continue
        elseif token == version
            expr = parse_version(clean_tokens, stack, start, qasm)
            push!(stack, expr)
        elseif token == pragma
            closing = findfirst(triplet->triplet[end] == newline, clean_tokens)
            isnothing(closing) && throw(QasmParseError("missing final newline for #pragma", stack, start, qasm))
            pragma_tokens = splice!(clean_tokens, 1:closing-1)
            push!(stack, parse_pragma[](pragma_tokens, stack, start, qasm))
        elseif token == include_token
            exprs = parse_include(clean_tokens, stack, start, qasm)
            foreach(ex->push!(stack, ex), exprs)
        elseif token == const_token
            closing   = findfirst(triplet->triplet[end] == semicolon, clean_tokens)
            isnothing(closing) && throw(QasmParseError("missing final semicolon for const declaration", stack, start, qasm))
            raw_expr  = parse_expression(splice!(clean_tokens, 1:closing), stack, start, qasm)
            push!(stack, QasmExpression(:const_declaration, raw_expr.args))
        elseif token == classical_type || token == frame_token || token == waveform_token
            closing     = findfirst(triplet->triplet[end] == semicolon, clean_tokens)
            isnothing(closing) && throw(QasmParseError("missing final semicolon for classical declaration", stack, start, qasm))
            line_tokens = pushfirst!(splice!(clean_tokens, 1:closing), (start, len, token))
            expr        = parse_expression(line_tokens, stack, start, qasm)
            push!(stack, expr)
        elseif token == cal_token
            expr = parse_block_body(QasmExpression(:cal), clean_tokens, stack, start, qasm)
            push!(stack, expr)
        elseif token == defcal_token
            expr = parse_gate_or_cal_def(:defcal, clean_tokens, stack, start, qasm)
            push!(stack, expr)
        elseif token == defcalgrammar_token
            next_token = first(clean_tokens)
            if next_token[end] != string_token || String(read_raw(next_token, qasm)) != "\"openpulse\""
                throw(QasmParseError("only \"openpulse\" grammar is currently supported", stack, start, qasm))
            end
        elseif token == extern_token
            closing     = findfirst(triplet->triplet[end] == semicolon, clean_tokens)
            isnothing(closing) && throw(QasmParseError("missing final semicolon for extern declaration", stack, start, qasm))
            line_tokens = splice!(clean_tokens, 1:closing-1)
            if first(line_tokens)[end] ∈ (port_token, frame_token)

            else
                push!(stack, parse_function_def(line_tokens, stack, start, qasm))
            end
        elseif token == input
            closing   = findfirst(triplet->triplet[end] == semicolon, clean_tokens)
            isnothing(closing) && throw(QasmParseError("missing final semicolon for input", stack, start, qasm))
            input_var = parse_classical_var(splice!(clean_tokens, 1:closing), stack, start, qasm)
            expr      = QasmExpression(:input, input_var...)
            push!(stack, expr)
        elseif token == output 
            closing    = findfirst(triplet->triplet[end] == semicolon, clean_tokens)
            isnothing(closing) && throw(QasmParseError("missing final semicolon for output", stack, start, qasm))
            output_var = parse_classical_var(splice!(clean_tokens, 1:closing), stack, start, qasm)
            expr       = QasmExpression(:output, output_var...)
            push!(stack, expr)
        elseif token == qubit
            closing = findfirst(triplet->triplet[end] == semicolon, clean_tokens)
            isnothing(closing) && throw(QasmParseError("missing final semicolon for qubit", stack, start, qasm))
            qubit_tokens = splice!(clean_tokens, 1:closing-1)
            expr = parse_qubit_declaration(qubit_tokens, stack, start, qasm)
            push!(stack, expr)
        elseif token == gate_def
            expr = parse_gate_or_cal_def(:gate_definition, clean_tokens, stack, start, qasm)
            push!(stack, expr)
        elseif token == function_def
            expr = parse_function_def(clean_tokens, stack, start, qasm)
            push!(stack, expr)
        elseif token == for_block
            loop_in   = findfirst(triplet->triplet[end] == in_token, clean_tokens)
            isnothing(loop_in) && throw(QasmParseError("for loop variable must have in declaration", stack, start, qasm))
            loop_var  = parse_classical_var(splice!(clean_tokens, 1:loop_in-1), stack, start, qasm)
            popfirst!(clean_tokens) # in
            loop_vals = parse_expression(clean_tokens, stack, start, qasm)
            expr      = parse_for_loop(clean_tokens, loop_var[1], loop_var[2], loop_vals, stack, start, qasm)
            push!(stack, expr)
        elseif token == while_block
            expr = parse_while_loop(clean_tokens, stack, start, qasm)
            push!(stack, expr)
        elseif token == if_block
            expr = parse_if_block(clean_tokens, stack, start, qasm)
            push!(stack, expr)
        elseif token == break_token
            push!(stack, QasmExpression(:break))
        elseif token == continue_token
            push!(stack, QasmExpression(:continue))
        elseif token == switch_block
            expr = parse_switch_block(clean_tokens, stack, start, qasm)
            push!(stack, expr)
        elseif token == line_comment
            eol = findfirst(triplet->triplet[end] == newline, clean_tokens)
            splice!(clean_tokens, 1:eol)
        elseif token == measure
            expr = parse_measure(clean_tokens, stack, start, qasm)
            push!(stack, expr)
        elseif token ∈ (negctrl_mod, control_mod, inverse_mod, power_mod)
            gate_mod_tokens = pushfirst!(clean_tokens, (start, len, token))
            expr = parse_gate_mods(gate_mod_tokens, stack, start, qasm)
            push!(stack, expr)
        elseif token == return_token
            eol = findfirst(triplet->triplet[end] == semicolon, clean_tokens)
            return_line_tokens = splice!(clean_tokens, 1:eol)
            line_body  = parse_qasm(return_line_tokens, qasm, QasmExpression(:return))
            line_exprs = collect(Iterators.reverse(line_body))[2:end]
            push!(stack, QasmExpression(:return, line_exprs))
        elseif token == box
            @warn "box expression encountered -- currently `box` is a no-op"
            box_expr = parse_block_body(QasmExpression(:box), clean_tokens, stack, start, qasm)
            push!(stack, box_expr)
        elseif token == reset_token
            @warn "reset expression encountered -- currently `reset` is a no-op"
            eol          = findfirst(triplet->triplet[end] == semicolon, clean_tokens)
            reset_tokens = splice!(clean_tokens, 1:eol)
            targets      = parse_list_expression(reset_tokens, stack, start, qasm)
            push!(stack, QasmExpression(:reset, targets))
        elseif token == barrier_token
            @warn "barrier expression encountered -- currently `barrier` is a no-op"
            eol            = findfirst(triplet->triplet[end] == semicolon, clean_tokens)
            barrier_tokens = splice!(clean_tokens, 1:eol)
            targets        = parse_list_expression(barrier_tokens, stack, start, qasm)
            push!(stack, QasmExpression(:barrier, targets))
        elseif token == delay_token
            @warn "delay expression encountered -- currently `delay` is a no-op"
            eol            = findfirst(triplet->triplet[end] == semicolon, clean_tokens)
            delay_tokens   = splice!(clean_tokens, 1:eol)
            delay_expr     = QasmExpression(:delay)
            # format is delay[duration]; or delay[duration] targets;
            delay_duration = extract_expression(delay_tokens, lbracket, rbracket, stack, start, qasm)
            push!(delay_expr, QasmExpression(:duration, parse_expression(delay_duration, stack, start, qasm)))
            target_expr    = QasmExpression(:targets)
            if first(delay_tokens)[end] != semicolon # targets present
                push!(target_expr, parse_list_expression(delay_tokens, stack, start, qasm))
            end
            push!(delay_expr, target_expr)
            push!(stack, delay_expr)
        elseif token == end_token
            push!(stack, QasmExpression(:end))
        elseif token == identifier || token == builtin_gate
            clean_tokens = pushfirst!(clean_tokens, (start, len, token))
            expr         = parse_expression(clean_tokens, stack, start, qasm)
            push!(stack, expr)
        end
    end
    return stack
end
function parse_qasm(qasm::String, root=QasmExpression(:program))::QasmExpression
    raw_tokens   = tokenize(Token, qasm)
    clean_tokens = filter(triplet->triplet[3] ∉ (spaces, block_comment), collect(raw_tokens))
    # add a final newline in case one is missing
    clean_tokens[end][end] == newline || push!(clean_tokens, (-1, Int32(-1), newline))
    stack       = parse_qasm(clean_tokens, qasm, root)
    stack_exprs = convert(Vector{QasmExpression}, collect(Iterators.reverse(stack)))::Vector{QasmExpression}
    foreach(ex->push!(stack_exprs[1], ex), stack_exprs[2:end])
    return stack_exprs[1] 
end

