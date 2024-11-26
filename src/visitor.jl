mutable struct ClassicalVariable
    name::String
    type
    val
    is_const::Bool
end

struct Qubit 
    name::String
    size::Int
end

"""
    InstructionArgument

Union type of `Symbol`, `Dates.Period`, `Real`, `Matrix{ComplexF64}` which
represents a possible argument to a [`CircuitInstruction`](@ref).
"""
const InstructionArgument = Union{Symbol, Dates.Period, Real, Matrix{ComplexF64}}

"""
    CircuitInstruction

A `NamedTuple` representing a circuit instruction (a gate, a noise operation, a timing operation). Fields are:

- `type::String` - the name of the operation (e.g. `rx`)
- `arguments::Vector{InstructionArgument}` - arguments, if any, to the operation (e.g. `π` for an angled gate)
- `targets::Vector{Int}` - qubit targets for the operation, **including control qubits, if any**
- `controls::Vector{Pair{Int, Int}}` - control qubits and bit-values for the operation, so that a `ctrl @ x 0, 2;` could have `controls = [0=>1]` and `negctrl @ x 0, 2;` could have `controls = [0=>0]`.
- `exponent::Float64` - exponent to which the operation is raised, if any

`CircuitInstruction`s can be used with a package like [`StructTypes.jl`](https://github.com/JuliaData/StructTypes.jl) to build the
actual types of your package from these `NamedTuple`s.
"""
const CircuitInstruction = @NamedTuple begin type::String; arguments::Vector{InstructionArgument}; targets::Vector{Int}; controls::Vector{Pair{Int, Int}}; exponent::Float64 end
"""
    CircuitResult

A `NamedTuple` representing a circuit result (e.g. an expectation value). Fields are:

- `type::Symbol` - the name of the result type (e.g. `:variance`)
- `operator::Vector{Union{String, Matrix{ComplexF64}}}` - the operator to measure, if applicable
- `targets::Vector{Int}` - qubit targets for the result
- `states::Vector{String}` - vector of bitstrings to measure the amplitudes of, if the result type is an `:amplitude` 

`CircuitResult`s can be used with a package like [`StructTypes.jl`](https://github.com/JuliaData/StructTypes.jl) to build the
actual result objects of your package from these `NamedTuple`s.
"""
const CircuitResult = @NamedTuple begin type::Symbol; operator::Vector{Union{String, Matrix{ComplexF64}}}; targets::Vector{Int}; states::Vector{String}; end

abstract type AbstractGateDefinition end

struct GateDefinition <: AbstractGateDefinition
    name::String
    arguments::Vector{String}
    qubit_targets::Vector{String} # keep this as string to support splatting
    body::QasmExpression
end

struct BuiltinGateDefinition <: AbstractGateDefinition
    name::String
    arguments::Vector{String}
    qubit_targets::Vector{String} # keep this as string to support splatting
    body::CircuitInstruction
end

struct FunctionDefinition
    name::String
    arguments::QasmExpression
    body::Vector{QasmExpression}
    return_type
end
FunctionDefinition(name::String, arguments::QasmExpression, body::QasmExpression, return_type) = FunctionDefinition(name, arguments, [body], return_type)

struct QasmVisitorError <: Exception
    message::String
    alternate_type::String
end
QasmVisitorError(message::String) = QasmVisitorError(message, "")
function Base.showerror(io::IO, err::QasmVisitorError)
    print(io, "QasmVisitorError: ")
    print(io, err.message)
end

abstract type AbstractVisitor end

basic_builtin_gates() = Dict{String, BuiltinGateDefinition}(
        "U"=>BuiltinGateDefinition("U", ["θ", "ϕ", "λ"], ["a"], (type="u", arguments=InstructionArgument[:θ, :ϕ, :λ], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)),
    )

mutable struct QasmProgramVisitor <: AbstractVisitor
    inputs::Dict{String, Any}
    classical_defs::Dict{String, ClassicalVariable}
    function_defs::Dict{String, FunctionDefinition}
    gate_defs::Dict{String, AbstractGateDefinition}
    qubit_defs::Dict{String, Qubit}
    qubit_mapping::Dict{String, Vector{Int}}
    qubit_count::Int
    instructions::Vector{CircuitInstruction}
    results::Vector{CircuitResult}
    function QasmProgramVisitor(inputs::Dict{String, <:Any} = Dict{String, Any}())
        new(inputs,
            Dict{String, ClassicalVariable}(),
            Dict{String, FunctionDefinition}(),
            builtin_gates[](),
            Dict{String, Qubit}(),
            Dict{String, Vector{Int}}(),
            0,
            CircuitInstruction[],
            CircuitResult[],
           )
    end
end

mutable struct QasmGateDefVisitor <: AbstractVisitor
    parent::AbstractVisitor
    classical_defs::Dict{String, ClassicalVariable}
    qubit_defs::Dict{String, Qubit}
    qubit_mapping::Dict{String, Vector{Int}}
    qubit_count::Int
    instructions::Vector{CircuitInstruction}
    function QasmGateDefVisitor(parent::AbstractVisitor, declared_arguments::Vector{String}, provided_arguments::QasmExpression, gate_qubits::Vector{String})
        qubit_defs    = Dict(q=>Qubit(q, 1) for q in gate_qubits)
        qubit_mapping = Dict(gate_qubits[ix+1]=>[ix] for ix in 0:length(gate_qubits)-1)
        for ix in 0:length(gate_qubits)-1
            qubit_mapping[gate_qubits[ix+1] * "[0]"] = [ix]
        end
        v = new(parent,
                deepcopy(classical_defs(parent)),
                qubit_defs,
                qubit_mapping,
                length(gate_qubits),
                CircuitInstruction[],
               )
        for (arg_name, arg_value) in zip(declared_arguments, parent(provided_arguments))
            classical_defs(v)[arg_name] = ClassicalVariable(arg_name, Real, arg_value, true)
        end
        return v
    end
end

mutable struct QasmForLoopVisitor <: AbstractVisitor
    parent::AbstractVisitor
    classical_defs::Dict{String, ClassicalVariable}
    QasmForLoopVisitor(parent::AbstractVisitor) = new(parent, classical_defs(parent))
end

mutable struct QasmWhileLoopVisitor <: AbstractVisitor
    parent::AbstractVisitor
    QasmWhileLoopVisitor(parent::AbstractVisitor) = new(parent)
end

mutable struct QasmFunctionVisitor <: AbstractVisitor
    parent::AbstractVisitor
    classical_defs::Dict{String, ClassicalVariable}
    qubit_defs::Dict{String, Qubit}
    qubit_mapping::Dict{String, Vector{Int}}
    qubit_count::Int
    instructions::Vector{CircuitInstruction}
    function QasmFunctionVisitor(parent::AbstractVisitor, declared_arguments::Vector{QasmExpression}, provided_arguments::Vector{QasmExpression})
        v = new(parent, 
            classical_defs(parent),
            deepcopy(parent.qubit_defs),
            deepcopy(parent.qubit_mapping),
            qubit_count(parent),
            CircuitInstruction[],
           )
        arg_map = Dict(zip(declared_arguments, provided_arguments))
        for arg in declared_arguments
            if head(arg) ∈ (:const_declaration, :classical_declaration)
                new_val = parent(arg_map[arg])
                if head(arg.args[2]) != :classical_assignment
                    arg_id = pop!(arg)
                    push!(arg, QasmExpression(:classical_assignment, QasmExpression(:binary_op, Symbol("="), arg_id, new_val)))
                else
                    arg.args[2].args[1].args[end] = new_val
                end
            end
            v(arg)
        end
        return v
    end
end
function QasmFunctionVisitor(parent::AbstractVisitor, declared_arguments::Vector{QasmExpression}, provided_arguments::QasmExpression)
    head(provided_arguments) == :array_literal && return QasmFunctionVisitor(parent, declared_arguments, convert(Vector{QasmExpression}, provided_arguments.args))
    QasmFunctionVisitor(parent, declared_arguments, [provided_arguments])
end
function QasmFunctionVisitor(parent::AbstractVisitor, declared_arguments::QasmExpression, provided_arguments)
    head(declared_arguments) == :array_literal && return QasmFunctionVisitor(parent, convert(Vector{QasmExpression}, declared_arguments.args), provided_arguments)
    QasmFunctionVisitor(parent, [declared_arguments], provided_arguments)
end
Base.parent(v::AbstractVisitor) = v.parent

hasgate(v::AbstractVisitor, gate_name::String)    = hasgate(parent(v), gate_name)
hasgate(v::QasmProgramVisitor, gate_name::String) = haskey(v.gate_defs, gate_name)
gate_defs(v::AbstractVisitor)    = gate_defs(parent(v))
gate_defs(v::QasmProgramVisitor) = v.gate_defs

function_defs(v::QasmProgramVisitor) = v.function_defs
function_defs(v::AbstractVisitor)    = function_defs(parent(v))

hasfunction(v::AbstractVisitor, function_name::String)    = haskey(function_defs(v), function_name)

qubit_defs(v::AbstractVisitor)     = qubit_defs(parent(v))
qubit_defs(v::QasmFunctionVisitor) = v.qubit_defs
qubit_defs(v::QasmProgramVisitor)  = v.qubit_defs

qubit_mapping(v::AbstractVisitor)     = qubit_mapping(parent(v))
qubit_mapping(v::QasmProgramVisitor)  = v.qubit_mapping
qubit_mapping(v::QasmFunctionVisitor) = v.qubit_mapping
qubit_mapping(v::QasmGateDefVisitor)  = v.qubit_mapping

qubit_count(v::AbstractVisitor)     = qubit_count(parent(v))
qubit_count(v::QasmProgramVisitor)  = v.qubit_count
qubit_count(v::QasmFunctionVisitor) = v.qubit_count
qubit_count(v::QasmGateDefVisitor)  = v.qubit_count

classical_defs(v::AbstractVisitor)     = classical_defs(parent(v))
classical_defs(v::QasmProgramVisitor)  = v.classical_defs
classical_defs(v::QasmGateDefVisitor)  = v.classical_defs
classical_defs(v::QasmFunctionVisitor) = v.classical_defs

instructions(v::AbstractVisitor)     = instructions(parent(v))
instructions(v::QasmProgramVisitor)  = v.instructions
instructions(v::QasmGateDefVisitor)  = v.instructions
instructions(v::QasmFunctionVisitor) = v.instructions

results(v::AbstractVisitor) = results(parent(v))
results(v::QasmProgramVisitor) = v.results

Base.push!(v::AbstractVisitor, ixs::Vector{CircuitInstruction}) = append!(instructions(v), ixs)
Base.push!(v::AbstractVisitor, ix::CircuitInstruction)          = push!(instructions(v), ix)

Base.push!(v::AbstractVisitor, rts::Vector{CircuitResult}) = append!(results(v), rts)
Base.push!(v::AbstractVisitor, rt::CircuitResult)          = push!(results(v), rt)

function evaluate_unary_op(op::Symbol, arg)
    op == :! && return !arg
    op == :~ && return .!arg
    op == :- && return -arg 
end
function evaluate_unary_op(op::Symbol, arg::BitVector)
    op == :! && return !any(arg)
    op == :~ && return .!arg
end

# semgrep rules can't handle this macro properly yet
# nosemgrep
function evaluate_binary_op(op::Symbol, @nospecialize(lhs), @nospecialize(rhs))
    op == :< && return lhs < rhs
    op == :> && return lhs > rhs
    op == :<= && return lhs <= rhs
    op == :>= && return lhs >= rhs
    op == Symbol("=") && return rhs
    op == Symbol("!=") && return lhs != rhs
    op == Symbol("==") && return lhs == rhs
    op == :+ && return lhs + rhs
    op == :- && return lhs - rhs
    op == :* && return lhs * rhs
    op == :/ && return lhs / rhs
    op == :% && return lhs % rhs
    op == Symbol("<<") && return lhs << rhs
    op == Symbol(">>") && return lhs >> rhs
    op == Symbol("**") && return lhs ^ rhs
    op == Symbol("&&") && return lhs && rhs
    op == Symbol("||") && return lhs || rhs
    op == :| && return lhs .| rhs
    op == :& && return lhs .& rhs
    op == :^ && return lhs .⊻ rhs
end

function evaluate_modifiers(v::V, expr::QasmExpression) where {V<:AbstractVisitor}
    if head(expr) == :power_mod
        pow_expr = QasmExpression(:pow, v(expr.args[1]::QasmExpression))
        return (pow_expr, expr.args[2])
    elseif head(expr) == :inverse_mod
        return (QasmExpression(:inv), expr.args[1]::QasmExpression)
    elseif head(expr) ∈ (:control_mod, :negctrl_mod)
        has_argument = length(expr.args) > 1
        if has_argument
            arg_val::Int = v(first(expr.args)::QasmExpression)::Int
            isinteger(arg_val) || throw(QasmVisitorError("cannot apply non-integer ($arg_val) number of controls or negcontrols."))
            true_inner = expr.args[2]::QasmExpression
            inner = QasmExpression(head(expr), true_inner)
            while arg_val > 2
                inner = QasmExpression(head(expr), inner)
                arg_val -= 1
            end
        else
            inner = expr.args[1]::QasmExpression
        end
        new_head = head(expr) == :control_mod ? :ctrl : :negctrl
        return (QasmExpression(new_head), inner)
    end
end

function name(expr::QasmExpression)::String
    head(expr) == :identifier         && return expr.args[1]::String
    head(expr) == :indexed_identifier && return name(expr.args[1]::QasmExpression)
    head(expr) == :qubit_declaration  && return name(expr.args[1]::QasmExpression)
    head(expr) == :classical_declaration && return name(expr.args[2]::QasmExpression)
    head(expr) == :input              && return name(expr.args[2]::QasmExpression)
    head(expr) == :function_call      && return name(expr.args[1]::QasmExpression)
    head(expr) == :gate_call          && return name(expr.args[1]::QasmExpression)
    head(expr) == :gate_definition    && return name(expr.args[1]::QasmExpression)
    head(expr) == :classical_assignment && return name(expr.args[1].args[2]::QasmExpression)
    head(expr) == :alias              && return name(expr.args[1]::QasmExpression)
    head(expr) == :hw_qubit           && return replace(expr.args[1], "\$"=>"")
    throw(QasmVisitorError("name not defined for expressions of type $(head(expr))"))
end

# nosemgrep
function _evaluate_qubits(::Val{:identifier}, v, qubit_expr::QasmExpression)::Vector{Int}
    qubit_name = name(qubit_expr)
    mapping    = qubit_mapping(v)::Dict{String, Vector{Int}}
    haskey(mapping, qubit_name) || throw(QasmVisitorError("Missing input variable '$qubit_name'.", "NameError"))
    return mapping[qubit_name]
end

# nosemgrep
function _evaluate_qubits(::Val{:indexed_identifier}, v, qubit_expr::QasmExpression)::Vector{Int}
    qubit_name = name(qubit_expr)
    mapping    = qubit_mapping(v)::Dict{String, Vector{Int}}
    haskey(mapping, qubit_name) || throw(QasmVisitorError("Missing input variable '$qubit_name'.", "NameError"))
    qubit_ix   = v(qubit_expr.args[2]::QasmExpression)
    qubits     = Iterators.flatmap(qubit_ix) do rq
        if rq >= 0
            haskey(mapping, qubit_name * "[$rq]") || throw(QasmVisitorError("Invalid qubit index '$rq' in '$qubit_name'.", "IndexError"))
            return mapping[qubit_name * "[$rq]"]
        else
            qubit_size = length(mapping[qubit_name])
            haskey(mapping, qubit_name * "[$(qubit_size + rq)]") || throw(QasmVisitorError("Invalid qubit index '$rq' in '$qubit_name'.", "IndexError"))
            return mapping[qubit_name * "[$(qubit_size + rq)]"]
        end
    end
    return collect(qubits)
end
_evaluate_qubits(::Val{:array_literal}, v, qubit_expr::QasmExpression)::Vector{Int} = collect(Iterators.flatmap(expr->_evaluate_qubits(Val(head(expr)), v, expr), qubit_expr.args))
_evaluate_qubits(::Val{:hw_qubit}, v, qubit_expr::QasmExpression)::Vector{Int} = Int[v(qubit_expr)::Int]
_evaluate_qubits(val, v, qubit_expr) = throw(QasmVisitorError("unable to evaluate qubits for expression $qubit_expr."))

function evaluate_qubits(v::AbstractVisitor, qubit_targets::Vector)::Vector{Int}
    final_qubits = map(qubit_expr->_evaluate_qubits(Val(head(qubit_expr)), v, qubit_expr), qubit_targets)
    return vcat(final_qubits...)
end
evaluate_qubits(v::AbstractVisitor, qubit_targets::QasmExpression) = evaluate_qubits(v::AbstractVisitor, [qubit_targets])

function remap(ix, target_mapper::Dict{Int, Int})
    mapped_targets  = map(t->getindex(target_mapper, t), ix.targets)
    mapped_controls = map(c->getindex(target_mapper, c[1])=>c[2], ix.controls)
    return (type=ix.type, arguments=ix.arguments, targets=mapped_targets, controls=mapped_controls, exponent=ix.exponent)
end
function bind_arguments!(ix::CircuitInstruction, argument_values::Dict{Symbol, <:Real})
    new_arguments = InstructionArgument[get(argument_values, arg, arg) for arg in ix.arguments]
    return (type=ix.type, arguments=new_arguments, targets=ix.targets, controls=ix.controls, exponent=ix.exponent)
end

function process_gate_arguments(v::AbstractVisitor, gate_name::String, defined_arguments::Vector{String}, called_arguments::QasmExpression, gate_body::Vector{CircuitInstruction})
    def_has_arguments  = !isempty(defined_arguments)
    call_has_arguments = !isempty(v(called_arguments))
    if def_has_arguments ⊻ call_has_arguments
        def_has_arguments && throw(QasmVisitorError("gate $gate_name requires arguments but none were provided.")) 
        call_has_arguments && throw(QasmVisitorError("gate $gate_name does not accept arguments but arguments were provided."))
    end
    if def_has_arguments
        evaled_args     = v(called_arguments)
        argument_values = Dict{Symbol, Real}(Symbol(arg_name)=>argument for (arg_name, argument) in zip(defined_arguments, evaled_args))
        return map(ix->bind_arguments!(ix, argument_values), gate_body)
    else
        return deepcopy(gate_body)
    end
end

function handle_gate_modifiers(ixs, mods::Vector{QasmExpression}, control_qubits::Vector{Int}, is_gphase::Bool)
    for mod in Iterators.reverse(mods)
        control_qubit = head(mod) ∈ (:negctrl, :ctrl) ? pop!(control_qubits) : -1
        for (ii, ix) in enumerate(ixs)
            if head(mod) == :pow
                ixs[ii] = (type=ix.type, arguments=ix.arguments, targets=ix.targets, controls=ix.controls, exponent=ix.exponent*mod.args[1])
            elseif head(mod) == :inv
                ixs[ii] = (type=ix.type, arguments=ix.arguments, targets=ix.targets, controls=ix.controls, exponent=-ix.exponent)
            # need to handle "extra" target
            elseif head(mod) ∈ (:negctrl, :ctrl)
                bit = head(mod) == :ctrl ? 1 : 0
                if is_gphase
                    ixs[ii] = (type=ix.type, arguments=ix.arguments, targets=ix.targets, controls=pushfirst!(ix.controls, control_qubit=>bit), exponent=ix.exponent)
                else
                    ixs[ii] = (type=ix.type, arguments=ix.arguments, targets=pushfirst!(ix.targets, control_qubit), controls=pushfirst!(ix.controls, control_qubit=>bit), exponent=ix.exponent)
                end
            end
        end
        head(mod) == :inv && reverse!(ixs)
    end
    return ixs
end

function splat_gate_targets(gate_targets::Vector{Vector{Int}})
    target_lengths::Vector{Int} = Int[length(t) for t in gate_targets]
    longest = maximum(target_lengths)
    must_splat::Bool = any(len->len!=1 || len != longest, target_lengths)
    !must_splat && return longest, gate_targets
    for target_ix in 1:length(gate_targets)
        if target_lengths[target_ix] == 1
            append!(gate_targets[target_ix], fill(only(gate_targets[target_ix]), longest-1))
        end
    end
    return longest, gate_targets
end

function visit_gphase_call(v::AbstractVisitor, program_expr::QasmExpression)
    has_modifiers = length(program_expr.args) == 4
    n_called_with::Int  = qubit_count(v)
    gate_targets::Vector{Int}    = collect(0:n_called_with-1)
    provided_arg::QasmExpression = only(program_expr.args[2].args)
    evaled_arg        = v(provided_arg)
    applied_arguments = CircuitInstruction[(type="gphase", arguments=[evaled_arg], targets=gate_targets, controls=Pair{Int,Int}[], exponent=1.0)]
    mods::Vector{QasmExpression} = has_modifiers ? program_expr.args[4].args : QasmExpression[]
    applied_arguments = handle_gate_modifiers(applied_arguments, mods, deepcopy(gate_targets), true)
    target_mapper     = Dict{Int, Int}(g_ix=>gate_targets[g_ix+1][1] for g_ix in 0:n_called_with-1)
    push!(v, map(ix->remap(ix, target_mapper), applied_arguments))
    return
end

function visit_gate_call(v::AbstractVisitor, program_expr::QasmExpression)
    gate_name        = name(program_expr)::String
    raw_call_targets = program_expr.args[3]::QasmExpression
    call_targets::Vector{QasmExpression}  = convert(Vector{QasmExpression}, head(raw_call_targets.args[1]) == :array_literal ? raw_call_targets.args[1].args : raw_call_targets.args)::Vector{QasmExpression}
    provided_args    = isempty(program_expr.args[2].args) ? QasmExpression(:empty) : only(program_expr.args[2].args)::QasmExpression
    has_modifiers    = length(program_expr.args) == 4
    hasgate(v, gate_name) || throw(QasmVisitorError("gate $gate_name not defined!"))
    gate_def         = gate_defs(v)[gate_name]
    gate_def_v       = QasmGateDefVisitor(v, gate_def.arguments, provided_args, gate_def.qubit_targets)
    gate_def_v(deepcopy(gate_def.body))
    gate_ixs         = instructions(gate_def_v)
    gate_targets     = Vector{Int}[evaluate_qubits(v, call_target)::Vector{Int} for call_target in call_targets]
    n_called_with    = length(gate_targets)
    n_defined_with   = length(gate_def.qubit_targets)
    # cases like `ccnot qs`;
    if n_called_with < n_defined_with && length(gate_targets[1]) == n_defined_with
        n_called_with = length(gate_targets[1])
        gate_targets  = Vector{Int}[[gt] for gt in gate_targets[1]]
    end
    applied_arguments = process_gate_arguments(v, gate_name, gate_def.arguments, provided_args, gate_ixs)
    control_qubits::Vector{Int}  = collect(0:(n_called_with-n_defined_with)-1)
    mods::Vector{QasmExpression} = has_modifiers ? convert(Vector{QasmExpression}, program_expr.args[4].args) : QasmExpression[]
    if !isempty(control_qubits)
        modifier_remap = Dict{Int, Int}(old_qubit=>(old_qubit + length(control_qubits)) for old_qubit in 0:length(gate_def.qubit_targets))
        for ii in 1:length(applied_arguments)
            applied_arguments[ii] = remap(applied_arguments[ii], modifier_remap)
        end
    end
    applied_arguments     = handle_gate_modifiers(applied_arguments, mods, control_qubits, false)
    longest, gate_targets = splat_gate_targets(gate_targets)
    for splatted_ix in 1:longest
        target_mapper = Dict{Int, Int}(g_ix=>gate_targets[g_ix+1][splatted_ix] for g_ix in 0:n_called_with-1)
        push!(v, map(ix->remap(ix, target_mapper), applied_arguments))
    end
    return
end

function visit_function_call(v, expr, function_name)
    function_def  = function_defs(v)[function_name]
    function_body = function_def.body::Vector{QasmExpression}
    declared_args = only(function_def.arguments.args)::QasmExpression
    provided_args = only(expr.args[2].args)::QasmExpression
    function_v    = QasmFunctionVisitor(v, declared_args, provided_args)
    return_val    = nothing
    body_exprs::Vector{QasmExpression} = head(function_body[1]) == :scope ? function_body[1].args : function_body
    for f_expr in body_exprs
        if head(f_expr) == :return
            return_val = function_v(f_expr.args[1])
        else
            function_v(f_expr)
        end
    end
    # remap qubits and classical variables
    function_args = if head(declared_args) == :array_literal
        convert(Vector{QasmExpression}, declared_args.args)::Vector{QasmExpression}
    else
        declared_args
    end
    called_args = if head(provided_args) == :array_literal
        convert(Vector{QasmExpression}, provided_args.args)::Vector{QasmExpression}
    else
        provided_args
    end
    reverse_arguments_map = Dict{QasmExpression, QasmExpression}(zip(called_args, function_args))
    reverse_qubits_map    = Dict{Int, Int}()
    for variable in filter(v->head(v) ∈ (:identifier, :indexed_identifier), keys(reverse_arguments_map))
        variable_name = name(variable)
        if haskey(classical_defs(v), variable_name) && classical_defs(v)[variable_name].type isa SizedArray && head(reverse_arguments_map[variable]) != :const_declaration
            inner_variable_name = name(reverse_arguments_map[variable])
            new_val = classical_defs(function_v)[inner_variable_name].val
            back_assignment = QasmExpression(:classical_assignment, QasmExpression(:binary_op, Symbol("="), variable, new_val))
            v(back_assignment)
        elseif haskey(qubit_defs(v), variable_name)
            outer_context_map = only(evaluate_qubits(v, variable))
            inner_context_map = only(evaluate_qubits(function_v, reverse_arguments_map[variable].args[1]))
            reverse_qubits_map[inner_context_map] = outer_context_map
        end
    end
    mapper = isempty(reverse_qubits_map) ? identity : ix->remap(ix, reverse_qubits_map)
    push!(v, map(mapper, function_v.instructions))
    return return_val
end

function declaration_init(v, expr::QasmExpression)
    var_type = expr.args[1].args[1]
    init = if var_type isa SizedNumber
            undef
        elseif var_type isa SizedArray
            fill(undef, v(var_type.size))
        elseif var_type isa SizedBitVector
            falses(max(0, v(var_type.size)))
        end
    return init, var_type 
end

(v::AbstractVisitor)(i::Number) = i
(v::AbstractVisitor)(i::String) = i
(v::AbstractVisitor)(i::BitVector) = i
(v::AbstractVisitor)(i::NTuple{N,<:Number}) where {N} = i
(v::AbstractVisitor)(i::Vector{<:Number}) = i
(v::AbstractVisitor)(program_exprs::Vector) = map(v, program_exprs)
(v::QasmGateDefVisitor)(ix::CircuitInstruction) = push!(v, ix)
function (v::AbstractVisitor)(program_expr::QasmExpression)
    var_name::String = ""
    if head(program_expr) == :program
        for expr in program_expr.args
            head(expr) == :end && return
            v(expr)
        end
    elseif head(program_expr) == :scope
        for expr in program_expr.args
            head(expr) == :end && return
            head(expr) == :continue && return :continue
            head(expr) == :break && return :break
            v(expr)
        end
    elseif head(program_expr) == :version
        return v
    elseif head(program_expr) == :reset
        targets = program_expr.args[1]::QasmExpression
        target_qubits = evaluate_qubits(v, targets)
        ixs = [(type="reset", arguments=InstructionArgument[], targets=[t], controls=Pair{Int, Int}[], exponent=1.0) for t in target_qubits]
        push!(v, ixs)
        return v
    elseif head(program_expr) == :barrier
        targets = program_expr.args[1]::QasmExpression
        target_qubits = evaluate_qubits(v, targets)
        ixs = [(type="barrier", arguments=InstructionArgument[], targets=[t], controls=Pair{Int, Int}[], exponent=1.0) for t in target_qubits]
        push!(v, ixs)
        return v
    elseif head(program_expr) == :delay
        duration_expr = program_expr.args[1].args[1]::QasmExpression
        targets       = program_expr.args[2].args[1]::QasmExpression
        target_qubits = evaluate_qubits(v, targets)
        duration      = v(duration_expr)
        ixs = [(type="delay", arguments=InstructionArgument[duration], targets=[t], controls=Pair{Int, Int}[], exponent=1.0) for t in target_qubits]
        push!(v, ixs)
        return v
    elseif head(program_expr) == :stretch
        return v
    elseif head(program_expr) == :duration
        return v
    elseif head(program_expr) == :input
        var_name = name(program_expr)
        var_type = program_expr.args[1].args[1]
        haskey(v.inputs, var_name) || throw(QasmVisitorError("Missing input variable '$var_name'.", "NameError"))
        var      = ClassicalVariable(var_name, var_type, v.inputs[var_name], true)
        v.classical_defs[var_name] = var
        return v
    elseif head(program_expr) ∈ (:continue, :break)
        v isa Union{QasmForLoopVisitor, QasmWhileLoopVisitor} && return head(program_expr)
        throw(QasmVisitorError(string(head(program_expr)) * " statement encountered outside a loop."))
    elseif head(program_expr) == :for
        for_v = QasmForLoopVisitor(v)
        for_loop             = convert(Vector{QasmExpression}, program_expr.args)
        loop_variable_type   = for_loop[1].args[1]
        loop_variable_name   = for_loop[2].args[1]::String
        loop_variable_values = for_v(for_loop[3])
        loop_body            = for_loop[4]::QasmExpression
        for loop_value in loop_variable_values
            loop_variable = ClassicalVariable(loop_variable_name, loop_variable_type, loop_value, false)
            for_v.classical_defs[loop_variable_name] = loop_variable
            if head(loop_body) == :scope
                for expr in convert(Vector{QasmExpression}, loop_body.args)
                    rt = for_v(expr)
                    rt == :continue && break
                    if rt == :break
                        delete!(classical_defs(v), loop_variable_name)
                        return v
                    end
                end
            else
                for_v(loop_body)
            end
        end
        delete!(classical_defs(v), loop_variable_name)
    elseif head(program_expr) == :switch
        case_val = v(program_expr.args[1])
        all_cases = convert(Vector{QasmExpression}, program_expr.args[2:end])
        default = findfirst(expr->head(expr) == :default, all_cases)
        case_found = false
        for case in all_cases
            if head(case) == :case && case_val ∈ v(case.args[1])
                case_found = true
                foreach(v, convert(Vector{QasmExpression}, case.args[2:end]))
                break
            end
        end
        if !case_found
            isnothing(default) && throw(QasmVisitorError("no case matched and no default defined."))
            foreach(v, convert(Vector{QasmExpression}, all_cases[default].args))
        end
    elseif head(program_expr) == :alias
        alias_name = name(program_expr)
        right_hand_side = program_expr.args[1].args[1].args[end]
        if head(right_hand_side) == :binary_op
            right_hand_side.args[1] == Symbol("++") || throw(QasmVisitorError("right hand side of alias must be either an identifier or concatenation"))
            concat_left    = right_hand_side.args[2]
            concat_right   = right_hand_side.args[3]
            is_left_qubit  = haskey(qubit_mapping(v), name(concat_left))
            is_right_qubit = haskey(qubit_mapping(v), name(concat_right))
            (is_left_qubit ⊻ is_right_qubit) && throw(QasmVisitorError("cannot concatenate qubit and classical arrays"))
            if is_left_qubit
                left_qs  = v(concat_left)
                right_qs = v(concat_right)
                alias_qubits = collect(vcat(left_qs, right_qs))
                qubit_size   = length(alias_qubits)
                qubit_defs(v)[alias_name]    = Qubit(alias_name, qubit_size)
                qubit_mapping(v)[alias_name] = alias_qubits
                for qubit_i in 0:qubit_size-1
                    qubit_mapping(v)["$alias_name[$qubit_i]"] = [alias_qubits[qubit_i+1]]
                end
            else # both classical
                left_array  = classical_defs(v)[name(concat_left)]  
                right_array = classical_defs(v)[name(concat_right)]  
                new_size = QasmExpression(:binary_op, :+, only(size(left_array.type)), only(size(right_array.type))) 
                if left_array.type isa SizedBitVector
                    classical_defs(v)[alias_name] = ClassicalVariable(alias_name, new_size, vcat(left_array.val, right_array.val), false)
                else 
                    classical_defs(v)[alias_name] = ClassicalVariable(alias_name, SizedArray(left_array.type.type, new_size), vcat(left_array.val, right_array.val), false)
                end
            end
        elseif head(right_hand_side) == :identifier
            referent_name = name(right_hand_side)
            is_qubit      = haskey(qubit_mapping(v), referent_name)
            if is_qubit
                qubit_defs(v)[alias_name]    = qubit_defs(v)[referent_name]
                qubit_mapping(v)[alias_name] = qubit_mapping(v)[referent_name]
                qubit_size = length(qubit_mapping(v)[alias_name])
                for qubit_i in 0:qubit_size-1
                    qubit_mapping(v)["$alias_name[$qubit_i]"] = qubit_mapping(v)["$referent_name[$qubit_i]"]
                end
            else
                classical_defs(v)[alias_name] = classical_defs(v)[referent_name]
            end
        elseif head(right_hand_side) == :indexed_identifier
            referent_name = name(right_hand_side)
            is_qubit      = haskey(qubit_mapping(v), referent_name)
            if is_qubit
                alias_qubits = v(right_hand_side)
                qubit_size   = length(alias_qubits)
                qubit_defs(v)[alias_name]    = Qubit(alias_name, qubit_size)
                qubit_mapping(v)[alias_name] = collect(alias_qubits)
                for qubit_i in 0:qubit_size-1
                    qubit_mapping(v)["$alias_name[$qubit_i]"] = [alias_qubits[qubit_i+1]]
                end
            else
                referent = classical_defs(v)[referent_name]
                ref_ixs  = v(right_hand_side.args[end])
                ixs = map(ref_ixs) do ix
                    ix >= 0 && return ix + 1
                    ix <  0 && return v(length(referent.type)) + 1 + ix
                end
                classical_defs(v)[alias_name] = ClassicalVariable(alias_name, referent.type, view(referent.val, ixs), referent.is_const)
            end
        end
    elseif head(program_expr) == :identifier
        id_name = name(program_expr)
        haskey(classical_defs(v), id_name) && return classical_defs(v)[id_name].val
        haskey(qubit_mapping(v), id_name) && return evaluate_qubits(v, program_expr)
        throw(QasmVisitorError("no identifier $id_name defined."))
    elseif head(program_expr) == :indexed_identifier
        identifier_name = name(program_expr)
        if haskey(classical_defs(v), identifier_name)
            var = classical_defs(v)[identifier_name]
            ix  = v(program_expr.args[2]::QasmExpression)
            if ix isa StepRange && ix.step > 0 && ix.stop < ix.start # -1 in place of end
                new_stop = var.type isa SizedNumber || var.type isa SizedBitVector ? v(var.type.size) : length(var.val)
                ix = StepRange(ix.start, ix.step, new_stop-1)
            end
            flat_ix = mapreduce(ix_ -> ix_ .+ 1, vcat, ix)
            if var.type isa SizedInt || var.type isa SizedUInt
                n_bits::Int = v(var.type.size)::Int
                int_val     = convert(Int, var.val)::Int
                values      = Int[(int_val >> (n_bits - index)) & 1 for index in flat_ix]
                return length(flat_ix) == 1 ? values[1] : values
            else
                return length(flat_ix) == 1 ? var.val[only(flat_ix)] : var.val[flat_ix]
            end
        elseif haskey(qubit_mapping(v), identifier_name)
            return evaluate_qubits(v, program_expr)
        else
            throw(QasmVisitorError("no identifier $identifier_name defined."))
        end
    elseif head(program_expr) == :if
        condition_value = v(program_expr.args[1]) > 0
        has_else  = findfirst(expr->head(expr) == :else, convert(Vector{QasmExpression}, program_expr.args))
        last_expr = !isnothing(has_else) ? length(program_expr.args) - 1 : length(program_expr.args)
        if condition_value
            for expr in program_expr.args[2:last_expr]
                rt = v(expr)
                rt == :continue && return :continue
                rt == :break && return :break
            end
        elseif !isnothing(has_else)
            for expr in program_expr.args[has_else].args
                rt = v(expr)
                rt == :continue && return :continue
                rt == :break && return :break
            end
        end
    elseif head(program_expr) == :while
        while_v = QasmWhileLoopVisitor(v)
        condition_value = v(program_expr.args[1]) > 0
        loop_body = program_expr.args[2]
        while condition_value
            if head(loop_body) == :scope
                for expr in loop_body.args
                    rt = while_v(expr)
                    rt == :continue && break
                    rt == :break && return v
                end
            else
                while_v(loop_body)
            end
            condition_value = while_v(program_expr.args[1])
        end
    elseif head(program_expr) == :classical_assignment
        op              = program_expr.args[1].args[1]::Symbol
        left_hand_side  = program_expr.args[1].args[2]::QasmExpression
        right_hand_side = program_expr.args[1].args[3]
        var_name  = name(left_hand_side)::String
        right_val = v(right_hand_side)
        left_val  = v(left_hand_side)
        classical_defs(v)[var_name].is_const && throw(QasmVisitorError("cannot reassign value of const variable!"))
        if head(left_hand_side) == :identifier
            var = classical_defs(v)[var_name]
            var_type = var.type
            if var_type isa SizedBitVector && right_val isa AbstractString # bitstring literal
                cleaned_val::String = replace(right_val, "\""=>"")
                bit_right = BitVector(tryparse(Int, "$b") for b in cleaned_val)
                new_val = evaluate_binary_op(op, left_val, bit_right)
            else
                new_val = evaluate_binary_op(op, left_val, right_val)
            end
            var.val = new_val 
        elseif head(left_hand_side) == :indexed_identifier
            inds = v(left_hand_side.args[2])
            var  = classical_defs(v)[var_name]
            var_type = var.type
            if inds isa StepRange && inds.step > 0 && inds.stop < inds.start # -1 in place of end
                new_stop = var.type isa SizedNumber ? v(var.type.size) - 1 : length(var.val) - 1 
                inds = StepRange(inds.start, inds.step, new_stop)
            end
            inds = inds .+ 1
            if var_type isa SizedBitVector && right_val isa AbstractString # bitstring literal
                cleaned_val = replace(right_val, "\""=>"")
                bit_right = BitVector(tryparse(Int, "$b") for b in cleaned_val)
                new_val = evaluate_binary_op(op, left_val, bit_right)
            else
                new_val = evaluate_binary_op(op, left_val, right_val)
            end
            if length(inds) > 1
                var.val[inds] .= new_val 
            else
                var.val[inds] = new_val 
            end
        end
    elseif head(program_expr) == :classical_declaration
        init, var_type = declaration_init(v, program_expr)
        # no initial value
        if head(program_expr.args[2]) == :identifier
            var_name = name(program_expr.args[2])
            v.classical_defs[var_name] = ClassicalVariable(var_name, var_type, init, false)
        elseif head(program_expr.args[2]) == :classical_assignment
            op, left_hand_side, right_hand_side = program_expr.args[2].args[1].args
            var_name = name(left_hand_side)
            v.classical_defs[var_name] = ClassicalVariable(var_name, var_type, init, false)
            v(program_expr.args[2])
        end
    elseif head(program_expr) == :const_declaration
        head(program_expr.args[2]) == :classical_assignment || throw(QasmVisitorError("const declaration must assign an initial value."))
        init, var_type = declaration_init(v, program_expr)
        op, left_hand_side, right_hand_side = program_expr.args[2].args[1].args
        var_name = name(left_hand_side)
        v.classical_defs[var_name] = ClassicalVariable(var_name, var_type, init, false)
        v(program_expr.args[2])
        v.classical_defs[var_name] = ClassicalVariable(var_name, var_type, v.classical_defs[var_name].val, true)
    elseif head(program_expr) == :qubit_declaration
        qubit_name::String = name(program_expr)
        qubit_size::Int = v(program_expr.args[2])::Int
        qubit_defs(v)[qubit_name] = Qubit(qubit_name, qubit_size)
        qubit_mapping(v)[qubit_name] = collect(qubit_count(v) : qubit_count(v) + qubit_size - 1)
        for qubit_i in 0:qubit_size-1
            qubit_mapping(v)["$qubit_name[$qubit_i]"] = [qubit_count(v) + qubit_i]
        end
        v.qubit_count += qubit_size
    elseif head(program_expr) ∈ (:power_mod, :inverse_mod, :control_mod, :negctrl_mod)
        mods = QasmExpression(:modifiers)
        mod_expr, inner = evaluate_modifiers(v, program_expr)
        push!(mods, mod_expr)
        while head(inner) != :gate_call # done
            mod_expr, inner = evaluate_modifiers(v, inner)
            push!(mods, mod_expr)
        end
        push!(inner, mods)
        v(inner)
    elseif head(program_expr) == :gate_call
        gate_name = name(program_expr)
        is_gphase = gate_name == "gphase"
        if is_gphase
            visit_gphase_call(v, program_expr)
        else
            visit_gate_call(v, program_expr)
        end
    elseif head(program_expr) == :box
        foreach(v, program_expr.args)
    elseif head(program_expr) == :gate_definition
        gate_def         = program_expr.args
        gate_name        = name(program_expr)
        gate_arguments   = gate_def[2]::QasmExpression
        gate_def_targets = gate_def[3]::QasmExpression
        gate_body        = gate_def[4]::QasmExpression
        single_argument  = !isempty(gate_arguments.args) && head(gate_arguments.args[1]) == :array_literal
        argument_exprs   = single_argument ? gate_arguments.args[1].args::Vector{Any} : gate_arguments.args::Vector{Any}
        argument_names   = String[arg.args[1] for arg::QasmExpression in argument_exprs]
        single_target    = head(gate_def_targets.args[1]) == :array_literal
        qubit_targets    = single_target ? map(name, gate_def_targets.args[1].args)::Vector{String} : map(name, gate_def_targets.args)::Vector{String}
        v.gate_defs[gate_name] = GateDefinition(gate_name, argument_names, qubit_targets, gate_body)
    elseif head(program_expr) == :function_call
        function_name = name(program_expr)
        if haskey(builtin_functions, function_name)
            concrete_arguments = v(convert(Vector{QasmExpression}, program_expr.args[2].args))
            if function_name != "sizeof"
                return_val = builtin_functions[function_name](Iterators.flatten(concrete_arguments)...)
            else
                return_val = builtin_functions[function_name](concrete_arguments...)
            end
            return return_val[1]
        else
            hasfunction(v, function_name) || throw(QasmVisitorError("function $function_name not defined!"))
            return visit_function_call(v, program_expr, function_name)
        end
    elseif head(program_expr) == :function_definition
        function_def         = program_expr.args
        function_name        = function_def[1].args[1]::String
        function_arguments   = function_def[2]
        function_return_type = function_def[3]::QasmExpression
        function_body        = function_def[4]::QasmExpression
        full_function_def    = FunctionDefinition(function_name, function_arguments, function_body, function_return_type)
        v.function_defs[function_name] = full_function_def
    elseif head(program_expr) == :pragma
        visit_pragma[](v, program_expr)
    elseif head(program_expr) ∈ (:integer_literal, :float_literal, :string_literal, :complex_literal, :irrational_literal, :boolean_literal, :duration_literal)
        return program_expr.args[1]
    elseif head(program_expr) == :array_literal
        return map(v, program_expr.args)
    elseif head(program_expr) == :range
        start::Int, step::Int, stop::Int = v(program_expr.args)
        return StepRange(start, step, stop)
    elseif head(program_expr) == :empty
        return ()
    elseif head(program_expr) == :measure
        qubits_to_measure = evaluate_qubits(v, program_expr.args[1])
        push!(v, CircuitInstruction[(type="measure", arguments=InstructionArgument[], targets=[q], controls=Pair{Int,Int}[], exponent=1.0) for q in qubits_to_measure])
        return false
    elseif head(program_expr) == :hw_qubit
        return tryparse(Int, name(program_expr))
    elseif head(program_expr) == :output
        throw(QasmVisitorError("Output not supported."))
    elseif head(program_expr) == :binary_op
        op  = program_expr.args[1]::Symbol
        lhs = v(program_expr.args[2])
        rhs = v(program_expr.args[3])
        val = evaluate_binary_op(op, lhs, rhs)
        return val
    elseif head(program_expr) == :unary_op
        op  = program_expr.args[1]::Symbol
        arg = v(program_expr.args[2])
        return evaluate_unary_op(op, arg)
    elseif head(program_expr) == :cast
        casting_to = program_expr.args[1].args[1]
        value      = v(program_expr.args[2])
        if casting_to == Bool && !(value isa Period)
            return value isa BitVector ? any(value) : value > 0
        elseif casting_to isa SizedBitVector && !(value isa Period || value isa AbstractFloat)
            new_size = v(casting_to.size)
            return value isa BitVector ? value[1:new_size] : BitVector(reverse(digits(value, base=2, pad=new_size)))
        elseif casting_to isa SizedNumber && !(value isa Period)
            num_value = value isa BitVector ? sum(reverse(value)[k]*2^(k-1) for k=1:length(value)) : value
            if casting_to isa SizedInt
                return Int(num_value)
            elseif casting_to isa SizedUInt
                return UInt(num_value)
            elseif casting_to isa SizedFloat
                return Float64(num_value)
            end
        else
            throw(QasmVisitorError("unable to evaluate cast expression $program_expr"))
        end
    else
        throw(QasmVisitorError("cannot visit expression $program_expr.")) 
    end
    return v
end

