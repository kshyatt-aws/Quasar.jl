struct QasmExpression
    head::Symbol
    args::Vector{Any}
    QasmExpression(head::Symbol, args::Vector) = new(head, args)
end
QasmExpression(head) = QasmExpression(head, [])
QasmExpression(head, @nospecialize(args...)) = QasmExpression(head, collect(args))
QasmExpression(head, arg) = QasmExpression(head, [arg])

Base.show(io::IO, qasm_expr::QasmExpression) = print_tree(io, qasm_expr, maxdepth=10)
Base.iterate(qasm_expr::QasmExpression) = (qasm_expr, nothing)
Base.iterate(qasm_expr::QasmExpression, ::Nothing) = nothing
Base.length(qasm_expr::QasmExpression) = 1
Base.push!(qasm_expr::QasmExpression, arg) = push!(qasm_expr.args, arg)
Base.append!(qasm_expr::QasmExpression, arg::QasmExpression) = push!(qasm_expr.args, arg)
Base.append!(qasm_expr::QasmExpression, args::Vector) = append!(qasm_expr.args, args)
Base.pop!(qasm_expr::QasmExpression) = pop!(qasm_expr.args)
Base.copy(qasm_expr::QasmExpression) = QasmExpression(qasm_expr.head, deepcopy(qasm_expr.args))

head(qasm_expr::QasmExpression) = qasm_expr.head

AbstractTrees.children(qasm_expr::QasmExpression) = qasm_expr.args
AbstractTrees.printnode(io::IO, qasm_expr::QasmExpression) = print(io, "QasmExpression :$(qasm_expr.head)")

function Base.:(==)(qasm_a::QasmExpression, qasm_b::QasmExpression)
    a_children = children(qasm_a)
    b_children = children(qasm_b)
    length(a_children) != length(b_children) && return false
    return a_children == b_children
end

