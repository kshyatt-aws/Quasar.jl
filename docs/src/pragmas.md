# Handling pragmas

OpenQASM3 allows you to define [pragmas](https://openqasm.com/language/directives.html#pragmas), which can be used to
support operations like results besides measurements (e.g. expectation values), noise channels, or compiler directives.

Since pragmas are by nature specific to the application, `Quasar.jl` doesn't provide support for parsing or visiting them.
However, you can implement support for this in order to handle your specific pragmas. This will
teach `Quasar.jl` how to incorporate your `pragma` instructions into the AST. You may want to use `pragma`s to support custom circuit operations (like noise channels) or result types (like expectation values).

You'll need to implement two functions: `parse_pragma` and `visit_pragma`.

## Parsing pragmas to `QasmExpression`s

`parse_pragma` should accept four arguments:

- `tokens` -- the list of tokens generated from tokenizing the raw OpenQASM 3.0 string
- `stack` -- the current AST stack (used for error reporting)
- `start` -- the start location for the beginning of this pragma (used for error reporting)
- `qasm` -- the raw QASM string (used for error reporting)

The `tokens` is a list of 3-tuples: the first item is an `Int64` indicating the location in the `qasm` string of the *start* of the token, the second is an `Int32` indicating the *length* of the token, and the third is a `Token` enum indicating the type of the token (e.g., a `semicolon` or an `identifier`).

The list of tokens provided to `parse_pragma` **does not** contain the leading `pragma` token, and contains **only** the tokens until the next newline. You're responsible for then walking through the line and generating appropriate `QasmExpression`s you'll visit later in `visit_pragma`. Your `parse_pragma` function should return a `QasmExpression` that has `:pragma` as its head. A skeleton of the function might be:

```julia
function my_parse_pragma(tokens, stack, start, qasm)
    pragma_expr = QasmExpression(:pragma)
    while !isempty(tokens)
        inner_expr = # do some lexing in here
        push!(pragma_expr, inner_expr)
    end
    return pragma_expr
end
```

If the submitted QASM has an invalid pragma, you can throw a `Quasar.QasmParseError` like so:

```julia
your_error_message = "Oh no! A sample error message!"
throw(Quasar.QasmParseError(your_error_message, stack, start, qasm))
```

## Visiting the parsed pragmas

Once you've generated the subtree(s) for your pragma(s), you can then visit them to generate your final instructions and results (if any) for your QASM file. When `Quasar.jl`'s visitor encounters a `QasmExpression` with a head of `:pragma`, it will call `visit_pragma[](v::AbstractVisitor, expr::QasmExpression)`. You can then dispatch on the type of pragma, if you support multiple. You can then push [`CircuitInstruction`](@ref Quasar.CircuitInstruction)s and [`CircuitResult`](@ref Quasar.CircuitResult)s to the visitor `v`.

## Loading your functions into `Quasar.jl`

Similar to [custom gate sets](@ref custom_gates), you can
let `Quasar.jl` know about these at module initialization time:

```julia
function __init__()
    Quasar.parse_pragma[] = my_parse_pragma
    Quasar.vist_pragma[]  = my_visit_pragma
end
```


