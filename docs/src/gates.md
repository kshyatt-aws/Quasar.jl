# [Defining your own gate sets](@id custom_gates)

OpenQASM3 has [two built-in gates](https://openqasm.com/language/gates.html#built-in-gates) -- `U` and `gphase`. Combined with [gate modifiers](https://openqasm.com/language/gates.html#quantum-gate-modifiers), these two are enough to define a universal gate set for quantum computation.

OpenQASM3 also offers a [standard library of gates](https://openqasm.com/language/standard_library.html), all defined in terms of the two built-in gates, in `stdgates.inc`. `Quasar.jl` provides a copy of `stdgates.inc` in its `test/` directory, which you can import with:

```
include "stdgates.inc";
```

However, many quantum computers and simulators have different built-in gates, so-called "native gates", which they can implement without reference to `gphase` or `U`.
You can provide your own built-in gate set to `Quasar.jl` to ensure that the output circuit instructions are in your "gate dialect".

In `Quasar.jl`, you can create a **function** which takes no arguments and returns a `Dict{String, BuiltinGateDefinition}`. The keys are the names of your built-in gates (e.g. `"msaa"`) and a `BuiltinGateDefinition` has the following fields:

- `name::String` -- the name of the gate
- `arguments::Vector{String}` -- the names of the gate's arguments, if any (can be empty)
- `qubit_targets::Vector{String}` -- the names of the gate's target(s).
- `body::CircuitInstruction` -- the **single** `CircuitInstruction` which defines the gate

So, for example, if the Pauli X gate `x` and the X rotation gate `Rx` are built-in gates, you could create the function:

```julia
my_builtin_gates() = Dict(
    "x"=>BuiltinGateDefinition("x", String[], ["a"], (type="x", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)), 
    "rx"=>BuiltinGateDefinition("rx", ["θ"], ["a"], (type="rx", arguments=InstructionArgument[θ], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)),
)
```

You can provide this definition to `Quasar.jl` by updating the reference `builtin_gates`:

```julia
using Quasar
Quasar.builtin_gates[] = my_builtin_gates
```
