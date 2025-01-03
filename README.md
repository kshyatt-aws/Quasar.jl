# Quasar

[![](https://img.shields.io/badge/docs-stable-blue.svg)](https://kshyatt-aws.github.io/Quasar.jl/stable)
[![](https://img.shields.io/badge/docs-dev-blue.svg)](https://kshyatt-aws.github.io/Quasar.jl/dev)
[![Build Status](https://github.com/kshyatt-aws/Quasar.jl/actions/workflows/CI.yml/badge.svg?branch=main)](https://github.com/kshyatt-aws/Quasar.jl/actions/workflows/CI.yml?query=branch%3Amain)
[![codecov](https://codecov.io/gh/kshyatt-aws/Quasar.jl/graph/badge.svg?token=LP1993TMXD)](https://codecov.io/gh/kshyatt-aws/Quasar.jl)
[![Aqua QA](https://raw.githubusercontent.com/JuliaTesting/Aqua.jl/master/badge.svg)](https://github.com/JuliaTesting/Aqua.jl)

`Quasar.jl`, the Qu(antum) as(sembly) (lex/pars)er, is a package for lexing and parsing the [OpenQASM quantum assembly IR](https://openqasm.com/).

`Quasar.jl` works in a two-step fashion: first, it uses a tokenizer to generate an [abstract syntax tree (AST)](https://en.wikipedia.org/wiki/Abstract_syntax_tree) representing the
quantum program, which is a nested structure of `QasmExpression`s.
Then, it walks (visits) the AST to evaluate all loops, conditionals, gate calls, and variable declarations.

`Quasar` has support for:

- Basic builtin OpenQASM 3 gates `gphase` and `U`
- Custom gate definitions
- `for` and `while` loops
- `if`, `else`, and `switch` conditional statements
- Builtin OpenQASM3 functions
- Function definitions and calls
- Casting classical types
- Timing statements `barrier` and `delay`
- Pragmas (see below)

What is *not* yet supported:
- `angle` types
- Annotations
- OpenPulse

If you need any of these features, feel free to open an issue requesting support for them!

## Quick Start

For more extensive examples, see the tests in `test/`. Here, we can parse and visit a simple OpenQASM 3.0 program to generate a Bell circuit:

```julia
qasm = """OPENQASM 3.0;
gate h a { U(π/2, 0, π) a; gphase(-π/4);};
gate x a { U(π, 0, π) a; gphase(-π/2);};
gate cx a, b { ctrl @ x a, b; };

def bell(qubit q0, qubit q1) {
    h q0;
    cx q0, q1;
}
qubit[2] q;
bell(q[0], q[1]);
bit[2] b = "00";
b[0] = measure q[0];
b[1] = measure q[1];
"""

parsed = Quasar.parse_qasm(qasm)
visitor = Quasar.QasmProgramVisitor()
visitor(parsed)
```

You can supply [inputs](https://openqasm.com/language/directives.html#input-output) to your program
by creating the `QasmProgramVisitor` with a `Dict{String, Any}` containing the names of the input variables
as the keys and their values as the `Dict`'s values. This allows you to re-use the same AST multiple times
for different inputs.

Once the visitor has walked the AST, its `instructions` field contains `CircuitInstruction` `NamedTuple`s you can
use to construct your own circuit instruction types. These tuples have fields:

- `type::String` - the name of the instruction, e.g. `"measure"` or `"cx"`
- `arguments` - any arguments the instruction accepts, such as an angle for an `rx` gate or a `Period` for a `duration` 
- `targets::Vector{Int}` - the qubits targeted by this instruction. This should include any control qubits. 
- `controls::Vector{Pair{Int,Int}}` - any controls (including `negctrl`) applied to the instruction. The first item in the pair is the qubit, the second is the value controlled upon, so that `2=>0` represents a `negctrl` on qubit 2.
- `exponent::Float64` - the `pow` modifier applied to the instruction, if any

You can use a package such as [`StructTypes.jl`](https://github.com/JuliaData/StructTypes.jl) to build your circuits and programs from these named tuples.

In the above example, the `h`, `x`, and `cx` were defined in terms of the built-in gates `U` and `gphase`. In many cases, you may have an implementation of these
gates that is simpler or more efficient. In that case, you can supply your own gate definition file using the [`include` instruction](https://openqasm.com/language/comments.html#included-files), or provide a *function* to generate a dictionary of builtin gates to `Quasar.jl`:

```julia
using Quasar

function my_builtin_generator()
    ...
end

Quasar.builtin_gates[] = my_builtin_generator
```

A generator function is used here in order to allow visitors to overwrite builtin functions in certain scopes without corrupting the reference definition. If you're writing a package which uses Quasar, the `Quasar.builtin_gates[] = my_builtin_generator` should be placed in your main module's `__init__` function. 
