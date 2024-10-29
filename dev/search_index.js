var documenterSearchIndex = {"docs":
[{"location":"gates/#custom_gates","page":"Supplying built-in gates","title":"Defining your own gate sets","text":"","category":"section"},{"location":"gates/","page":"Supplying built-in gates","title":"Supplying built-in gates","text":"OpenQASM3 has two built-in gates – U and gphase. Combined with gate modifiers, these two are enough to define a universal gate set for quantum computation.","category":"page"},{"location":"gates/","page":"Supplying built-in gates","title":"Supplying built-in gates","text":"OpenQASM3 also offers a standard library of gates, all defined in terms of the two built-in gates, in stdgates.inc. Quasar.jl provides a copy of stdgates.inc in its test/ directory, which you can import with:","category":"page"},{"location":"gates/","page":"Supplying built-in gates","title":"Supplying built-in gates","text":"include \"stdgates.inc\";","category":"page"},{"location":"gates/","page":"Supplying built-in gates","title":"Supplying built-in gates","text":"However, many quantum computers and simulators have different built-in gates, so-called \"native gates\", which they can implement without reference to gphase or U. You can provide your own built-in gate set to Quasar.jl to ensure that the output circuit instructions are in your \"gate dialect\".","category":"page"},{"location":"gates/","page":"Supplying built-in gates","title":"Supplying built-in gates","text":"In Quasar.jl, you can create a function which takes no arguments and returns a Dict{String, BuiltinGateDefinition}. The keys are the names of your built-in gates (e.g. \"msaa\") and a BuiltinGateDefinition has the following fields:","category":"page"},{"location":"gates/","page":"Supplying built-in gates","title":"Supplying built-in gates","text":"name::String – the name of the gate\narguments::Vector{String} – the names of the gate's arguments, if any (can be empty)\nqubit_targets::Vector{String} – the names of the gate's target(s).\nbody::CircuitInstruction – the single CircuitInstruction which defines the gate","category":"page"},{"location":"gates/","page":"Supplying built-in gates","title":"Supplying built-in gates","text":"So, for example, if the Pauli X gate x and the X rotation gate Rx are built-in gates, you could create the function:","category":"page"},{"location":"gates/","page":"Supplying built-in gates","title":"Supplying built-in gates","text":"my_builtin_gates() = Dict(\n    \"x\"=>BuiltinGateDefinition(\"x\", String[], [\"a\"], (type=\"x\", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)), \n    \"rx\"=>BuiltinGateDefinition(\"rx\", [\"θ\"], [\"a\"], (type=\"rx\", arguments=InstructionArgument[θ], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)),\n)","category":"page"},{"location":"gates/","page":"Supplying built-in gates","title":"Supplying built-in gates","text":"You can provide this definition to Quasar.jl by updating the reference builtin_gates:","category":"page"},{"location":"gates/","page":"Supplying built-in gates","title":"Supplying built-in gates","text":"using Quasar\nQuasar.builtin_gates[] = my_builtin_gates","category":"page"},{"location":"pragmas/#Handling-pragmas","page":"Handling pragmas","title":"Handling pragmas","text":"","category":"section"},{"location":"pragmas/","page":"Handling pragmas","title":"Handling pragmas","text":"OpenQASM3 allows you to define pragmas, which can be used to support operations like results besides measurements (e.g. expectation values), noise channels, or compiler directives.","category":"page"},{"location":"pragmas/","page":"Handling pragmas","title":"Handling pragmas","text":"Since pragmas are by nature specific to the application, Quasar.jl doesn't provide support for parsing or visiting them. However, you can implement support for this in order to handle your specific pragmas. This will teach Quasar.jl how to incorporate your pragma instructions into the AST. You may want to use pragmas to support custom circuit operations (like noise channels) or result types (like expectation values).","category":"page"},{"location":"pragmas/","page":"Handling pragmas","title":"Handling pragmas","text":"You'll need to implement two functions: parse_pragma and visit_pragma.","category":"page"},{"location":"pragmas/#Parsing-pragmas-to-QasmExpressions","page":"Handling pragmas","title":"Parsing pragmas to QasmExpressions","text":"","category":"section"},{"location":"pragmas/","page":"Handling pragmas","title":"Handling pragmas","text":"parse_pragma should accept four arguments:","category":"page"},{"location":"pragmas/","page":"Handling pragmas","title":"Handling pragmas","text":"tokens – the list of tokens generated from tokenizing the raw OpenQASM 3.0 string\nstack – the current AST stack (used for error reporting)\nstart – the start location for the beginning of this pragma (used for error reporting)\nqasm – the raw QASM string (used for error reporting)","category":"page"},{"location":"pragmas/","page":"Handling pragmas","title":"Handling pragmas","text":"The tokens is a list of 3-tuples: the first item is an Int64 indicating the location in the qasm string of the start of the token, the second is an Int32 indicating the length of the token, and the third is a Token enum indicating the type of the token (e.g., a semicolon or an identifier).","category":"page"},{"location":"pragmas/","page":"Handling pragmas","title":"Handling pragmas","text":"The list of tokens provided to parse_pragma does not contain the leading pragma token, and contains only the tokens until the next newline. You're responsible for then walking through the line and generating appropriate QasmExpressions you'll visit later in visit_pragma. Your parse_pragma function should return a QasmExpression that has :pragma as its head. A skeleton of the function might be:","category":"page"},{"location":"pragmas/","page":"Handling pragmas","title":"Handling pragmas","text":"function my_parse_pragma(tokens, stack, start, qasm)\n    pragma_expr = QasmExpression(:pragma)\n    while !isempty(tokens)\n        inner_expr = # do some lexing in here\n        push!(pragma_expr, inner_expr)\n    end\n    return pragma_expr\nend","category":"page"},{"location":"pragmas/","page":"Handling pragmas","title":"Handling pragmas","text":"If the submitted QASM has an invalid pragma, you can throw a Quasar.QasmParseError like so:","category":"page"},{"location":"pragmas/","page":"Handling pragmas","title":"Handling pragmas","text":"your_error_message = \"Oh no! A sample error message!\"\nthrow(Quasar.QasmParseError(your_error_message, stack, start, qasm))","category":"page"},{"location":"pragmas/#Visiting-the-parsed-pragmas","page":"Handling pragmas","title":"Visiting the parsed pragmas","text":"","category":"section"},{"location":"pragmas/","page":"Handling pragmas","title":"Handling pragmas","text":"Once you've generated the subtree(s) for your pragma(s), you can then visit them to generate your final instructions and results (if any) for your QASM file. When Quasar.jl's visitor encounters a QasmExpression with a head of :pragma, it will call visit_pragma[](v::AbstractVisitor, expr::QasmExpression). You can then dispatch on the type of pragma, if you support multiple. You can then push CircuitInstructions and CircuitResults to the visitor v.","category":"page"},{"location":"pragmas/#Loading-your-functions-into-Quasar.jl","page":"Handling pragmas","title":"Loading your functions into Quasar.jl","text":"","category":"section"},{"location":"pragmas/","page":"Handling pragmas","title":"Handling pragmas","text":"Similar to custom gate sets, you can let Quasar.jl know about these at module initialization time:","category":"page"},{"location":"pragmas/","page":"Handling pragmas","title":"Handling pragmas","text":"function __init__()\n    Quasar.parse_pragma[] = my_parse_pragma\n    Quasar.vist_pragma[]  = my_visit_pragma\nend","category":"page"},{"location":"","page":"Home","title":"Home","text":"DocTestSetup = quote using Quasar end\nCurrentModule = Quasar","category":"page"},{"location":"#Quasar","page":"Home","title":"Quasar","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"Quasar.jl, the Qu(antum) as(sembly) (lex/pars)er, is a package for lexing and parsing the OpenQASM quantum assembly IR.","category":"page"},{"location":"","page":"Home","title":"Home","text":"Quasar works in a two-step fashion: first, it uses a tokenizer to generate an abstract syntax tree (AST) representing the quantum program, which is a nested structure of QasmExpressions. Then, it walks (visits) the AST to evaluate all loops, conditionals, gate calls, and variable declarations.","category":"page"},{"location":"","page":"Home","title":"Home","text":"Quasar has support for:","category":"page"},{"location":"","page":"Home","title":"Home","text":"Basic builtin OpenQASM 3 gates gphase and U\nCustom gate definitions\nfor and while loops\nif, else, and switch conditional statements\nBuiltin OpenQASM3 functions\nFunction definitions and calls\nCasting classical types\nTiming statements barrier and delay\nPragmas","category":"page"},{"location":"","page":"Home","title":"Home","text":"What is not yet supported:","category":"page"},{"location":"","page":"Home","title":"Home","text":"angle types\nAnnotations\nOpenPulse","category":"page"},{"location":"","page":"Home","title":"Home","text":"If you need any of these features, feel free to open an issue requesting support for them!","category":"page"},{"location":"#Quick-Start","page":"Home","title":"Quick Start","text":"","category":"section"},{"location":"","page":"Home","title":"Home","text":"For more extensive examples, see the tests in test/. Here, we can parse and visit a simple OpenQASM 3.0 program to generate a Bell circuit:","category":"page"},{"location":"","page":"Home","title":"Home","text":"julia> qasm = \"\"\"OPENQASM 3.0;\\ngate h a { U(π/2, 0, π) a; gphase(-π/4);};\\ngate x a { U(π, 0, π) a; gphase(-π/2);};\\ngate cx a, b { ctrl @ x a, b; };\\ndef bell(qubit q0, qubit q1) {\\n    h q0;\\n    cx q0, q1;\\n}\\nqubit[2] q;\\nbell(q[0], q[1]);\\nbit[2] b = \"00\";\\nb[0] = measure q[0];\\nb[1] = measure q[1];\\n\"\"\";\n\njulia> parsed = Quasar.parse_qasm(qasm);\n\njulia> visitor = Quasar.QasmProgramVisitor();\n\njulia> visitor(parsed);","category":"page"},{"location":"","page":"Home","title":"Home","text":"You can supply inputs to your program by creating the QasmProgramVisitor with a Dict{String, Any} containing the names of the input variables as the keys and their values as the Dict's values. This allows you to re-use the same AST multiple times for different inputs.","category":"page"},{"location":"","page":"Home","title":"Home","text":"Once the visitor has walked the AST, its instructions field contains CircuitInstruction NamedTuples you can use to construct your own circuit instruction types. These tuples have fields:","category":"page"},{"location":"","page":"Home","title":"Home","text":"type::String - the name of the instruction, e.g. \"measure\" or \"cx\"\narguments - any arguments the instruction accepts, such as an angle for an rx gate or a Period for a duration \ntargets::Vector{Int} - the qubits targeted by this instruction. This should include any control qubits. \ncontrols::Vector{Pair{Int,Int}} - any controls (including negctrl) applied to the instruction. The first item in the pair is the qubit, the second is the value controlled upon, so that 2=>0 represents a negctrl on qubit 2.\nexponent::Float64 - the pow modifier applied to the instruction, if any","category":"page"},{"location":"","page":"Home","title":"Home","text":"You can use a package such as StructTypes.jl to build your circuits and programs from these named tuples.","category":"page"},{"location":"","page":"Home","title":"Home","text":"In the above example, the h, x, and cx were defined in terms of the built-in gates U and gphase. In many cases, you may have an implementation of these gates that is simpler or more efficient. In that case, you can supply your own gate definition file using the include instruction, or provide a function to generate a dictionary of builtin gates to Quasar.jl:","category":"page"},{"location":"","page":"Home","title":"Home","text":"using Quasar\n\nfunction my_builtin_generator()\n    ...\nend\n\nQuasar.builtin_gates[] = my_builtin_generator","category":"page"},{"location":"","page":"Home","title":"Home","text":"A generator function is used here in order to allow visitors to overwrite builtin functions in certain scopes without corrupting the reference definition. If you're writing a package which uses Quasar, the Quasar.builtin_gates[] = my_builtin_generator should be placed in your main module's __init__ function. ","category":"page"},{"location":"internals/#Quasar.jl-internals","page":"Internals","title":"Quasar.jl internals","text":"","category":"section"},{"location":"internals/","page":"Internals","title":"Internals","text":"Here we document some of the internal types useful for developers and those extending Quasar.jl with custom gate sets and pragmas.","category":"page"},{"location":"internals/","page":"Internals","title":"Internals","text":"Quasar.InstructionArgument\nQuasar.CircuitInstruction\nQuasar.CircuitResult","category":"page"},{"location":"internals/#Quasar.InstructionArgument","page":"Internals","title":"Quasar.InstructionArgument","text":"InstructionArgument\n\nUnion type of Symbol, Dates.Period, Real, Matrix{ComplexF64} which represents a possible argument to a CircuitInstruction.\n\n\n\n\n\n","category":"type"},{"location":"internals/#Quasar.CircuitInstruction","page":"Internals","title":"Quasar.CircuitInstruction","text":"CircuitInstruction\n\nA NamedTuple representing a circuit instruction (a gate, a noise operation, a timing operation). Fields are:\n\ntype::String - the name of the operation (e.g. rx)\narguments::Vector{InstructionArgument} - arguments, if any, to the operation (e.g. π for an angled gate)\ntargets::Vector{Int} - qubit targets for the operation, including control qubits, if any\ncontrols::Vector{Pair{Int, Int}} - control qubits and bit-values for the operation, so that a ctrl @ x 0, 2; could have controls = [0=>1] and negctrl @ x 0, 2; could have controls = [0=>0].\nexponent::Float64 - exponent to which the operation is raised, if any\n\nCircuitInstructions can be used with a package like StructTypes.jl to build the actual types of your package from these NamedTuples.\n\n\n\n\n\n","category":"type"},{"location":"internals/#Quasar.CircuitResult","page":"Internals","title":"Quasar.CircuitResult","text":"CircuitResult\n\nA NamedTuple representing a circuit result (e.g. an expectation value). Fields are:\n\ntype::Symbol - the name of the result type (e.g. :variance)\noperator::Vector{Union{String, Matrix{ComplexF64}}} - the operator to measure, if applicable\ntargets::Vector{Int} - qubit targets for the result\nstates::Vector{String} - vector of bitstrings to measure the amplitudes of, if the result type is an :amplitude \n\nCircuitResults can be used with a package like StructTypes.jl to build the actual result objects of your package from these NamedTuples.\n\n\n\n\n\n","category":"type"}]
}
