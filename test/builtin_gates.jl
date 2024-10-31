# OpenQASM 3 Braket Standard Gates
complex_builtin_gates() = Dict{String, Quasar.BuiltinGateDefinition}(
    # identity gate
    "i"=>Quasar.BuiltinGateDefinition("i", String[], ["a"], (type="i", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)),
    # phase gate
    "phaseshift"=>Quasar.BuiltinGateDefinition("phaseshift", ["λ"], ["a"], (type="phaseshift", arguments=InstructionArgument[:λ], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)),
    # pauli X gate
    "x"=>Quasar.BuiltinGateDefinition("x", String[], ["a"], (type="x", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)),
    # pauli Y gate
    "y"=>Quasar.BuiltinGateDefinition("y", String[], ["a"], (type="y", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)),
    # pauli Z gate
    "z"=>Quasar.BuiltinGateDefinition("z", String[], ["a"], (type="z", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)),
    # Hadamard gate
    "h"=>Quasar.BuiltinGateDefinition("h", String[], ["a"], (type="h", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)),
    # S gate
    "s"=>Quasar.BuiltinGateDefinition("s", String[], ["a"], (type="s", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)),
    # Si gate
    "si"=>Quasar.BuiltinGateDefinition("si", String[], ["a"], (type="si", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)),
    # T gate
    "t"=>Quasar.BuiltinGateDefinition("t", String[], ["a"], (type="t", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)),
    # Ti gate
    "ti"=>Quasar.BuiltinGateDefinition("ti", String[], ["a"], (type="ti", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)),
    # V gate
    "v"=>Quasar.BuiltinGateDefinition("v", String[], ["a"], (type="v", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)),
    # Vi gate
    "vi"=>Quasar.BuiltinGateDefinition("vi", String[], ["a"], (type="vi", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)),
    # RotX gate
    "rx"=>Quasar.BuiltinGateDefinition("rx", ["θ"], ["a"], (type="rx", arguments=InstructionArgument[:θ], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)),
    # RotY gate
    "ry"=>Quasar.BuiltinGateDefinition("ry", ["θ"], ["a"], (type="ry", arguments=InstructionArgument[:θ], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)),
    # RotZ gate
    "rz"=>Quasar.BuiltinGateDefinition("rz", ["θ"], ["a"], (type="rz", arguments=InstructionArgument[:θ], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)),
    # CNot gate
    "cnot"=>Quasar.BuiltinGateDefinition("cnot", String[], ["a", "b"], (type="cnot", arguments=InstructionArgument[], targets=[0, 1], controls=Pair{Int,Int}[], exponent=1.0)),
    # CY gate
    "cy"=>Quasar.BuiltinGateDefinition("cy", String[], ["a", "b"], (type="cy", arguments=InstructionArgument[], targets=[0, 1], controls=Pair{Int,Int}[], exponent=1.0)),
    # CZ gate
    "cz"=>Quasar.BuiltinGateDefinition("cz", String[], ["a", "b"], (type="cz", arguments=InstructionArgument[], targets=[0, 1], controls=Pair{Int,Int}[], exponent=1.0)),
    # CV gate
    "cv"=>Quasar.BuiltinGateDefinition("cv", String[], ["a", "b"], (type="cv", arguments=InstructionArgument[], targets=[0, 1], controls=Pair{Int,Int}[], exponent=1.0)),
    # controlled-phase
    "cphaseshift"=>Quasar.BuiltinGateDefinition("cphaseshift", ["λ"], ["a", "b"], (type="cphaseshift", arguments=InstructionArgument[:λ], targets=[0, 1], controls=Pair{Int,Int}[], exponent=1.0)),
    # controlled-phase-00
    "cphaseshift00"=>Quasar.BuiltinGateDefinition("cphaseshift00", ["λ"], ["a", "b"], (type="cphaseshift00", arguments=InstructionArgument[:λ], targets=[0, 1], controls=Pair{Int,Int}[], exponent=1.0)),
    # controlled-phase-01
    "cphaseshift01"=>Quasar.BuiltinGateDefinition("cphaseshift01", ["λ"], ["a", "b"], (type="cphaseshift01", arguments=InstructionArgument[:λ], targets=[0, 1], controls=Pair{Int,Int}[], exponent=1.0)),
    # controlled-phase-10
    "cphaseshift10"=>Quasar.BuiltinGateDefinition("cphaseshift10", ["λ"], ["a", "b"], (type="cphaseshift10", arguments=InstructionArgument[:λ], targets=[0, 1], controls=Pair{Int,Int}[], exponent=1.0)),
    # Swap gate
    "swap"=>Quasar.BuiltinGateDefinition("swap", String[], ["a", "b"], (type="swap", arguments=InstructionArgument[], targets=[0, 1], controls=Pair{Int,Int}[], exponent=1.0)),
    # ISwap gate
    "iswap"=>Quasar.BuiltinGateDefinition("iswap", String[], ["a", "b"], (type="iswap", arguments=InstructionArgument[], targets=[0, 1], controls=Pair{Int,Int}[], exponent=1.0)),
    # ISwap gate
    "pswap"=>Quasar.BuiltinGateDefinition("pswap", ["θ"], ["a", "b"], (type="pswap", arguments=InstructionArgument[:θ], targets=[0, 1], controls=Pair{Int,Int}[], exponent=1.0)),
    # controlled-swap gate
    "cswap"=>Quasar.BuiltinGateDefinition("cswap", String[], ["a", "b", "c"], (type="cswap", arguments=InstructionArgument[], targets=[0, 1, 2], controls=Pair{Int,Int}[], exponent=1.0)),
    # ccnot/Toffoli gate
    "ccnot"=>Quasar.BuiltinGateDefinition("ccnot", String[], ["a", "b", "c"], (type="ccnot", arguments=InstructionArgument[], targets=[0, 1, 2], controls=Pair{Int,Int}[], exponent=1.0)),
    # XX gate
    "xx"=>Quasar.BuiltinGateDefinition("xx", ["θ"], ["a", "b"], (type="xx", arguments=InstructionArgument[:θ], targets=[0, 1], controls=Pair{Int,Int}[], exponent=1.0)),
    # XY gate
    "xy"=>Quasar.BuiltinGateDefinition("xy", ["θ"], ["a", "b"], (type="xy", arguments=InstructionArgument[:θ], targets=[0, 1], controls=Pair{Int,Int}[], exponent=1.0)),
    # YY gate
    "yy"=>Quasar.BuiltinGateDefinition("yy", ["θ"], ["a", "b"], (type="yy", arguments=InstructionArgument[:θ], targets=[0, 1], controls=Pair{Int,Int}[], exponent=1.0)),
    # ZZ gate
    "zz"=>Quasar.BuiltinGateDefinition("zz", ["θ"], ["a", "b"], (type="zz", arguments=InstructionArgument[:θ], targets=[0, 1], controls=Pair{Int,Int}[], exponent=1.0)),
    # ECR gate
    "ecr"=>Quasar.BuiltinGateDefinition("ecr", String[], ["a", "b"], (type="ecr", arguments=InstructionArgument[], targets=[0, 1], controls=Pair{Int,Int}[], exponent=1.0)),
    # MS gate
    "ms"=>Quasar.BuiltinGateDefinition("ms", ["ϕ", "θ", "λ"], ["a", "b"], (type="ms", arguments=InstructionArgument[:ϕ, :θ, :λ], targets=[0, 1], controls=Pair{Int,Int}[], exponent=1.0)),
    # GPi gate
    "gpi"=>Quasar.BuiltinGateDefinition("gpi", ["θ"], ["a"], (type="gpi", arguments=InstructionArgument[:θ], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)),
    # GPi2 gate
    "gpi2"=>Quasar.BuiltinGateDefinition("gpi2", ["θ"], ["a"], (type="gpi2", arguments=InstructionArgument[:θ], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)),
    # PRx gate
    "prx"=>Quasar.BuiltinGateDefinition("prx", ["θ", "ϕ"], ["a"], (type="prx", arguments=InstructionArgument[:θ, :ϕ], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)),
    # 3-angle U gate
    "U"=>Quasar.BuiltinGateDefinition("U", ["θ", "ϕ", "λ"], ["a"], (type="u", arguments=InstructionArgument[:θ, :ϕ, :λ], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)),
)