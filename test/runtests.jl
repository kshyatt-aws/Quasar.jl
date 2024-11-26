using Quasar
using Quasar: InstructionArgument
using Test
using Aqua, Dates, Statistics, LinearAlgebra

Aqua.test_all(Quasar)

include("builtin_gates.jl")
Quasar.builtin_gates[] = complex_builtin_gates

@testset "Quasar.jl" begin
    @testset "QasmExpression" begin
        @testset "Printing" begin
            expr = Quasar.QasmExpression(:head, Quasar.QasmExpression(:integer_literal, 2))
            @test sprint(show, expr) == "QasmExpression :head\n└─ QasmExpression :integer_literal\n   └─ 2\n"
            bad_expression = Quasar.QasmExpression(:invalid_expression, Quasar.QasmExpression(:error))
            err = Quasar.QasmVisitorError("unable to evaluate expression $bad_expression")
            @test sprint(showerror, err) == "QasmVisitorError: unable to evaluate expression $bad_expression"
        end
        @testset "Expression basics" begin
            expr = Quasar.QasmExpression(:head, Quasar.QasmExpression(:integer_literal, 2))
            @test length(expr) == 1
            push!(expr, Quasar.QasmExpression(:integer_literal, 3))
            @test length(expr.args) == 2
            append!(expr, Quasar.QasmExpression(:integer_literal, 4))
            @test length(expr.args) == 3 
            append!(expr, [Quasar.QasmExpression(:integer_literal, 5), Quasar.QasmExpression(:integer_literal, 6)])
            @test length(expr.args) == 5
        end
        @testset "Equality" begin
            expr_a = Quasar.QasmExpression(:head, Quasar.QasmExpression(:integer_literal, 2))
            expr_b = Quasar.QasmExpression(:head, Quasar.QasmExpression(:float_literal, 2.2))
            @test expr_a != expr_b
            expr_a = Quasar.QasmExpression(:head, Quasar.QasmExpression(:integer_literal, 2))
            @test expr_a == copy(expr_a) 
        end
        @testset "Name undefined" begin
            expr = Quasar.QasmExpression(:undef, Quasar.QasmExpression(:integer_literal, 2))
            @test_throws Quasar.QasmVisitorError("name not defined for expressions of type undef") Quasar.name(expr)
        end
    end
    @testset "Visiting/evaluating an invalid expression" begin
        visitor = Quasar.QasmProgramVisitor()
        bad_expression = Quasar.QasmExpression(:invalid_expression, Quasar.QasmExpression(:error))
        @test_throws Quasar.QasmVisitorError("cannot visit expression $bad_expression.") visitor(bad_expression)
        bad_expression = Quasar.QasmExpression(:invalid_expression, Quasar.QasmExpression(:error))
        @test_throws Quasar.QasmVisitorError("unable to evaluate qubits for expression $bad_expression.") Quasar.evaluate_qubits(visitor, bad_expression)
        cast_expression = Quasar.QasmExpression(:cast, Quasar.QasmExpression(:type_expr, Int32), Quasar.QasmExpression(:float_literal, 2.0))
        @test_throws Quasar.QasmVisitorError("unable to evaluate cast expression $cast_expression") visitor(cast_expression)
    end
    @testset "Visitors and parents" begin
        qasm = """
               def flip(qubit q) {
                 x q;
               }
               qubit[2] qs;
               flip(qs[0]);
               """
        parsed = parse_qasm(qasm)
        visitor = Quasar.QasmProgramVisitor()
        visitor(parsed)
        for_visitor = Quasar.QasmForLoopVisitor(visitor)
        @test Quasar.function_defs(for_visitor) == Quasar.function_defs(visitor)
        @test Quasar.qubit_defs(for_visitor)    == Quasar.qubit_defs(visitor)
        @test Quasar.qubit_count(for_visitor)   == Quasar.qubit_count(visitor)
        push!(for_visitor, (type=:amplitude, operator=Union{String,Matrix{ComplexF64}}[], targets=Int[], states=["00"]))
        @test visitor.results[1] == (type=:amplitude, operator=Union{String,Matrix{ComplexF64}}[], targets=Int[], states=["00"])
        push!(for_visitor, [(type=:state_vector, operator=Union{String,Matrix{ComplexF64}}[], targets=Int[], states=String[]), (type=:probability, operator=Union{String,Matrix{ComplexF64}}[], targets=Int[], states=String[])])
        @test visitor.results[2] == (type=:state_vector, operator=Union{String,Matrix{ComplexF64}}[], targets=Int[], states=String[])
        @test visitor.results[3] == (type=:probability, operator=Union{String,Matrix{ComplexF64}}[], targets=Int[], states=String[])
        push!(for_visitor, [(type="x", arguments=Union{Symbol,Dates.Period,Real,Matrix{ComplexF64}}[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0), (type="y", arguments=Union{Symbol,Dates.Period,Real,Matrix{ComplexF64}}[], targets=[1], controls=Pair{Int,Int}[], exponent=1.0)])
        @test visitor.instructions == [(type="x", arguments=Union{Symbol,Dates.Period,Real,Matrix{ComplexF64}}[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0), (type="x", arguments=Union{Symbol,Dates.Period,Real,Matrix{ComplexF64}}[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0), (type="y", arguments=Union{Symbol,Dates.Period,Real,Matrix{ComplexF64}}[], targets=[1], controls=Pair{Int,Int}[], exponent=1.0)]
        gate_visitor = Quasar.QasmGateDefVisitor(visitor, String[], Quasar.QasmExpression(:empty), String[])
        push!(gate_visitor, [(type="x", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0), (type="y", arguments=InstructionArgument[], targets=[1], controls=Pair{Int,Int}[], exponent=1.0)])
        @test gate_visitor.instructions == [(type="x", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0), (type="y", arguments=InstructionArgument[], targets=[1], controls=Pair{Int,Int}[], exponent=1.0)]
        function_visitor = Quasar.QasmFunctionVisitor(visitor, Quasar.QasmExpression[], Quasar.QasmExpression[])
        push!(function_visitor, [(type="x", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0), (type="y", arguments=InstructionArgument[], targets=[1], controls=Pair{Int,Int}[], exponent=1.0)])
        @test function_visitor.instructions == [(type="x", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0), (type="y", arguments=InstructionArgument[], targets=[1], controls=Pair{Int,Int}[], exponent=1.0)]
    end
    @testset "Sized types" begin
        for (t_string, T) in (("SizedBitVector", Quasar.SizedBitVector),
                              ("SizedInt", Quasar.SizedInt), 
                              ("SizedUInt", Quasar.SizedUInt),
                              ("SizedFloat", Quasar.SizedFloat),
                              ("SizedAngle", Quasar.SizedAngle),
                              ("SizedComplex", Quasar.SizedComplex))
            object = T(Quasar.QasmExpression(:integer_literal, 4))
            @test sprint(show, object) == t_string * "{4}"
            array_object = Quasar.SizedArray(object, (10,))
            @test sprint(show, array_object) == "SizedArray{" * t_string * "{4}, 1}"
            @test_throws BoundsError size(array_object, 1)
            @test size(array_object, 0) == 10
            new_object = T(object)
            @test sprint(show, new_object) == t_string * "{4}"
        end
        bitvector = Quasar.SizedBitVector(Quasar.QasmExpression(:integer_literal, 10))
        @test length(bitvector) == Quasar.QasmExpression(:integer_literal, 10)
        @test size(bitvector)   == (Quasar.QasmExpression(:integer_literal, 10),)
    end
    @testset "Adder" begin
        sv_adder_qasm = """
        OPENQASM 3;

        input uint[4] a_in;
        input uint[4] b_in;

        gate majority a, b, c {
            cnot c, b;
            cnot c, a;
            ccnot a, b, c;
        }

        gate unmaj a, b, c {
            ccnot a, b, c;
            cnot c, a;
            cnot a, b;
        }

        qubit cin;
        qubit[4] a;
        qubit[4] b;
        qubit cout;

        // set input states
        for int[8] i in [0: 3] {
          if(bool(a_in[i])) x a[i];
          if(bool(b_in[i])) x b[i];
        }

        // add a to b, storing result in b
        majority cin, b[3], a[3];
        for int[8] i in [3: -1: 1] { majority a[i], b[i - 1], a[i - 1]; }
        cnot a[0], cout;
        for int[8] i in [1: 3] { unmaj a[i], b[i - 1], a[i - 1]; }
        unmaj cin, b[3], a[3];
        """
        parsed = parse_qasm(sv_adder_qasm)
        visitor = QasmProgramVisitor(Dict("a_in"=>3, "b_in"=>7))
        visitor(parsed)
        @test visitor.qubit_count == 10
        correct_instructions = [ 
            (type="x", arguments=InstructionArgument[], targets=[6], controls=Pair{Int,Int}[], exponent=1.0)
            (type="x", arguments=InstructionArgument[], targets=[3], controls=Pair{Int,Int}[], exponent=1.0)
            (type="x", arguments=InstructionArgument[], targets=[7], controls=Pair{Int,Int}[], exponent=1.0)
            (type="x", arguments=InstructionArgument[], targets=[4], controls=Pair{Int,Int}[], exponent=1.0)
            (type="x", arguments=InstructionArgument[], targets=[8], controls=Pair{Int,Int}[], exponent=1.0)
            (type="cnot", arguments=InstructionArgument[], targets=[4, 8], controls=Pair{Int,Int}[], exponent=1.0)
            (type="cnot", arguments=InstructionArgument[], targets=[4, 0], controls=Pair{Int,Int}[], exponent=1.0)
            (type="ccnot", arguments=InstructionArgument[], targets=[0, 8, 4], controls=Pair{Int,Int}[], exponent=1.0)
            (type="cnot", arguments=InstructionArgument[], targets=[3, 7], controls=Pair{Int,Int}[], exponent=1.0)
            (type="cnot", arguments=InstructionArgument[], targets=[3, 4], controls=Pair{Int,Int}[], exponent=1.0)
            (type="ccnot", arguments=InstructionArgument[], targets=[4, 7, 3], controls=Pair{Int,Int}[], exponent=1.0)
            (type="cnot", arguments=InstructionArgument[], targets=[2, 6], controls=Pair{Int,Int}[], exponent=1.0)
            (type="cnot", arguments=InstructionArgument[], targets=[2, 3], controls=Pair{Int,Int}[], exponent=1.0)
            (type="ccnot", arguments=InstructionArgument[], targets=[3, 6, 2], controls=Pair{Int,Int}[], exponent=1.0)
            (type="cnot", arguments=InstructionArgument[], targets=[1, 5], controls=Pair{Int,Int}[], exponent=1.0)
            (type="cnot", arguments=InstructionArgument[], targets=[1, 2], controls=Pair{Int,Int}[], exponent=1.0)
            (type="ccnot", arguments=InstructionArgument[], targets=[2, 5, 1], controls=Pair{Int,Int}[], exponent=1.0)
            (type="cnot", arguments=InstructionArgument[], targets=[1, 9], controls=Pair{Int,Int}[], exponent=1.0)
            (type="ccnot", arguments=InstructionArgument[], targets=[2, 5, 1], controls=Pair{Int,Int}[], exponent=1.0)
            (type="cnot", arguments=InstructionArgument[], targets=[1, 2], controls=Pair{Int,Int}[], exponent=1.0)
            (type="cnot", arguments=InstructionArgument[], targets=[2, 5], controls=Pair{Int,Int}[], exponent=1.0)
            (type="ccnot", arguments=InstructionArgument[], targets=[3, 6, 2], controls=Pair{Int,Int}[], exponent=1.0)
            (type="cnot", arguments=InstructionArgument[], targets=[2, 3], controls=Pair{Int,Int}[], exponent=1.0)
            (type="cnot", arguments=InstructionArgument[], targets=[3, 6], controls=Pair{Int,Int}[], exponent=1.0)
            (type="ccnot", arguments=InstructionArgument[], targets=[4, 7, 3], controls=Pair{Int,Int}[], exponent=1.0)
            (type="cnot", arguments=InstructionArgument[], targets=[3, 4], controls=Pair{Int,Int}[], exponent=1.0)
            (type="cnot", arguments=InstructionArgument[], targets=[4, 7], controls=Pair{Int,Int}[], exponent=1.0)
            (type="ccnot", arguments=InstructionArgument[], targets=[0, 8, 4], controls=Pair{Int,Int}[], exponent=1.0)
            (type="cnot", arguments=InstructionArgument[], targets=[4, 0], controls=Pair{Int,Int}[], exponent=1.0)
            (type="cnot", arguments=InstructionArgument[], targets=[0, 8], controls=Pair{Int,Int}[], exponent=1.0)
        ]
        for (ix, c_ix) in zip(visitor.instructions, correct_instructions)
            @test ix == c_ix
        end
    end
    @testset "Block comment" begin
        qasm = """
        qubit[2] qs;
        int[8] two = 2;
        gphase(π);
        /*** Apply inverse of gphase here ***/
        inv @ gphase(π / 2);
        negctrl @ ctrl @ gphase(2 * π) qs[0], qs[1];
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor.instructions == [
                                       (type="gphase", arguments=InstructionArgument[π], targets=[0, 1], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="gphase", arguments=InstructionArgument[π/2], targets=[0, 1], controls=Pair{Int,Int}[], exponent=-1.0),
                                       (type="gphase", arguments=InstructionArgument[2*π], targets=[0, 1], controls=[0=>0, 1=>1], exponent=1.0),
                                      ]
    end
    @testset "Qubit aliasing" begin
        qasm = """
        qubit[2] one;
        qubit[2] two;
        // Aliased register of four qubits
        let concatenated = two ++ one;
        // First qubit in aliased qubit array
        let first = concatenated[0];
        // Last qubit in aliased qubit array
        let last = concatenated[-1];
        let new_cat = concatenated;
        h concatenated[2];
        x concatenated[1];
        y first;
        z last;
        i new_cat[0];
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor.instructions == [(type="h", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="x", arguments=InstructionArgument[], targets=[3], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="y", arguments=InstructionArgument[], targets=[2], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="z", arguments=InstructionArgument[], targets=[1], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="i", arguments=InstructionArgument[], targets=[2], controls=Pair{Int,Int}[], exponent=1.0),
                                      ]
        qasm = """
        qubit[2] one;
        bit[2] two = "10";
        let concatenated = two ++ one;
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        @test_throws Quasar.QasmVisitorError("cannot concatenate qubit and classical arrays") visitor(parsed)

        qasm = """
        qubit[2] one;
        qubit[2] two;
        let concatenated = two - one;
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        @test_throws Quasar.QasmVisitorError("right hand side of alias must be either an identifier or concatenation") visitor(parsed)
    end
    @testset "Classical aliasing" begin
        qasm = """
        bit[2] one = "01";
        bit[2] two = "10";
        // Aliased register of four bits
        let concatenated = two ++ one; // "1001"
        // First bit in aliased qubit array
        let first   = concatenated[0];
        // Last qubit in aliased qubit array
        let last    = concatenated[-1];
        let new_cat = concatenated;
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        @test_throws Quasar.QasmVisitorError("classical array concatenation not yet supported!") visitor(parsed)
        #@test collect(visitor.classical_defs["concatenated"].val) == BitVector((true, false, false, true))
        #@test visitor.classical_defs["first"].val == true
        #@test visitor.classical_defs["last"].val == true
        #@test collect(visitor.classical_defs["new_cat"].val) == BitVector((true, false, false, true))
        # test that these are *references*
        qasm = """
        bit[2] one = "01";
        bit[2] two = "10";
        // Aliased register of four bits
        let concatenated = one; // "01"
        // First bit in aliased qubit array
        let first   = concatenated[0];
        concatenated[1] = false;
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor.classical_defs["one"].val   == BitVector((false, false))
        @test only(visitor.classical_defs["first"].val) == false
    end
    @testset "Randomized Benchmarking" begin
        qasm = """
        qubit[2] q;
        bit[2] c;

        h q[0];
        cz q[0], q[1];
        s q[0];
        cz q[0], q[1];
        s q[0];
        z q[0];
        h q[0];
        measure q -> c;
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor.instructions == [(type="h", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="cz", arguments=InstructionArgument[], targets=[0, 1], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="s", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="cz", arguments=InstructionArgument[], targets=[0, 1], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="s", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="z", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="h", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="measure", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="measure", arguments=InstructionArgument[], targets=[1], controls=Pair{Int,Int}[], exponent=1.0)]
    end
    @testset "Bare measure" begin
        qasm = """
        qubit[2] q;
        measure q;
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor.instructions == [(type="measure", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="measure", arguments=InstructionArgument[], targets=[1], controls=Pair{Int,Int}[], exponent=1.0)]
    end
    @testset "Measure with density matrix" begin
        qasm = """
        qubit[3] qs;
        qubit q;

        x qs[{0, 2}];
        h q;
        cphaseshift(1) qs, q;
        phaseshift(-2) q;
        measure qs;
        measure q;
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor.instructions == [
                                       (type="x", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="x", arguments=InstructionArgument[], targets=[2], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="h", arguments=InstructionArgument[], targets=[3], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="cphaseshift", arguments=InstructionArgument[1.0], targets=[0, 3], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="cphaseshift", arguments=InstructionArgument[1.0], targets=[1, 3], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="cphaseshift", arguments=InstructionArgument[1.0], targets=[2, 3], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="phaseshift", arguments=InstructionArgument[-2.0], targets=[3], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="measure", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="measure", arguments=InstructionArgument[], targets=[1], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="measure", arguments=InstructionArgument[], targets=[2], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="measure", arguments=InstructionArgument[], targets=[3], controls=Pair{Int,Int}[], exponent=1.0),
                                      ]
    end
    @testset "Pragma" begin
        qasm = """
        qubit[4] q;
        #pragma fake_pragma
        """
        @test_throws MethodError parse_qasm(qasm)
    end
    @testset "Box" begin
        qasm = """
        OPENQASM 3.0;
        bit[2] b;
        qubit[2] q;
        box{
            cnot q[0], q[1];
            cnot q[0], q[1];
            rx(1.57) q[0];
        }
        b[0] = measure q[0];
        b[1] = measure q[1];
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor.instructions == [
                                       (type="cnot", arguments=InstructionArgument[], targets=[0, 1], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="cnot", arguments=InstructionArgument[], targets=[0, 1], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="rx", arguments=InstructionArgument[1.57], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="measure", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="measure", arguments=InstructionArgument[], targets=[1], controls=Pair{Int,Int}[], exponent=1.0),
                                      ]
    end
    @testset "GPhase" begin
        qasm = """
        qubit[2] qs;
        int[8] two = 2;
        gphase(π);
        inv @ gphase(π / 2);
        negctrl @ ctrl @ gphase(2 * π) qs[0], qs[1];
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor.instructions == [
                                       (type="gphase", arguments=InstructionArgument[π], targets=[0, 1], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="gphase", arguments=InstructionArgument[π/2], targets=[0, 1], controls=Pair{Int,Int}[], exponent=-1.0),
                                       (type="gphase", arguments=InstructionArgument[2*π], targets=[0, 1], controls=[0=>0, 1=>1], exponent=1.0),
                                      ]
    end
    @testset "Operator precedence $expr -> $val" for (expr, val) in (("complex[float] a = 1/sqrt(2)+1/sqrt(2)im;", (1+im)/√2),
                                                                     ("float a = 2*3 - 4*5;", 6-20),
                                                                     ("float a = 2*(3 - 4)*5;", -10),
                                                                     ("int a = (3 - 4)*5;", -5),
                                                                     ("int a = (3-4)*(5+2);", -7),
                                                                     ("int a = int(2.0) + int(3.0);", 5),
                                                                     ("float a = 1/2+4;", 4.5),
                                                                     ("float a = 2**int(2.0);", 4.0),
                                                                     ("int a = int(2**2);", 4),
                                                                     ("int a = 2 + 3*4 - 5;", 14 - 5),
                                                                     ("complex[float] a = -5+4im;", -5+4im),
                                                                     ("complex[float] a = -(5+4im);", -5-4im),
                                                                     ("complex[float] a = 2+1/3im;", 2-(im/3)),
                                                                     ("bool a = 1 << 2 == 5;", false),
                                                                     ("bool a = true && true || false;", true),
                                                                    ) 
        qasm = """
        $expr
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor.classical_defs["a"].val == val
    end
    @testset "Waveforms" begin
        qasm = """
        // arbitrary complex samples
        waveform arb_waveform = [1, 2, 3, 4];
        """
        parsed  = parse_qasm(qasm)
        @test parsed == Quasar.QasmExpression(:program, Quasar.QasmExpression(:classical_declaration, Quasar.QasmExpression(:waveform), Quasar.QasmExpression(:classical_assignment, Quasar.QasmExpression(:binary_op, Symbol("="), Quasar.QasmExpression(:identifier, "arb_waveform"), Quasar.QasmExpression(:array_literal, Quasar.QasmExpression(:integer_literal, 1), Quasar.QasmExpression(:integer_literal, 2), Quasar.QasmExpression(:integer_literal, 3), Quasar.QasmExpression(:integer_literal, 4))))))
        qasm = """
        // arbitrary complex samples
        extern gaussian(complex[float[size]] amp, duration d, duration sigma) -> waveform;
        """
        parsed  = parse_qasm(qasm)
        @test Quasar.head(only(parsed.args)) == :function_definition
        @test only(parsed.args).args[1] == Quasar.QasmExpression(:identifier, "gaussian")
        @test only(parsed.args).args[3] == Quasar.QasmExpression(:waveform)
        @test only(parsed.args).args[4] == Quasar.QasmExpression(:extern)
    end
    @testset "Casting" begin
        @testset "Casting to $to_type from $from_type" for (to_type, to_value) in (("bool", true),), (from_type, from_value) in (("int[32]", "32",),
                                                                                                                                 ("uint[16]", "1",),
                                                                                                                                 ("float", "2.5",),
                                                                                                                                 ("bool", "true",),
                                                                                                                                 ("bit", "\"1\"",),
                                                                                                                                )
            qasm = """
            $from_type a = $from_value;
            $to_type b = $to_type(a);
            """
            parsed  = parse_qasm(qasm)
            visitor = QasmProgramVisitor()
            visitor(parsed)
            @test visitor.classical_defs["b"].val == to_value
        end
        @testset "Casting to $to_type from $from_type" for (to_type, to_value) in (("uint[32]", 32), ("int[16]", 32), ("float", 32.0)), (from_type, from_value) in (("int[32]", "32",),
                                                                                                                                                   ("uint[16]", "32",),
                                                                                                                                                   ("float", "32.0",),
                                                                                                                                                   ("bit[6]", "\"100000\"",),
                                                                                                                                                  )
            qasm = """
            $from_type a = $from_value;
            $to_type b = $to_type(a);
            """
            parsed  = parse_qasm(qasm)
            visitor = QasmProgramVisitor()
            visitor(parsed)
            @test visitor.classical_defs["b"].val == to_value
        end
        @testset "Casting to $to_type from $from_type" for (to_type, to_value) in (("bit[6]", BitVector([1,0,0,0,0,0])),), (from_type, from_value) in (("int[32]", "32",),
                                                                                                                                                   ("uint[16]", "32",),
                                                                                                                                                   ("bit[6]", "\"100000\"",),
                                                                                                                                                  )
            qasm = """
            $from_type a = $from_value;
            $to_type b = $to_type(a);
            """
            parsed  = parse_qasm(qasm)
            visitor = QasmProgramVisitor()
            visitor(parsed)
            @test visitor.classical_defs["b"].val == to_value
        end
    end
    @testset "Numbers $qasm_str" for (qasm_str, var_name, output_val, type_name) in (("float[32] a = 1.24e-3;", "a", 1.24e-3, "SizedFloat{32}"),
                                                                                     ("complex[float] b = 1-0.23im;", "b", 1-0.23im, "SizedComplex{-1}"),
                                                                                     ("bit c = \"0\";", "c", falses(1), "SizedBitVector{-1}"),
                                                                                     ("bool d = false;", "d", false, "Bool"),
                                                                                     ("complex[float] e = -0.23+2im;", "e", -0.23+2im, "SizedComplex{-1}"),
                                                                                     ("uint f = 0x123456789abcdef;", "f", 0x123456789abcdef, "SizedUInt{-1}"),
                                                                                     ("int g = 0o010;", "g", 0o010, "SizedInt{-1}"),
                                                                                     ("float[64] h = 2*π;", "h", 2π, "SizedFloat{64}"),
                                                                                     ("float[64] i = τ/2;", "i", Float64(π), "SizedFloat{64}"),
                                                                                     ("complex[float] j = 0.23im;", "j", 0.23im, "SizedComplex{-1}"),
                                                                                     ("complex[float] k = -0.23im;", "k", -0.23im, "SizedComplex{-1}"),
                                                                                     ("complex[float] l = 2im;", "l", 2*im, "SizedComplex{-1}"),
                                                                                     ("complex[float] m = -0.23 + -2.0im;", "m", -0.23-2.0*im, "SizedComplex{-1}"),
                                                                                    )


        qasm = """
        $qasm_str
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor.classical_defs[var_name].val == output_val
        @test startswith(sprint(show, parsed), """
        QasmExpression :program
        └─ QasmExpression :classical_declaration
           ├─ QasmExpression :classical_type
           │  └─ $type_name
           └─ QasmExpression :classical_assignment
              └─ QasmExpression :binary_op
                 ├─ :(=)
                 ├─ QasmExpression :identifier
                 │  └─ "$var_name"
        """)
    end
    @testset "Qubit identifiers" begin
        qasm = """
        qubit[2] q;
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor(Quasar.QasmExpression(:identifier, "q")) == [0, 1]
        @test visitor(Quasar.QasmExpression(:indexed_identifier, Quasar.QasmExpression(:identifier, "q"), Quasar.QasmExpression(:integer_literal, 1))) == [1]
        @test_throws Quasar.QasmVisitorError("no identifier p defined.") visitor(Quasar.QasmExpression(:identifier, "p"))
        @test_throws Quasar.QasmVisitorError("no identifier p defined.") visitor(Quasar.QasmExpression(:indexed_identifier, Quasar.QasmExpression(:identifier, "p"), Quasar.QasmExpression(:integer_literal, 1)))
    end
    @testset "Integers next to irrationals" begin
        qasm = """
        float[64] h = 2π;
        """
        @test_throws Quasar.QasmParseError parse_qasm(qasm)
        try
            parse_qasm(qasm)
        catch e
            msg = sprint(showerror, e)
            @test startswith(msg, "QasmParseError: expressions of form 2π are not supported -- you must separate the terms with a * operator.")
        end
    end
    @testset "Gate def with argument manipulation" begin
        qasm = """
        qubit[2] __qubits__;
        gate u3(θ, ϕ, λ) q {
            gphase(-(ϕ+λ)/2);
            U(θ, ϕ, λ) q;
        }
        u3(0.1, 0.2, 0.3) __qubits__[0];
        """
        parsed  = parse_qasm(qasm)
        visitor = Quasar.QasmProgramVisitor()
        visitor(parsed)
        canonical_ixs = [(type="gphase", arguments=InstructionArgument[-(0.2 + 0.3)/2], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                         (type="u", arguments=InstructionArgument[0.1, 0.2, 0.3], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)]
        @test visitor.instructions == canonical_ixs
    end
    @testset "Bad classical type" begin
        qasm = """
        angle[64] h = 2π;
        """
        @test_throws Quasar.QasmParseError parse_qasm(qasm)
    end
    @testset "Physical qubits" begin
        qasm = """
        h \$0;
        cnot \$0, \$1;
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor.instructions == [(type="h", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="cnot", arguments=InstructionArgument[], targets=[0, 1], controls=Pair{Int,Int}[], exponent=1.0)]
    end
    @testset "For loop and subroutines" begin
        qasm = """
        OPENQASM 3.0;
        def bell(qubit q0, qubit q1) {
            h q0;
            cnot q0, q1;
        }
        def n_bells(int[32] n, qubit q0, qubit q1) {
            for int i in [0:n - 1] {
                h q0;
                cnot q0, q1;
            }
        }
        qubit[4] __qubits__;
        bell(__qubits__[0], __qubits__[1]);
        n_bells(5, __qubits__[2], __qubits__[3]);
        bit[4] __bit_0__ = "0000";
        __bit_0__[0] = measure __qubits__[0];
        __bit_0__[1] = measure __qubits__[1];
        __bit_0__[2] = measure __qubits__[2];
        __bit_0__[3] = measure __qubits__[3];
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor(Dict("theta"=>0.2))
        visitor(parsed)
        expected_ixs = [(type="h", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                        (type="cnot", arguments=InstructionArgument[], targets=[0, 1], controls=Pair{Int,Int}[], exponent=1.0),
                        (type="h", arguments=InstructionArgument[], targets=[2], controls=Pair{Int,Int}[], exponent=1.0),
                        (type="cnot", arguments=InstructionArgument[], targets=[2, 3], controls=Pair{Int,Int}[], exponent=1.0),
                        (type="h", arguments=InstructionArgument[], targets=[2], controls=Pair{Int,Int}[], exponent=1.0),
                        (type="cnot", arguments=InstructionArgument[], targets=[2, 3], controls=Pair{Int,Int}[], exponent=1.0),
                        (type="h", arguments=InstructionArgument[], targets=[2], controls=Pair{Int,Int}[], exponent=1.0), 
                        (type="cnot", arguments=InstructionArgument[], targets=[2, 3], controls=Pair{Int,Int}[], exponent=1.0),
                        (type="h", arguments=InstructionArgument[], targets=[2], controls=Pair{Int,Int}[], exponent=1.0),
                        (type="cnot", arguments=InstructionArgument[], targets=[2, 3], controls=Pair{Int,Int}[], exponent=1.0),
                        (type="h", arguments=InstructionArgument[], targets=[2], controls=Pair{Int,Int}[], exponent=1.0),
                        (type="cnot", arguments=InstructionArgument[], targets=[2, 3], controls=Pair{Int,Int}[], exponent=1.0)
                       ]
        for (v_ix, e_ix) in zip(visitor.instructions, expected_ixs)
            @test v_ix == e_ix
        end
    end
    @testset "For Loop" begin
        qasm_with_scope = """
        int[8] x = 0;
        int[8] y = -100;
        int[8] ten = 10;

        for uint[8] i in [0:2:ten - 3] {
            x += i;
        }

        for int[8] i in {2, 4, 6} {
            y += i;
        }
        """
        qasm_no_scope = """
        int[8] x = 0;
        int[8] y = -100;
        int[8] ten = 10;

        for uint[8] i in [0:2:ten - 3] x += i;
        for int[8] i in {2, 4, 6} y += i;
        """
        @testset "$label" for (label, qasm) in (("With scoping {}", qasm_with_scope), ("Without scoping {}", qasm_no_scope))
            parsed  = parse_qasm(qasm)
            visitor = QasmProgramVisitor()
            visitor(parsed)
            @test visitor.classical_defs["x"].val == sum((0, 2, 4, 6))
            @test visitor.classical_defs["y"].val == sum((-100, 2, 4, 6))
        end
    end
    @testset "While Loop" begin
        qasm_with_scope = """
        int[8] x = 0;
        int[8] i = 0;

        while (i < 7) {
            x += i;
            i += 1;
        }
        """
        qasm_no_scope = """
        int[8] x = 0;

        while (x < 7) x += 1;
        """
        @testset "$label" for (label, qasm, val) in (("With scoping {}", qasm_with_scope, sum(0:6)), ("Without scoping {}", qasm_no_scope, 7))
            parsed  = parse_qasm(qasm)
            visitor = QasmProgramVisitor()
            visitor(parsed)
            @test visitor.classical_defs["x"].val == val
        end
    end
    @testset "Break and continue" begin
        @testset for str in ("continue;", "{ continue; }")
            qasm = """
            int[8] x = 0;
            for int[8] i in [0: 3] {
                if (mod(i, 2) == 0) $str
                x += i;
            }
            """
            parsed  = parse_qasm(qasm)
            visitor = QasmProgramVisitor()
            visitor(parsed)
            @test visitor.classical_defs["x"].val == 4
            qasm = """
            int[8] x = 0;
            int[8] i = 0;

            while (i < 7) {
                i += 1;
                if (i == 5) $str
                x += i;
            }
            """
            parsed  = parse_qasm(qasm)
            visitor = QasmProgramVisitor()
            visitor(parsed)
            @test visitor.classical_defs["x"].val == sum(0:7) - 5
        end
        @testset for str in ("break;", "{ break; }")
            qasm = """
            int[8] x = 0;
            for int[8] i in [0: 3] {
                x += i;
                if (mod(i, 2) == 1) $str
            }
            """
            parsed  = parse_qasm(qasm)
            visitor = QasmProgramVisitor()
            visitor(parsed)
            @test visitor.classical_defs["x"].val == 1
            qasm = """
            int[8] x = 0;
            int[8] i = 0;

            while (i < 7) {
                x += i;
                i += 1;
                if (i == 5) $str
            }
            """
            parsed  = parse_qasm(qasm)
            visitor = QasmProgramVisitor()
            visitor(parsed)
            @test visitor.classical_defs["x"].val == sum(0:4)
        end
        @testset for str in ("continue", "break")
            qasm = """
            int[8] x = 0;
            int[8] i = 0;
            $str;
            """
            parsed  = parse_qasm(qasm)
            visitor = QasmProgramVisitor()
            @test_throws Quasar.QasmVisitorError("$str statement encountered outside a loop.") visitor(parsed)
        end
    end
    @testset "Builtin functions $fn" for (fn, type, arg, result) in (("arccos", "float[64]", 1, acos(1)),
                                                                     ("arcsin", "float[64]", 1, asin(1)),
                                                                     ("arctan", "float[64]", 1, atan(1)),
                                                                     ("ceiling", "int[64]", "π", 4),
                                                                     ("floor", "int[64]", "π", 3),
                                                                     ("mod", "int[64]", "4, 3", 1),
                                                                     ("mod", "float[64]", "5.2, 2.5", mod(5.2, 2.5)),
                                                                     ("cos", "float[64]", 1, cos(1)),
                                                                     ("sin", "float[64]", 1, sin(1)),
                                                                     ("tan", "float[64]", 1, tan(1)),
                                                                     ("sqrt", "float[64]", 2, sqrt(2)),
                                                                     ("exp", "float[64]", 2, exp(2)),
                                                                     ("log", "float[64]", "ℇ", log(ℯ)),
                                                                     ("popcount", "int[64]", "true", 1),
                                                                     ("popcount", "int[64]", "\"1001110\"", 4),
                                                                     ("popcount", "int[64]", "78", 4),
                                                                     ("popcount", "int[64]", "0x78", 4),
                                                                    )
        qasm = """
            const $type $(fn)_result = $fn($arg);
            """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor.classical_defs["$(fn)_result"].val == result
    end
    @testset "Builtin functions" begin
        qasm = """
            bit[2] bitvec = "11";
            const int[64] popcount_bitvec_result = popcount(bitvec);
            """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor.classical_defs["popcount_bitvec_result"].val == 2
        qasm = """
            const int[64] popcount_empty_result = popcount();
            """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor.classical_defs["popcount_empty_result"].val == 0
        @testset "Symbolic" begin
            qasm = """
                input float x;
                input float y;
                rx(x) \$0;
                rx(arccos(x)) \$0;
                rx(arcsin(x)) \$0;
                rx(arctan(x)) \$0; 
                rx(ceiling(x)) \$0;
                rx(cos(x)) \$0;
                rx(exp(x)) \$0;
                rx(floor(x)) \$0;
                rx(log(x)) \$0;
                rx(mod(x, y)) \$0;
                rx(sin(x)) \$0;
                rx(sqrt(x)) \$0;
                rx(tan(x)) \$0;
                """
            x       = 1.0
            y       = 2.0
            parsed  = parse_qasm(qasm)
            visitor = Quasar.QasmProgramVisitor(Dict("x"=>x, "y"=>y))
            visitor(parsed)
            ixs = [
                   (type="rx", arguments=InstructionArgument[x], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),  
                   (type="rx", arguments=InstructionArgument[acos(x)], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),  
                   (type="rx", arguments=InstructionArgument[asin(x)], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),  
                   (type="rx", arguments=InstructionArgument[atan(x)], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),  
                   (type="rx", arguments=InstructionArgument[ceil(x)], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),  
                   (type="rx", arguments=InstructionArgument[cos(x)], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),  
                   (type="rx", arguments=InstructionArgument[exp(x)], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),  
                   (type="rx", arguments=InstructionArgument[floor(x)], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),  
                   (type="rx", arguments=InstructionArgument[log(x)], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),  
                   (type="rx", arguments=InstructionArgument[mod(x, y)], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),  
                   (type="rx", arguments=InstructionArgument[sin(x)], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),  
                   (type="rx", arguments=InstructionArgument[sqrt(x)], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),  
                   (type="rx", arguments=InstructionArgument[tan(x)], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                  ]
            @test visitor.instructions == ixs
        end
        @testset "popcount!" begin
            @test Quasar.popcount() == 0
            @test Quasar.popcount(vcat(trues(3), falses(2))) == 3
            @test Quasar.popcount("01010101") == 4 
        end
    end
    @testset "Delay $duration" for (duration, ix) in (("200us", Microsecond(200)),
                                                      ("100μs", Microsecond(100)),
                                                      ("50ns", Nanosecond(50)),
                                                      ("20dt", Nanosecond(20)),
                                                      ("10ms", Millisecond(10)),
                                                      ("1s", Second(1)),
                                                     )
        

        qasm = """
        qubit[4] q;
        x q[0];
        delay[$duration] q[0], q[1];
        """
        @test_warn "delay expression encountered -- currently `delay` is a no-op" parse_qasm(qasm)
        parsed  = parse_qasm(qasm)
        visitor = Quasar.QasmProgramVisitor()
        visitor(parsed)
        @test visitor.instructions == [(type="x", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="delay", arguments=InstructionArgument[ix], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="delay", arguments=InstructionArgument[ix], targets=[1], controls=Pair{Int,Int}[], exponent=1.0),
                                      ]
    end
    @testset "Barrier" begin
        qasm = """
        qubit[4] q;
        x q[0];
        barrier q[0];
        """
        @test_warn "barrier expression encountered -- currently `barrier` is a no-op" parse_qasm(qasm)
        parsed  = parse_qasm(qasm)
        visitor = Quasar.QasmProgramVisitor()
        visitor(parsed)
        @test visitor.instructions == [(type="x", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="barrier", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                      ]
    end
    @testset "Stretch" begin
        qasm = """
        stretch a;
        """
        @test_warn "stretch expression encountered -- currently `stretch` is a no-op" parse_qasm(qasm)
        visitor = Quasar.QasmProgramVisitor()
        @test visitor(Quasar.QasmExpression(:stretch)) === visitor
    end
    @testset "Duration" begin
        qasm = """
        duration a = 10μs;
        """
        @test_warn "duration expression encountered -- currently `duration` is a no-op" parse_qasm(qasm)
        visitor = Quasar.QasmProgramVisitor()
        @test visitor(Quasar.QasmExpression(:duration)) === visitor
    end
    @testset "Reset" begin
        qasm = """
        qubit[4] q;
        x q[0];
        reset q[0];
        """
        @test_warn "reset expression encountered -- currently `reset` is a no-op" parse_qasm(qasm)
        parsed  = parse_qasm(qasm)
        visitor = Quasar.QasmProgramVisitor()
        visitor(parsed)
        @test visitor.instructions == [(type="x", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="reset", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                      ]
        @testset "Reset after Measure" begin
            good_qasm = """
            qubit[4] q;
            measure q[0];
            reset q[0];
            x q[0];
            """
            parsed  = parse_qasm(good_qasm)
            visitor = Quasar.QasmProgramVisitor()
            visitor(parsed)
            @test visitor.instructions == [
                                           (type="measure", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                           (type="reset", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                           (type="x", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                          ]
        end
    end
    @testset "Gate call missing/extra args" begin
        qasm = """
        qubit[4] q;
        rx q[0];
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        @test_throws Quasar.QasmVisitorError("gate rx requires arguments but none were provided.") visitor(parsed)
        qasm = """
        qubit[4] q;
        x(0.1) q[0];
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        @test_throws Quasar.QasmVisitorError("gate x does not accept arguments but arguments were provided.") visitor(parsed)
    end
    @testset "Assignment operators" begin
        @testset "Op: $op" for (op, initial, arg, final) in (("+", 0, 1, 1), ("*", 1, 2, 2), ("/", 2, 2, 1), ("-", 1, 5, -4))
            qasm = """
            int[16] x;
            x = $initial;
            x $op= $arg;
            """
            parsed  = parse_qasm(qasm)
            visitor = QasmProgramVisitor()
            visitor(parsed)
            @test visitor.classical_defs["x"].val == final 
        end
        @testset "Op: $op" for (op, initial, arg, final) in (("|", "0000", "11", BitVector([0,0,1,1])),
                                                             ("&", "0010", "11", BitVector([0,0,1,0])),
                                                             ("^", "0001", "10", BitVector([0,0,1,1])),
                                                            )
            qasm = """
            bit[4] xs = \"$initial\";
            xs[2:] $op= \"$arg\";
            """
            parsed  = parse_qasm(qasm)
            visitor = QasmProgramVisitor()
            visitor(parsed)
            @test visitor.classical_defs["xs"].val == final
        end
    end
    @testset "Binary bit operators $op" for (op, result) in (("&", BitVector([0,1,0,0])),
                                                             ("|", BitVector([1,1,0,1])),
                                                             ("^", BitVector([1,0,0,1])),
                                                             (">", false),
                                                             ("<", true),
                                                             (">=", false),
                                                             ("<=", true),
                                                             ("==", false),
                                                             ("!=", true),
                                                            )

        qasm = """
        bit[4] x = "0101";
        bit[4] y = "1100";

        bit[4] result = x $op y;
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor.classical_defs["x"].val == BitVector([0,1,0,1])
        @test visitor.classical_defs["y"].val == BitVector([1,1,0,0])
        @test visitor.classical_defs["result"].val == result 
    end
    @testset "Binary bit operators $op" for (op, arg1, arg2, result) in (("<<", "x", 2, BitVector([0,1,0,0])),
                                                                         (">>", "y", 2, BitVector([0,0,1,1])),
                                                                        )
        qasm = """
        bit[4] x = "0101";
        bit[4] y = "1100";

        bit[4] result = $arg1 $op $arg2;
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor.classical_defs["result"].val == result
    end
    @testset "Unary bit operators $op" for (op, typ, arg, result) in (("~", "bit[4]", "x", BitVector([1,0,1,0])),
                                                                      ("!", "bit", "x", false),
                                                                      ("!", "bit", "x << 4", false),
                                                                      ("!", "bit", "(x << 4)", true),
                                                                     )

        qasm = """
        bit not;
        bit not_zero;

        bit[4] x = "0101";
        $typ result = $(op)$(arg);
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor.classical_defs["x"].val == BitVector([0,1,0,1])
        @test visitor.classical_defs["result"].val == result
    end
    @testset "End statement" begin
        qasm = """
        z \$0;
        x \$1;
        h \$2;
        y \$3;
        end;
        y \$2;
        h \$3;
        """
        parsed = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor.instructions == [(type="z", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="x", arguments=InstructionArgument[], targets=[1], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="h", arguments=InstructionArgument[], targets=[2], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="y", arguments=InstructionArgument[], targets=[3], controls=Pair{Int,Int}[], exponent=1.0),
                                      ]
    end
    @testset "Switch/case" begin
        qasm = """
        input int[8] x;
        switch (x + 1) {
            case 0b00 {}
            default { z \$0; }
        }
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor(Dict("x"=>-1))
        visitor(parsed)
        @test isempty(visitor.instructions)
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor(Dict("x"=>0))
        visitor(parsed)
        @test only(visitor.instructions) == (type="z", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)
        qasm = """
        input int[8] x;
        switch (x) { case 0 {} case 1, 2 { z \$0; }  }
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor(Dict("x"=>0))
        visitor(parsed)
        @test isempty(visitor.instructions)
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor(Dict("x"=>1))
        visitor(parsed)
        @test only(visitor.instructions) == (type="z", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor(Dict("x"=>1))
        visitor(parsed)
        @test only(visitor.instructions) == (type="z", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)
        qasm = """
        input int[8] x;
        switch (x) { case 0 {} default { z \$0; } default { x \$0; } }
        """
        @test_throws Quasar.QasmParseError parse_qasm(qasm)
        qasm = """
        input int[8] x;
        switch (x) { default { z \$0; } case 0 {} }
        """
        @test_throws Quasar.QasmParseError parse_qasm(qasm)
        qasm = """
        input int[8] x;
        switch (x) { case 0 { z \$0; } true {} }
        """
        @test_throws Quasar.QasmParseError parse_qasm(qasm)
    end
    @testset "If/Else" begin
        qasm = """
        int[8] two = 2;
        bit[3] m = "000";

        if (two + 1) {
            m[0] = 1;
        } else {
            m[1] = 1;
        }

        if (!bool(two - 2)) {
            m[2] = 1;
        }
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        bit_vec = BitVector([1,0,1])
        @test visitor.classical_defs["m"].val == bit_vec
        qasm = """
        int[8] two = -2;
        bit[3] m = "000";

        if (two + 1) {
            m[0] = 1;
        } else {
            m[1] = 1;
        }

        if (!bool(two - 2)) {
            m[2] = 1;
        }
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        bit_vec = BitVector([0,1,1])
        @test visitor.classical_defs["m"].val == bit_vec
        qasm = """
        input bool which_gate;
        qubit q;

        if (which_gate) {
            x q;
        } else {
            y q;
        }
        """
        parsed  = parse_qasm(qasm)
        for (flag, which_gate) in ((true, "x"), (false, "y"))
            visitor = QasmProgramVisitor(Dict("which_gate"=>flag))
            visitor(parsed)
            @test only(visitor.instructions) == (type=which_gate, arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)
        end
    end
    @testset "Global gate control" begin
        qasm = """
        qubit q1;
        qubit q2;

        h q1;
        h q2;
        ctrl @ s q1, q2;
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor.instructions == [
                                       (type="h", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="h", arguments=InstructionArgument[], targets=[1], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="s", arguments=InstructionArgument[], targets=[0, 1], controls=[0=>1], exponent=1.0),
                                      ]
    end
    @testset "Simple Pow" for gate in ("z", "x")
        qasm = """
        qubit q1;
        qubit q2;
        h q1;
        h q2;
        
        pow(1/2) @ $gate q1;
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor.instructions == [
                                       (type="h", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="h", arguments=InstructionArgument[], targets=[1], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type=gate, arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=0.5),
                                      ]
    end
    @testset "Complex Pow" begin
        qasm = """
        int[8] two = 2;
        gate x a { U(π, 0, π) a; }
        gate cx c, a {
            pow(1) @ ctrl @ x c, a;
        }
        gate cxx_1 c, a {
            pow(two) @ cx c, a;
        }
        gate cxx_2 c, a {
            pow(1/2) @ pow(4) @ cx c, a;
        }
        gate cxxx c, a {
            pow(1) @ pow(two) @ cx c, a;
        }

        qubit q1;
        qubit q2;
        qubit q3;
        qubit q4;
        qubit q5;

        pow(1/2) @ x q1;       // half flip
        pow(1/2) @ x q1;       // half flip
        cx q1, q2;   // flip
        cxx_1 q1, q3;    // don't flip
        cxx_2 q1, q4;    // don't flip
        cx q1, q5;    // flip
        x q3;       // flip
        x q4;       // flip

        s q1;   // sqrt z
        s q1;   // again
        inv @ z q1; // inv z
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        expected_ixs = [(type="u", arguments=InstructionArgument[π, 0, π], targets=[0], controls=Pair{Int,Int}[], exponent=0.5),
                        (type="u", arguments=InstructionArgument[π, 0, π], targets=[0], controls=Pair{Int,Int}[], exponent=0.5),
                        (type="u", arguments=InstructionArgument[π, 0, π], targets=[0, 1], controls=[0=>1], exponent=1.0),
                        (type="u", arguments=InstructionArgument[π, 0, π], targets=[0, 2], controls=[0=>1], exponent=2.0),
                        (type="u", arguments=InstructionArgument[π, 0, π], targets=[0, 3], controls=[0=>1], exponent=2.0),
                        (type="u", arguments=InstructionArgument[π, 0, π], targets=[0, 4], controls=[0=>1], exponent=1.0),
                        (type="u", arguments=InstructionArgument[π, 0, π], targets=[2], controls=Pair{Int,Int}[], exponent=1.0),
                        (type="u", arguments=InstructionArgument[π, 0, π], targets=[3], controls=Pair{Int,Int}[], exponent=1.0),
                        (type="s", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                        (type="s", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                        (type="z", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=-1.0),
                       ]
        for (ix, e_ix) in zip(visitor.instructions, expected_ixs)
            @test ix == e_ix
        end
    end
    @testset "Gate control" begin
        qasm = """
        int[8] two = 2;
        gate x a { U(π, 0, π) a; }
        gate cx c, a {
            ctrl @ x c, a;
        }
        gate ccx_1 c1, c2, a {
            ctrl @ ctrl @ x c1, c2, a;
        }
        gate ccx_2 c1, c2, a {
            ctrl(two) @ x c1, c2, a;
        }
        gate ccx_3 c1, c2, a {
            ctrl @ cx c1, c2, a;
        }

        qubit q1;
        qubit q2;
        qubit q3;
        qubit q4;
        qubit q5;

        // doesn't flip q2
        cx q1, q2;
        // flip q1
        x q1;
        // flip q2
        cx q1, q2;
        // doesn't flip q3, q4, q5
        ccx_1 q1, q4, q3;
        ccx_2 q1, q3, q4;
        ccx_3 q1, q3, q5;
        // flip q3, q4, q5;
        ccx_1 q1, q2, q3;
        ccx_2 q1, q2, q4;
        ccx_2 q1, q2, q5;
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        expected_ixs = [
                        (type="u", arguments=InstructionArgument[π, 0, π], targets=[0, 1], controls=[0=>1], exponent=1.0),
                        (type="u", arguments=InstructionArgument[π, 0, π], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                        (type="u", arguments=InstructionArgument[π, 0, π], targets=[0, 1], controls=[0=>1], exponent=1.0),
                        (type="u", arguments=InstructionArgument[π, 0, π], targets=[0, 3, 2], controls=[0=>1, 3=>1], exponent=1.0),
                        (type="u", arguments=InstructionArgument[π, 0, π], targets=[0, 2, 3], controls=[0=>1, 2=>1], exponent=1.0),
                        (type="u", arguments=InstructionArgument[π, 0, π], targets=[0, 2, 4], controls=[0=>1, 2=>1], exponent=1.0),
                        (type="u", arguments=InstructionArgument[π, 0, π], targets=[0, 1, 2], controls=[0=>1, 1=>1], exponent=1.0),
                        (type="u", arguments=InstructionArgument[π, 0, π], targets=[0, 1, 3], controls=[0=>1, 1=>1], exponent=1.0),
                        (type="u", arguments=InstructionArgument[π, 0, π], targets=[0, 1, 4], controls=[0=>1, 1=>1], exponent=1.0),
                       ]
        @test length(visitor.instructions) == length(expected_ixs)
        @testset for ix in 1:length(expected_ixs)
            @test visitor.instructions[ix] == expected_ixs[ix]
        end
        qasm = """
        gate cccx c1, c2, c3, a {
            ctrl(3) @ x c1, c2, c3, a;
        }
        gate ncccx c1, c2, c3, a {
            negctrl(3) @ x c1, c2, c3, a;
        }

        qubit q1;
        qubit q2;
        qubit q3;
        qubit q4;
        qubit q5;
        
        h q1;
        h q2;
        h q3;
        h q4;
        h q5;
        cccx q1, q2, q5, q3;
        ncccx q4, q2, q5, q3;
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor.instructions == [(type="h", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="h", arguments=InstructionArgument[], targets=[1], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="h", arguments=InstructionArgument[], targets=[2], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="h", arguments=InstructionArgument[], targets=[3], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="h", arguments=InstructionArgument[], targets=[4], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="x", arguments=InstructionArgument[], targets=[0, 1, 4, 2], controls=[0=>1, 1=>1, 4=>1], exponent=1.0),
                                       (type="x", arguments=InstructionArgument[], targets=[3, 1, 4, 2], controls=[3=>0, 1=>0, 4=>0], exponent=1.0),
                                      ]
    end
    @testset "Gate inverses" begin
        qasm = """
        gate both a {
            x a;
            y a;
        }
        gate both_inv a {
            inv @ both a;
        }
        qubit q;

        both q;
        both_inv q;
        inv @ both_inv q;
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor.instructions == [
                                       (type="x", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="y", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="y", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=-1.0),
                                       (type="x", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=-1.0),
                                       (type="x", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                       (type="y", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0),
                                      ]
    end
    @testset "Output" begin
        qasm = """
               output int[8] out_int;
               """
        parsed  = parse_qasm(qasm)
        visitor = Quasar.QasmProgramVisitor()
        @test_throws Quasar.QasmVisitorError("Output not supported.") visitor(parsed)
    end
    @testset "Input" begin
        qasm = """
        input int[8] in_int;
        input bit[8] in_bit;
        int[8] doubled;

        doubled = in_int * 2;
        """
        in_bit = 0b10110010
        parsed  = parse_qasm(qasm)
        @testset for in_int in (0, 1, -2, 5)
            inputs  = Dict("in_int"=>in_int, "in_bit"=>in_bit)
            visitor = QasmProgramVisitor(inputs)
            visitor(parsed)
            @test visitor.classical_defs["doubled"].val == in_int * 2
            @test visitor.classical_defs["in_bit"].val  == in_bit
        end
    end
    @testset "Gate on qubit registers" begin 
        qasm = """
        qubit[3] qs;
        qubit q;

        x qs[{0, 2}];
        h q;
        cphaseshift(1) qs, q;
        phaseshift(-2) q;
        ccnot qs;
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor.instructions[1] == (type="x", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)
        @test visitor.instructions[2] == (type="x", arguments=InstructionArgument[], targets=[2], controls=Pair{Int,Int}[], exponent=1.0)
        @test visitor.instructions[3] == (type="h", arguments=InstructionArgument[], targets=[3], controls=Pair{Int,Int}[], exponent=1.0)
        @test visitor.instructions[4] == (type="cphaseshift", arguments=InstructionArgument[1], targets=[0, 3], controls=Pair{Int,Int}[], exponent=1.0)
        @test visitor.instructions[5] == (type="cphaseshift", arguments=InstructionArgument[1], targets=[1, 3], controls=Pair{Int,Int}[], exponent=1.0)
        @test visitor.instructions[6] == (type="cphaseshift", arguments=InstructionArgument[1], targets=[2, 3], controls=Pair{Int,Int}[], exponent=1.0)
        @test visitor.instructions[7] == (type="phaseshift", arguments=InstructionArgument[-2], targets=[3], controls=Pair{Int,Int}[], exponent=1.0)
        @test visitor.instructions[8] == (type="ccnot", arguments=InstructionArgument[], targets=[0, 1, 2], controls=Pair{Int,Int}[], exponent=1.0)
    end
    @testset "Subroutine" begin
        qasm = """
        const int[8] n = 4;
        def parity(bit[n] cin) -> bit {
          bit c = false;
          for int[8] i in [0: n - 1] {
            c ^= cin[i];
          }
          return c;
        }

        bit[n] c = "1011";
        bit p = parity(c);
        """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor.classical_defs["p"].val == true
    end
    @testset "Void subroutine" begin
        qasm = """
               def flip(qubit q) {
                 x q;
               }
               qubit[2] qs;
               flip(qs[0]);
               """
        parsed  = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test only(visitor.instructions) == (type="x", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)
    end
    @testset "Array ref subroutine" begin
        qasm = """
        int[16] total_1;
        int[16] total_2;

        def sum(readonly array[int[8], #dim = 1] arr) -> int[16] {
            int[16] size = sizeof(arr);
            int[16] x = 0;
            for int[8] i in [0:size - 1] {
                x += arr[i];
            }
            return x;
        }

        array[int[8], 5] array_1 = {1, 2, 3, 4, 5};
        array[int[8], 10] array_2 = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

        total_1 = sum(array_1);
        total_2 = sum(array_2);
        """
        parsed = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor.classical_defs["total_1"].val == 15
        @test visitor.classical_defs["total_2"].val == 55
        @test visitor.classical_defs["array_1"].val == [1, 2, 3, 4, 5]
        @test visitor.classical_defs["array_2"].val == collect(1:10)
    end
    @testset "Const array argument" begin
        qasm_string = """
        qubit q;

        def sum(const array[int[8], #dim = 1] arr) -> int {
            int size = sizeof(arr);
            int x = 0;
            for int i in [0:size - 1] {
                x += arr[i];
            }
            return x;
        }

        array[int, 10] arr = {9, 3, 6, 2, 2, 4, 3, 1, 12, 7};
        int s = sum(arr);
        rx(s*π/4) q;
        """
        parsed = parse_qasm(qasm_string)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor.classical_defs["s"].val == sum([ 9, 3, 6, 2, 2, 4, 3, 1, 12, 7 ])
    end
    @testset "Array ref subroutine with mutation" begin
        qasm = """
        def mutate_array(mutable array[int[8], #dim = 1] arr) {
            int[16] size = sizeof(arr);
            for int[8] i in [0:size - 1] {
                arr[i] = 0;
            }
        }

        array[int[8], 5] array_1 = {1, 2, 3, 4, 5};
        array[int[8], 10] array_2 = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
        array[int[8], 10] array_3 = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

        mutate_array(array_1);
        mutate_array(array_2);
        mutate_array(array_3[4:2:-1]);
        """
        parsed = parse_qasm(qasm)
        visitor = QasmProgramVisitor()
        visitor(parsed)
        @test visitor.classical_defs["array_1"].val == zeros(Int, 5) 
        @test visitor.classical_defs["array_2"].val == zeros(Int, 10) 
        @test visitor.classical_defs["array_3"].val == [1, 2, 3, 4, 0, 6, 0, 8, 0, 10]
    end
    @testset "Rotation parameter expressions" begin
        @testset  "Operation: $operation, argument: $arg" for (operation, arg) in
            [
                ["rx(π) q[0];", π],
                ["rx(pi) q[0];", π],
                ["rx(ℇ) q[0];", ℯ],
                ["rx(euler) q[0];", ℯ],
                ["rx(τ) q[0];", 2π],
                ["rx(tau) q[0];", 2π],
                ["rx(pi + pi) q[0];", 2π],
                ["rx(pi - pi) q[0];", 0],
                ["rx(-pi + pi) q[0];", 0],
                ["rx(-pi - pi) q[0];", -2π],
                ["rx(pi * 2) q[0];", 2π],
                ["rx(pi / 2) q[0];", π/2],
                ["rx(-pi / 2) q[0];", -π/2],
                ["rx(-pi) q[0];", -π],
                ["rx(pi + 2 * pi) q[0];", 3π],
                ["rx(pi + pi / 2) q[0];", 3π/2],
                ["rx((pi / 4) + (pi / 2) / 2) q[0];", π/2],
                ["rx(0) q[0];", 0],
                ["rx(0 + 0) q[0];", 0],
                ["rx((1.1 + 2.04) / 2) q[0];", 3.14/2],
                ["rx((6 - 2.86) * 0.5) q[0];", (6 - 2.86) * 0.5],
                ["rx(pi ** 2) q[0];", π^2],
            ]
            qasm = """
            OPENQASM 3.0;
            bit[1] b;
            qubit[1] q;
            $operation
            """
            parsed  = parse_qasm(qasm)
            visitor = QasmProgramVisitor()
            visitor(parsed)
            @test visitor.instructions[1] == (type="rx", arguments=InstructionArgument[arg], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)
        end
    end
    @testset "Qubits with variable as size" begin
        qasm_string = """
        OPENQASM 3.0;

        def ghz(int[32] n) {
            h q[0];
            for int i in [0:n - 1] {
                cnot q[i], q[i + 1];
            }
        }

        int[32] n = 5;
        bit[n + 1] c;
        qubit[n + 1] q;

        ghz(n);

        c = measure q;
        """
        parsed  = parse_qasm(qasm_string)
        visitor = Quasar.QasmProgramVisitor()
        visitor(parsed)
        @test Quasar.qubit_count(visitor) == 6
    end
    @testset "String inputs" begin
        qasm_string = """
        const int[8] n = 4;
        input bit[n] x;

        qubit q;

        def parity(bit[n] cin) -> bit {
          bit c = false;
          for int[8] i in [0: n - 1] {
            c ^= cin[i];
          }
          return c;
        }

        if(parity(x)) {
            x q;
        } else {
            i q;
        }
        """
        parsed  = parse_qasm(qasm_string)
        visitor = Quasar.QasmProgramVisitor(Dict("x"=>"1011"))
        visitor(parsed)
        @test only(visitor.instructions) == (type="x", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)
        visitor = Quasar.QasmProgramVisitor(Dict("x"=>"0011"))
        visitor(parsed)
        @test only(visitor.instructions) == (type="i", arguments=InstructionArgument[], targets=[0], controls=Pair{Int,Int}[], exponent=1.0)
    end
    @testset "Include" begin
        mktempdir() do dir_path
            inc_path = joinpath(dir_path, "new_gate.inc")
            write(inc_path, """\ngate u3(θ, ϕ, λ) q { gphase(-(ϕ+λ)/2); U(θ, ϕ, λ) q; }\n""")
            qasm = """
            OPENQASM 3;
            include "$inc_path";
            """
            parsed  = parse_qasm(qasm)
            visitor = QasmProgramVisitor()
            visitor(parsed)
            @test haskey(visitor.gate_defs, "u3")
        end
        @testset "stdgates.inc" begin
            qasm = """
            OPENQASM 3.0;
            include "stdgates.inc";
            """
            parsed  = parse_qasm(qasm)
            visitor = QasmProgramVisitor()
            visitor(parsed)
            @test haskey(visitor.gate_defs, "tdg")
        end
    end
    @testset "Qubit spectroscopy" begin
        qasm = """
        defcalgrammar "openpulse";

        // sweep parameters would be programmed in by some higher level bindings
        const float frequency_start = 4.5e9;
        const float frequency_step = 1e6;
        const int frequency_num_steps = 301;

        // define a long saturation pulse of a set duration and amplitude
        defcal saturation_pulse \$0 {
            // assume frame can be linked from a vendor supplied `cal` block
            play(driveframe, constant(0.1, 100e-6));
        }

        // step into a `cal` block to set the start of the frequency sweep
        cal {
            set_frequency(driveframe, frequency_start);
        }

        for int i in [1:frequency_num_steps] {
            // step into a `cal` block to adjust the pulse frequency via the frame frequency
            cal {
                shift_frequency(driveframe, frequency_step);
            }

            saturation_pulse \$0;
            measure \$0;
        }
        """
        parsed = parse_qasm(qasm)
        # Not implemented yet!
        #visitor = Quasar.QasmProgramVisitor()
        #visitor(parsed)
    end
    @testset "Rabi time spectroscopy" begin
        qasm = """
        defcalgrammar "openpulse";

        const duration pulse_length_start = 20dt;
        const duration pulse_length_step = 1dt;
        const int pulse_length_num_steps = 100;

        for int i in [1:pulse_length_num_steps] {
            duration pulse_length = pulse_length_start + (i-1)*pulse_length_step;
            duration sigma = pulse_length / 4;
            // since we are manipulating pulse lengths it is easier to define and play the waveform in a `cal` block
            cal {
                waveform wf = gaussian(0.5, pulse_length, sigma);
                // assume frame can be linked from a vendor supplied `cal` block
                play(driveframe, wf);
            }
            measure \$0;
        }
        """
        parsed = parse_qasm(qasm)
        # Not yet implemented!
        #visitor = Quasar.QasmProgramVisitor()
        #visitor(parsed)
    end
    @testset "Cross-frequency resonance" begin
        qasm = """
        defcalgrammar "openpulse";
        cal {
           // Access globally (or externally) defined ports
           extern port d0;
           extern port d1;
           frame frame0 = newframe(d0, 5.0e9, 0);
        }

        defcal cross_resonance \$0, \$1 {
            waveform wf1 = gaussian_square(1., 1024dt, 128dt, 32dt);
            waveform wf2 = gaussian_square(0.1, 1024dt, 128dt, 32dt);

            // generate new frame for second drive that is locally scoped
            // initialized to time at the beginning of `cross_resonance`
            frame temp_frame = newframe(d1, get_frequency(frame0), get_phase(frame0));

            play(frame0, wf1);
            play(temp_frame, wf2);

        }
        """
        parsed = parse_qasm(qasm)
        # Not yet implemented!
        #visitor = Quasar.QasmProgramVisitor()
        #visitor(parsed)
    end
end
