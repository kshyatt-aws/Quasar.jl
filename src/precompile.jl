@setup_workload begin
    # Putting some things in `setup` can reduce the size of the
    # precompile file and potentially make loading faster.
    using Quasar
    Quasar.dt_type[] = Quasar.Nanosecond
    Quasar.builtin_gates[] = Quasar.basic_builtin_gates
    Quasar.visit_pragma[]  = Quasar.basic_visit_pragma
    Quasar.parse_pragma[]  = Quasar.basic_parse_pragma
    stdgates_path = joinpath(@__DIR__, "..", "test", "stdgates.inc")
    include(joinpath(@__DIR__, "..", "test", "builtin_gates.jl"))
    @compile_workload begin
        qasm_src = """
               int[8] two = 2;
               include \"$stdgates_path\";
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
               """;
        parsed = Quasar.parse_qasm(qasm_src)
        visitor = Quasar.QasmProgramVisitor()
        visitor(parsed);
        qasm_src = """
            OPENQASM 3;
            include \"$stdgates_path\";
            input uint[4] a_in;
            input uint[4] b_in;

            gate majority a, b, c {
                cx c, b;
                cx c, a;
                cx a, b, c;
            }

            gate unmaj a, b, c {
                ctrl @ cx a, b, c;
                cx c, a;
                cx a, b;
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
            cx a[0], cout;
            for int[8] i in [1: 3] { unmaj a[i], b[i - 1], a[i - 1]; }
            unmaj cin, b[3], a[3];
            measure cin, a, b, cout;
            """
        parsed = Quasar.parse_qasm(qasm_src)
        visitor = Quasar.QasmProgramVisitor(Dict("a_in"=>3, "b_in"=>7));
        visitor(parsed);
        qasm_src = """
            OPENQASM 3.0;

            include \"$stdgates_path\";
            qubit[16] q;

            h q;
            cz q[0], q[1];
            cz q[6], q[7];
            cz q[8], q[9];
            cz q[14], q[15];

            t q[2:5];
            t q[10:13];

            cz q[4], q[8];
            cz q[6], q[10];

            ry(π/2) q[{0, 1, 14}];
            rx(π/2) q[{7, 9, 15}];

            cz q[1], q[2];
            cz q[9], q[10];

            t q[0];

            ry(π/2) q[4];
            rx(π/2) q[6];

            t q[7];

            rx(π/2) q[8];

            t q[14:15];

            cz q[0], q[4];
            cz q[9], q[13];
            cz q[2], q[6];
            cz q[11], q[15];

            ry(π/2) q[1];

            t q[8];

            ry(π/2) q[10];

            cz q[2], q[3];
            cz q[4], q[5];
            cz q[10], q[11];
            cz q[12], q[13];

            ry(π/2) q[0];

            t q[1];

            rx(π/2) q[6];
            ry(π/2) q[{9, 15}];

            cz q[5], q[9];
            cz q[7], q[11];

            t q[0];

            rx(π/2) q[2];
            ry(π/2) q[3:4];

            t q[6];

            ry(π/2) q[{10, 12}];
            rx(π/2) q[13];

            t q[15];

            cz q[5], q[6];
            cz q[13], q[14];

            t q[2:4];

            ry(π/2) q[{7, 9}];

            t q[10];

            ry(π/2) q[11];

            t q[12];

            cz q[8], q[12];
            cz q[1], q[5];
            cz q[10], q[14];
            cz q[3], q[7];

            rx(π/2) q[6];

            t q[{9, 11}];

            rx(π/2) q[13];

            cz q[0], q[1];
            cz q[6], q[7];
            cz q[8], q[9];
            cz q[14], q[15];

            rx(π/2) q[3];
            ry(π/2) q[{5, 10, 12}];

            t q[13];

            h q;
            measure q;
        """
        parsed = Quasar.parse_qasm(qasm_src)
        visitor = Quasar.QasmProgramVisitor()
        visitor(parsed);
        
        qasm_src = """
            OPENQASM 3.0;
            include \"$stdgates_path\";
            bit[2] b;
            qubit[2] q;
            ry(-4.97894242803364) q[0];
            ry(-4.435983179322655) q[1];
            cz q[0], q[1];
            ry(6.249142469550989) q[0];
            ry(2.509637558409141) q[1];
            cz q[0], q[1];
            ry(-5.476946260844031) q[0];
            ry(5.937223228770655) q[1];
            cz q[0], q[1];
            ry(1.6839702128823246) q[0];
            ry(-3.211915934619051) q[1];
            b[0] = measure q[0];
            b[1] = measure q[1];
            """
        parsed = Quasar.parse_qasm(qasm_src)
        visitor = Quasar.QasmProgramVisitor()
        visitor(parsed);


        # all calls in this block will be precompiled, regardless of whether
        # they belong to your package or not (on Julia 1.8 and higher)
        qasm_src = """OPENQASM 3.0;
                   include \"$stdgates_path\";
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

                   bell(q[0], q[1]);
                   int xx = 1;
                   xx = 3;
                   """
        parsed = Quasar.parse_qasm(qasm_src)
        visitor = Quasar.QasmProgramVisitor()
        visitor(parsed);
        
        # run a precompilation workload with more complex builtin gates
        Quasar.builtin_gates[] = complex_builtin_gates
        qasm_src = """
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
        parsed  = Quasar.parse_qasm(qasm_src)
        visitor = Quasar.QasmProgramVisitor(Dict("a_in"=>3, "b_in"=>7))
        visitor(parsed)
    end
end
