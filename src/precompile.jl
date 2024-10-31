@setup_workload begin
    # Putting some things in `setup` can reduce the size of the
    # precompile file and potentially make loading faster.
    using Quasar
    @compile_workload begin
        # all calls in this block will be precompiled, regardless of whether
        # they belong to your package or not (on Julia 1.8 and higher)
        qasm_src = """OPENQASM 3.0;
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
        Quasar.dt_type[] = Quasar.Nanosecond
        Quasar.builtin_gates[] = Quasar.basic_builtin_gates
        Quasar.visit_pragma[]  = Quasar.basic_visit_pragma
        Quasar.parse_pragma[]  = Quasar.basic_parse_pragma
        visitor = Quasar.QasmProgramVisitor()
        visitor(parsed);
    end
end
