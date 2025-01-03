popcount()              = 0
popcount(s::String)     = count(c->c=='1', s)
popcount(c::Char...)    = count(c_->c_=='1', c)
popcount(bv::BitVector) = count(bv)
popcount(b::Bool...)    = count(b)
popcount(i::Integer)    = count_ones(i)
popcount(u::Unsigned)   = count_ones(u)

const builtin_functions = Dict{String, Function}(
    "sizeof" => size,
    "arccos" => acos,
    "arcsin" => asin,
    "arctan" => atan,
    "ceiling" => ceil,
    "floor" => floor,
    "cos" => cos,
    "sin" => sin,
    "tan" => tan,
    "exp" => exp,
    "log" => log,
    "mod" => mod,
    "pow" => ^,
    "sqrt" => sqrt,
    "popcount" => popcount,
)

const builtin_pulse_functions = Dict{String, Function}("mix"=>((wf1, wf2)->broadcast(*, wf1, wf2)),
                                                       "sum"=>((wf1, wf2)->broadcast(+, wf1, wf2)),
                                                       "phase_shift"=>((wf, angle)->(wf .* exp(im*angle))),
                                                       "scale"=>((wf, factor)->(wf .* factor)),
                                                      )

