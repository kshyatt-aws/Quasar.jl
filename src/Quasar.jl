module Quasar

using Automa, AbstractTrees, DataStructures, Dates, PrecompileTools
using DataStructures: Stack

export parse_qasm, QasmProgramVisitor

struct QasmParseError <: Exception
    message::String
    parse_stack::Stack
    position::Int
    qasm::String
end
function Base.showerror(io::IO, err::QasmParseError)
    print(io, "QasmParseError: ")
    print(io, err.message * "\n")
    max_codeunits = min(length(err.qasm), err.position+100)
    print(io, "Qasm location: ", err.qasm[err.position:max_codeunits])
end

const dt_type       = Ref{DataType}()
const builtin_gates = Ref{Function}()
const visit_pragma  = Ref{Function}()
const parse_pragma  = Ref{Function}()

function basic_parse_pragma end
function basic_visit_pragma end

function __init__()
    dt_type[]       = Nanosecond
    builtin_gates[] = basic_builtin_gates
    visit_pragma[]  = basic_visit_pragma
    parse_pragma[]  = basic_parse_pragma
end

include("tokenizer.jl")
include("builtin_functions.jl")
include("qasm_expression.jl")
include("types.jl")
include("parser.jl")
include("visitor.jl")
include("precompile.jl")

end
