struct Port end
struct Frame
    port::Port
    frequency::Float64
    phase::Float64
end

abstract type AbstractWaveform end
struct ArrayWaveform <: AbstractWaveform
    values::Vector{ComplexF64}
end
struct FunctionWaveform <: AbstractWaveform
    generator::Function
end

struct SizedBitVector <: AbstractArray{Bool, 1}
    size::QasmExpression
    SizedBitVector(size::QasmExpression) = new(size)
    SizedBitVector(sbv::SizedBitVector) = new(sbv.size)
end
Base.length(s::SizedBitVector) = s.size
Base.size(s::SizedBitVector) = (s.size,)
Base.show(io::IO, s::SizedBitVector) = print(io, "SizedBitVector{$(s.size.args[end])}")
struct SizedInt <: Integer
    size::QasmExpression
    SizedInt(size::QasmExpression) = new(size)
    SizedInt(sint::SizedInt) = new(sint.size)
end
Base.show(io::IO, s::SizedInt) = print(io, "SizedInt{$(s.size.args[end])}")
struct SizedUInt <: Unsigned 
    size::QasmExpression
    SizedUInt(size::QasmExpression) = new(size)
    SizedUInt(suint::SizedUInt) = new(suint.size)
end
Base.show(io::IO, s::SizedUInt) = print(io, "SizedUInt{$(s.size.args[end])}")
struct SizedFloat <: AbstractFloat
    size::QasmExpression
    SizedFloat(size::QasmExpression) = new(size)
    SizedFloat(sfloat::SizedFloat) = new(sfloat.size)
end
Base.show(io::IO, s::SizedFloat) = print(io, "SizedFloat{$(s.size.args[end])}")
struct SizedAngle <: AbstractFloat
    size::QasmExpression
    SizedAngle(size::QasmExpression) = new(size)
    SizedAngle(sangle::SizedAngle) = new(sangle.size)
end
Base.show(io::IO, s::SizedAngle) = print(io, "SizedAngle{$(s.size.args[end])}")
struct SizedComplex <: Number
    size::QasmExpression
    SizedComplex(size::QasmExpression) = new(size)
    SizedComplex(scomplex::SizedComplex) = new(scomplex.size)
end
Base.show(io::IO, s::SizedComplex) = print(io, "SizedComplex{$(s.size.args[end])}")

struct SizedArray{T,N} <: AbstractArray{T, N} 
    type::T
    size::NTuple{N, Int}
end
function SizedArray(eltype::QasmExpression, size::QasmExpression)
    arr_size = if head(size) == :n_dims
        ntuple(i->0, size.args[1].args[1])
    else
        ntuple(i->size.args[i], length(size.args))
    end
    return SizedArray(eltype.args[1], arr_size)
end
Base.show(io::IO, s::SizedArray{T, N}) where {T, N} = print(io, "SizedArray{$(sprint(show, s.type)), $N}")
Base.size(a::SizedArray{T, N}, dim::Int=0) where {T, N} = a.size[dim+1]

const SizedNumber = Union{SizedComplex, SizedAngle, SizedFloat, SizedInt, SizedUInt}
Base.iterate(::Union{SizedArray, SizedBitVector, SizedNumber})            = nothing
Base.iterate(::Union{SizedArray, SizedBitVector, SizedNumber}, ::Nothing) = nothing
if v"1.9" <= VERSION < v"1.11" 
    Base.Iterators.iterlength(::Union{SizedNumber, SizedBitVector, SizedArray}) = -1
end
