const FloatBits = NamedTuple{(:all, :sign, :exponent, :significand),T} where T

"""
     StandardFloat

A `StandardFloat` is a floating point type that respects IEEE754-2019.

This registers the type so it is available for use with pkg functions.
"""
struct StandardFloat{T,R} # item Type, is Registered
    name::Symbol
    type::Union{Nothing, T}
    bits::FloatBits
end

function StandardFloat(type::T, name::Symbol, bits::FloatBits) where T
    if isdefined(Base, :exponent_bits)
        Base.exponent_bits(::Type{T)} = bits.exponent
        Base.significand_bits(::Type{T}) = bits.significand
    else
        exponent_bits(::Type{T)} = bits.exponent
        significand_bits(::Type{T}) = bits.significand
    end
    sign_bits(::Type{T}) = bits.sign
end
            
function register_ieeefloat(stdfloat::StandardFloat{T,R}) where {T,R}
end

"""
    Float128

This placeholder is used to reference related constants.
""" Float128

"""
    Float8

This placeholder is used to reference related constants.
""" Float8

"""
    Binary8

This placeholder is used to reference related constants.
""" Binary8

if !isdefined(Main, :Float128)
    primitive type Float128 <: AbstractFloat 128 end
end
if !isdefined(Main, :Float128)
    primitive type Float8   <: AbstractFloat 8 end
end
if !isdefined(Main, :Binary8)
    primitive type Binary8  <: AbstractFloat 8 end
end

sign_bits(::Type{Float128}) = 1
sign_filter(::Type{Float128}) = ~zero(UInt128) >> 1
sign_mask(::Type{Float128}) = ~sign_filter(Float128)

sign_bits(::Type{Float8}) = 1
sign_filter(::Type{Float8}) = ~zero(UInt8) >> 1
sign_mask(::Type{Float8}) = ~sign_filter(Float8)

if isdefined(Base, :exponent_bits)
    Base.exponent_bits(::Type{Float128}) = 15
    Base.exponent_bits(::Type{Float8}) = 3
else
    exponent_bits(::Type{Float128}) = 15
    exponent_bits(::Type{Float8}) = 3
end

if isdefined(Base, :exponent_mask)
    Base.exponent_mask(::Type{Float128}) = ((UInt128(1) << Base.significand_bits(Float128)) - UInt128(1)) | sign_mask(Float128)
    Base.exponent_mask(::Type{Float8}) = ((UInt8(1) << Base.significand_bits(Float8)) - UInt8(1)) | sign_mask(Float8)
else
    exponent_mask(::Type{Float128}) = ((UInt128(1) << Base.significand_bits(Float128)) - UInt128(1)) | sign_mask(Float128)
    exponent_mask(::Type{Float8}) = ((UInt8(1) << Base.significand_bits(Float8)) - UInt8(1)) | sign_mask(Float8)
end

if isdefined(Base, :significand_bits)
    Base.significand_bits(::Type{Float128}) = 112
    Base.significand_bits(::Type{Float8}) = 4
else
    significand_bits(::Type{Float128}) = 112
    significand_bits(::Type{Float8}) = 4
end

if isdefined(Base, :significand_mask)
    Base.significand_mask(::Type{Float128}) = (UInt128(1) << Base.significand_bits(Float128)) - UInt128(1)
    Base.significand_mask(::Type{Float8}) = (UInt8(1) << Base.significand_bits(Float8)) - UInt8(1)
else
    significand_mask(::Type{Float128}) = (UInt128(1) << Base.significand_bits(Float128)) - UInt128(1)
    significand_mask(::Type{Float8}) = (UInt8(1) << Base.significand_bits(Float8)) - UInt8(1)
end

exponent_filter(::Type{Float128}) = ~exponent_mask(Float128)
exponent_filter(::Type{Float8}) = ~exponent_mask(Float8)

significand_filter(::Type{Float128}) = ~significand_mask(Float128)
significand_filter(::Type{Float8}) = ~significand_mask(Float8)
