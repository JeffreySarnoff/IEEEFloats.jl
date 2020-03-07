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

struct Float128 <: AbstractFloat end
struct Float8   <: AbstractFloat end
struct Binary8  <: AbstractFloat end


sign_bits(::Type{Float128}) = 1
sign_filter(::Type{Float128}) = ~zero(UInt128) >> 1
sign_mask(::Type{Float128}) = ~sign_filter(Float128)

Base.exponent_bits(::Type{Float128}) = 15
Base.exponent_mask(::Type{Float128}) = ((UInt128(1) << Base.significand_bits(Float128)) - UInt128(1)) | sign_mask(Float128)
exponent_filter(::Type{Float128}) = ~exponent_mask(Float128)

Base.significand_bits(::Type{Float128}) = 112
Base.significand_mask(::Type{Float128}) =  (UInt128(1) << Base.significand_bits(Float128)) - UInt128(1)
significand_filter(::Type{Float128}) = ~significand_mask(Float128)


sign_bits(::Type{Float8}) = 1
sign_filter(::Type{Float8}) = ~zero(UInt8) >> 1
sign_mask(::Type{Float8}) = ~sign_filter(Float8)

Base.exponent_bits(::Type{Float8}) = 3
Base.exponent_mask(::Type{Float8}) = ((UInt8(1) << Base.significand_bits(Float8)) - UInt8(1)) | sign_mask(Float8)
exponent_filter(::Type{Float8}) = ~exponent_mask(Float8)

Base.significand_bits(::Type{Float8}) = 4
Base.significand_mask(::Type{Float8}) =  (UInt8(1) << Base.significand_bits(Float8)) - UInt8(1)
significand_filter(::Type{Float128}) = ~significand_mask(Float8)
