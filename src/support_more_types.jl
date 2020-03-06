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

Base.exponent_bits(::Type{Float128}) = 15
Base.significand_bits(::Type{Float128}) = 112

Base.exponent_bits(::Type{Float8}) = 3
Base.significand_bits(::Type{Float8}) = 4
