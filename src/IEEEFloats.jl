module IEEEFloats

export IEEEFloat, ieeefloat,
       sign_mask, significand_mask, exponent_mask,
       significand_bits, exponent_bits,
       exponent_bias, exponent_biasing, exponent_unbiasing,
       unbiased_exponent_max, unbiased_exponent_min,
       biased_exponent_max, biased_exponent_min,
       get_sign, get_exponent, get_signficand,
       set_sign, set_exponent, set_signficand,
 
using Base: IEEEFloat, precision, significand_bits, exponent_bits, significand_mask, exponent_mask
# Base: signed, unsigned, maxintfloat

# `f(xT)` denotes a function `f` that accepts either a value `x::T` or a type `T

sign_bits(::Type{T}) where T<:IEEEFloat = 1

include("support_more_types.jl")  # Float128, Float8
include("corresponding_sizes.jl") # ieeefloat(xT), unsigned(xT), signed(xT)

bitwidth(::Type{T}) where T = sizeof(T) * 8

exponent_bias(n_exponent_bits) = 2^(n_exponent_bits - 1) - 1
exponent_bias(::Type{T}) where T = exponent_bias(exponent_bits(T))

# unbiased_exponent_max(T) == exponent_bias(T)
unbiased_exponent_max(::Type{T}) where T = 2^(exponent_bits(T) - 1) - 1  # for normal values
unbiased_exponent_min(::Type{T}) where T = 1 - exponent_max(T)           # for normal values

biased_exponent_max(::Type{T}) where T = 2^(exponent_bits(T)) - 1  
biased_exponent_min(::Type{T}) where T = 0

exponent_biasing(::Type{T}, unbiased) where T = unbiased + exponent_bias(T)
exponent_unbiasing(::Type{T}, biased) where T = biased - exponent_bias(T)


include("use_fields.jl") # get_x_field, set_x_field, isolate_x_field, clear_x_field




# ============== #


decode_normal_value(nexpbits, nsigbits, significand, unbiasedexponent=0, isneg::Bool=false) =
    (isneg ? -1 : 1) *
    (1 + (significand / 2^nsigbits)) *
    (2.0^(unbiasedexponent - exponent_bias(nexpbits)))
decode_subnormal_value(nexpbits, nsigbits, significand, unbiasedexponent=0, isneg::Bool=false) =
    (isneg ? -1 : 1) *
    (significand) *
    (2.0^(1 - exponent_bias(nexpbits) - nsigbits))

              
              
              
 #=
const bias128 = 16_383 # 15 bits
const bias64 = exponent_bias(exponent_bits(Float64)) # 1023
const bias32 = exponent_bias(exponent_bits(Float32)) # 127
const bias16 = exponent_bias(exponent_bits(Float16)) # 15
const bias8 = exponent_bias(3) # 3
  
exponent_bias(::Type{T}) where T<:IEEEFloat = exponent_max(T)
exponent_max(::Type{T}) where T =   2^(exponent_bits(T) - 1) - 1  # normal values
exponent_min(::Type{T}) where T =  1 - exponent_max(T)             # normal values

biased_exponentmax(::Type{T}) where T = 2^(exponent_bits(T)) - 1  
biased_exponentmin(::Type{T}) where T = 0
=#
             
              
              
end # module IEEEFloats
