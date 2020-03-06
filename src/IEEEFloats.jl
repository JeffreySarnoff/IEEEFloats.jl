module IEEEFloats

export ieeefloat, bitwidth, 
       signbit, sign, precision, exponent, significand,
       exponentmax, exponentmin, exponentfieldmax,
       intfloatmax, intfloatmin, floatintmax, floatintmin,
       get_sign_field, get_exponent_field, get_signficand_field,
       get_sign_and_exponent_fields, get_exponent_and_significand_fields,
       set_sign_field, set_exponent_field, set_signficand_field,
       set_sign_and_exponent_fields, set_exponent_and_significand_fields,
       IEEEFloat, ieeefloat

using Base: IEEEFloat, precision, significand_bits, exponent_bits, significand_mask, exponent_mask,
            signed, unsigned

# `f(xT)` denotes a function `f` that accepts either a value `x::T` or a type `T

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





# field[s] offset (shift by)

@inline sign_field_offset(::Type{T}) where T<:IEEEFloat = bitwidth(T) - one(signed(T))
@inline exponent_field_offset(::Type{T}) where T<:IEEEFloat = sign_field_offset(T) - exponent_bits(T)
@inline significand_field_offset(::Type{T}) where T<:IEEEFloat = zero(signed(T))
@inline sign_and_exponent_fields_offset(::Type{T}) where T<:IEEEFloat = exponent_field_offset(T)
@inline exponent_and_significand_fields_offset(::Type{T}) where T<:IEEEFloat = significand_field_offset(T)

# field[s] filter and mask

@inline sign_field_filter(::Type{T}) where T<:IEEEFloat = ~(zero(unsigned(T)) >>> 1
@inline sign_and_exponent_fields_filter(::Type{T}) where T<:IEEEFloat = ~(zero(unsigned(T)) >>> (exponent_bits(T) + 1)
@inline exponent_field_filter(::Type{T}) where T<:IEEEFloat = sign_and_exponent_fields_filter(T) | sign_field_mask(T)
@inline significand_field_filter(::Type{T}) where T<:IEEEFloat = ~sign_and_exponent_fields_filter(T)
@inline exponent_and_significand_fields_filter(::Type{T}) where T<:IEEEFloat = ~(sign_field_filter(T))

@inline sign_field_mask(::Type{T}) where T<:IEEEFloat = ~sign_field_filter(T)
@inline sign_and_exponent_fields_mask(::Type{T}) where T<:IEEEFloat = ~sign_and_exponent_fields_filter(T)
@inline exponent_field_mask(::Type{T}) where T<:IEEEFloat = ~exponent_field_filter(T)
@inline significand_field_mask(::Type{T}) where T<:IEEEFloat = ~sign_and_exponent_fields_mask(T)
@inline exponent_and_significand_fields_mask(::Type{T}) where T<:IEEEFloat = ~exponent_and_significand_fields_mask(T)

@inline sign_field_mask_lsbs(::Type{T}) where T<:IEEEFloat = sign_field_mask(T) >> sign_field_offset(T)
@inline sign_and_exponent_fields_mask_lsbs(::Type{T}) where T<:IEEEFloat = sign_and_exponent_fields_mask(T) >> exponent_field_offset(T)
@inline exponent_field_mask_lsbs(::Type{T}) where T<:IEEEFloat = exponent_field_mask(T) >> exponent_field_offset(T)
@inline significand_field_mask_lsbs(::Type{T}) where T<:IEEEFloat = significand_fields_mask(T) >> significand_field_offset(T)
@inline exponent_and_significand_fields_mask_lsbs(::Type{T}) where T<:IEEEFloat = exponent_and_significand_fields_mask(T) >> significand_field_offset(T)

# isolate the field[s] from other bits and yield the field value, as Unsigned bits in place
       
@inline sign_field(x::T) where T<:IEEEFloat = convert(Unsigned, x) & sign_field_mask(T)
@inline exponent_field(x::T) where T<:IEEEFloat = convert(Unsigned, x) & exponent_field_mask(T)
@inline significand_field(x::T) where T<:IEEEFloat = convert(Unsigned, x) & significand_field_mask(T)
@inline sign_and_exponent_fields(x::T) where T<:IEEEFloat = convert(Unsigned, x) & sign_and_exponent_field_mask(T)
@inline exponent_and_significand_fields(x::T) where T<:IEEEFloat = convert(Unsigned, x) & sign_field_filter(T)
@inline sign_and_significand_fields(x::T) where T<:IEEEFloat = convert(Unsigned, x) & exponent_field_mask(T)

# clear the field[s] and yield the value, as Unsigned bits in place

@inline clear_sign_field(x::T) where T<:IEEEFloat = convert(Unsigned, x) & sign_field_filter(T)
@inline clear_exponent_field(x::T) where T<:IEEEFloat = convert(Unsigned, x) & exponent_field_filter(T)
@inline clear_significand_field(x::T) where T<:IEEEFloat = convert(Unsigned, x) & significand_field_filter(T)
@inline clear_sign_and_exponent_fields(x::T) where T<:IEEEFloat = convert(Unsigned, x) & sign_and_exponent_field_filter(T)
@inline clear_exponent_and_significand_fields(x::T) where T<:IEEEFloat = convert(Unsigned, x) & exponent_and_significand_fields_filter(T)
@inline clear_sign_and_significand_fields(x::T) where T<:IEEEFloat = convert(Unsigned, x) & exponent_field_mask(T)

# fetch the field[s] into the low order bits of an Unsigned

@inline get_sign_field(x::T) where T<:Unsigned = sign_field(x) >> sign_field_offset(T)
@inline get_exponent_field(x::T) where T<:Unsigned = exponent_field(x) >> exponent_field_offset(T)
@inline get_significand_field(x::T) where T<:Unsigned = significand_field(x) >> significand_field_offset(T)
@inline get_sign_and_exponent_fields(x::T) where T<:Unsigned = sign_and_exponent_fields(x) >> exponent_field_offset(T)
@inline get_exponent_and_significand_fields(x::T) where T<:Unsigned = exponent_and_significand_fields(x) >> significand_field_offset(T)

# prepare Unsigned low order bits to occupy field[s]

@inline set_sign_field(x::T) where T<:Unsigned = (x & sign_field_mask_lsbs(T)) << sign_field_offset(T)
@inline set_exponent_field(x::T) where T<:Unsigned = (x & exponent_field_mask_lsbs(T)) << exponent_field_offset(T)
@inline set_significand_field(x::T) where T<:Unsigned = (x & significand_field_mask_lsbs(T)) << significand_field_offset(T)
@inline set_sign_and_exponent_fields(x::T) where T<:Unsigned = (x & sign_and_exponent_fields_mask_lsbs(T)) << exponent_field_offset(T)
@inline set_exponent_and_significand_fields(x::T) where T<:Unsigned = (x & exponent_and_significand_fields_mask_lsbs(T)) << exponent_and_significand_fields_offset(T)

# fetch the field[s] into the low order bits of a Float

@inline get_sign_field(x::T) where T<:Base.IEEEFloat = sign_field(x) >> sign_field_offset(T)
@inline get_exponent_field(x::T) where T<:Base.IEEEFloat = exponent_field(x) >> exponent_field_offset(T)
@inline get_significand_field(x::T) where T<:Base.IEEEFloat = significand_field(x) >> significand_field_offset(T)
@inline get_sign_and_exponent_fields(x::T) where T<:Base.IEEEFloat = sign_and_exponent_fields(x) >> exponent_field_offset(T)
@inline get_exponent_and_significand_fields(x::T) where T<:Base.IEEEFloat = exponent_and_significand_fields(x) >> significand_field_offset(T)


# set field[s]: set_sign_field(1.0, 1%UInt64) == -1.0

for (S,F) in ((:set_sign_field, :filter_sign_field), (:set_exponent_field, :filter_exponent_field),
              (:set_significand_field, :filter_exponent_field), (:set_sign_and_exponent_fields, :filter_sign_and_exponent_fields),
              (:set_exponent_and_significand_fields, :filter_exponent_and_significand_fields))
  for (T,U) in ((:Float64, :UInt64), (:Float32, :UInt32), (:Float16, :UInt16))
    @eval begin
        @inline $S(x::$T, y::$U) = convert($T, $F(convert($U, x)) | $S(y))
    end
  end
end

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
