module IEEEFloats

export bitwidth, signbit, sign, precision, exponent, significand,
       exponentmax, exponentmin, exponentfieldmax,
       intfloatmax, intfloatmin, floatintmax, floatintmin,
       get_sign_field, get_exponent_field, get_signficand_field,
       get_sign_and_exponent_fields, get_exponent_and_significand_fields,
       set_sign_field, set_exponent_field, set_signficand_field,
       set_sign_and_exponent_fields, set_exponent_and_significand_fields,
       IEEEFloat

import Base.Math.IEEEFloat
import Base.Math: precision, significand_bits, exponent_bits
import Base: signed, unsigned

signed(::Type{Float64}) = Int64
signed(::Type{Float32}) = Int32
signed(::Type{Float16}) = Int16

unsigned(::Type{Float64}) = UInt64
unsigned(::Type{Float32}) = UInt32
unsigned(::Type{Float16}) = UInt16

@inline bitwidth(::Type{T}) where T<:IEEEFloat = sizeof(T) * 8

@inline exponentmax(::Type{Float16})  =     15
@inline exponentmax(::Type{Float32})  =    127
@inline exponentmax(::Type{Float64})  =   1023

@inline intfloatmax(::Type{Float16}) = Int16(2048)
@inline intfloatmax(::Type{Float32}) = Int32(16777216)
@inline intfloatmax(::Type{Float64}) = Int64(9007199254740992)

@inline intfloatmin(::Type{Float16}) = Int16(-2048)
@inline intfloatmin(::Type{Float32}) = Int32(-16777216)
@inline intfloatmin(::Type{Float64}) = Int64(-9007199254740992)

@inline floatintmax(::Type{Float16}) = Float16(intfloat_max(Float16))
@inline floatintmax(::Type{Float32}) = Float32(intfloat_max(Float32))
@inline floatintmax(::Type{Float64}) = Float64(intfloat_max(Float64))

@inline floatintmin(::Type{Float16}) = Float16(intfloat_min(Float16))
@inline floatintmin(::Type{Float32}) = Float32(intfloat_min(Float32))
@inline floatintmin(::Type{Float64}) = Float64(intfloat_min(Float64))

@inline exponentmin(::Type{T}) where T<:IEEEFloat = 1 - exponent_max(T)

@inline exponentbias(::Type{T}) where T<:IEEEFloat = exponent_max(T)

@inline exponentfieldmax(::Type{T}) where T<:IEEEFloat = exponent_max(T) + one(signed(T))

# field[s] offset (shift by)

@inline sign_field_offset(::Type{T}) where T<:IEEEFloat = bitwidth(T) - one(signed(T))
@inline exponent_field_offset(::Type{T}) where T<:IEEEFloat = sign_field_offset(T) - exponent_bits(T)
@inline significand_field_offset(::Type{T}) where T<:IEEEFloat = zero(signed(T))
@inline sign_and_exponent_fields_offset(::Type{T}) where T<:IEEEFloat = exponent_field_offset(T)
@inline exponent_and_significand_fields_offset(::Type{T}) where T<:IEEEFloat = significand_field_offset(T)

# field[s] filter and mask

@inline sign_field_filter(::Type{T}) where T<:IEEEFloat = ~(zero(convert(Unsigned,T))) >>> 1
@inline sign_and_exponent_fields_filter(::Type{T}) where T<:IEEEFloat = ~(zero(convert(Unsigned,T))) >>> (exponent_bits(T) + 1)
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

exponent_bias(::Type{T}) = 2^(exponent_bits(T) - 1) - 1


const bias128 = 16_383 # 15 bits
const bias64 = exponent_bias(Float64) # 1023
const bias32 = exponent_bias(Float64) # 127
const bias16 = exponent_bias(Float64) # 15

exponent_bias(nexpbits) = 2^(nexpbits-1) -1
unbiased_exponent(nexpbits, biasedexp) = biasedexp - exponent_bias(nexpbits)
biased_exponent(nexpbits, unbiasedexp) = unbiasedexp + exponent_bias(nexpbits)

decode_normal_value(nexpbits, nsigbits, significand, unbiasedexponent=0, isneg::Bool=false) =
    (isneg ? -1 : 1) *
    (1 + (significand / 2^nsigbits)) *
    (2.0^(unbiasedexponent - exponent_bias(nexpbits)))
decode_subnormal_value(nexpbits, nsigbits, significand, unbiasedexponent=0, isneg::Bool=false) =
    (isneg ? -1 : 1) *
    (significand) *
    (2.0^(1 - exponent_bias(nexpbits) - nsigbits))

end # module IEEEFloats
