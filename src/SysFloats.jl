module SysFloats

export SysFloat,
       bitwidth, signbit, sign, precision, exponent, significand,
       exponent_max, exponent_min, exponent_field_max


import Base.Math: precision, significand_bits, exponent_bits

const SysFloats = Union{Float64, Float32, Float16}

@inline bitwidth(::Type{T}) where T<:SysFloats = sizeof(T) * 8

@inline exponent_max(::Type{Float16})  =     15
@inline exponent_max(::Type{Float32})  =    127
@inline exponent_max(::Type{Float64})  =   1023

@inline exponent_min(::Type{T}) where T<:SysFloats = 1 - exponent_max(T)

@inline exponent_bias(::Type{T}) where T<:SysFloats = exponent_max(T)

@inline exponent_field_max(::Type{T}) where T<:SysFloats = exponent_max(T) + one(convert(Signed, T))

# ~~~~~~~~~~~~~~~~

@inline sign_field_offset(::Type{T}) where T<:SysFloats = bitwidth(T) - one(convert(Signed, T))
@inline exponent_field_offset(::Type{T}) where T<:SysFloats = sign_field_offset(T) - exponent_bits(T)
@inline significand_field_offset(::Type{T}) where T<:SysFloats = zero(convert(Signed, T))

@inline sign_field_filter(::Type{T}) where T<:SysFloats = ~(zero(convert(Unsigned,T))) >>> 1
@inline sign_and_exponent_fields_filter(::Type{T}) where T<:SysFloats = ~(zero(convert(Unsigned,T))) >>> (exponent_bits(T) + 1)
@inline exponent_field_filter(::Type{T}) where T<:SysFloats = sign_and_exponent_fields_filter(T) | sign_field_mask(T)
@inline significand_field_filter(::Type{T}) where T<:SysFloats = ~sign_and_exponent_fields_filter(T)

@inline sign_field_mask(::Type{T}) where T<:SysFloats = ~sign_field_filter(T)
@inline sign_and_exponent_fields_mask(::Type{T}) where T<:SysFloats = ~sign_and_exponent_fields_filter(T)
@inline exponent_field_mask(::Type{T}) where T<:SysFloats = ~exponent_field_filter(T)
@inline significand_field_mask(::Type{T}) where T<:SysFloats = ~sign_and_exponent_fields_mask(T)

# ~~~~~~~~~~~~~~~~

# isolate the field[s] from other bits and yield the field value, as Unsigned bits in place

sign_field(x::T) where T<:SysFloat = reinterpret(Unsigned, x) & sign_field_mask(T)
exponent_field(x::T) where T<:SysFloat = reinterpret(Unsigned, x) & exponent_field_mask(T)
significand_field(x::T) where T<:SysFloat = reinterpret(Unsigned, x) & significand_field_mask(T)
sign_and_exponent_fields(x::T) where T<:SysFloat = reinterpret(Unsigned, x) & sign_and_exponent_field_mask(T)
exponent_and_significand_fields(x::T) where T<:SysFloat = reinterpret(Unsigned, x) & sign_field_filter(T)
sign_and_significand_fields(x::T) where T<:SysFloat = reinterpret(Unsigned, x) & exponent_field_mask(T)


# ~~~~~~~~~~~~~~~~

@inline Base.convert(::Type{Unsigned}, ::Type{Float16}) = UInt16
@inline Base.convert(::Type{Unsigned}, ::Type{Float32}) = UInt32
@inline Base.convert(::Type{Unsigned}, ::Type{Float64}) = UInt64

@inline Base.convert(::Type{SysFloat}, ::Type{UInt16}) = Float16
@inline Base.convert(::Type{SysFloat}, ::Type{UInt32}) = Float32
@inline Base.convert(::Type{SysFloat}, ::Type{UInt64}) = Float64

@inline Base.convert(::Type{Unsigned}, x::Float16) = reinterpret(UInt16, x)
@inline Base.convert(::Type{Unsigned}, x::Float32) = reinterpret(UInt32, x)
@inline Base.convert(::Type{Unsigned}, x::Float64) = reinterpret(UInt64, x)

@inline Base.convert(::Type{SysFloat}, x::UInt16) = reinterpret(Float16, x)
@inline Base.convert(::Type{SysFloat}, x::UInt32) = reinterpret(Float32, x)
@inline Base.convert(::Type{SysFloat}, x::UInt64) = reinterpret(Float64, x)

end # SysFloats

#=

import Base.Math: precision, significand_bits, exponent_bits

const SysFloats = Union{Float64, Float32, Float16}

(::Type{T}) where T<:SysFloats =@inline bitwidth(::Type{T}) where T = sizeof(T) * 8

@inline exponent_max(::Type{Float16})  =     15
@inline exponent_max(::Type{Float32})  =    127
@inline exponent_max(::Type{Float64})  =   1023

@inline exponent_min(::Type{T}) where T<:SysFloats = 1 - exponent_max(T)

@inline Base.convert(::Type{Unsigned}, ::Type{Float16}) = UInt16
@inline Base.convert(::Type{Unsigned}, ::Type{Float32}) = UInt32
@inline Base.convert(::Type{Unsigned}, ::Type{Float64}) = UInt64

@inline Base.convert(::Type{SysFloat}, ::Type{UInt16}) = Float16
@inline Base.convert(::Type{SysFloat}, ::Type{UInt32}) = Float32
@inline Base.convert(::Type{SysFloat}, ::Type{UInt64}) = Float64

@inline Base.convert(::Type{Unsigned}, x::Float16) = reinterpret(UInt16, x)
@inline Base.convert(::Type{Unsigned}, x::Float32) = reinterpret(UInt32, x)
@inline Base.convert(::Type{Unsigned}, x::Float64) = reinterpret(UInt64, x)

@inline Base.convert(::Type{SysFloat}, x::UInt16) = reinterpret(Float16, x)
@inline Base.convert(::Type{SysFloat}, x::UInt32) = reinterpret(Float32, x)
@inline Base.convert(::Type{SysFloat}, x::UInt64) = reinterpret(Float64, x)
@inline exponent_bias(::Type{T}) where T<:SysFloats = exponent_max(T)

@inline exponent_field_max(::Type{T}) where T<:SysFloats = exponent_max(T) + one(convert(Signed, T))



# ~~~~~~~~~~~~~~~~

@inline sign_field_offset(::Type{T}) where T<:SysFloats = bitwidth(T) - one(convert(Signed, T))
@inline exponent_field_offset(::Type{T}) where T<:SysFloats = sign_field_offset(T) - exponent_bits(T)
@inline significand_field_offset(::Type{T}) where T<:SysFloats = zero(convert(Signed, T))


@inline Base.convert(::Type{Signed}, ::Type{Float16}) = Int16
@inline Base.convert(::Type{Signed}, ::Type{Float32}) = Int32
@inline Base.convert(::Type{Signed}, ::Type{Float64}) = Int64

@inline Base.convert(::Type{SysFloat}, ::Type{Int16}) = Float16
@inline Base.convert(::Type{SysFloat}, ::Type{Int32}) = Float32
@inline Base.convert(::Type{SysFloat}, ::Type{Int64}) = Float64

@inline Base.convert(::Type{Signed}, x::Float16) = reinterpret(Int16, x)
@inline Base.convert(::Type{Signed}, x::Float32) = reinterpret(Int32, x)
@inline Base.convert(::Type{Signed}, x::Float64) = reinterpret(Int64, x)

@inline Base.convert(::Type{SysFloat}, x::Int16) = reinterpret(Float16, x)
@inline Base.convert(::Type{SysFloat}, x::Int32) = reinterpret(Float32, x)
@inline Base.convert(::Type{SysFloat}, x::Int64) = reinterpret(Float64, x)

end # module
=#

#=
export SysFloats, precision, significand_bits, exponent_bits,
       exponent_max, exponent_min, exponent_bias, exponent_field_max,

import Base.Math: precision, significand_bits, exponent_bits

const SysFloats = Union{Float64, Float32, Float16}

(::Type{T}) where T<:SysFloats =@inline bitwidth(::Type{T}) where T = sizeof(T) * 8

@inline exponent_max(::Type{Float16})  =     15
@inline exponent_max(::Type{Float32})  =    127
@inline exponent_max(::Type{Float64})  =   1023

@inline exponent_min(::Type{T}) where T<:SysFloats = 1 - exponent_max(T)

@inline exponent_bias(::Type{T}) where T<:SysFloats = exponent_max(T)

@inline exponent_field_max(::Type{T}) where T<:SysFloats = exponent_max(T) + one(convert(Signed, T))



# ~~~~~~~~~~~~~~~~

@inline sign_field_offset(::Type{T}) where T<:SysFloats = bitwidth(T) - one(convert(Signed, T))
@inline exponent_field_offset(::Type{T}) where T<:SysFloats = sign_field_offset(T) - exponent_bits(T)
@inline significand_field_offset(::Type{T}) where T<:SysFloats = zero(convert(Signed, T))

@inline sign_field_filter(::Type{T}) where T<:SysFloats = ~(zero(convert(Unsigned,T))) >>> 1
@inline sign_and_exponent_fields_filter(::Type{T}) where T<:SysFloats = ~(zero(convert(Unsigned,T))) >>> (exponent_bits(T) + 1)
@inline exponent_field_filter(::Type{T}) where T<:SysFloats = sign_and_exponent_fields_filter(T) | sign_field_mask(T)

@inline sign_field_mask(::Type{T}) where T<:SysFloats = ~sign_field_filter(T)
@inline sign_and_exponent_fields_mask(::Type{T}) where T<:SysFloats = ~sign_and_exponent_field_filter(T)
@inline exponent_field_mask(::Type{T}) where T<:SysFloats = ~exponent_field_filter(T)

# ~~~~~~~~~~~~~~~~

#
# isolate the field[s] from other bits and yield the field value, as Unsigned bits in place
#

sign_field(x::T) where T<:SysFloat = reinterpret(Unsigned, x) & sign_field_mask(T)
exponent_field(x::T) where T<:SysFloat = reinterpret(Unsigned, x) & exponent_field_mask(T)
significand_field(x::T) where T<:SysFloat = reinterpret(Unsigned, x) & significand_field_mask(T)
sign_and_exponent_fields(x::T) where T<:SysFloat = reinterpret(Unsigned, x) & sign_and_exponent_field_mask(T)
exponent_and_significand_fields(x::T) where T<:SysFloat = reinterpret(Unsigned, x) & sign_field_filter(T)
sign_and_significand_fields(x::T) where T<:SysFloat = reinterpret(Unsigned, x) & exponent_field_mask(T)


SysFloat,
       bitwidth, signbit, sign, precision, exponent, significand,
       exponent_max, exponent_min, exponent_field_max,
       
       iszero, isone, isnormal, issubnormal, isfinite, isinf, isnan, isqnan, issnan, notfinite,
       
       sign_field_mask,   exponent_field_mask,   significand_field_mask,   sign_exponent_fields_mask,
       sign_field_filter, exponent_field_filter, significand_field_filter, sign_exponent_fields_filter,
       
       sign_field_offset,   exponent_field_offset,   significand_field_offset,
       sign_field_bitwidth, exponent_field_bitwidth, significand_field_bitwidth,
       
       exponent_max, exponent_min, exponent_bias,  normal_exponent_max, normal_exponent_min, 
       
       explicit_significand_max, explicit_significand_min, implicit_significand_max, implicit_significand_min,
       
       subnormal_exponent_max, subnormal_exponent_min, subnormal_significand_max, subnormal_significand_min,
       
       
       
       
       

#=
   for Float128, Float256 types
   
precision(::Type{Float128}) = 113
precision(::Type{Float256}) = 237

significand_bits(::Type{Float128}) = 112
significand_bits(::Type{Float256}) = 236

exponent_bits(::Type{Float128}) = 15
exponent_bits(::Type{Float256}) = 19

exponent_max(::Type{Float128}) =  16383
exponent_max(::Type{Float256}) = 262143

exponent_min(::Type{Float128}) =  -16382
exponent_min(::Type{Float256}) = -262142

exponent_bias(::Type{Float128}) =  16383
exponent_bias(::Type{Float256}) = 262143

exponent_field_max(::Type{Float128}) =  16384
exponent_field_max(::Type{Float256}) = 262144
=#


## IEEE754-2008 conformant constants for Float64, Float32, Float16

```julia
const SysFloat = Union{Float16, Float32, Float64}

@inline bitwidth(::Type{T}) where T<:SysFloat = sizeof(T) * 8

# precision, significand_bits and exponent_bits are unchanged

precision(::Type{T}) where T<:SysFloat = Base.Math.precision(T)
significand_bits(::Type{T}) where T<:SysFloat = Base.Math.significand_bits(T)
exponent_bits(::Type{T}) where T<:SysFloat = Base.Math.exponent_bits(T)

# exponent_max (Emax in the standard) is an IEEE754-2008 standard term;
#   The standard tabulates its values (see Table 3.2 on page 8).
# Julia had defined it in a nonstandard manner. This is conformant:

exponent_max(::Type{Float16}) =   15
exponent_max(::Type{Float32}) =  127
exponent_max(::Type{Float64}) = 1023

# exponent_min (Emin) is another standard term
# it is fully determined by exponent_max:

exponent_min(::Type{T}) where T<:StdFloat = 1 - exponent_max(T)

# exponent_bias (bias) is defined equal to exponent_max

exponent_bias(::Type{T}) where T<:StdFloat = exponent_max(T)

# The value Julia has called exponent_max does not appear
# in the standard (nor do its values).  It is useful.

exponent_field_max(::Type{T}) where T<:StdFloat = exponent_max(T) + 1

```

## all values

```julia
precision(::Type{Float16})  =  11
precision(::Type{Float32})  =  24
precision(::Type{Float64})  =  53
precision(::Type{Float128}) = 113
precision(::Type{Float256}) = 237

significand_bits(::Type{Float16})  =  10
significand_bits(::Type{Float32})  =  23
significand_bits(::Type{Float64})  =  52
significand_bits(::Type{Float128}) = 112
significand_bits(::Type{Float256}) = 236

exponent_bits(::Type{Float16})  =  5
exponent_bits(::Type{Float32})  =  8
exponent_bits(::Type{Float64})  = 11
exponent_bits(::Type{Float128}) = 15
exponent_bits(::Type{Float256}) = 19

exponent_max(::Type{Float16})  =     15
exponent_max(::Type{Float32})  =    127
exponent_max(::Type{Float64})  =   1023
exponent_max(::Type{Float128}) =  16383
exponent_max(::Type{Float256}) = 262143

exponent_min(::Type{Float16})  =     -14
exponent_min(::Type{Float32})  =    -126
exponent_min(::Type{Float64})  =   -1022
exponent_min(::Type{Float128}) =  -16382
exponent_min(::Type{Float256}) = -262142

exponent_bias(::Type{Float16})  =     15
exponent_bias(::Type{Float32})  =    127
exponent_bias(::Type{Float64})  =   1023
exponent_bias(::Type{Float128}) =  16383
exponent_bias(::Type{Float256}) = 262143

exponent_field_max(::Type{Float16})  =     16
exponent_field_max(::Type{Float32})  =    128
exponent_field_max(::Type{Float64})  =   1024
exponent_field_max(::Type{Float128}) =  16384
exponent_field_max(::Type{Float256}) = 262144
```

=#
