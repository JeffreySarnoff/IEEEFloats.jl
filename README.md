## IEEE754-2008 conformant constants for Float64, Float32, Float16

```julia

IEEEFloat == Base.IEEEFloat == Union{Float16, Float32, Float64}

@inline bitwidth(::Type{T}) where T<:IEEEFloat = sizeof(T) * 8

# precision, significand_bits and exponent_bits are unchanged

precision(::Type{T}) where T<:IEEEFloat = Base.Math.precision(T)
significand_bits(::Type{T}) where T<:IEEEFloat = Base.Math.significand_bits(T)
exponent_bits(::Type{T}) where T<:IEEEFloat = Base.Math.exponent_bits(T)

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
precision(Float16)  =  11
precision(Float32)  =  24
precision(Float64)  =  53

significand_bits(Float16)  =  10
significand_bits(Float32)  =  23
significand_bits(Float64)  =  52

exponent_bits(Float16)  =  5
exponent_bits(Float32)  =  8
exponent_bits(Float64)  = 11

exponent_max(Float16)  =     15
exponent_max(Float32)  =    127
exponent_max(Float64)  =   1023

exponent_min(Float16)  =     -14
exponent_min(Float32)  =    -126
exponent_min(Float64)  =   -1022

exponent_bias(Float16)  =     15
exponent_bias(Float32)  =    127
exponent_bias(Float64)  =   1023

exponent_field_max(Float16)  =     16
exponent_field_max(Float32)  =    128
exponent_field_max(Float64)  =   1024

# intfloat_max(FloatNN) is the most positive IntNN I where FloatNN(I-1) is representable
# intfloat_min(FloatNN) is the most negative IntNN I where FloatNN(I+1) is representable 

intfloat_max(Float16) = Int16(2048)
intfloat_max(Float32) = Int32(16777216)
intfloat_max(Float64) = Int64(9007199254740992)

intfloat_min(Float16) = Int16(-2048)
intfloat_min(Float32) = Int32(-16777216)
intfloat_min(Float64) = Int64(-9007199254740992)

# floatint_max(FloatNN) is FloatNN(intfloat_max(FloatNN))
# floatint_min(FloatNN) is FloatNN(intfloat_min(FloatNN))

floatint_max(Float16) = Float16(2048)
floatint_max(Float32) = Float32(16777216)
floatint_max(Float64) = Float64(9007199254740992)

floatint_min(Float16) = Float16(-2048)
floatint_min(Float32) = Float32(-16777216)
floatint_min(Float64) = Float64(-9007199254740992)
```
