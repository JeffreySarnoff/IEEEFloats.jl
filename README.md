## IEEE754-2008
### Standard conformant constants for Float64, Float32, Float16

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
