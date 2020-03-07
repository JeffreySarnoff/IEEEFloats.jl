# IEEEFloats.jl
### IEEE754-2019 structural introspection for Float8, Float16, Float32, Float64, Float128



#### Copyright © 2015-2020 by Jeffrey Sarnoff. This work is released under The MIT License.

----

[![Build Status](https://travis-ci.org/JeffreySarnoff/IEEEFloats.jl.svg?branch=master)](https://travis-ci.org/JeffreySarnoff/IEEEFloats.jl)&nbsp;&nbsp;&nbsp;[![codecov](https://codecov.io/gh/JeffreySarnoff/IEEEFloats.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/JeffreySarnoff/IEEEFloats.jl)

----




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

exponentmax(Float64) == 1023
exponentmax(Float32) ==  127
exponentmax(Float16) ==   15

# exponent_min (Emin) is another standard term
# it is fully determined by exponent_max:
# exponent_min(T) = 1 - exponent_max(T)

exponentmin(Float64) == -1022
exponentmin(Float32) ==  -126
exponentmin(Float16) ==   -14

# exponent_bias (bias) is defined equal to exponent_max

exponentbias(Float64) == 1023
exponentbias(Float32) ==  127
exponentbias(Float16) ==   15

# The value Julia has called exponent_max does not appear
# in the standard (nor do its values).  It is a useful quantity
# (Emax + 1) that we rename `exponent_field_max`.

exponentfieldmax(Float64) == 1024
exponentfieldmax(Float32) ==  128
exponentfieldmax(Float16) ==   16

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

exponentmax(Float16)  =     15
exponentmax(Float32)  =    127
exponentmax(Float64)  =   1023

exponentmin(Float16)  =     -14
exponentmin(Float32)  =    -126
exponentmin(Float64)  =   -1022

exponentbias(Float16)  =     15
exponentbias(Float32)  =    127
exponentbias(Float64)  =   1023

exponentfieldmax(Float16)  =     16
exponentfieldmax(Float32)  =    128
exponentfieldmax(Float64)  =   1024

# intfloatmax(FloatNN) is the most positive IntNN I where FloatNN(I-1) is representable
# intfloatmin(FloatNN) is the most negative IntNN I where FloatNN(I+1) is representable 

intfloatmax(Float16) = Int16(2048)
intfloatmax(Float32) = Int32(16777216)
intfloatmax(Float64) = Int64(9007199254740992)

intfloatmin(Float16) = Int16(-2048)
intfloatmin(Float32) = Int32(-16777216)
intfloatmin(Float64) = Int64(-9007199254740992)

# floatintmax(FloatNN) is FloatNN(intfloat_max(FloatNN))
# floatintmin(FloatNN) is FloatNN(intfloat_min(FloatNN))

floatintmax(Float16) = Float16(2048)
floatintmax(Float32) = Float32(16777216)
floatintmax(Float64) = Float64(9007199254740992)

floatintmin(Float16) = Float16(-2048)
floatintmin(Float32) = Float32(-16777216)
floatintmin(Float64) = Float64(-9007199254740992)
```
