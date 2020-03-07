"""
    unsigned(T)
    unsigned(x::T)

Obtain the unsigned type or unsigned value that corresponds.
""" unsigned

"""
    signed(T)
    signed(x::T)

Obtain the signed type or signed value that corresponds.
""" signed

"""
    ieeefloat(T)
    ieeefloat(x::T)

Obtain the IEEEFloat type or float value with the same number of bits as T.
""" ieeefloat


Base.unsigned(::Type{Float128}) = UInt128
Base.unsigned(::Type{Float64}) = UInt64
Base.unsigned(::Type{Float32}) = UInt32
Base.unsigned(::Type{Float16}) = UInt16
Base.unsigned(::Type{Float8}) = UInt8

Base.unsigned(x::Float128) = reinterpret(UInt128, x)
Base.unsigned(x::Float64) = reinterpret(UInt64, x)
Base.unsigned(x::Float32) = reinterpret(UInt32, x)
Base.unsigned(x::Float16) = reinterpret(UInt16, x)
Base.unsigned(x::Float8) = reinterpret(UInt8, x)

Base.signed(::Type{Float128}) = Int128
Base.signed(::Type{Float64}) = Int64
Base.signed(::Type{Float32}) = Int32
Base.signed(::Type{Float16}) = Int16
Base.signed(::Type{Float8}) = Int8

Base.signed(x::Float128) = reinterpret(Int128, x)
Base.signed(x::Float64) = reinterpret(Int64, x)
Base.signed(x::Float32) = reinterpret(Int32, x)
Base.signed(x::Float16) = reinterpret(Int16, x)
Base.signed(x::Float8) = reinterpret(Int8, x)

ieeefloat(::Type{UInt128}) = Float128
ieeefloat(::Type{UInt64}) = Float64
ieeefloat(::Type{UInt32}) = Float32
ieeefloat(::Type{UInt16}) = Float16
ieeefloat(::Type{UInt8}) = Float8

ieeefloat(x::UInt128) = reinterpret(Float128, x)
ieeefloat(x::UInt64) = reinterpret(Float64, x)
ieeefloat(x::UInt32) = reinterpret(Float32, x)
ieeefloat(x::UInt16) = reinterpret(Float16, x)
ieeefloat(x::UInt8) = reinterpret(Float8, x)

ieeefloat(::Type{Int128}) = Float128
ieeefloat(::Type{Int64}) = Float64
ieeefloat(::Type{Int32}) = Float32
ieeefloat(::Type{Int16}) = Float16
ieeefloat(::Type{Int8}) = Float8

ieeefloat(x::Int128) = reinterpret(Float128, x)
ieeefloat(x::Int64) = reinterpret(Float64, x)
ieeefloat(x::Int32) = reinterpret(Float32, x)
ieeefloat(x::Int16) = reinterpret(Float16, x)
ieeefloat(x::Int8) = reinterpret(Float8, x)
