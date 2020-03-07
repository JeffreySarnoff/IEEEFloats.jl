if isdefined(Base, :exponent_bits)
    using Base: exponent_bits
else
    exponent_bits(::Type{Float64}) = 52
    exponent_bits(::Type{Float32}) = 23
    exponent_bits(::Type{Float16}) = 10
end
if isdefined(Base, :significand_bits)
    using Base: significand_bits
else
    significand_bits(::Type{Float64}) = 52
    significand_bits(::Type{Float32}) = 23
    significand_bits(::Type{Float16}) = 10
end
