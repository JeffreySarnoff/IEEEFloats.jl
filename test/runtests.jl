using IEEEFloats
if VERSION >= v"0.7-"
    using Test
else
    using Base.Test
end

@test intfloatmax(Float64) === Int64(9007199254740992)
@test intfloatmin(Float16) === Int16(-2048)

@test exponentfieldmax(Float32) == 128
@test exponentmax(Float32) == 127

