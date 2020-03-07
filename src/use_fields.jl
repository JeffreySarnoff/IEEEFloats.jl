
# isolate the field from other bits 
# (yields the field value, as Unsigned bits in place)
   
isolate_sign_field(x::T) where T = unsigned(x) & sign_mask(T)
isolate_exponent_field(x::T) where T = unsigned(x) & exponent_mask(T)
isolate_significand_field(x::T) where T = unsigned(x) & significand_mask(T)
isolate_sign_exponent_field(x::T) where T = unsigned(x) & sign_exponent_mask(T)
isolate_exponent_significand_field(x::T) where T = unsigned(x) & sign_filter(T)
isolate_sign_significand_field(x::T) where T = unsigned(x) & exponent_mask(T)

# clear the field in place
# (yields the other fields, as unsigned bits)

clear_sign_field(x::T) where T = unsigned(x) & sign_filter(T)
clear_exponent_field(x::T) where T = unsigned(x) & exponent_filter(T)
clear_significand_field(x::T) where T = unsigned(x) & significand_filter(T)
clear_sign_exponent_field(x::T) where T = unsigned(x) & sign_exponent_filter(T)
clear_exponent_significand_field(x::T) where T = unsigned(x) & exponent_significand_filter(T)
clear_sign_significand_field(x::T) where T = unsigned(x) & exponent_mask(T)

# fetch the field into the low order bits

get_sign_field(x::T) where T = isolate_sign_field(x) >> sign_offset(T)
get_exponent_field(x::T) where T = isolate_exponent_field(x) >> exponent_offset(T)
get_significand_field(x::T) where T = isolate_significand_field(x) >> significand_offset(T)
get_sign_exponent_field(x::T) where T = isolate_sign_exponent_field(x) >> exponent_offset(T)
get_exponent_significand_field(x::T) where T = isolate_exponent_significand_field(x) >> significand_offset(T)

# prepare field in low order bits, shift field into its place

set_sign_field(x::T) where T<:Unsigned = (x & sign_mask_lsbs(T)) << sign_offset(T)
set_exponent_field(x::T) where T<:Unsigned = (x & exponent_mask_lsbs(T)) << exponent_offset(T)
set_significand_field(x::T) where T<:Unsigned = (x & significand_mask_lsbs(T)) << significand_offset(T)
set_sign_exponent_field(x::T) where T<:Unsigned = (x & sign_exponent_mask_lsbs(T)) << exponent_offset(T)
set_exponent_significand_field(x::T) where T<:Unsigned = (x & exponent_significand_mask_lsbs(T)) << exponent_significand_offset(T)


# field offsets (shift by)

sign_offset(::Type{T}) where T = bitwidth(T) - one(signed(T))
exponent_offset(::Type{T}) where T = sign_offset(T) - exponent_bits(T)
significand_offset(::Type{T}) where T = zero(signed(T))
sign_exponent_offset(::Type{T}) where T = exponent_offset(T)
exponent_significand_offset(::Type{T}) where T = significand_offset(T)

# field filters

sign_filter(::Type{T}) where T = ~(zero(unsigned(T)) >>> 1
sign_exponent_filter(::Type{T}) where T = ~(zero(unsigned(T)) >>> (exponent_bits(T) + 1)
exponent_filter(::Type{T}) where T = sign_exponent_filter(T) | sign_mask(T)
significand_filter(::Type{T}) where T = ~sign_exponent_filter(T)
exponent_significand_filter(::Type{T}) where T = ~(sign_filter(T))

# field masks

sign_mask(::Type{T}) where T = ~sign_filter(T)
sign_exponent_mask(::Type{T}) where T = ~sign_exponent_filter(T)
exponent_mask(::Type{T}) where T = ~exponent_filter(T)
significand_mask(::Type{T}) where T = ~sign_exponent_mask(T)
exponent_significand_mask(::Type{T}) where T = ~exponent_significand_mask(T)

# place plain masks in the least significant bits

sign_mask_lsbs(::Type{T}) where T = sign_mask(T) >> sign_offset(T)
sign_exponent_mask_lsbs(::Type{T}) where T = sign_exponent_mask(T) >> exponent_offset(T)
exponent_mask_lsbs(::Type{T}) where T = exponent_mask(T) >> exponent_offset(T)
significand_mask_lsbs(::Type{T}) where T = significand_mask(T) >> significand_offset(T)
exponent_significand_mask_lsbs(::Type{T}) where T = exponent_significand_mask(T) >> significand_offset(T)

