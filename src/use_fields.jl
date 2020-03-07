
# fetch the field into the low order bits
# all other bits are zeroed

get_sign(x::T) where T = isolate_sign(x) >> sign_offset(T)
get_exponent(x::T) where T = isolate_exponent(x) >> exponent_offset(T)
get_significand(x::T) where T = isolate_significand(x) >> significand_offset(T)
get_sign_exponent(x::T) where T = isolate_sign_exponent(x) >> exponent_offset(T)
get_exponent_significand(x::T) where T = isolate_exponent_significand(x) >> significand_offset(T)

# prepared field is in the low order bits, shift field into its place
# all other bits are zeroed

set_sign(x::T) where T = (x & sign_low_mask(T)) << sign_offset(T)
set_exponent(x::T) where T = (x & exponent_low_mask(T)) << exponent_offset(T)
set_significand(x::T) where T = (x & significand_low_mask(T)) << significand_offset(T)
set_sign_exponent(x::T) where T = (x & sign_exponent_low_mask(T)) << exponent_offset(T)
set_exponent_significand(x::T) where T = (x & exponent_significand_low_mask(T)) << exponent_significand_offset(T)


# isolate the field in place 
# (yields the field value, other bits are zeroed)
   
isolate_sign(x::T) where T = unsigned(x) & sign_mask(T)
isolate_exponent(x::T) where T = unsigned(x) & exponent_mask(T)
isolate_significand(x::T) where T = unsigned(x) & significand_mask(T)
isolate_sign_exponent(x::T) where T = unsigned(x) & sign_exponent_mask(T)
isolate_exponent_significand(x::T) where T = unsigned(x) & sign_filter(T)

# clear the field in place
# (yields the other fields, this field is zeroed)

clear_sign(x::T) where T = unsigned(x) & sign_filter(T)
clear_exponent(x::T) where T = unsigned(x) & exponent_filter(T)
clear_significand(x::T) where T = unsigned(x) & significand_filter(T)
clear_sign_exponent(x::T) where T = unsigned(x) & sign_exponent_filter(T)
clear_exponent_significand(x::T) where T = unsigned(x) & exponent_significand_filter(T)

# field offsets (shift by)

sign_offset(::Type{T}) where T = bitwidth(T) - one(signed(T))
exponent_offset(::Type{T}) where T = sign_offset(T) - exponent_bits(T)
significand_offset(::Type{T}) where T = zero(signed(T))
sign_exponent_offset(::Type{T}) where T = exponent_offset(T)
exponent_significand_offset(::Type{T}) where T = significand_offset(T)

# field filters

sign_filter(::Type{T}) where T = ~zero(unsigned(T)) >>> 1
exponent_filter(::Type{T}) where T = sign_exponent_filter(T) | sign_mask(T)
significand_filter(::Type{T}) where T = ~sign_exponent_filter(T)
sign_exponent_filter(::Type{T}) where T = ~(zero(unsigned(T)) >>> (exponent_bits(T) + 1)
exponent_significand_filter(::Type{T}) where T = ~(sign_filter(T))

# field masks

sign_mask(::Type{T}) where T = ~sign_filter(T)
exponent_mask(::Type{T}) where T = ~exponent_filter(T)
significand_mask(::Type{T}) where T = ~sign_exponent_mask(T)
sign_exponent_mask(::Type{T}) where T = ~sign_exponent_filter(T)
exponent_significand_mask(::Type{T}) where T = ~exponent_significand_mask(T)

# place plain masks in the least significant bits

sign_low_mask(::Type{T}) where T = sign_mask(T) >> sign_offset(T)
exponent_low_mask(::Type{T}) where T = exponent_mask(T) >> exponent_offset(T)
significand_low_mask(::Type{T}) where T = significand_mask(T) >> significand_offset(T)
sign_exponent_low_mask(::Type{T}) where T = sign_exponent_mask(T) >> exponent_offset(T)
exponent_significand_low_mask(::Type{T}) where T = exponent_significand_mask(T) >> significand_offset(T)
