
# field[s] offset (shift by)

@inline sign_field_offset(::Type{T}) where T<:IEEEFloat = bitwidth(T) - one(signed(T))
@inline exponent_field_offset(::Type{T}) where T<:IEEEFloat = sign_field_offset(T) - exponent_bits(T)
@inline significand_field_offset(::Type{T}) where T<:IEEEFloat = zero(signed(T))
@inline sign_and_exponent_fields_offset(::Type{T}) where T<:IEEEFloat = exponent_field_offset(T)
@inline exponent_and_significand_fields_offset(::Type{T}) where T<:IEEEFloat = significand_field_offset(T)

# field[s] filter and mask

@inline sign_field_filter(::Type{T}) where T<:IEEEFloat = ~(zero(unsigned(T)) >>> 1
@inline sign_and_exponent_fields_filter(::Type{T}) where T<:IEEEFloat = ~(zero(unsigned(T)) >>> (exponent_bits(T) + 1)
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

# fetch the field[s] into the low order bits of a Float

@inline get_sign_field(x::T) where T<:Base.IEEEFloat = sign_field(x) >> sign_field_offset(T)
@inline get_exponent_field(x::T) where T<:Base.IEEEFloat = exponent_field(x) >> exponent_field_offset(T)
@inline get_significand_field(x::T) where T<:Base.IEEEFloat = significand_field(x) >> significand_field_offset(T)
@inline get_sign_and_exponent_fields(x::T) where T<:Base.IEEEFloat = sign_and_exponent_fields(x) >> exponent_field_offset(T)
@inline get_exponent_and_significand_fields(x::T) where T<:Base.IEEEFloat = exponent_and_significand_fields(x) >> significand_field_offset(T)

