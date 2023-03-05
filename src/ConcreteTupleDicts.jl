module ConcreteTupleDicts

using Base

export TupleDict

"""
    struct TupleDict{TD, K, V} <: AbstractDict{K, V}

A `Dict`-like structure which maps keys of various types to values of various types
    in a type stable manner. Crucially, for every key of type `TK`, it must map to
    a value of type `TV`. 

With a traditional `Dict`, keys of distinct types mapping to values of distinct types
    cannot lead to type stable retrieval. However, a `TupleDict` is type stable, 
    even when the `valtype` for each `keytype` is different. 
"""
struct TupleDict{TD <: Tuple, K, V} <: AbstractDict{K, V}
    dicts::TD
    function TupleDict{TD}(td::TD) where {TD}
        ks = (keytype(d) for d in td)
        vs = (valtype(d) for d in td)
        K = Union{ks...}
        V = Union{vs...}

        return new{TD, K, V}(td)
    end
end

function TupleDict(td)
    # Only want to work with concrete types to avoid ambgiuities in the Dict retrieval
    if !all(_concrete_or_union, td)
        throw(
            error(
                "All input `Dict` keytypes to a `TupleDict` must be concrete types, or union of concrete types"
                )
            )
    end
    # Duplicated key types with different value types also means ambiguity
    if _duplicated_key_types(td)
        throw(
            error(
                "All input `Dict` keytypes to a `TupleDict` must be unique. Duplicate keys will cause ambiguity"
                )
            )
    end
    valtypes = map(valtype, td)
    u_valtypes = t_unique(valtypes)
    tuple_dicts = map(Base.Fix2(map_valtypes, td), u_valtypes)

    if length(tuple_dicts) == 1
        return only(tuple_dicts) # That's just a regular Dict wrapped in a Tuple
    else
        TupleDict{typeof(tuple_dicts)}(tuple_dicts)
    end

end

function map_valtypes(v, td)
        ds = filter(isequal(v) ∘ valtype, td)
        keytypes = map(keytype, ds)
        ktype = Union{keytypes...}
        vtype = v
        converted_dicts = map(d -> convert_dict(d, ktype, vtype), ds)
        merged_dicts = merge(converted_dicts...)

        return merged_dicts
end

function convert_dict(d, ktype, vtype)
    return convert(Dict{ktype, vtype}, d)
end

function Base.getindex(_d::TupleDict, k)
    d = _find_dict(_d, k)
    return Base.getindex(d, k)
end

function Base.setindex!(_d::TupleDict, v, k)
    d = _find_dict(_d, k)
    return Base.setindex!(d, v, k)
end

Base.keys(td::TupleDict) = mapreduce(keys, union, td.dicts)

Base.values(td::TupleDict) = mapreduce(values, union, td.dicts)

function Base.iterate(td::TupleDict, st = (0, 1, 0))
    # i is the index of the Dict in the tuple 
    # n is the number of the key being iterated over
    # dict_state is the state of the previous dict iteration
    i, n, dict_state = st
    n > length(td) && return nothing

    dict_lims = cumsum(
        ntuple(j -> length(keys(td.dicts[j])), length(td.dicts))
        )
    idx = findfirst(>=(n), dict_lims)

    d = td.dicts[idx]

    rval, new_state = idx == i ? iterate(d, dict_state) : iterate(d)

    return (rval, (idx, n + 1, new_state))
end

Base.length(td::TupleDict) = length(keys(td))


#=
    Utilities, helpers
=#

function _concrete_or_union(::AbstractDict{TK, TV}) where {TK, TV}
    if isconcretetype(TK) 
        return true
    elseif TK isa Union
        return all(isconcretetype, fieldnames(TK))
    else
        return false
    end
end

function _duplicated_key_types(ds)
    state = iterate(ds)
    while !isnothing(state)
        next_it, _state = state
        remainder = Base.rest(ds, _state)
        if any(_conflicting_kvs(next_it), remainder)
            return true
        end
        state = iterate(ds, _state)
    end
    return false
end

_conflicting_kvs(a) = Base.Fix1(_conflicting_kvs,a)

# 1. No problem if the value types are the same, we'll merge Dicts later
# 2. No problem if the keys don't intersect
# 3. if the valtypes differ but the keytypes intersect, could be ambiguous
function _conflicting_kvs(a, b)
    if valtype(a) == valtype(b)
        return false
    elseif typeintersect(keytype(a), keytype(b)) == Union{}
        return false
    else
        return true
    end
end
_conflicting_kvs(a, b::Nothing) = false


# Crucially, this is O(1) thanks to the Julia compiler evaluating `findfirst` in constant time
#   over the tuple _d.dicts
function _find_dict(_d::TupleDict, ::T) where {T}
    d_idx = findfirst(d -> T <: keytype(d), _d.dicts)

    return _d.dicts[d_idx]
end


# Tuple unique over NTuple{N, Datatype}
t_unique(x::Type{T}) where T = (x,)

function t_unique(y::Type{T}, x::Type...) where T
    ys = t_unique(x...)
    y in ys ? ys : (y, ys...)
end
t_unique(x::Tuple{Vararg{<:Type}}) = t_unique(x...)

end # module TupleDict
