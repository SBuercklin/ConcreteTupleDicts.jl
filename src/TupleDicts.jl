module TupleDicts

using Base

export TupleDict

struct TupleDict{TD <: Tuple, K, V} <: AbstractDict{K, V}
    dicts::TD
    function TupleDict{TD}(td::TD) where {TD}
        ks = (keytype(d) for d in td)
        vs = (eltype(values(d)) for d in td)
        K = Union{ks...}
        V = Union{vs...}

        return new{TD, K, V}(td)
    end
end
function TupleDict(td::TD) where {TD}
    return TupleDict{TD}(td)
end

function _find_dict(_d::TupleDict, ::T) where {T}
    d_idx = findfirst(d -> T <: keytype(d), _d.dicts)
    return _d.dicts[d_idx]
end

function Base.getindex(_d::TupleDict, k)
    d = _find_dict(_d, k)
    return Base.getindex(d, k)
end

function Base.setindex!(_d::TupleDict, v, k)
    d = _find_dict(_d, k)
    return Base.setindex!(d, v, k)
end

function Base.keys(td::TupleDict)
    mapreduce(keys, union, td.dicts)
end

function Base.values(td::TupleDict)
    mapreduce(values, union, td.dicts)
end

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

end # module TupleDict
