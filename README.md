# ConcreteTupleDicts.jl
TupleDicts are type-stable dictionaries which support multiple sets of key-value pair types

# Basic Usage
Say you would like to make the following `Dict`, but you want key retrieval to be type stable

```julia
using Test

d = Dict{Union{Int,Float64,String},Union{Int,Float64}}(1 => 2, 2.0 => 3.0, "three" => 4)

@inferred d[1]     # Inference fails because it infers Union{Int, Float64}
```

A `TupleDict` can be constructed by passing in a tuple of `Dict`s, and it preserves the association of each keytype to its correspdonding valtype. 

These `Dict`s can then be treated as one single `Dict`, while maintaining type stable retrieval

```julia
using ConcreteTupleDicts

d1 = Dict(1 => 2)
d2 = Dict(2.0 => 3.0)
d3 = Dict("three" => 4)

ds = (d1, d2, d3)

td = TupleDict(ds)

@inferred td[1]        # Properly infers Int
@inferred td[2.0]      # Properly infers Float64
@inferred td["three"]  # Properly infers Int
```

These `Dict`s must not have overlapping key types in order to permit an unambiguous mapping from a given keytype to a valtype.

```julia
d1 = Dict(1 => 2)
d2 = Dict(3 => 4.0)

TupleDict((d1, d2))  # Throws an error, Int maps to both Int and Float64
```

But `TupleDict`s are smart enough to merge keytypes which map to the same valtype

```julia
d1 = Dict(1 => 2)
d2 = Dict{Union{Int, String}, Int}(3 => 4, "five" => 6)

td = TupleDict((d1, d2)) # no issues, everything unamibuously maps to `Int`

@inferred td["five"]     # Properly infers Int
```

And the expected `Dict` behavior is still there

```julia
d1 = Dict(1 => 2)
d2 = Dict(2.0 => 3.0)
d3 = Dict("three" => 4)

ds = (d1, d2, d3)

td = TupleDict(ds)

keytype(td)     # Union{Float64, Int64, String}
valtype(td)     # Union{Float64, Int64)  
```
