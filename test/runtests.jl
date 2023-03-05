using Test

using ConcreteTupleDicts

@testset "Basic Behavior" begin
    d1 = Dict(1 => 2)
    d2 = Dict(1.0 => 2.0)
    d3 = Dict("One" => 2)

    ds = (d1, d2, d3)

    td = TupleDict(ds)

    @test keytype(td) <: Union{Float64, Int, String}
    @test valtype(td) <: Union{Float64, Int}

    @test td[1] isa Int
    @test td[1.0] isa Float64
    @test td["One"] isa Int 

    @test_nowarn @inferred td[1]
    @test_nowarn @inferred td[1.0]
    @test_nowarn @inferred td["One"]
end

@testset "Concrete or Union Keys" begin
    # d1 is a Dict{Real}
    d1 = Dict(1 => 2, 3.0 => 4)
    d2 = Dict("Five" => 6)

    @test_throws "All input Dict keytypes to a TupleDict must be concrete types, or union of concrete types" TupleDict((d1, d2)) 

    # But this is okay, because the ketypes are unions of concrete
    d3 = Dict{Union{Int, String}, Float64}(1 => 2.0, "three" => 4.0)
    d4 = Dict(5.0 => 6)

    @test_nowarn TupleDict((d3, d4))
end

@testset "Conflicted Key/Values" begin
    @testset "Repeated Key Types, Distinct Valtypes" begin
        d1 = Dict(1 => 2)
        d2 = Dict(3 => 4.0)

        @test_throws "All input Dict keytypes to a TupleDict must be unique. Duplicate keys will cause ambiguity" TupleDict((d1, d2))
    end
    @testset "Keytypes Overlap, Valtypes Match" begin
        d1 = Dict(1 => 2)
        d2 = Dict{Union{Int, String}, Int}(3 => 4, "five" => 6)

        @test_nowarn TupleDict((d1, d2))
    end
end

@testset "Type Stable Construction" begin
    d1 = Dict(1 => 2)
    d2 = Dict(1.0 => 2.0)
    d3 = Dict("One" => 2)

    ds = (d1, d2, d3)

    @test_broken @test_nowarn @inferred TupleDict(ds)
end