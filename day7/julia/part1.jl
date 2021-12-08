#!/usr/bin/env julia

using Test
using DelimitedFiles

function readdata(fname::String)
    readdlm(fname, ',', Int, '\n')[1, :]
end

function cost1(d::Vector{Int64}, x::Int64)::Int64
    sum(broadcast(abs, d .- x))
end

function triangle(n::Int64)::Int64
    (n * (n + 1))/2
end

@test (triangle ∘ abs)(5) == 15
@test (triangle ∘ abs)(16 - 5) == 66
@test (triangle ∘ abs)(1 - 5) == 10

function cost2(d::Vector{Int64}, x::Int64)::Int64
    sum(broadcast(triangle ∘ abs, d .- x))
end

function solve(d::Vector{Int64}, cost::Function)::Int64
    dmin, dmax = extrema(x -> x, d)
    minimum(x -> cost(d, x), dmin:dmax)
end

example = readdata("../example.txt")
@test cost1(example, 2) == 37
@test cost1(example, 10) == 71
@test cost1(example, 1) == 41
@test cost1(example, 3) == 39
@test solve(example, cost1) == 37

@test cost2(example, 2) == 206
@test cost2(example, 5) == 168
@test solve(example, cost2) == 168

input = readdata("../input.txt")
println(solve(input, cost1))
@test solve(input, cost1) == 343441
println(solve(input, cost2))
@test solve(input, cost2) == 98925151
