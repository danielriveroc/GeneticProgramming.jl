
mutable struct Individual
    genotype::Union{Tree,Vector{Float64}}
    fitness::Float64
    expression::String
    Individual(genotype::Union{Tree,Vector{Float64}})                                       = new(genotype, NaN,     "")
    Individual(genotype::Union{Tree,Vector{Float64}}, fitness::Float64)                     = new(genotype, fitness, "")
    Individual(genotype::Union{Tree,Vector{Float64}}, fitness::Float64, expression::String) = new(genotype, fitness, expression)
end

function string(individual::Individual)
    if isempty(individual.expression)
        individual.expression = string(individual.genotype);
    end;
    return individual.expression;
end;

clone(genotype::Vector{Float64}) = copy(genotype);


function clone(individual::Individual)
    return Individual(clone(individual.genotipo), individual.fitness, individual.expression);
end;
