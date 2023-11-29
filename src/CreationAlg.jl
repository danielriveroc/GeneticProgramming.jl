
function CreationAlg(tipo::DataType, populationSize::UInt, maximumHeight::UInt, maxNumNodes::Union{Nothing,UInt}, prototypes::Prototypes, fitnessFunction::Function)
    population = Array{Individual,1}(undef, populationSize);
    @assert (maximumHeight!=nothing);
    if (populationSize<=0)
        return Array{Individual,1}[];
    end;
    numIndividuals = 0;
    while (true)
        for height in min(maximumHeight,4):maximumHeight
            for complete = [false, true]
                tree = buildTree(prototypes, tipo, height, complete, maxNumNodes);
                if !isnothing(tree)
                    numIndividuals = numIndividuals + 1;
                    individual = nothing;
                    try
                        fitness = fitnessFunction(tree);
                        individual = Individual(tree, fitness);
                    catch ex
                        if ex isa LoadError
                            individual = Individual(tree);
                            individual.fitness = fitnessFunction(individual);
                        else
                            rethrow(ex)
                        end
                    end;

                    if (isinf(individual.fitness)) error("fitness is Inf"); end;
                    if (isnan(individual.fitness)) error("fitness is NaN"); end;
                    population[numIndividuals] = individual;
                    if (numIndividuals==populationSize)
                        return population;
                    end;
                else
                    if (height==maximumHeight) & (!complete)
                        @assert (numIndividuals==0) "";
                        return nothing;
                    end
                end;
            end;
        end;
    end;
end
