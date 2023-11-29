

# Replace if each child improves its parent
function ReplaceIfImprovement!(population::Array{Individual,1}, genotypeChild1::Union{Tree,Vector{Float64}}, genotypeChild2::Union{Tree,Vector{Float64}}, indexParent1::Int, indexParent2::Int, fitnessChild1::Float64, fitnessChild2::Float64, minimize::Bool)

    fitnessParent1 = population[indexParent1].fitness;
    fitnessParent2 = population[indexParent2].fitness;

    if (minimize)
        child1ImprovesItsParent = (fitnessChild1<=fitnessParent1);
        child2ImprovesItsParent = (fitnessChild2<=fitnessParent2);
    else
        child1ImprovesItsParent = (fitnessChild1>=fitnessParent1);
        child2ImprovesItsParent = (fitnessChild2>=fitnessParent2);
    end;

    if (child1ImprovesItsParent)
        population[indexParent1].genotype = genotypeChild1;
        population[indexParent1].fitness = fitnessChild1;
        population[indexParent1].expression = "";
    end;
    if (child2ImprovesItsParent)
        population[indexParent2].genotype = genotypeChild2;
        population[indexParent2].fitness = fitnessChild2;
        population[indexParent2].expression = "";
    end;

    # In this form of replacement, elitism is already included
    #  in the form of replacement

    # Moreover, if any son is better than his father, he should be
    #  to put him among the best so that he will not be lost.
    # This is also included in this form of replacement.
end;


ReplaceAlg!(population::Array{Individual,1}, genotypeChild1::Union{Tree,Vector{Float64}}, genotypeChild2::Union{Tree,Vector{Float64}}, indexParent1::Int, indexParent2::Int, fitnessChild1::Float64, fitnessChild2::Float64, minimize::Bool) = ReplaceIfImprovement!(population, genotypeChild1, genotypeChild2, indexParent1, indexParent2, fitnessChild1, fitnessChild2, minimize)


