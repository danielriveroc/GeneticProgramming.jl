
function TournamentSelection(population::Array{Individual,1}, minimize::Bool; individualsNotToChoose=[])
    numIndividualsTournament = 2;

    indicesIndividuals = shuffle(setdiff(1:length(population), individualsNotToChoose));

    if (isempty(indicesIndividuals))
        return nothing;
    end;

    if (length(indicesIndividuals)<numIndividualsTournament)
        numIndividualsTournament = length(indicesIndividuals);
    end;

    if (minimize)
        bestFitness = Inf;
    else
        bestFitness = -Inf;
    end;
    indicePadre = nothing;
    for posiblePadre = indicesIndividuals[1:numIndividualsTournament]
        if (minimize)
            itsBetter = (population[posiblePadre].fitness<bestFitness);
        else
            itsBetter = (population[posiblePadre].fitness>bestFitness);
        end;
        if (itsBetter)
            bestFitness = population[posiblePadre].fitness;
            indicePadre = posiblePadre;
        end;
    end;
    @assert (indicePadre!=nothing);
    return indicePadre;
end


SelectionAlg(population::Array{Individual,1}, minimize::Bool; individualsNotToChoose=[]) = TournamentSelection(population, minimize; individualsNotToChoose=individualsNotToChoose)
