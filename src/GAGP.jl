
include("Tree.jl")
include("Individual.jl")
include("PrototypeSet.jl");
include("CreationAlg.jl")
include("SelectionAlg.jl")
include("CrossoverAlg.jl")
include("MutationAlg.jl")
include("ReplacementAlg.jl")

using Random
using JLD2

mutable struct GAGP
    isGA::Bool
    genLimits::Tuple{Float64,Float64}
    genLength::UInt
    type::Type
    fitnessFunction::Function
    minimize::Bool
    populationSize::UInt
    crossoverRate::Float64
    maximumHeight::Union{UInt,Nothing}
    maximumInitialHeight::UInt
    maxNumNodes::Union{UInt,Nothing}
    population::Array{Individual,1}
    prototypes::Prototypes
    mutationProb::Float64
    nonTerminalSelectionProbMutation::Union{Float64,Nothing}
    currentGeneration::Int
    GAGP(isGA::Bool, fitnessFunction::Function, minimize::Bool; type::Type=Any, genLimits=(0.,0.), genLength=UInt(0)) = new(isGA, genLimits, genLength, type, fitnessFunction, minimize, 1000, 0.95, nothing, 6, nothing, Individual[], Prototypes(), 0.04, nothing, -1)
end

GP(fitnessFunction::Function, minimize::Bool; type::Type=Any) =                                     GAGP(false, fitnessFunction, minimize; type=type)
GA(fitnessFunction::Function, minimize::Bool, genLimits::Tuple{Float64,Float64}, genLength::Int)  = GAGP(true,  fitnessFunction, minimize; genLimits=genLimits, genLength=UInt(genLength))

function addPrototype!(obj::GAGP, name::String, type::Type, evalFunction::Function)
    @assert(!obj.isGA);
    addPrototype!(obj.prototypes, TerminalFunction(name, evalFunction; type=type));
end
addPrototype!(obj::GAGP, name::String, evalFunction::Function) = addPrototype!(obj, name, Any, evalFunction);


function addPrototype!(obj::GAGP, name::String, type::Type, evalFunction::Function, childrenType::Vector{DataType})
    @assert(!obj.isGA);
    if isempty(childrenType)
        addPrototype!(obj, name, type, evalFunction);
    elseif (length(childrenType)==2)
        addPrototype!(obj.prototypes, BinaryNode(name, evalFunction; type=type, typeChild1=childrenType[1], typeChild2=childrenType[2]));
    else
        addPrototype!(obj.prototypes, NonBinaryNode(name, evalFunction, type, childrenType));
    end;
end

# function addPrototype!(obj::GAGP, name::String, type::Type, evalFunction::Function, childrenType::Vector{DataType})
#     @assert(!obj.isGA);
#     if isempty(childrenType)
#         addPrototype!(obj, name, type, evalFunction);
#     elseif (length(childrenType)==2)
#         addPrototype!(obj.prototypes, BinaryNode(name, evalFunction; type=type, typeChild1=childrenType[1], typeChild2=childrenType[2]));
#     else
#         addPrototype!(obj.prototypes, NonBinaryNode(name, evalFunction, type, childrenType));
#     end;
# end

function addPrototype!(obj::GAGP, nombre::String, numChildren::Int64, evalFunction::Function)
    @assert(!obj.isGA);
    # addPrototype!(obj, nombre, Any, evalFunction, convert(Array{DataType,1},repeat([Any],numChildren)));
    addPrototype!(obj, nombre, Any, evalFunction, repeat([Any],numChildren));
end

function addPrototype!(obj::GAGP, nombre::String; type::DataType=Any)
    @assert(!obj.isGA);
    if (nombre=="+")
        addPrototype!(obj.prototypes, AddNode(; type=type))
    elseif (nombre=="-")
        addPrototype!(obj.prototypes, SubNode(; type=type))
    elseif (nombre=="*")
        addPrototype!(obj.prototypes, MulNode(; type=type))
    elseif (nombre=="%")
        addPrototype!(obj.prototypes, DivNode(; type=type))
    elseif (nombre=="if")
        addPrototype!(obj.prototypes, IfNode(; type=type))
    elseif (nombre==">")
        addPrototype!(obj.prototypes, HigherNode(; type=type))
    elseif (nombre=="<")
        addPrototype!(obj.prototypes, LowerNode(; type=type))
    elseif (nombre=="AND")
        addPrototype!(obj.prototypes, ANDNode())
    elseif (nombre=="OR")
        addPrototype!(obj.prototypes, ORNode())
    else
        error("Don't know this function")
    end;
end


function addPrototype!(obj::GAGP, lowerLimit::Float64, upperLimit::Float64; type::DataType=Any)
    @assert(!obj.isGA);
    addPrototype!(obj.prototypes, RandomConstant(lowerLimit, upperLimit; type=type));
end

function addPrototype!(obj::GAGP, value::Real; type::DataType=Any)
    @assert(!obj.isGA);
    addPrototype!(obj.prototypes, Constant(value; type=type));
end


function addVariables!(obj::GAGP, inputs::Array{Float64,2}; dataInRows=true, type::Union{Type,AbstractVector{<:Type}}=Any)
    @assert(!obj.isGA);
    if (dataInRows)
        numInputs = size(inputs, 2);
    else
        numInputs = size(inputs, 1);
    end;
    if !isa(type, AbstractVector{<:Type})
        type = repeat([type], numInputs);
    end;
    GPVariables = Array{Array{Float64,1},1}(undef,numInputs);
    for numInput in 1:numInputs
        if (dataInRows)
            GPVariables[numInput] = inputs[:,numInput];
        else
            GPVariables[numInput] = inputs[numInput,:];
        end;
        if type[numInput]==Bool
            addPrototype!(obj.prototypes, Variable(UInt(numInput); type=type[numInput], semantic=Bool.(GPVariables[numInput])));
        else
            addPrototype!(obj.prototypes, Variable(UInt(numInput); type=type[numInput], semantic=GPVariables[numInput]));
        end;
    end;
    return nothing;
end;

function evaluateIndividuals(obj::GAGP, numIndividuals)
    numIndividuals = min(length(obj.population), numIndividuals);
    for numIndividual = 1:numIndividuals
        obj.population[numIndividual].fitness = obj.fitnessFunction(obj.population[numIndividual].genotype);
    end;
    SortPopulation(obj);
end

evaluatePopulation(obj::GAGP) = EvaluateIndividuals(obj, length(obj.population));


function SortPopulation(obj::GAGP)
    fitness = [ind.fitness for ind in obj.population]
    issorted(fitness, rev=!obj.minimize) && return;
    if (obj.minimize)
        indicesSort = sortperm(fitness);
    else
        indicesSort = sortperm(fitness, rev=true);
    end;
    obj.population = obj.population[indicesSort];
    return nothing;
end

function Step!(obj::GAGP)
#             if (isempty(obj.population)),
    if (obj.currentGeneration < 0)
        if (length(obj.population)<obj.populationSize)
            FillPopulation(obj);
            obj.currentGeneration = 0;
            return nothing;
        end;
    end;

    # Primero completamos la population
    FillPopulation(obj);

    numCrossovers = Int(round(obj.crossoverRate*obj.populationSize/2));

    for numCruce in 1:numCrossovers

        genotypeChild1 = nothing;
        genotypeChild2 = nothing;

        individualsNotToChooseParent1 = Array{Int,1}([]);

        indexParent1 = nothing; indexParent2 = nothing;

        while isnothing(indexParent1)

            indexParent1 = SelectionAlg(obj.population, obj.minimize; individualsNotToChoose=individualsNotToChooseParent1);

            if isnothing(indexParent1)
                SortPopulation(obj);
                return nothing;
            end;

            # individualsNotToChooseParent2 = Array{Int,1}([indexParent1]);
            individualsNotToChooseParent2 = copy(individualsNotToChooseParent1);
            push!(individualsNotToChooseParent2, indexParent1);
            indexParent2 = nothing;
            while isnothing(indexParent2) && !isnothing(indexParent1)

                indexParent2 = SelectionAlg(obj.population, obj.minimize; individualsNotToChoose=individualsNotToChooseParent2);

                # En esta forma de cruzar no se hace copia de los padres porque se supone que los hijos sustituyen siempre a los padres
                # De esta forma, el sistema es mucho mas eficiente, por no tener que hacer copias de los nodos del arbol
                # Si se quisiera implementar otro algoritmo de reemplazo, se puede deshacer el cruce
                # Sin embargo, si se deshace el cruce, habria que deshacer ambos individuos

                if !isnothing(indexParent2)

                    # (arbolHijo1, arbolHijo2, infoDeshacerCruce) = Cruzar(obj.population[indexParent1].arbol, obj.population[indexParent2].arbol, obj.probSeleccionNoTerminalCruce, obj.maximumHeight);
                    if obj.isGA
                        (genotypeChild1, genotypeChild2) = Crossover(obj.population[indexParent1].genotype, obj.population[indexParent2].genotype)
                    else
                        (genotypeChild1, genotypeChild2) = Crossover(obj.population[indexParent1].genotype, obj.population[indexParent2].genotype, obj.type, obj.maximumHeight, obj.maxNumNodes);
                    end;

                    # Si no se pueden cruzar, descartando las raices
                    if !isnothing(genotypeChild1)
                        @assert (!isnothing(genotypeChild2));
                        # cruceCorrecto = true;
                    else
                        @assert (isnothing(genotypeChild2));
                        push!(individualsNotToChooseParent2, indexParent2);
                        indexParent2 = nothing;
                    end;

                else
                    push!(individualsNotToChooseParent1, indexParent1);
                    indexParent1 = nothing;
                end;
            end;
        end;

        # Cruce realizado correctamente

        @assert(!isnothing(genotypeChild1) && !isnothing(genotypeChild2));

        # @assert (alturaArbol(arbolHijo1)<=obj.maximumHeight);
        # @assert (alturaArbol(arbolHijo2)<=obj.maximumHeight);

        # Vamos a ver si hay que mutar los hijos
        if (rand()<obj.mutationProb)
            if obj.isGA
                genotypeChild1 = Mutate(genotypeChild1, obj.genLimits)
            else
                genotypeChild1 = Mutate(genotypeChild1, obj.type, obj.prototypes, obj.maximumHeight, obj.maxNumNodes; nonTerminalSelectionProb=obj.nonTerminalSelectionProbMutation);
            end;
        end;
        if (rand()<obj.mutationProb)
            if obj.isGA
                genotypeChild1 = Mutate(genotypeChild2, obj.genLimits)
            else
                genotypeChild2 = Mutate(genotypeChild2, obj.type, obj.prototypes, obj.maximumHeight, obj.maxNumNodes; nonTerminalSelectionProb=obj.nonTerminalSelectionProbMutation);
            end;
        end;

        # @assert (alturaArbol(arbolHijo1)<=obj.maximumHeight);
        # @assert (alturaArbol(arbolHijo2)<=obj.maximumHeight);

        # ResetearValoresEvaluacion(arbolHijo1);
        # ResetearValoresEvaluacion(arbolHijo2);

        # Evaluamos los arboles o los individuos
        fitnessChild1 = NaN;
        fitnessChild2 = NaN;
        # try
            fitnessChild1 = obj.fitnessFunction(genotypeChild1);
            fitnessChild2 = obj.fitnessFunction(genotypeChild2);
        # catch ex
        #     if ex isa LoadError
        #         fitnessChild1 = obj.fitnessFunction(Individuo(arbolHijo1,NaN,escribirArbol(arbolHijo1)));
        #         fitnessChild2 = obj.fitnessFunction(Individuo(arbolHijo2,NaN,escribirArbol(arbolHijo2)));
        #     else
        #         rethrow(ex)
        #     end
        # end;

        if (isinf(fitnessChild1)) error("Fitness value can not be Inf"); end;
        if (isnan(fitnessChild1)) error("Fitness value can not be NaN"); end;
        if (isinf(fitnessChild2)) error("Fitness value can not be Inf"); end;
        if (isnan(fitnessChild2)) error("Fitness value can not be NaN"); end;

        # El algoritmo de reemplazo ya implementa el elitismo
#         (sustituyeAlPadre1, sustituyeAlPadre2) = Reemplazar(obj.population[indexParent1].ajuste, obj.population[indexParent2].ajuste, hijo1.ajuste, hijo2.ajuste, indexParent1, indexParent2, numIndividuosElitismo, obj.minimize);
# # println([sustituyeAlPadre1 sustituyeAlPadre2]);
#         if (sustituyeAlPadre1)
#             obj.population[indexParent1] = hijo1;
#         end;
#         if (sustituyeAlPadre2)
#             obj.population[indexParent2] = hijo2;
#         end;

        ReplaceAlg!(obj.population, genotypeChild1, genotypeChild2, indexParent1, indexParent2, fitnessChild1, fitnessChild2, obj.minimize);

    end;

    SortPopulation(obj);

    if (length(obj.population)>obj.populationSize)
        obj.population = obj.population(1:obj.populationSize);
    elseif (length(obj.population)<obj.populationSize)
        error("To be done");
    end;

    obj.currentGeneration = obj.currentGeneration + 1;

    return nothing;
end



function SaveIndividuals(obj::GAGP, fileName, numIndividualsSave)
    population = obj.population[1:numIndividualsSave];
    # save(fileName, "population", population);
    jldopen(fileName, "w") do file
        write(file, "population", obj.population)  # alternatively, say "@write file A"
        write(file, "currentGeneration", obj.currentGeneration)  # alternatively, say "@write file A"
    end
    return nothing;
end

function SaveIndividual(obj::GAGP, fileName, individual)
    population = [individual];
    # save(fileName, "population", population);
    jldopen(fileName, "w") do file
        write(file, "population", obj.population)  # alternatively, say "@write file A"
        write(file, "currentGeneration", obj.currentGeneration)  # alternatively, say "@write file A"
    end
    return nothing;
end

function SavePopulation(obj::GAGP, fileName)
    # save(fileName,"population",obj.population,"currentGeneration",obj.currentGeneration);
    jldopen(fileName, "w") do file
        write(file, "population", obj.population)  # alternatively, say "@write file A"
        write(file, "currentGeneration", obj.currentGeneration)  # alternatively, say "@write file A"
    end
    return nothing;
end

function FillPopulation(obj::GAGP)
    numIndividuals = length(obj.population);
    numIndividuals>=obj.populationSize && return;
    !obj.isGA && @assert(obj.maximumInitialHeight<=obj.maximumHeight);
    if obj.isGA
        newPopulation = Array{Individual,1}(undef, obj.populationSize - numIndividuals);
        for i in 1:(obj.populationSize - numIndividuals)
#            genotype = rand(obj.genLimits[1]:(obj.genLimits[2]-obj.genLimits[1])/100000:obj.genLimits[2], obj.genLength);
            genotype = rand(obj.genLength) .* (obj.genLimits[2]-obj.genLimits[1]) .+ obj.genLimits[1];
            newPopulation[i] = Individual(genotype, obj.fitnessFunction(genotype));
        end;
    else
        newPopulation = CreationAlg(obj.type, obj.populationSize - numIndividuals, obj.maximumInitialHeight, obj.maxNumNodes, obj.prototypes, obj.fitnessFunction);
    end;
    if !isnothing(newPopulation)
        obj.population = vcat(obj.population, newPopulation);
        SortPopulation(obj);
    end;
    return nothing;
end

function LoadPopulation(obj, fileName)
    # load(fileName, "population", "currentGeneration");
# https://github.com/JuliaIO/JLD.jl
    currentGeneration = jldopen(fileName, "r") do file
        read(file,"currentGeneration");
    end
    population = jldopen(fileName, "r") do file
        read(file,"population");
    end
    obj.population = population;
    SortPopulation(obj);
    obj.populationSize = length(obj.population);
# %             if (length(obj.population)>=populationSize),
# %                 obj.population = obj.population
# %             end;
    obj.currentGeneration = currentGeneration;
    return nothing;
end

function LoadIndividuals(obj::GAGP, fileName)
    # load(fileName);
    population = jldopen(fileName, "r") do file
        read(file,"population");
    end
    numLoadedIndividuals = length(population);
    numIndividualsInThePopulation = length(obj.population);
    if (numIndividualsInThePopulation==0)
        obj.population = population;
        SortPopulation(obj);
        return nothing;
    end;
    if (numLoadedIndividuals>=numIndividualsInThePopulation)
        obj.population = population;
        SortPopulation(obj);
        return nothing;
    end;
    indicePoblacion = numIndividualsInThePopulation;
    for numIndividuo in 1:numLoadedIndividuals
        obj.population[indicePoblacion] = population[numIndividuo];
        indicePoblacion = indicePoblacion - 1;
    end;
    SortPopulation(obj);
    return nothing;
end

# function BestIndividual(obj::GAGP)
#     return obj.population[1];
# end;


ResetearValoresEvaluacion(obj::GAGP, tree::Tree) = ResetearValoresEvaluacion(tree);
ResetearValoresEvaluacion(obj::GAGP, individual::Individual) = ResetearValoresEvaluacion(obj, individual.tree);
# Si cambian las "condiciones" (patrones), hay que llamar a esta funcion
function ResetearValoresEvaluacion(obj::GAGP)
    @assert(!obj.isGA)
    for individual in obj.population
        ResetearValoresEvaluacion(obj, individual);
    end;
end;

EvaluateIndividual(obj::GAGP, tree::Tree) = ( clearEvaluationValues!(tree); evaluateTree(tree); );
EvaluateIndividual(obj::GAGP, individual::Individual) = EvaluateIndividual(obj, individual.genotype);

BestIndividual(obj::GAGP) = obj.population[1];


Reset!(obj::GAGP) = (obj.population = Individual[];)
