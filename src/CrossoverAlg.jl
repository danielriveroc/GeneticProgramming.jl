

#################################################################################################################################################################
#
# Standard crossover for GP
#

function Crossover(treeParent1::Tree, treeParent2::Tree, treeType::Type, maximumHeight, maxNumNodes; nonTerminalSelectionProb::Float64=0.9)

    treeParent1 = clone(treeParent1);
    treeParent2 = clone(treeParent2);


checkForErrors = false;

    checkForErrors && @assert(height(treeParent1)<=maximumHeight && height(treeParent2)<=maximumHeight);

    (nodes1, heights1, depths1, numChildren1, numNodes1, types1) = iterateTree(treeParent1);
    (nodes2, heights2, depths2, numChildren2, numNodes2, types2) = iterateTree(treeParent2);

    numNodesParent1 = numNodes1[1];
    numNodesParent2 = numNodes2[1];

if (checkForErrors)
    @assert(numNodesParent1==numNodes(treeParent1))
    @assert(numNodesParent2==numNodes(treeParent2))
end;

    nodesTree1StillNotTried = trues(length(types1));
    while (sum(nodesTree1StillNotTried)>0)
        if (rand()<nonTerminalSelectionProb)
            possibleNodesTree1 = nodesTree1StillNotTried .& (numChildren1.>0);
            if (sum(possibleNodesTree1)==0)
                possibleNodesTree1 = nodesTree1StillNotTried .& (numChildren1.==0);
            end;
        else
            possibleNodesTree1 = nodesTree1StillNotTried .& (numChildren1.==0);
            if (sum(possibleNodesTree1)==0)
                possibleNodesTree1 = nodesTree1StillNotTried .& (numChildren1.>0);
            end;
        end;
        possibleNodesTree1 = (possibleNodesTree1 .& nodesTree1StillNotTried);
        # Cogemos uno al azar: primero los desordenamos
        possibleNodesTree1 = findall(possibleNodesTree1);
        # possibleNodesTree1 = possibleNodesTree1(randperm(length(possibleNodesTree1)));
        shuffle!(possibleNodesTree1);

        numNodeTree1 = possibleNodesTree1[1];

        if (numNodeTree1==1)
            typeNodeTree1 = treeType;
        else
            (node1Parent, numHijo) = findParent(treeParent1, nodes1[numNodeTree1])
            if isa(node1Parent,BinaryNode)
                if (numHijo==1)
                    typeNodeTree1 = node1Parent.typeChild1;
                elseif (numHijo==2)
                    typeNodeTree1 = node1Parent.typeChild2;
                end;
            elseif isa(node1Parent,NonBinaryNode)
                typeNodeTree1 = node1Parent.childrenType[numHijo];
            end;
        end;
        # typeNodeTree1 = types1[numNodeTree1];
        heightNodeTree1 = heights1[numNodeTree1];
        depthNodeTree1 = depths1[numNodeTree1];

        # Cogemos uno del arbol 2 que encaje
        possibleNodesTree2 = [nodeType <: typeNodeTree1 for nodeType in types2];

        # Si el nodo escogido del arbol 1 es la raiz, se evita coger la raiz del arbol 2
        if (numNodeTree1==1)
            possibleNodesTree2[1] = false;
        end;

        if (maximumHeight!=nothing)
            possibleNodesTree2 .&= ((depths2 .+ heightNodeTree1 .- 1).<= maximumHeight);
            possibleNodesTree2 .&= ((heights2 .+ depthNodeTree1 .- 1) .<= maximumHeight);
        end;

        if (maxNumNodes!=nothing)
            possibleNodesTree2 .&= (numNodes2 .<= (maxNumNodes-numNodes1[1]+numNodes1[numNodeTree1]));
            possibleNodesTree2 .&= (((maxNumNodes-numNodes2[1]-numNodes1[numNodeTree1]) .- numNodes2) .<= maxNumNodes);
        end;

        if (rand()<nonTerminalSelectionProb)
            possibleNodesTree2Temp = possibleNodesTree2 .& (numChildren2.>0);
            if (sum(possibleNodesTree2Temp)==0)
                possibleNodesTree2 = possibleNodesTree2 .& (numChildren2.==0);
            else
                possibleNodesTree2 = possibleNodesTree2Temp;
            end;
        else
            possibleNodesTree2Temp = possibleNodesTree2 .& (numChildren2.==0);
            if (sum(possibleNodesTree2Temp).==0)
                possibleNodesTree2 = possibleNodesTree2 .& (numChildren2.>0);
            else
                possibleNodesTree2 = possibleNodesTree2Temp;
            end;
        end;

        # Cogemos uno al azar: primero los desordenamos
        possibleNodesTree2 = findall(possibleNodesTree2);

        # possibleNodesTree2 = possibleNodesTree2(randperm(length(possibleNodesTree2)));
        shuffle!(possibleNodesTree2);

        while !isempty(possibleNodesTree2)

            numNodeTree2 = possibleNodesTree2[1];

            @assert((numNodeTree1!=1) || (numNodeTree2!=1));

            subtreeParent1 = nodes1[numNodeTree1];
            subtreeParent2 = nodes2[numNodeTree2];

            # Aqui vamos a ver si el subarbol1 encaja en el hueco del arbol 2 por el tipado
            if (numNodeTree2==1)
                typeNodeTree2 = treeType;
            else
                (node2Parent, numHijo) = findParent(treeParent2, subtreeParent2);
                if isa(node2Parent,BinaryNode)
                    if (numHijo==1)
                        typeNodeTree2 = node2Parent.typeChild1;
                    elseif (numHijo==2)
                        typeNodeTree2 = node2Parent.typeChild2;
                    end;
                elseif isa(node2Parent,NonBinaryNode)
                    typeNodeTree2 = node2Parent.childrenType[numHijo];
                end;
            end;
            if !(subtreeParent1.type <: typeNodeTree2)
                popfirst!(possibleNodesTree2);
                continue;
            end;

            if (numNodeTree1==1)
                @assert(numNodeTree2!=1)
                child1 = subtreeParent2;
            else
                child1 = treeParent1;
                res = replaceSubtree!(child1, subtreeParent1, subtreeParent2);
                @assert(res);
            end;

            if (numNodeTree2==1)
                @assert(numNodeTree1!=1)
                child2 = subtreeParent1;
            else
                child2 = treeParent2;
                res = replaceSubtree!(child2, subtreeParent2, subtreeParent1);
                @assert(res);
            end;

            if (checkForErrors)
                (maximumHeight!=nothing) && @assert((height(child1)<=maximumHeight) && (height(child2)<=maximumHeight));
                (maxNumNodes!=nothing) && @assert((numNodes(child1)<=maxNumNodes) && (numNodes(child2)<=maxNumNodes));
                @assert (correctTypes(child1));
                @assert (correctTypes(child2));
            end;

            return (child1, child2);

        end; # while !isempty(possibleNodesTree2)

        @assert isempty(possibleNodesTree2)
        nodesTree1StillNotTried[numNodeTree1] = false;

    end; # while (sum(nodesTree1StillNotTried)>0)

    # Si llega aqui, es que no se pueden cruzar los arboles, solamente las raices, es decir,
    #  intercambiar un arbol por otro, y esto no se quiere hacer

    return (nothing, nothing);

end



function DeshacerCruceAmbos(arbol1::Tree, arbol2::Tree, caminoRecorridoPadre1::Array{Int,1}, subtreeParent1::Tree, valoresEvaluacionPadre1::Array{Any,1}, caminoRecorridoPadre2::Array{Int,1}, subtreeParent2::Tree, valoresEvaluacionPadre2::Array{Any,1})
    arbol1 = DeshacerMutacion(arbol1, caminoRecorridoPadre1, subtreeParent1, valoresEvaluacionPadre1);
    arbol2 = DeshacerMutacion(arbol2, caminoRecorridoPadre2, subtreeParent2, valoresEvaluacionPadre2);
    return (arbol1, arbol2);
end

function DeshacerCruceAmbos(arbol1::Tree, arbol2::Tree, infoDeshacerCruce::Tuple{Array{Int,1},Tree,Array{Any,1},Array{Int,1},Tree,Array{Any,1}})
    (caminoRecorridoPadre1, subtreeParent1, valoresEvaluacionPadre1, caminoRecorridoPadre2, subtreeParent2, valoresEvaluacionPadre2) = infoDeshacerCruce;
    return DeshacerCruceAmbos(arbol1, arbol2, caminoRecorridoPadre1, subtreeParent1, valoresEvaluacionPadre1, caminoRecorridoPadre2, subtreeParent2, valoresEvaluacionPadre2);
end

function DeshacerCruceArbol1(arbol1::Tree, infoDeshacerCruce::Tuple{Array{Int,1},Tree,Array{Any,1},Array{Int,1},Tree,Array{Any,1}})
    (caminoRecorridoPadre1, subtreeParent1, valoresEvaluacionPadre1, _, _, _) = infoDeshacerCruce;
    return DeshacerMutacion(arbol1, caminoRecorridoPadre1, Clonar(subtreeParent1), valoresEvaluacionPadre1);
end

function DeshacerCruceArbol2(arbol2::Tree, infoDeshacerCruce::Tuple{Array{Int,1},Tree,Array{Any,1},Array{Int,1},Tree,Array{Any,1}})
    (_, _, _, caminoRecorridoPadre2, subtreeParent2, valoresEvaluacionPadre2) = infoDeshacerCruce;
    return DeshacerMutacion(arbol2, caminoRecorridoPadre2, Clonar(subtreeParent2), valoresEvaluacionPadre2);
end



############################################################################################################
#
# GA crossover algorithms
#

function TwoPointCrossover(genotype1::Vector{Float64}, genotype2::Vector{Float64})
    numGen1 = rand(1:length(genotype1));
    numGen2 = rand(1:length(genotype2));
    if numGen1>numGen2
        temp = numGen2; numGen2 = numGen1; numGen1 = temp;
    end;
    genotypeChild1 = copy(genotype1);
    genotypeChild2 = copy(genotype2);
    for i in numGen1:numGen2
        genotypeChild1[i] = genotype2[i];
        genotypeChild2[i] = genotype1[i];
    end;
    return (genotypeChild1, genotypeChild2)
end;

Crossover(genotype1::Vector{Float64}, genotype2::Vector{Float64}) = TwoPointCrossover(genotype1, genotype2)
