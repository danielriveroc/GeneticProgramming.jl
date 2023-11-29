

function Mutate(tree::Tree, type::Type, prototypes::Prototypes, maximumHeight::Union{UInt,Nothing}, maxNumNodes::Union{UInt,Nothing}; nonTerminalSelectionProb=nothing)

    @assert ((maximumHeight==nothing) || (height(tree)<=maximumHeight));
    @assert ((maxNumNodes==nothing) || (numNodes(tree)<=maxNumNodes));

    (nodes, heights, depths, numChildren, numNodesEach, types) = iterateTree(tree);

    # Miramos a ver si hay que coger terminales o no terminales
    if (nonTerminalSelectionProb!=nothing)
        if (rand()<nonTerminalSelectionProb)
            numPossibleNodes = (numChildren.>0);
            if (sum(numPossibleNodes)==0)
                numPossibleNodes = trues(length(numChildren));
            end;
        else
            numPossibleNodes = (numChildren.==0);
        end;
    else
        numPossibleNodes = trues(length(numChildren));
    end;

    # Cogemos uno al azar
    numNodoArbol = rand((1:length(types))[numPossibleNodes]);
    # alturaNodoArbol = heights[numNodoArbol];

    if (numNodoArbol==1)
        nodeTreeType = type;
    else
        (parentNode, numChild) = findParent(tree, nodes[numNodoArbol]);
        @assert(parentNode!=nothing);
        @assert(isa(parentNode,NonTerminal));
        if (isa(parentNode,BinaryNode))
            if (numChild==1)
                nodeTreeType = parentNode.typeChild1;
            elseif (numChild==2)
                nodeTreeType = parentNode.typeChild2;
            end;
        else
            nodeTreeType = parentNode.childrenType[numChild];
        end;
    end;


    # Creamos el subarbol
    if (maximumHeight==nothing) && (maxNumNodes==nothing)
        subTree = buildTree(prototypes, nodeTreeType, heights[numNodoArbol] + 3,                                                             false,              nothing                                                                           );
    else
        subTree = buildTree(prototypes, nodeTreeType, maximumHeight==nothing ? maximumHeight : maximumHeight - depths[numNodoArbol] + 1, false, maxNumNodes==nothing ? maxNumNodes : maxNumNodes - numNodesEach[1] + numNodesEach[numNodoArbol]);
    end;
    @assert (subTree!=nothing);

    # Y lo asignamos
    if (numNodoArbol==1)
        tree = subTree;
    else
        setChild(parentNode, subTree, numChild);
    end;

    # Comprobaciones (quitarlas cuando ya lleve mucho ejecutandose)
    @assert ((maximumHeight==nothing) || (height(tree)<=maximumHeight));
    @assert ((maxNumNodes==nothing) || (numNodes(tree)<=maxNumNodes));
    @assert (correctTypes(tree));

    return tree;
end






function Mutate(genotype::Vector{Float64}, genLimits::Tuple{Float64,Float64})

    numGen = rand(1:length(genotype));

    genotype[numGen] = rand()*(genLimits[2]-genLimits[1]) + genLimits[1];

    return genotype;

end
