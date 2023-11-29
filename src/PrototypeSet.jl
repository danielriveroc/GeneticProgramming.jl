
mutable struct Prototypes
    nodes::Array{Tree,1}
    types::Array{DataType,1}
    numChildrens::Array{Int,1}
    childrenTypes::Array{Array{DataType,1},1}
    Prototypes() = new([],[],[],[]);
end

function addPrototype!(obj::Prototypes, node::Terminal)
    push!(obj.nodes, node);
    push!(obj.types, node.type);
    push!(obj.numChildrens, 0);
    push!(obj.childrenTypes, []);
    return nothing;
end

function addPrototype!(obj::Prototypes, node::BinaryNode)
    push!(obj.nodes, node);
    push!(obj.types, node.type);
    push!(obj.numChildrens, 2);
    push!(obj.childrenTypes, [node.typeChild1, node.typeChild2]);
    return nothing;
end

function addPrototype!(obj::Prototypes, node::NonBinaryNode)
    push!(obj.nodes, node);
    push!(obj.types, node.type);
    push!(obj.numChildrens, length(node.childrenType));
    push!(obj.childrenTypes, node.childrenType);
    return nothing;
end






function buildTree(obj::Prototypes, type, buildHeight, complete::Bool, maxNumNodes::Union{Nothing,UInt})

    !isnothing(maxNumNodes) && (maxNumNodes<=0) && return nothing;
    !isnothing(buildHeight) && (buildHeight<=0) && return nothing;

    validPrototypes = [opType <: type for opType in obj.types];

    if (!isnothing(buildHeight) && (buildHeight==1)) || (!isnothing(maxNumNodes) && (maxNumNodes==1))
        validPrototypes = validPrototypes .& (obj.numChildrens.==0);
        validPrototypes = findall(validPrototypes);
        shuffle!(validPrototypes);
    else
        if (complete)
            if (sum(validPrototypes .& (obj.numChildrens.>0))>0)
                validPrototypes = validPrototypes .& (obj.numChildrens.>0);
            end;
            validPrototypes = findall(validPrototypes);
            shuffle!(validPrototypes);
        else
            indicesFunciones = findall(validPrototypes .& (obj.numChildrens.>0));
            indicesTerminales = findall(validPrototypes .& (obj.numChildrens.==0));
            if (randn()>=0)
                # Try to take functions first
                validPrototypes = [shuffle!(indicesFunciones); shuffle!(indicesTerminales)];
            else
                # Try to take terminals first
                validPrototypes = [shuffle!(indicesTerminales); shuffle!(indicesFunciones)];
            end;
        end;
    end;

    # As long as there are valid prototypes
    while (!isempty(validPrototypes))
        # Take the last
        thisPrototipe = pop!(validPrototypes);

        node = obj.nodes[thisPrototipe];
        numChildren = obj.numChildrens[thisPrototipe];

        if (isa(node, Terminal))
            return clone(node);
        end;

        childrenType = obj.childrenTypes[thisPrototipe];
        children = Array{Any,1}(undef,numChildren); # Should be Array{Tree,1}...
        correctTree = true;
        numNodesSubTree = 1;
        for numChild in 1:numChildren
            if (correctTree)
                child = buildTree(obj,obj.childrenTypes[thisPrototipe][numChild], isnothing(buildHeight) ? buildHeight : buildHeight-1, complete, isnothing(maxNumNodes) ? maxNumNodes : maxNumNodes-numNodesSubTree);
                if isnothing(child)
                    correctTree = false;
                else
                    children[numChild] = child;
                    numNodesSubTree += numNodes(child);
                end;
            end;
        end;
        if (correctTree)
            tree = clone(node);
            if isa(tree, BinaryNode)
                tree.child1 = children[1];
                tree.child2 = children[2];
            else
                tree.children = children;
            end;

            @assert correctTypes(tree) "";
            !isnothing(buildHeight) && @assert(height(tree)<=buildHeight);
            !isnothing(maxNumNodes) && @assert(numNodes(tree)<=maxNumNodes);
            return tree;
        end;
    end;

    return nothing;

end;



# eval(Meta.parse("rand"))
# e = Expr(:call, :+, 1, 4)
# e2 = Expr(:call, :+, 1, 4)
# e.args[3] = e2;
# dump(e);
# println(obj.nombreFuncion[thisPrototipe]);
# println(numChildren);
# return;
