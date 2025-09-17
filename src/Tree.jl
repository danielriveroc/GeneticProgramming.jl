
Semantic = Union{Float64,Array{Float64,1}}
Semantic = Any

macro def(name, definition)
  return quote
      macro $(esc(name))()
          esc($(Expr(:quote, definition)))
      end
  end
end

@def addNodeFields begin
    type::DataType
end



abstract type Tree end

abstract type Terminal <: Tree end

mutable struct Variable <: Terminal
    @addNodeFields
    variableNumber::UInt
    semantic::Vector{Real}
    Variable(num::UInt; type=Any, semantic=[]) = new(type, num, semantic)
end

mutable struct Constant <: Terminal
    @addNodeFields
    semantic::Real
    Constant(value::Real; type=Any) = new(type, value)
end

mutable struct RandomConstant <: Terminal
    @addNodeFields
    lowerLimit::Float64
    upperLimit::Float64
    RandomConstant(lowerLimit::Float64, upperLimit::Float64; type=Any) = new(type, lowerLimit, upperLimit)
end

mutable struct TerminalFunction <: Terminal
    @addNodeFields
    name::String
    evalFunction::Function
    TerminalFunction(name::String, evalFunction::Function; type=Any) = new(type, name, evalFunction)
end


@def addNonTerminalFields begin
    name::String
    semantic::Union{Nothing,Semantic}
    evalFunction::Function
end

abstract type NonTerminal <: Tree end

mutable struct BinaryNode <: NonTerminal
    @addNodeFields
    @addNonTerminalFields
    child1::Union{Tree,Nothing}
    child2::Union{Tree,Nothing}
    typeChild1::DataType
    typeChild2::DataType
    function BinaryNode(name::String, evalFunction::Function, child1::Union{Tree,Nothing}, child2::Union{Tree,Nothing}; type=Any, semantic=nothing, typeChild1::DataType=Any, typeChild2::DataType=Any)
        return new(type, name, semantic, evalFunction, child1, child2, typeChild1, typeChild2);
    end;
    function BinaryNode(name::String, evalFunction::Function; type=Any, semantic=nothing, typeChild1::DataType=Any, typeChild2::DataType=Any)
        return new(type, name, semantic, evalFunction, nothing, nothing, typeChild1, typeChild2);
    end;
end

mutable struct NonBinaryNode <: NonTerminal
    @addNodeFields
    @addNonTerminalFields
    children::Array{Union{Tree,Nothing},1}
    childrenType::Array{Type,1}
    function NonBinaryNode(name::String, evalFunction::Function, children::AbstractVector{Union{Tree,Nothing}}=Nothing[]; type=Any, semantic=nothing, childrenType::AbstractVector{DataType}=DataType[])
        return new(type, name, semantic, evalFunction, children, childrenType);
    end;
    function NonBinaryNode(name::String, evalFunction::Function, type::DataType, childrenType::AbstractVector{DataType}=DataType[]; semantic=nothing)
        return new(type, name, semantic, evalFunction, repeat([nothing], length(childrenType)), childrenType);
    end;
#     # function NonBinaryNode(name::String, evalFunction::Function; type=Any, semantic=nothing, children::AbstractVector{Union{Tree,Nothing}}=[], childrenType::AbstractVector{Type}=[])
#     # function NonBinaryNode(name::String, evalFunction::Function; type=Any, semantic=nothing, childrenType::AbstractVector{Type}=[])
#     function NonBinaryNode(name::String, evalFunction::Function; type=Any, semantic=nothing, childrenType::AbstractVector{Type}=Type[])
# #             @assert(!isempty(children) || !isempty(childrenType))
# # println(typeof(childrenArgs))
# hola;
#         # if isa(childrenArgs, Union{Tree,Nothing})
#         # return new(type, name, semantic, evalFunction, children, childrenType);
#         # return new(type, name, semantic, evalFunction, repeat([nothing], length(childrenType)), childrenType);
#     end;
end


@inline addFunction(x, y) = x.+y;
@inline subFunction(x, y) = x.-y;
@inline mulFunction(x, y) = x.*y;
@inline divFunction(x, y) = ( result=x./y; if isa(y,Array) result[y.==0].=1; elseif (y==0) result=1; end; return result; )

# @inline ifFunction(x, y, z) = isa(x,AbstractArray) ? (result = copy(z); result[x].=y[x]; return result;) : (x ? y : z)
@inline function ifFunction(x, y, z)
    x = Bool.(x);
    all(x)  && return copy(y);
    !any(x) && return copy(z);

    @assert(isa(x,AbstractArray))
    if isa(z,AbstractArray)
        result = copy(z);
    else
        result = fill(z, length(x));
    end;
    if isa(y,AbstractArray)
        result[x].=y[x];
    else
        result[x].=y;
    end;
    return result;
end;
@inline higherFunction(x, y) = x.>y;
@inline lowerFunction(x, y) = x.<y;

@inline ANDFunction(x, y) = x.&y;
@inline ORFunction(x, y) = x.|y;

AddNode(child1::Union{Tree,Nothing}, child2::Union{Tree,Nothing}; type=Any) = BinaryNode("+", addFunction, child1, child2; type=type, typeChild1=type, typeChild2=type);
SubNode(child1::Union{Tree,Nothing}, child2::Union{Tree,Nothing}; type=Any) = BinaryNode("-", subFunction, child1, child2; type=type, typeChild1=type, typeChild2=type);
MulNode(child1::Union{Tree,Nothing}, child2::Union{Tree,Nothing}; type=Any) = BinaryNode("*", mulFunction, child1, child2; type=type, typeChild1=type, typeChild2=type);
DivNode(child1::Union{Tree,Nothing}, child2::Union{Tree,Nothing}; type=Any) = BinaryNode("%", divFunction, child1, child2; type=type, typeChild1=type, typeChild2=type);

IfNode(child1::Union{Tree,Nothing}, child2::Union{Tree,Nothing}, child3::Union{Tree,Nothing}; type=Any) = NonBinaryNode("if", ifFunction, Array{Union{Nothing, Tree},1}([child1, child2, child3]); type=type, childrenType=Vector{DataType}([Bool, type, type]));
HigherNode(child1::Union{Tree,Nothing}, child2::Union{Tree,Nothing}; type=Any) = BinaryNode(">", higherFunction, child1, child2; type=Bool, typeChild1=type, typeChild2=type);
LowerNode( child1::Union{Tree,Nothing}, child2::Union{Tree,Nothing}; type=Any) = BinaryNode("<", lowerFunction,  child1, child2; type=Bool, typeChild1=type, typeChild2=type);

ANDNode(child1::Union{Tree,Nothing}, child2::Union{Tree,Nothing}) = BinaryNode(" AND ", ANDFunction, child1, child2; type=Bool, typeChild1=Bool, typeChild2=Bool);
ORNode( child1::Union{Tree,Nothing}, child2::Union{Tree,Nothing}) = BinaryNode(" OR ",   ORFunction, child1, child2; type=Bool, typeChild1=Bool, typeChild2=Bool);

AddNode(; type=Any) = AddNode(nothing, nothing; type=type);
SubNode(; type=Any) = SubNode(nothing, nothing; type=type);
MulNode(; type=Any) = MulNode(nothing, nothing; type=type);
DivNode(; type=Any) = DivNode(nothing, nothing; type=type);

IfNode(; type=Any) = IfNode(nothing, nothing, nothing; type=type);
HigherNode(; type=Any) = HigherNode(nothing, nothing; type=type);
LowerNode(; type=Any) = LowerNode(nothing, nothing; type=type);

ANDNode() = ANDNode(nothing, nothing);
ORNode() = ORNode(nothing, nothing);


clearEvaluationValues!(node::Constant) = nothing;
# clearEvaluationValues!(node::Constant, path::Array{Int,1}) = nothing;
clearEvaluationValues!(node::RandomConstant) = error("");
clearEvaluationValues!(node::Variable) = nothing;
clearEvaluationValues!(node::TerminalFunction) = nothing;
# clearEvaluationValues!(node::Variable, path::Array{Int,1}) = nothing;
function clearEvaluationValues!(node::BinaryNode)
    node.semantic = nothing;
    clearEvaluationValues!(node.child1);
    clearEvaluationValues!(node.child2);
    return nothing;
end
# function clearEvaluationValues!(node::BinaryNode, path::Array{Int,1})
#     node.semantic = nothing;
#     isempty(path) && return nothing;
#     if (path[1]==1)
#         clearEvaluationValues!(node.child1, path[2:end]);
#     else
#         clearEvaluationValues!(node.child2, path[2:end]);
#     end;
#     return nothing;
# end
function clearEvaluationValues!(node::NonBinaryNode)
    node.semantic = nothing;
    for childNode in node.children
        clearEvaluationValues!(childNode);
    end;
    return nothing;
end
# function clearEvaluationValues!(node::NonBinaryNode, path::Array{Int,1})
#     node.semantic = nothing;
#     isempty(path) && return nothing;
#     clearEvaluationValues!(node.children[path[1]], path[2:end]);
#     return nothing;
# end




setChild(node::Terminal, subTree::Tree, numChild::Int64) = error("Can not set child to a terminal node")
setChild(node::BinaryNode, subTree::Tree, numChild::Int64) = if (numChild==1) node.child1=subTree; elseif (numChild==2) node.child2=subTree; else error("Wrong child number") end;
setChild(node::NonBinaryNode, subTree::Tree, numChild::Int64) = node.children[numChild]=subTree;




import Base.string
string(node::Constant) = node.semantic>=0 ? string(node.semantic) : string("(",node.semantic,")");
string(node::RandomConstant) = error("");
string(node::Variable) = string("X", node.variableNumber);
string(node::TerminalFunction) = string(node.name, "()");
string(node::BinaryNode) = string("(", node.name in ["+","-","*","%",">","<",">=","<=","==","!="] ? string(node.child1)*node.name*string(node.child2) : node.name*"("*string(node.child1)*","*string(node.child2)*")" ,")")
function string(node::NonBinaryNode)
    if (length(node.children)==2)
        # text = string(node.name,"(",string(node.children[1]),",",string(node.children[2]),")")
        text = string("(",string(node.children[1]),node.name,string(node.children[2]),")")
    else
        text = string(node.name, "(");
        for numChildren = 1:length(node.children)
            text = string(text, string(node.children[numChildren]));
            if (numChildren!=length(node.children))
                text = string(text, ',');
            end;
        end;
        text = string(text, ")");
    end;
    return text;
end;



evaluateTree(tree::RandomConstant) = error("");
evaluateTree(tree::Terminal) = tree.semantic;
evaluateTree(tree::TerminalFunction) = tree.evalFunction();
function evaluateTree(tree::BinaryNode)
    if isnothing(tree.semantic)
        tree.semantic = tree.evalFunction( evaluateTree(tree.child1), evaluateTree(tree.child2) );
    end;
    return tree.semantic;
end;
function evaluateTree(tree::NonBinaryNode)
    if isnothing(tree.semantic)
        evaluationChildren = [evaluateTree(child) for child in tree.children];
        tree.semantic = tree.evalFunction( evaluationChildren... );
    end;
    return tree.semantic;
end;

reevaluatePath(tree::Terminal, path::Array{Int64,1}, indexPath::Int64) = tree.semantic;
reevaluatePath(tree::TerminalFunction, path::Array{Int64,1}, indexPath::Int64) = evaluateTree(tree);
reevaluatePath(tree::RandomConstant, path::Array{Int64,1}, indexPath::Int64) = error("");
function reevaluatePath(tree::BinaryNode, path::Array{Int64,1}, indexPath::Int64=1)
    if (indexPath>length(path))
        semanticChild1 =   evaluateTree(tree.child1);
        semanticChild2 =   evaluateTree(tree.child2);
    elseif (path[indexPath]==1)
        semanticChild1 = reevaluatePath(tree.child1, path, indexPath+1);
        semanticChild2 =   evaluateTree(tree.child2);
    elseif (path[indexPath]==2)
        semanticChild1 =   evaluateTree(tree.child1);
        semanticChild2 = reevaluatePath(tree.child2, path, indexPath+1);
    else
        error("Don't know which path to go")
    end;
    tree.semantic = tree.evalFunction( semanticChild1, semanticChild2, tree.semantic );
    # tree.semantic = tree.evalFunction( semanticChild1, semanticChild2 );
    return tree.semantic;
end;
function reevaluatePath(tree::NonBinaryNode, path::Array{Int64,1}, indexPath::Int64=1)
    # @assert !isnothing(tree.semantic);
    evaluationChildren = [evaluateTree(child) for child in tree.children];
    if (indexPath<=length(path))
        numChild = path[indexPath];
        evaluationChildren[numChild] = reevaluatePath(tree.children[numChild], path, indexPath+1);
    end;
    tree.semantic = tree.evalFunction( evaluationChildren... );

    return tree.semantic;
end;




numNodes(node::Nothing) = UInt(0);
numNodes(node::Terminal) = UInt(1);
numNodes(node::BinaryNode) = UInt(1 + numNodes(node.child1) + numNodes(node.child2));
numNodes(node::NonBinaryNode) = UInt(1 + sum([numNodes(child) for child in node.children]));

height(node::Terminal) = UInt(1);
height(node::BinaryNode) = UInt(1) + max(height(node.child1), height(node.child2));
height(node::NonBinaryNode) = UInt(1) + (isempty(node.children) ? UInt(0) : maximum([height(child) for child in node.children]));

clone(node::Nothing) = nothing;
clone(node::Constant) = Constant(node.semantic; type=node.type);
clone(node::RandomConstant) = Constant(rand()*(node.upperLimit-node.lowerLimit) + node.lowerLimit; type=node.type);
clone(node::Variable) = Variable(node.variableNumber; semantic=node.semantic, type=node.type);
clone(node::TerminalFunction) = TerminalFunction(node.name, node.evalFunction; type=node.type);
# clone(node::BinaryNode) = BinaryNode(node.name, node.evalFunction, clone(node.child1), clone(node.child2); semantic=((node.semantic==nothing) ? node.semantic : copy(node.semantic)));
# clone(node::NonBinaryNode) = NonBinaryNode(node.name, node.evalFunction, node.equationsFunction; semantic=((node.semantic==nothing) ? node.semantic : copy(node.semantic)), children=convert(Array{Tree,1},[clone(child) for child in node.children]));
clone(node::BinaryNode) = BinaryNode(node.name, node.evalFunction, clone(node.child1), clone(node.child2); type=node.type, semantic=node.semantic, typeChild1=node.typeChild1, typeChild2=node.typeChild2);
# clone(node::NonBinaryNode) = NonBinaryNode(node.name, node.evalFunction, node.equationsFunction; semantic=node.semantic, children=convert(Array{Union{Nothing,Tree},1},[clone(child) for child in node.children]));
clone(node::NonBinaryNode) = NonBinaryNode(node.name, node.evalFunction, Vector{Union{Nothing, Tree}}(clone.(node.children)); type=node.type, semantic=node.semantic, childrenType=Vector{DataType}(node.childrenType));


iterateTree(tree::Terminal) = (convert(Array{Tree,1},[tree]), [0x0000000000000001], [0x0000000000000001], [0x0000000000000000], [0x0000000000000001], [tree.type]);
function iterateTree(tree::BinaryNode)
    # nodes = [tree];
    # heights = [0x0000000000000000];
    # depths = [0x0000000000000001];
    # numChildren = [0x0000000000000001];
    # numNodes = [0x0000000000000000];
# println(string(tree))
    (nodesChild1, heightsChild1, depthsChild1, numChildrenChild1, numNodesChild1, typesChild1) = iterateTree(tree.child1);
    (nodesChild2, heightsChild2, depthsChild2, numChildrenChild2, numNodesChild2, typesChild2) = iterateTree(tree.child2);
    nodes = [nodesChild1; nodesChild2]; pushfirst!(nodes, tree);
    heights = [heightsChild1; heightsChild2]; pushfirst!(heights, 0x0000000000000000);
    depths = [depthsChild1; depthsChild2]; pushfirst!(depths, 0x0000000000000001);
    numChildren = [numChildrenChild1; numChildrenChild2]; pushfirst!(numChildren, 0x0000000000000001);
    numNodes = [numNodesChild1; numNodesChild2]; pushfirst!(numNodes, 0x0000000000000000);
    heights[1] = maximum(heights[2:end])+1;
    numNodes[1] = length(numNodes);
    depths[2:end] .+= 1;
    types = [typesChild1; typesChild2]; pushfirst!(types, tree.type);
    return (nodes, heights, depths, numChildren, numNodes, types);
end
function iterateTree(tree::NonBinaryNode)
    nodes = [tree];
    heights = [0x0000000000000000];
    depths = [0x0000000000000001];
    numChildren = [length(tree.children)];
    numNodes = [0x0000000000000000];
    types = [tree.type];
    for child in tree.children
# println(string(child))
        (nodesThisChild, heightsThisChild, depthsThisChild, numChildrenThisChild, numNodesThisChild, typesThisChild) = iterateTree(child);
        nodes = [nodes; nodesThisChild];
        # hcat(tipos, tiposEsteHijo);
        heights = [heights; heightsThisChild];
        depths = [depths; depthsThisChild.+1];
        numChildren = [numChildren; numChildrenThisChild];
        numNodes = [numNodes; numNodesThisChild];
        types = [types; typesThisChild];
    end;
    heights[1] = maximum(heights[2:end])+1;
    numNodes[1] = length(numNodes);
    # depths[2:end] .+= 1;
# println(string(tree))
# println(types)
# println(Int64.(depths))
# println(Int64.(heights))
# println(Int64.(numNodes))
# hola;
    return (nodes, heights, depths, numChildren, numNodes, types);
end







setVariableValues!(tree::Terminal, variableValues::Array{Array{Float64,1}}) = ()
setVariableValues!(tree::Variable, variableValues::Array{Array{Float64,1}}) = (tree.semantic = variableValues[tree.variableNumber]; )
setVariableValues!(tree::BinaryNode, variableValues::Array{Array{Float64,1}}) = (setVariableValues!(tree.child1, variableValues); setVariableValues!(tree.child2, variableValues); tree.semantic = nothing; )
setVariableValues!(tree::NonBinaryNode, variableValues::Array{Array{Float64,1}}) = (for child in tree.children setVariableValues!(child, variableValues); end; tree.semantic = nothing; )




findNode(tree::Terminal, node::Tree) = (tree==node) ? Array{Int,1}([]) : nothing
function findNode(tree::BinaryNode, node::Tree)
    (tree==node) && return Array{Int,1}([]);
    path = findNode(tree.child1, node);
    if (path!=nothing)
        pushfirst!(path, 1);
        return path;
    end;
    path = findNode(tree.child2, node);
    if (path!=nothing)
        pushfirst!(path, 2);
        return path;
    end;
    return nothing;
end;
function findNode(tree::NonBinaryNode, node::Tree)
    (tree==node) && return Array{Int,1}([]);
	for numChild in 1:length(tree.children)
        path = findNode(tree.children[numChild], node);
        if (path!=nothing)
            pushfirst!(path, numChild);
            return path;
        end;
    end;
    return nothing;
end;


findParent(tree::Nothing, node::Tree)  = (nothing, 0);
findParent(tree::Terminal, node::Tree) = (nothing, 0);
function findParent(tree::BinaryNode, node::Tree)
    if (tree.child1==node)
        return (tree,1);
    elseif (tree.child2==node)
        return (tree,2);
    else
        (parent, numChild) = findParent(tree.child1, node);
        if (parent!=nothing)
            return (parent, numChild)
        else
            return findParent(tree.child2, node);
        end;
    end;
end;
function findParent(tree::NonBinaryNode, node::Tree)
	for numChild in 1:length(tree.children)
        if (tree.children[numChild]==node)
            return (tree, numChild);
        end;
        (parent, n) = findParent(tree.children[numChild], node);
        if (parent!=nothing)
            return (parent, n)
        end;
    end;
    return (nothing, 0);
end;


# setSubtree(tree::Terminal, node::Tree, subTree::Tree) = (tree!=node) ? error("Node not found in terminal tree") : subTree;
# setSubtree(tree::Terminal, node::Tree, subTree::Tree) = (tree==node) ? tree : subTree;
replaceSubtree!(tree::Terminal, oldSubTree::Tree, newSubTree::Tree) = false;
function replaceSubtree!(tree::BinaryNode, oldSubTree::Tree, newSubTree::Tree)
    # (tree==node) && error("To assign a subtree as root, do it outside this function");
    # (tree==node) && return subTree;
    replaced = false;
    if (tree.child1==oldSubTree)
# if newSubTree.type==AbstractFloat
# println(newSubTree.type, " ", tree.typeChild1)
# println(string(tree))
# println(string(oldSubTree))
# println(string(newSubTree))
# end;
        @assert(newSubTree.type <: tree.typeChild1)
        tree.child1 = newSubTree;
        replaced = true;
    elseif (tree.child2==oldSubTree)
        @assert(newSubTree.type <: tree.typeChild2)
        tree.child2 = newSubTree;
        replaced = true;
    else
        replaced = replaceSubtree!(tree.child1, oldSubTree, newSubTree);
        if !replaced
            replaced = replaceSubtree!(tree.child2, oldSubTree, newSubTree);
        end;
    end;
    if (replaced)
# evaluationChildren = [evaluateTree(child) for child in tree.children];
# tree.semantic = tree.evalFunction( evaluateTree(tree.child1, tree.child2), tree.semantic );
# tree.semantic = tree.evalFunction( evaluateTree(tree.child1), evaluateTree(tree.child2), tree.semantic );
# return true;
        # tree.semantic = reevaluatePath(tree, Int64[]);
        tree.semantic = nothing;
        # tree.semantic = evaluateTree(tree);
        return true;
    end;
    return false;
end
function replaceSubtree!(tree::NonBinaryNode, oldSubTree::Tree, newSubTree::Tree)
    # (tree==node) && error("To assign a subtree as root, do it outside this function");
    # (tree==node) && return subTree;
	for numChild in 1:length(tree.children)
        child = tree.children[numChild]
        if (child==oldSubTree)
            @assert(newSubTree.type <: tree.childrenType[numChild])
            tree.children[numChild] = newSubTree;
# evaluationChildren = [evaluateTree(child) for child in tree.children];
# tree.semantic = tree.evalFunction( tree.semantic, evaluationChildren... );
# return true;

            tree.semantic = reevaluatePath(tree, Int64[]);
            # tree.semantic = nothing;
            # tree.semantic = evaluateTree(tree);
            return true;
        else
            if (replaceSubtree!(child, oldSubTree, newSubTree))
# evaluationChildren = [evaluateTree(child) for child in tree.children];
# tree.semantic = tree.evalFunction( tree.semantic, evaluationChildren... );
# return true;
                # tree.semantic = reevaluatePath(tree, Int64[]);
                tree.semantic = nothing;
                # tree.semantic = evaluateTree(tree);
                return true;
            end;
        end;
    end;
    return false;
end





correctTypes(tree::Terminal) = true;
correctTypes(tree::BinaryNode) = (tree.child1.type <: tree.typeChild1) && (tree.child2.type <: tree.typeChild2) && correctTypes(tree.child1) && correctTypes(tree.child2)
function correctTypes(tree::NonBinaryNode)
    isempty(tree.children) && return true;
    for numHijo in 1:length(tree.children)
        !(tree.children[numHijo].type <: tree.childrenType[numHijo]) && return false;
        !correctTypes(tree.children[numHijo]) && return false;
    end;
    return true;
end;


