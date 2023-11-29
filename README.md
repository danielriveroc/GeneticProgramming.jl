# GeneticProgramming.jl


This package contains the functions to run Genetic Programming in Julia. This library tries to be simple and easy to use. Below is a simple example of use:

```
using Statistics
using Random: seed!

using GeneticProgramming
seed!(1);

# Dataset with 100 instances, 3 variables
inputs = randn(100,3);
targets = inputs[:,1] .+ (2.3 .* inputs[:,2] .* sin.(inputs[:,3])) .- 0.18;

function fitnessFunction end;

# Create the GP object. Necessary to specify:
#  Fitness function
#  Are we going to minimise (true) or maximise (false)
gp = GP(fitnessFunction, true);

# Fitness function: receives the genotype (tree) and calls the function "EvaluateIndividual" to evaluate it
function fitnessFunction(genotype)
    global gp;
    outputs = EvaluateIndividual(gp, genotype);
    return mean(abs.(outputs.-targets));
end

# Set some hyperpatameters
gp.maximumHeight = 5;
gp.maximumInitialHeight = 5;
gp.populationSize = 100;

# Add some functions that can be used as nodes in the tree
# These are already implemented in the library, so it is only necessary to put its name
# The call receives:
#   The GP object
#   The name of the function
addPrototype!(gp, "+");
addPrototype!(gp, "-");
addPrototype!(gp, "*");
addPrototype!(gp, "%");
# This function is defined by the user. It receives:
#   The GP object
#   The name of the function
#   The number of children
#   The function to be evaluated
#    Note that this function is defined in vector form
addPrototype!(gp, "mySin", 1, x -> sin.(x));

# Add random constants in an interval as leaves of the tree. This call receives:
#   The GP object
#   Lower bound of the constants
#   Upper bound of the constants
#   The name of the function
addPrototype!(gp, 0., 1.);

# Add the variables as leaves of the tree. This call receives:
#   The GP object
#   The inputs matrix
#   An optional parameter that specifies if the samples are in rows (by default) or in columns
addVariables!(gp, inputs; dataInRows=true)

# Run the algorithm 20 generations
for i in 1:50
    Step!(gp);
    # With the function BestIndividual(gp) we can retrieve the best-so-far individual
    # With this individual, we can get its fitness or a string with its equation
    println("Generation ", i, ", Fitness of the best individual: ", BestIndividual(gp).fitness, " - expression: ", string(BestIndividual(gp)))
end;
```

This package also supports typing in the nodes of the tree. For this purpose, types can be set when defining the nodes of the tree. Here is a simple code:

```
using Statistics
using Random: seed!

using GeneticProgramming
seed!(1);

# Dataset with 100 instances, 3 variables. The first one is a boolean
boolVar = randn(100).>=0;
inputs = [boolVar randn(100,2)];
# Targets with different value depending on the first (boolean) variable
targets = zeros(size(inputs,1));
targets[  boolVar] = inputs[boolVar,2] .+ ( 5.6 .* sin.(inputs[boolVar,3]) );
targets[.!boolVar] = sin.(0.8.*inputs[.!boolVar,2]) .% ( inputs[.!boolVar,3] );

function fitnessFunction end;

# Create the GP object. Necessary to specify:
#  Fitness function
#  Are we going to minimise (true) or maximise (false)
gp = GP(fitnessFunction, true);

# Fitness function: receives the genotype (tree) and calls the function "EvaluateIndividual" to evaluate it
function fitnessFunction(genotype)
    outputs = EvaluateIndividual(gp, genotype);
    return mean(abs.(outputs.-targets));
end

# Set some hyperpatameters
gp.maximumHeight = 5;
gp.maximumInitialHeight = 5;
gp.populationSize = 100;

# Add some functions that can be used as nodes in the tree
# These are already implemented in the library, so it is only necessary to put its name
# The call receives:
#   The GP object
#   The name of the function
#   Optionally, the type of the outputs and children
addPrototype!(gp, "+";  type=Float64);
addPrototype!(gp, "-";  type=Float64);
addPrototype!(gp, "*";  type=Float64);
addPrototype!(gp, "%";  type=Float64);
addPrototype!(gp, "if"; type=Float64);
# This function is defined by the user. It receives:
#   The GP object
#   The name of the function
#   The type of the value returned by this function
#   The function to be evaluated
#    Note that this function is defined in vector form
#   A vector with the types of its children
#    Note that the number of children is specified in this vector
addPrototype!(gp, "mySin", AbstractFloat, x -> sin.(x), [Float64]);

# Add random constants in an interval as leaves of the tree. This call receives:
#   The GP object
#   Lower bound of the constants
#   Upper bound of the constants
#   The name of the function
#   Optionally, the type of the constant
addPrototype!(gp, 0., 1.; type=Float64);

# Add the variables as leaves of the tree. This call receives:
#   The GP object
#   The inputs matrix
#   An optional parameter that specifies if the samples are in rows (by default) or in columns
#   An optional parameter that specifies the type of each variable
addVariables!(gp, inputs; dataInRows=true, type = [Bool, Float64, Float64])

# Run the algorithm 20 generations
for i in 1:20
    Step!(gp);
    # With the function BestIndividual(gp) we can retrieve the best-so-far individual
    # With this individual, we can get its fitness or a string with its equation
    println("Generation ", i, ", Fitness of the best individual: ", BestIndividual(gp).fitness, " - expression: ", string(BestIndividual(gp)))
end;
```

