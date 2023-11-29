
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

