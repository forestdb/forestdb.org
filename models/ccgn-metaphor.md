---
layout: model
title: Caouette, Corpening, Gil, Nishikawa - Metaphor
model-language: webppl
---

This model aims to explore the use of metaphors in our speech. For example, imagine you hear “John is a whale”, what do you infer about John? It’s unlikely that you’d assume John is a physical whale.  More reasonably, you’d think along the lines of John being a person who has whale-like qualities. That is the phenomenon being investigated. 
This concept is tackled using an RSA model (rational speech act), which seeks to computationally reproduce the higher level cognition used to reason about ambiguity that exists between the speaker and the listener of an utterance. In this case, the ambiguity occurs from the inherent falsehood of the speaker’s non-literal speech act, i.e. the metaphor. The model will reason about metaphors by aligning utterances with “stereotypical” features, where there is possible uncertainty about which feature is the topic of conversation.


~~~~
// John could either be a whale or a person.
var categories = ["whale", "person"]

// It is extremely unlikely that John is actually a whale.
var categoriesPrior = function() {
  categorical([0.01, 0.99], categories)
}

// The speaker could either say "John is a whale" or "John is a person."
var utterances = ["whale", "person"]

// The utterances are equally costly.
var utterancePrior = function() {
  categorical([1,1], utterances)
}

// The features of John being considered are "large", "graceful",
// "majestic." Features are binary.
var featureSets = [
  {large : 1, graceful : 1, majestic : 1},
  {large : 1, graceful : 1, majestic : 0},
  {large : 1, graceful : 0, majestic : 1},
  {large : 1, graceful : 0, majestic : 0},
  {large : 0, graceful : 1, majestic : 1},
  {large : 0, graceful : 1, majestic : 0},
  {large : 0, graceful : 0, majestic : 1},
  {large : 0, graceful : 0, majestic : 0}
]

// information about feature priors (probabilistic world knowledge)
// obtained by an experimental study (see paper)
var featureSetPrior = function(category) {
  category === "whale" ? categorical([0.30592786494628, 0.138078454222818,
                                      0.179114768847673, 0.13098781834847,
                                      0.0947267162507846, 0.0531420411185539,
                                      0.0601520520596695, 0.0378702842057509],
                                     featureSets) :
  category === "person" ? categorical([0.11687632453038, 0.105787535267869,
                                       0.11568145784997, 0.130847056136141,
                                       0.15288225956497, 0.128098151176801,
                                       0.114694702836614, 0.135132512637255],
                                      featureSets) :
  true
}

featureSetPrior("whale")
//featureSetPrior("person")
//marginalize(Infer(function() {featureSetPrior("whale")}),"large")
~~~~

The categories in this model that describe John in the actual state of the world are “whale” and “person”. The categoriesPrior function assigns categorical distribution to each category. The category “whale” is assigned 0.01, which means that it is very unlikely that John is a literal whale. Conversely, “person” is assigned 0.99, which means that it is almost certain that John is a person. As for the utterances, the speaker can either say John is a “whale” or a “person”. The utterancePrior function places a categorical over the utterances as 1 to 1 respectively, making both equally likely to be chosen.


The featureSets are the possible states of the world, in which the possible features attributed to John are "large", "graceful", and "majestic". The truth values of each feature in the set are rated with a binary value: 1 = True and 0 = False.

featureSetPrior corresponds to a categorical list of probabilities to the featureSets, depending on the given category. The numbers within the categorical in the featureSetPrior correspond to likelihood of each set. These numbers come from the “Kao et al.” study. Running the function on its own for either category will generate a random set according to the probability of that set being called (according to the categorical list).
This function is structured as an if/then statement, similar to those in Ch.2.1 in the Scalar Implicature model, which uses an if/then statement in its cost function. In that function, it checks its utterances one by one and assigns the cost according to the utterance that matches. However, in our function, a categorical is used to apply probabilities to another variable, depending on the category that matches.

~~~~
// Speaker's possible goals are to communicate feature 1, 2, or 3
var goals = ["large", "graceful", "majestic"]

// Prior probability of speaker's goal is set to uniform but can
// change with context/QUD.
var goalPrior = function() {
  categorical([1,1,1], goals)
}

goalPrior()
~~~~

The Speaker's possible goals, i.e. the question under discussion (QUD), are communicating something about John, that he is either "large", "graceful", or "majestic".
The goalPrior will just generate any of the 3 goals at random because they are all equally likely, based on the probabilities in the categorical list.

~~~~
// John could either be a whale or a person.
var categories = ["whale", "person"]

// The speaker could either say "John is a whale" or "John is a person."
var utterances = ["whale", "person"]

// information about feature priors (probabilistic world knowledge)
// obtained by an experimental study (see paper)
var featureSetPrior = function(category) {
  category === "whale" ? categorical([0.30592786494628, 0.138078454222818,
                                      0.179114768847673, 0.13098781834847,
                                      0.0947267162507846, 0.0531420411185539,
                                      0.0601520520596695, 0.0378702842057509],
                                     featureSets) :
  category === "person" ? categorical([0.11687632453038, 0.105787535267869,
                                       0.11568145784997, 0.130847056136141,
                                       0.15288225956497, 0.128098151176801,
                                       0.114694702836614, 0.135132512637255],
                                      featureSets) :
  true
}

// Check if interpreted category is identical to utterance
var literalInterpretation = function(utterance, category) {
  utterance === category
}

literalInterpretation("whale","whale")
// literalInterpretation("whale", "person")
~~~~

The literalInterpretation function checks if the utterance and category are the same. If so, it returns True, if not it returns False.This will come into play in the LiteralListener, where the condition statement only lets the utterance pass if the function returns True (ie that the utt. = cat.)

~~~~
// The features of John being considered are "large", "graceful",
// "majestic." Features are binary.
var featureSets = [
  {large : 1, graceful : 1, majestic : 1},
  {large : 1, graceful : 1, majestic : 0},
  {large : 1, graceful : 0, majestic : 1},
  {large : 1, graceful : 0, majestic : 0},
  {large : 0, graceful : 1, majestic : 1},
  {large : 0, graceful : 1, majestic : 0},
  {large : 0, graceful : 0, majestic : 1},
  {large : 0, graceful : 0, majestic : 0}
]

// information about feature priors (probabilistic world knowledge)
// obtained by an experimental study (see paper)
var featureSetPrior = function(category) {
  category === "whale" ? categorical([0.30592786494628, 0.138078454222818,
                                      0.179114768847673, 0.13098781834847,
                                      0.0947267162507846, 0.0531420411185539,
                                      0.0601520520596695, 0.0378702842057509],
                                     featureSets) :
  category === "person" ? categorical([0.11687632453038, 0.105787535267869,
                                       0.11568145784997, 0.130847056136141,
                                       0.15288225956497, 0.128098151176801,
                                       0.114694702836614, 0.135132512637255],
                                      featureSets) :
  true
}

// Speaker's possible goals are to communicate feature 1, 2, or 3
var goals = ["large", "graceful", "majestic"]

// Prior probability of speaker's goal is set to uniform but can
// change with context/QUD.
var goalPrior = function() {
  categorical([1,1,1], goals)
}

// Check if goal is satisfied
var goalState = function(goal, featureSet) {
  goal === "large" ? featureSet.large :
  goal === "graceful" ? featureSet.graceful :
  goal === "majestic" ? featureSet.majestic :
  true
}

goalState("large", {large : 0, graceful : 1, majestic : 1})
~~~~

goalState uses an if/then statement to check inside the featureSet for the goal given and returns what the binary value is. Essentially, it checks the answer to the QUD, returning if it's True or False.

~~~~
// The speaker could either say "John is a whale" or "John is a person."
var utterances = ["whale", "person"]

// John could either be a whale or a person.
var categories = ["whale", "person"]

// The features of John being considered are "large", "graceful",
// "majestic." Features are binary.
var featureSets = [
  {large : 1, graceful : 1, majestic : 1},
  {large : 1, graceful : 1, majestic : 0},
  {large : 1, graceful : 0, majestic : 1},
  {large : 1, graceful : 0, majestic : 0},
  {large : 0, graceful : 1, majestic : 1},
  {large : 0, graceful : 1, majestic : 0},
  {large : 0, graceful : 0, majestic : 1},
  {large : 0, graceful : 0, majestic : 0}
]

// information about feature priors (probabilistic world knowledge)
// obtained by an experimental study (see paper)
var featureSetPrior = function(category) {
  category === "whale" ? categorical([0.30592786494628, 0.138078454222818,
                                      0.179114768847673, 0.13098781834847,
                                      0.0947267162507846, 0.0531420411185539,
                                      0.0601520520596695, 0.0378702842057509],
                                     featureSets) :
  category === "person" ? categorical([0.11687632453038, 0.105787535267869,
                                       0.11568145784997, 0.130847056136141,
                                       0.15288225956497, 0.128098151176801,
                                       0.114694702836614, 0.135132512637255],
                                      featureSets) :
  true
}

// Check if interpreted category is identical to utterance
var literalInterpretation = function(utterance, category) {
  utterance === category
}

// Check if goal is satisfied
var goalState = function(goal, featureSet) {
  goal === "large" ? featureSet.large :
  goal === "graceful" ? featureSet.graceful :
  goal === "majestic" ? featureSet.majestic :
  true
}

//  Define a literal listener
var literalListener = function(utterance, goal) {
  Infer({model: function() {
    var category = uniformDraw(categories)
    var featureSet = featureSetPrior(category)
    condition(literalInterpretation(utterance, category))
    return goalState(goal, featureSet)
  }})
} 

literalListener("person", "graceful")
~~~~

The LiteralListener takes an utterance and goal, and infers the goalState, which returns the probability of the binary for the feature described in the goal. It does this by taking a uniformDraw of the categories and running that category into the featureSetPrior, which tells the likelihood of each featureSet, given that category. 
Given the condition that the literalInterpretation of the utterance matches the category, then it returns what the LiteralListener believes is the most probable answer to the QUD. It predicts the likelihood of a feature being the answer to the QUD by looking at the likelihood of that feature's binary value in the featureSets, based on the categorical weights in the Prior. The graph represents a probability distribution over answers to the QUD, which will resemble the marginal distribution of the featureSetPrior for that category and QUD (goal), where 1 = T and 0 = F.

Given the utterance "whale" and the goal "large,” the Literal Listener believes that the person being described by the utterance is very likely to be large (0.75:0.25). Given the utterance "person" and goal "large,” it's more likely to be F but it's a close call (0.53:0.47). 
In Ch. 1: Bayesian Inference Model, the Literal Listener just takes an utterance like ‘blue’ and returns the corresponding objects by mapping each utterance to a probability distribution over world states ‘blue square, ‘blue circle’ ‘green square,’ assuming a flat prior belief according to which each object is equally likely.

~~~~
// The speaker could either say "John is a whale" or "John is a person."
var utterances = ["whale", "person"]

// The utterances are equally costly.
var utterancePrior = function() {
  categorical([1,1], utterances)
}

// John could either be a whale or a person.
var categories = ["whale", "person"]

// The features of John being considered are "large", "graceful",
// "majestic." Features are binary.
var featureSets = [
  {large : 1, graceful : 1, majestic : 1},
  {large : 1, graceful : 1, majestic : 0},
  {large : 1, graceful : 0, majestic : 1},
  {large : 1, graceful : 0, majestic : 0},
  {large : 0, graceful : 1, majestic : 1},
  {large : 0, graceful : 1, majestic : 0},
  {large : 0, graceful : 0, majestic : 1},
  {large : 0, graceful : 0, majestic : 0}
]

// information about feature priors (probabilistic world knowledge)
// obtained by an experimental study (see paper)
var featureSetPrior = function(category) {
  category === "whale" ? categorical([0.30592786494628, 0.138078454222818,
                                      0.179114768847673, 0.13098781834847,
                                      0.0947267162507846, 0.0531420411185539,
                                      0.0601520520596695, 0.0378702842057509],
                                     featureSets) :
  category === "person" ? categorical([0.11687632453038, 0.105787535267869,
                                       0.11568145784997, 0.130847056136141,
                                       0.15288225956497, 0.128098151176801,
                                       0.114694702836614, 0.135132512637255],
                                      featureSets) :
  true
}

// Check if interpreted category is identical to utterance
var literalInterpretation = function(utterance, category) {
  utterance === category
}

// Speaker's possible goals are to communicate feature 1, 2, or 3
var goals = ["large", "graceful", "majestic"]

// Check if goal is satisfied
var goalState = function(goal, featureSet) {
  goal === "large" ? featureSet.large :
  goal === "graceful" ? featureSet.graceful :
  goal === "majestic" ? featureSet.majestic :
  true
}

//  Define a literal listener
var literalListener = function(utterance, goal) {
  Infer({model: function() {
    var category = uniformDraw(categories)
    var featureSet = featureSetPrior(category)
    condition(literalInterpretation(utterance, category))
    return goalState(goal, featureSet)
  }})
}

// Speaker optimality parameter
var alpha = 3

// Speaker model
var speaker = function(large, graceful, majestic, goal) {
  Infer({model: function() {
    var utterance = utterancePrior()
    factor(alpha *
           literalListener(utterance,goal).score(goalState(goal, {large : large, graceful : graceful, majestic : majestic})))
    return utterance
  }})
}

viz.table(speaker(1,1,1,"graceful"))
viz.table(speaker(0,1,1,"graceful"))
viz.table(speaker(1,0,1,"graceful"))
viz.table(speaker(1,1,0,"graceful"))
viz.table(speaker(0,0,1,"graceful"))
viz.table(speaker(0,1,0,"graceful"))
viz.table(speaker(1,0,0,"graceful"))
viz.table(speaker(0,0,0,"graceful"))
~~~~

Speaker function takes the binary values for each of the features and observes them independently, as well as taking in the goal, and returns the inference of the best utterance to communicate that goal to the LiteralListener. It does this by looking at the utterances in the utterancePrior, which is currently a categorical equivalent to a uniformDraw, then calculates the probability that the LiteralListener will arrive at the correct goal based on the informativity of the utterance, multiplied by the Speaker's optimality (alpha, set in this model at 3), which runs through factor to generate the probability of the utterance the Speaker will take.

If the Speaker's goal is "large", they are most likely to say "whale" when the feature set has {1} as the value for "large", and most likely to say "person" when the set has {0} for large. This is because the Literal Listener hearing "large" is very likely to believe that the feature is T for the utterance "whale". 

~~~~
// John could either be a whale or a person.
var categories = ["whale", "person"]

// It is extremely unlikely that John is actually a whale.
var categoriesPrior = function() {
  categorical([0.01, 0.99], categories)
}

// The speaker could either say "John is a whale" or "John is a person."
var utterances = ["whale", "person"]

// The utterances are equally costly.
var utterancePrior = function() {
  categorical([1,1], utterances)
}

// The features of John being considered are "large", "graceful",
// "majestic." Features are binary.
var featureSets = [
  {large : 1, graceful : 1, majestic : 1},
  {large : 1, graceful : 1, majestic : 0},
  {large : 1, graceful : 0, majestic : 1},
  {large : 1, graceful : 0, majestic : 0},
  {large : 0, graceful : 1, majestic : 1},
  {large : 0, graceful : 1, majestic : 0},
  {large : 0, graceful : 0, majestic : 1},
  {large : 0, graceful : 0, majestic : 0}
]

// information about feature priors (probabilistic world knowledge)
// obtained by an experimental study (see paper)
var featureSetPrior = function(category) {
  category === "whale" ? categorical([0.30592786494628, 0.138078454222818,
                                      0.179114768847673, 0.13098781834847,
                                      0.0947267162507846, 0.0531420411185539,
                                      0.0601520520596695, 0.0378702842057509],
                                     featureSets) :
  category === "person" ? categorical([0.11687632453038, 0.105787535267869,
                                       0.11568145784997, 0.130847056136141,
                                       0.15288225956497, 0.128098151176801,
                                       0.114694702836614, 0.135132512637255],
                                      featureSets) :
  true
}

// Speaker's possible goals are to communicate feature 1, 2, or 3
var goals = ["large", "graceful", "majestic"]

// Prior probability of speaker's goal is set to uniform but can
// change with context/QUD.
var goalPrior = function() {
  categorical([1,1,1], goals)
}

// Speaker optimality parameter
var alpha = 3

// Check if interpreted category is identical to utterance
var literalInterpretation = function(utterance, category) {
  utterance === category
}

// Check if goal is satisfied
var goalState = function(goal, featureSet) {
  goal === "large" ? featureSet.large :
  goal === "graceful" ? featureSet.graceful :
  goal === "majestic" ? featureSet.majestic :
  true
}

//  Define a literal listener
var literalListener = function(utterance, goal) {
  Infer({model: function() {
    var category = uniformDraw(categories)
    var featureSet = featureSetPrior(category)
    condition(literalInterpretation(utterance, category))
    return goalState(goal, featureSet)
  }})
}         

// Speaker model
var speaker = function(large, graceful, majestic, goal) {
  Infer({model: function() {
    var utterance = utterancePrior()
    factor(alpha *
           literalListener(utterance,goal).score(goalState(goal, {large : large, graceful : graceful, majestic : majestic})))
    return utterance
  }})
}

// Define a pragmatic listener
var pragmaticListener = function(utterance) {
  Infer({model: function() {
    var category = categoriesPrior()
    var featureSet = featureSetPrior(category)
    var large = featureSet.large
    var graceful = featureSet.graceful
    var majestic = featureSet.majestic
    var goal = goalPrior()
    observe(speaker(large, graceful, majestic, goal), utterance)
    return {category, large, graceful, majestic}
  }})
}

viz.table(pragmaticListener("whale"))
~~~~

The PragmaticListener hears an utterance and infers the probability of either category (whale or person) as well as the probability of the corresponding features intended to describe it. It does this by getting a category from the categoriesPrior, which heavily favors "person", then goes into the featureSet via the featureSetPrior for that category and check the probabilities for each feature individually. It also selects a goal from the goalPrior, in which the categorical is evenly weighted so the draw is random, and then observes all possibilities for the Speaker over the given utterance.

When the PragmaticListener hears "whale", it concludes that the Speaker meant "a person who is large, graceful, and majestic". The most likely feature to be the answer to the QUD is "majestic", which appears in the top 3 most likely sets, given the category of "person". It is also shown that the PragmaticListener assumes that it's more likely that the Speaker is trying to communicate a state of the world where the person has at least 2 of the features. The only feature likely to appear on its own is "large", appearing 4th most likely.

If the PragmaticListener hears "person", they conclude that the Speaker is describing "a person who isn't large, graceful, or majestic". They deem the possibility of the feature "large" to be most unlikely given the category "person", and "graceful" to be slightly more probable than "majestic".
Interestingly, if they assume the category is "whale", given the utterance "person", then the most likely featureSet is {1,1,1}, implying that the utterance "person", if used to describe a whale, would mean that the whale is "large, graceful, and majestic".

So what happens when you start to change things? 

~~~~
// John could either be a whale or a person.
var categories = ["whale", "person"]

// It is extremely unlikely that John is actually a whale.
var categoriesPrior = function() {
  categorical([0.01, 0.99], categories)
}

// The speaker could either say "John is a whale" or "John is a person."
var utterances = ["whale", "person"]

// The utterances are equally costly.
var utterancePrior = function() {
  categorical([1,1], utterances)
}

// The features of John being considered are "large", "graceful",
// "majestic." Features are binary.
var featureSets = [
  {large : 1, graceful : 1, majestic : 1},
  {large : 1, graceful : 1, majestic : 0},
  {large : 1, graceful : 0, majestic : 1},
  {large : 1, graceful : 0, majestic : 0},
  {large : 0, graceful : 1, majestic : 1},
  {large : 0, graceful : 1, majestic : 0},
  {large : 0, graceful : 0, majestic : 1},
  {large : 0, graceful : 0, majestic : 0}
]

// information about feature priors (probabilistic world knowledge)
// obtained by an experimental study (see paper)
var featureSetPrior = function(category) {
  category === "whale" ? categorical([0.30592786494628, 0.138078454222818,
                                      0.179114768847673, 0.13098781834847,
                                      0.0947267162507846, 0.0531420411185539,
                                      0.0601520520596695, 0.0378702842057509],
                                     featureSets) :
  category === "person" ? categorical([0.11687632453038, 0.105787535267869,
                                       0.11568145784997, 0.130847056136141,
                                       0.15288225956497, 0.128098151176801,
                                       0.114694702836614, 0.135132512637255],
                                      featureSets) :
  true
}

// Speaker's possible goals are to communicate feature 1, 2, or 3
var goals = ["large", "graceful", "majestic"]

//// Prior probability of speaker's goal is set to uniform but can
//// change with context/QUD.
var goalPrior = function() {
  categorical([5,1,1], goals)
}
// var goalPrior = function() {
//   categorical([1,5,1], goals)
// }
// var goalPrior = function() {
//   categorical([1,1,5], goals)
// }

// Speaker optimality parameter
var alpha = 3

// Check if interpreted category is identical to utterance
var literalInterpretation = function(utterance, category) {
  utterance === category
}

// Check if goal is satisfied
var goalState = function(goal, featureSet) {
  goal === "large" ? featureSet.large :
  goal === "graceful" ? featureSet.graceful :
  goal === "majestic" ? featureSet.majestic :
  true
}

//  Define a literal listener
var literalListener = function(utterance, goal) {
  Infer({model: function() {
    var category = uniformDraw(categories)
    var featureSet = featureSetPrior(category)
    condition(literalInterpretation(utterance, category))
    return goalState(goal, featureSet)
  }})
}         

// Speaker model
var speaker = function(large, graceful, majestic, goal) {
  Infer({model: function() {
    var utterance = utterancePrior()
    factor(alpha *
           literalListener(utterance,goal).score(goalState(goal, {large : large, graceful : graceful, majestic : majestic})))
    return utterance
  }})
}

// Define a pragmatic listener
var pragmaticListener = function(utterance) {
  Infer({model: function() {
    var category = categoriesPrior()
    var featureSet = featureSetPrior(category)
    var large = featureSet.large
    var graceful = featureSet.graceful
    var majestic = featureSet.majestic
    var goal = goalPrior()
    observe(speaker(large, graceful, majestic, goal), utterance)
    return {category, large, graceful, majestic}
  }})
}

viz.table(pragmaticListener("whale"))
~~~~

When you change the weight of the categorical values in the goalPrior, the following changes can be witnessed:
Putting more weight onto "large" makes the PragmaticListener more likely to believe that "large" is going to appear in the featureSet the Speaker is referring to, ie. that "large" is likely to be accurate of the state of the world. Therefore, in the model's predictions, the featureSets where "large" has the value 1 have higher probability. This is likewise for the other goals as well, where the featureSets containing the goal receiving the most weight are prioritized as most probable.

~~~~
// The speaker could either say "John is a whale" or "John is a person."
var utterances = ["whale", "person"]

// The utterances are equally costly.
var utterancePrior = function() {
  categorical([1,1], utterances)
}

// John could either be a whale or a person.
var categories = ["whale", "person"]

// The features of John being considered are "large", "graceful",
// "majestic." Features are binary.
var featureSets = [
  {large : 1, graceful : 1, majestic : 1},
  {large : 1, graceful : 1, majestic : 0},
  {large : 1, graceful : 0, majestic : 1},
  {large : 1, graceful : 0, majestic : 0},
  {large : 0, graceful : 1, majestic : 1},
  {large : 0, graceful : 1, majestic : 0},
  {large : 0, graceful : 0, majestic : 1},
  {large : 0, graceful : 0, majestic : 0}
]

// information about feature priors (probabilistic world knowledge)
// obtained by an experimental study (see paper)
var featureSetPrior = function(category) {
  category === "whale" ? categorical([0.30592786494628, 0.138078454222818,
                                      0.179114768847673, 0.13098781834847,
                                      0.0947267162507846, 0.0531420411185539,
                                      0.0601520520596695, 0.0378702842057509],
                                     featureSets) :
  category === "person" ? categorical([0.11687632453038, 0.105787535267869,
                                       0.11568145784997, 0.130847056136141,
                                       0.15288225956497, 0.128098151176801,
                                       0.114694702836614, 0.135132512637255],
                                      featureSets) :
  true
}

// Check if interpreted category is identical to utterance
var literalInterpretation = function(utterance, category) {
  utterance === category
}

// Speaker's possible goals are to communicate feature 1, 2, or 3
var goals = ["large", "graceful", "majestic"]

// Check if goal is satisfied
var goalState = function(goal, featureSet) {
  goal === "large" ? featureSet.large :
  goal === "graceful" ? featureSet.graceful :
  goal === "majestic" ? featureSet.majestic :
  true
}

//  Define a literal listener
var literalListener = function(utterance, goal) {
  Infer({model: function() {
    var category = uniformDraw(categories)
    var featureSet = featureSetPrior(category)
    condition(literalInterpretation(utterance, category))
    return goalState(goal, featureSet)
  }})
}

// Speaker optimality parameter
var alpha = 3

//Adding a cost
var cost = function(utterance){
  utterance == "whale" ? 2 :
  utterance == "person" ? 1 :
  0
}

// Speaker model
var speaker = function(large, graceful, majestic, goal) {
  Infer({model: function() {
    var utterance = utterancePrior()
    factor(alpha *
           (literalListener(utterance,goal).score(goalState(goal, {large : large, graceful : graceful, majestic : majestic}))) - cost(utterance))
    return utterance
  }})
}

viz.table(speaker(1,1,1,"graceful"))
viz.table(speaker(0,1,1,"graceful"))
viz.table(speaker(1,0,1,"graceful"))
viz.table(speaker(1,1,0,"graceful"))
viz.table(speaker(0,0,1,"graceful"))
viz.table(speaker(0,1,0,"graceful"))
viz.table(speaker(1,0,0,"graceful"))
viz.table(speaker(0,0,0,"graceful"))
~~~~

In the original model, the cost for the Speaker is not included, meaning it's implicitly set at 0. But what if there was cost associated with the possible utterances?
In our alterations, the cost of speech for "person" was noted as 1, while "whale" was 2 because the metaphorical use is more of a stretch in meaning. Having the cost of "person" be less resulted in a good amount of probability being taken away from saying "whale", and in some cases, even adding enough weight to make "person" more probable than "whale".

Overall, we did not find this model and its predictions to match our intuitions. When someone is referred to as “whale”, our assumption is that it would be meant as an insult, and that the most likely goal (QUD) would be “large”. However, we see in this model that it leans towards a more positive interpretation of the utterance, associating it with features like “graceful” and “majestic”. The ultimate conclusion of the Pragmatic Listener is that John is likely to be all three of these features, which we can’t say aligns with our own intuitive conclusions.