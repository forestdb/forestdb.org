---
layout: model
title: Ashqar, Sizemore, Telahun, Torres - Metaphor
model-language: webppl
---

The metaphor model uses non-literal language and deals with reasoning through uncertainty about what the speaker is trying to communicate. It describes a situation in which a speaker says “John is a whale”, and the listener has to reason if the speaker is saying that John is actually a whale or if he has qualities of a whale. The model implements this reasoning process by aligning utterances (e.g., “whale”, “person”) with stereotypical features (large, majestic, graceful), then introducing uncertainty about which feature is currently the topic of conversation.  
The metaphor model has similarities to other models like the irony model and the hyperbole model. All three of these models deal with a situation in which non-literal language is being used, and there is an exaggeration about the true state of the world. For example, the hyperbole model describes a situation in which the speaker expresses frustration for how much they spent on a cup of coffee when they say that they paid “a thousand dollars” for it. In both the metaphor model John is not actually a whale and a thousand dollars was not spent on a cup of coffee. Both of these utterances by the speakers were an exaggeration on the true state of the world. 
In addition, the same exaggeration occurs with the irony model. The irony model depicts a situation in which the speaker uses sarcasm in response to the true state of the world, which is that the weather is nice. The speaker states that the weather is “terrible” but that is not the actual state of the world. The irony model is similar to the metaphor model because in both situations, non-literal language is used within the conversation. All three of these models require reasoning through uncertainty on what the speaker is trying to communicate about the state of the world. 
The literal interpretation function in the metaphor model is utterance = category. There are similar functions in the hyperbole (meaning function) and the irony model (literal Interpretation).
Some differences to highlight between the metaphor model and the other non-literal language models would be the difference in the situation in which the communication happens. For example, although all three models use non-literal language, the metaphor model is the only one in which a person is being spoken about. The other two models have to do with an exaggeration of a situation or object, like the cost of a cup of coffee or the weather. 
Another difference to highlight would be the comparison between the metaphor model and the social reasoning model. The social reasoning model depicts a situation in which a person’s date baked them cookies, and the speaker tries to be polite in describing how they taste even though they did not taste good. The speaker uses a white lie to describe the taste of the cookies their date made them. This makes the social reasoning model less efficient because the speaker is not conveying their true feelings or the actual state of the world, but is instead choosing to be polite about how they actually feel. This is different from the metaphor model because although John is being referred to as a whale, information is still being conveyed and that is that John has qualities of a whale. The speaker is the metaphor model for not being polite to spare the feelings of another person. 
To best understand the elements involved in creating a functional model that captures this ability to reason about non-literal language a determine a speaker's goal, the code of this model has been broken down into small chunks so that they may be discussed without being overwhelming. The first blocks of code involve establishing the possible categories John could be. The model contains two categories: whale and person. This means that John could be either a whale or a person. The category priors show the prior probabilities of each of these categories. Since it is highly unlikely that John is a whale, the prior for “whale” is 0.01, or only 1%, while it is 0.99, or 99%, for “person”. 

~~~~
// John could either be a whale or a person.
var categories = ["whale", "person"]

// It is extremely unlikely that John is actually a whale.
var categoriesPrior = function() {
  categorical([0.01, 0.99], categories)
}
~~~~

After establishing what categories exist in the model, one must define what utterances can be used to describe those categories. For this model, there are two possible utterances: “whale” and “person”. This means that the speaker could either say “John is a whale” or “John is a person”. The utterancePrior for each of these utterances is set to 1, showing that both utterances are equally costly for the speaker to produce. 

~~~~
// The speaker could either say "John is a whale" or "John is a person."
var utterances = ["whale", "person"]

// The utterances are equally costly.
var utterancePrior = function() {
  categorical([1,1], utterances)
}
~~~~

Since one is trying to associated John with certain qualities using these utterances, these qualities must be defined and associated with a probability. The featureSets are all the possible combinations of features that might describe John. The features are binary, meaning they are given a value of 1 or 0. If the John has said feature, then it is given the value 1, or true. If he does not have the feature, then it is 0, or false. In total there are eight sets, covering all combinations of features. 
The featureSetPrior shows the probability of each feature set. The featureSetPrior is category specific. The probabilities shown in the model are all taken from experimental data outlined in Kao et al. (2014).


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
~~~~

Next, the goal of the speaker, that is the feature or features of John the speaker is trying to share with the listener, must be defined. Since the goals are what the speaker is communicating in this model, they represent the QUDs, or question under discussion. In the model there are three goals: “Is John large?”, “Is John graceful?” and “Is John majestic?”.
These goals are then incorporated into the variable goalPrior. The goalPrior is the probability of each goal that the speaker is trying to communicate. The probabilities are all the same, seen in the model as all having a value of 1 in a categorical draw. Therefore, the speaker has an equal chance of communicating each feature.
Another variable that must be accounted for is the alpha. The alpha shows how optimal the speaker is speaking. If there is a higher alpha then the speaker is speaking more optimally. If alpha is lower, then the speaker is speaking less optimally. In the model, if the alpha is a higher value, then the speaker will choose the utterance that gives the most information about the three possible features.

~~~~
// Speaker's possible goals are to communicate feature 1, 2, or 3
var goals = ["large", "graceful", "majestic"]

// Prior probability of speaker's goal is set to uniform but can
// change with context/QUD.
var goalPrior = function() {
  categorical([1,1,1], goals)
}

// Speaker optimality parameter
var alpha = 3
~~~~

To function properly, the model must account for the meaning of the utterances apart from their possible metaphorical qualities. This is done by introducing the literalInterpretion function which states that the utterance is equal to the category. This means that a person is literally a person and a whale is literally a whale are equally good interpretations of the utterance. 
The final helper function that needs to be implemented is the goalState.The goalState says to check if the featureSet for a particular goal is true. For example, if the goal is ‘large’ then check if the feature set for large is true.

~~~~
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
~~~~

Now that all the basic variables and helper functions are defined, one can begin structuring the literal listener, the speaker, and the pragmatic listener using these building blocks. The first layer to impliment is the literal listener. The literal listener is a function that takes in an utterance and a goal. The function takes a uniformDraw of the categories: “whale” and “person”. Whichever category was returned from the uniformDraw is used as the category for the featureSetPrior. The condition sees if a literal interpretation is possible to decide if it is literal or not literal. The literal listener returns goalState, which takes in a goal and a featureSet. If the feature set of the goal is true, then that is what the speaker wants to communicate, and returns a value of 1. The model shows the probability of the goal state being 0 or 1 based on the probabilities associated with that feature based on the category from the featureSetPrior.  

~~~~
//  Define a literal listener
var literalListener = function(utterance, goal) {
  Infer({model: function() {
    var category = uniformDraw(categories)
    var featureSet = featureSetPrior(category)
    condition(literalInterpretation(utterance, category))
    return goalState(goal, featureSet)
  }})
}
~~~~

The next layer added is the speaker layer. The speaker is a function that takes in a value of 0 or 1 for each goal, and then the name of one of the goals. An utterance is chosen from the utterancePrior, which is equally likely to choose either utterance, based on the categorical draw with equal values for each utterance. The alpha value then optimizes this utterance. The literal listener is run using an utterance from the utterance prior and the goal that was input, and it is scored against a goalState, which replaces the featureSetPrior in the literal listener function with the feature set input into the speaker model. The function returns an utterance, and the model returns the probability of each utterance being used.

~~~~
// Speaker model
var speaker = function(large, graceful, majestic, goal) {
  Infer({model: function() {
    var utterance = utterancePrior()
    factor(alpha *
           literalListener(utterance,goal).score(goalState(goal, {large : large, graceful : graceful, majestic : majestic})))
    return utterance
  }})
}
~~~~

Finally, the pragmatic listener is added to complete the model. The pragmatic listener is a function that just takes in an utterance. It then selects a category from the categoriesPrior and pulls the feature set for the utterance based on the featureSetPrior of the category selected from the categoriesPrior. Then the variables for large, graceful, and majestic look inside the feature set to see if it returns a 0 or 1 for any of those goals and a goal is selected from the goal prior. The pragmatic listener then observes the speaker model for the value of large, graceful, and majestic found in the featureSet, the goal from the goalPrior, and the utterance that was inputted into the function. It returns a category, and value of either 0 or 1 for large, graceful, or majestic. Since it is wrapped in an infer function, it then returns a probability for each feature set in both categories as infer runs all possibilities. The pragmatic listener is therefore inferring the probability that a feature set contains the goals for each category influenced by the utterance that the speaker chose.

~~~~
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
~~~~

Using this completed model, one can utilize it to predicate the behavior of the literal listener in comparison to the behavior of the pragmatic listener. As the literal listener has no way of inferring the goal the speaker is trying to communicate from the utterance alone, the literal listener function takes in both an utterance and the goal of the speaker. Using these elements, one can use viz.table to visualize what predictions the literal listener returns when given each utterance ("whale" and "person") and each goal ("large", "graceful", and "majestic").. 

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

display("The literal listener's belief when the speaker says whale")
viz.table(literalListener("whale", "large"))
viz.table(literalListener("whale", "graceful"))
viz.table(literalListener("whale", "majestic"))

display("The literal listener's belief when the speaker says person")
viz.table(literalListener("person", "large"))
viz.table(literalListener("person", "graceful"))
viz.table(literalListener("person", "majestic"))
~~~~

When the literal listener is presented with the utterance “whale”, for all the possible goal states the literal listener is inclined to believe that the various goal states (large, graceful, and majestic) are true. For large, the probability the literal listener returns from adding all the associated probabilities for large in the whale category of the featureSetPrior is approximately 75% for large being a quality that John possesses. Graceful returns a probability of approximately 64% and majestic returns a probability of about 64%. 
When the literal listener is given the utterance “person”, the probabilities for John possessing any of the qualities associated with the goal states is lower than those for “whale” in accordance to the lower probabilities found in the featureSetPrior for the person category. For large, the higher probability is for John NOT being large at approximately 53%. Graceful and majestic still are more likely to be qualities that John has, but at a lower rate than when the utterance was whale, with the probabilities being 50.4% and 50.01% respectively. 
Instead of requiring the combination of an utterance and a goal to return a useful prediction as the literal listener does, the pragmatic speaker function only needs to be given an utterance from the speaker to run. This means that the pragmatic speaker has the extra element of inferring what the goal state the speaker intends to communicate using only the utterance. Similar to above, using viz.table can be used to best visualize the predictions that the pragmatic listener makes when given both possible utterances. 

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

display("The pragmatic listener's interpretation when the speaker says whale")
viz.table(pragmaticListener("whale"))

display("The pragmatic listener's interpretation when the speaker says person")
viz.table(pragmaticListener("person"))
~~~~

For the utterance “whale”, due to the pragmatic speaker consulting the the categoryPrior and using its categorical draw to determine how likely it is that John is literally a whale, the high probability associated with John being a person results in the model predicting that John is likely to be a person even though that is not the literal interpretation of the utterance given. From there the pragmatic listener must determine why the speaker would use the non-literal utterance of "whale" to describe John to communicate a certain goal state. As seen in the predictions for the literal listener, whale results in higher probabilities for the various goal states being true of John. The pragmatic listener can then reason that the speaker is aware of this, and would use the utterance "whale" to indicate one of these goal states. This leads to the pragmatic listener putting higher probabilities on states where John is a person and possesses the majority of the possible features. 
When the speaker says "person", this same consulting of the categoryPrior and comparing the probabilities associated with the goal states by the pragmatic listener occurs. Based on the high probability of John being a person as presented in the categoryPrior combined with the low probabilities for the literal listener predicting each feature is true as observed by the speaker, the pragmatic speaker infers that the speaker's most probable goal was not to imply John possesses a certain state, but instead that "person" is being used as a literal description of John. This leads to the highest probability state being that John is a person who possesses none of the three possible features. 
By using an RSA framework, one can create an accurate model of how metaphors could be interpreted and used to communicate useful information about a subject. Specifically, the metaphor model uses non-literal language and deals with reasoning through uncertainty about what the speaker is trying to communicate. Goals, like QUDs, convey the purpose and meaning behind an utterance, with the probability of the possible goals associated with the utterances being decided by what category they belonged to and prior research. While the literal listener can only respond to the probability that an utterance is literal and reflects a certain goal, the pragmatic listener is able to use the speaker's reasoning about what the literal listener believes to infer a deeper meaning about the non-literal nature of an utterance and it's usefulness in answering the possible goals.