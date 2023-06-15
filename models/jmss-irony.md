---
layout: model
title: Jin, Mai, Saavedra, Syracuse - Irony
model-language: webppl
---


# Irony
Probabalistic Language Understanding  
Spring 2023  
- Ky-Vinh Mai  
- Soleil Saavedra  
- Jordan Jin  
- Shannon Syracuse

## Empirical Phenomenon of Interest
Built on top of the hyperbole model, the Irony Model accounts for a situation where the speaker's intended meaning is the opposite of the utterance’s literal interpretation. In other words, the literal meaning of an utterance does not match the speaker's intended meaning. This violates the Gricean Principle's maxim of quality, which refers to remaining truthful in conversation. One would imagine that this complication would confuse the listener. While it may initially, this model explores how it provides insight into the real feelings had by the speaker. The irony model is unique in how it approaches non-literal interpretations, despite still utilizing the Question-Under-Discussion (QUD), state of the world, and valence, all of which are pre-established in the hyperbole model. Its uniqueness is contributed to a new factor: arousal, which measures how intensely a speaker feels about a state.

## Modeling Approach Taken
The Rational Speech Act framework views communication as reasoning between a speaker and a listener. There are three levels of inference, the most sophisticated being the pragmatic listener. The pragmatic listener interprets an utterance given by the speaker to a naive listener about some state of the world. This pragmatic listener uses Bayesian inference to reason what state of the world the speaker is trying to convey to the literal listener given that this speaker is reasoning about how the literal listener will interpret that utterance. The speaker infers the state of the world and chooses to produce an utterance for the literal listener, maximizing the probability that the literal listener will correctly infer the state of the world. The literal listener only has its priors and the utterance of the speaker to infer the state of the world.

## Irony Model
### States
In this model, we will be using California weather to demonstrate ironic statements. We begin by observing the ```statePriors``` associated with this model: 

~~~~
// There are three possible states of the weather : 
// terrible, ok, or amazing
var states = ['terrible', 'ok', 'amazing']

// Since we are in California, the prior over these states
// are the following. One could also imagine this being 
// the prior in a certain context, e.g. when it's clearly
// sunny and nice out.
var statePrior = function() {
  categorical([1, 50, 50], states)
}

display("State prior")
viz(Infer(statePrior))
~~~~

### Utterances
The utterances in this model are the same as the states. UtterancePrior uses uniformDraw to take an equal probability sample from the utterances in the function. The previous hyperbole model also used uniformDraw to sample from it's utterances, but the utterances in that model matched to a price rather than a state of the world. 

~~~~
// Assume possible utterances are identical to possible states
var utterances = states

var utterancePrior = function() {
  uniformDraw(utterances)
}
~~~~

### Valence
Once we have the states of the world and the ```statePrior```, we can move on to the ```valencePrior```. The probabilities associated with the coin flip correspond with the probability that the speaker will be upset, having a negative valence (-1). When the coin flip doesn't result in -1, it will return 1, which represents the speaker having positive valence.
For the state ```"terrible"```, the probability is high (.99) that the speaker is upset about the weather. If the state is ```"ok"```, the probability is (.5), meaning the speaker has an equal chance of being upset and not being upset. If the state is "amazing", it is unlikely (.01) that the speaker will be upset. 

~~~~
var valencePrior = function(state) {
  state === "terrible" ? flip(0.99) ? -1 : 1 :
  state === "ok" ? flip(0.5) ? -1 : 1 :
  state === "amazing" ? flip(0.01) ? -1 : 1 :
  true
}

display("Valence prior for terrible state")
viz(Infer(function(){valencePrior("terrible")}))
display("\nValence prior for ok state")
viz(Infer(function(){valencePrior("ok")}))
display("\nValence prior for amazing state")
viz(Infer(function(){valencePrior("amazing")}))
~~~~

Up until this point, these components have matched the features present in the hyperbole model.
 
### Arousal
Now, we focus on the new piece to this puzzle, which contributes to the interpretation of ironic statements. 
Arousal is recorded in the code as ```"low"``` or ```"high"```. The lower the arousal, the less intensely the speaker feels about the weather.

~~~~
// Define binary arousals (could model as continuous).
var arousals = ["low", "high"]
~~~~

```arousalPrior``` is not a function that we have seen in previous models. As explained before, Arousal means how passionate or how strongly someone feels about the state of the weather, which is different from Valence. Valence is how upset the speaker feels about the weather (or the speakers attitude toward the state of the world). The categorical draw here in this function serves to choose whether the arousal will be low or high. In the states ```"terrible"``` and ```"amazing"```, the probability of high arousal is 0.9 (or 90%) while the probability for low is on 0.1 (or 10%). The state for ```"ok"``` has a 90% probability for low arousal with the 10% for high arousal. This function is intuitive because you can guess that the highest arousals would be for the utterances ```"amazing"``` and ```"terrible"``` since those are words we would choose to use to describe something we feel strongly about over a word like ```"ok"```.

~~~~
// Sample arousal given a state.
var arousalPrior = function(state) {
  state === "terrible" ? categorical([0.1, 0.9], arousals) :  
  state === "ok" ? categorical([0.9, 0.1], arousals) :
  state === "amazing" ? categorical([0.1, 0.9], arousals) :
  true
}
~~~~

### Goals
After establishing these features, we assess the QUD, which is also present in the hyperbole model. In this case, the possible options are categorized as ```goals```, serving as the communicative goal of the speaker's statement. What is the speaker trying to convey? Either the state of California weather, valence, or arousal.  
Here we are using a categorical draw with equal probability over the goals, but if we wanted to, we can manually change the probability to make one more probable than others. Each number in the categorical line matches to either ```"goalState"```, ```"goalValence"```, or ```"goalArousal"```.  

~~~~
// Define goals and goal priors. Could want to communicate the state of the world,    
// valence about it, or arousal (intensity of feeling) about it.
var goals = ["goalState", "goalValence", "goalArousal"]

var goalPrior = function() {
  categorical([1, 1, 1], goals)
}
~~~~

### Literal Interpretation
The ```literalInterpretation``` function takes the utterance and the state of the world as arguments, and if they are the same, then the function is true. I have run the function with the ```"amazing"``` utterance and state, and when ran, it returns true. 

~~~~
var literalInterpretation = function(utterance, state) {
  utterance === state
}

literalInterpretation("amazing", "amazing")
~~~~

If you were to change either the state or the utterance to something else like ```"ok"```, then it would return false. 

~~~~
var literalInterpretation = function(utterance, state) {
  utterance === state
}

literalInterpretation("amazing", "ok")
~~~~

### Goal State
In our model, ```goalState``` is a function that takes arguments ```(goal, state, valence, arousal)``` and can return either state, valence or arousal depending on what argument is given to the goal.

~~~~
var goalState = function(goal, state, valence, arousal) {
  goal === "goalState" ? state :
  goal === "goalValence" ? valence :
  goal === "goalArousal" ? arousal :
  true
}
~~~~

### Literal Listener
Similarly to the literal listener in the hyperbole model in Chapter 3 of the textbook, the literal listener in this irony model "updates [the aforementioned] prior belief distributions by conditioning on the literal meaning of the utterance. The [goal] determines which kind of distribution will be returned" (Scontras et al.). The main difference between the two models is the additional component of arousal in the irony model.

~~~~
// Define a literal listener
var literalListener = function(utterance, goal) {
  Infer({model: function(){
    var state = uniformDraw(states)
    var valence = valencePrior(state)
    var arousal = arousalPrior(state)
    condition(literalInterpretation(utterance,state))
    return goalState(goal, state, valence, arousal)
  }})
}
~~~~

### Speaker
The speaker then produces the correct utterance it believes would best allow the literal listener to infer about the correct and relevant information the speaker is trying to convey. This is done through multiple combinations of selecting utterances, but returning the utterance which best matches the literal listener's interpretation.

~~~~
// Define a speaker
var speaker = function(state, valence, arousal, goal) {
  Infer({model: function(){
    var utterance = utterancePrior()
    factor(1 * literalListener(utterance, 
                    goal).score(goalState(goal, 
                                          state, 
                                          valence, 
                                          arousal)))
    return utterance
  }})
}
~~~~

### Pragmatic Listener
To model ironic expressions, the pragmatic listener accounts for the additional information dimension of arousal. Different speaker posteriors are simulated given different combinations of full-states (state, valence, arousal) and goal (QUD). Then, those posteriors are used to infer the informativity of each full-state combination while accounting for the full-state priors which then result in the pragmatic listener posterior.

~~~~
// Define a pragmatic listener
var pragmaticListener = function(utterance) {
  Infer({model: function(){
    var state = statePrior()
    var valence = valencePrior(state)
    var arousal = arousalPrior(state)
    var goal = goalPrior()
    observe(speaker(state, valence, arousal, goal),utterance)
    return {state, valence, arousal}
  }})
}
~~~~

### Full Model
Here is the full model:

~~~~
// There are three possible states the weather could be in: 
// terrible, ok, or amazing
var states = ['terrible', 'ok', 'amazing']

// Since we are in California, the prior over these states
// are the following. Once could also imagine this being 
// the prior in a certain context, e.g. when it's clearly
// sunny and nice out.
var statePrior = function() {
  categorical([1, 50, 50], states)
}

// Valence prior defined in terms of negative valence. 
// If the current state is terrible, it's extremely likely
// that the valence associated is negative. If it's ok, then 
// the valence could be negative or positive with equal 
// probability.
var valencePrior = function(state) {
  state === "terrible" ? flip(0.99) ? -1 : 1 :
  state === "ok" ? flip(0.5) ? -1 : 1 :
  state === "amazing" ? flip(0.01) ? -1 : 1 :
  true
}

// Define binary arousals (could model as continuous).
var arousals = ["low", "high"]

// Define goals and goal priors. Could want to communicate state of the world,    
// valence about it, or arousal (intensity of feeling) about it.
var goals = ["goalState", "goalValence", "goalArousal"]

var goalPrior = function() {
  categorical([1, 1, 1], goals)
}

// Assume possible utterances are identical to possible states
var utterances = states

// Assume cost of utterances is uniform.
var utterancePrior = function() {
  uniformDraw(utterances)
}

// Sample arousal given a state.
var arousalPrior = function(state) {
  state === "terrible" ? categorical([0.1, 0.9], arousals) :
  state === "ok" ? categorical([0.9, 0.1], arousals) :
  state === "amazing" ? categorical([0.1, 0.9], arousals) :
  true
}

// Literal interpretation is just whether utterance equals state
var literalInterpretation = function(utterance, state) {
  utterance === state
}

// A speaker's goal is satisfied if the listener infers the correct 
// and relevant information.
var goalState = function(goal, state, valence, arousal) {
  goal === "goalState" ? state :
  goal === "goalValence" ? valence :
  goal === "goalArousal" ? arousal :
  true
}

// Define a literal listener
var literalListener = function(utterance, goal) {
  Infer({model: function(){
    var state = uniformDraw(states)
    var valence = valencePrior(state)
    var arousal = arousalPrior(state)
    condition(literalInterpretation(utterance,state))
    return goalState(goal, state, valence, arousal)
  }})
}

// Define a speaker
var speaker = function(state, valence, arousal, goal) {
  Infer({model: function(){
    var utterance = utterancePrior()
    factor(1 * literalListener(utterance, 
                    goal).score(goalState(goal, 
                                          state, 
                                          valence, 
                                          arousal)))
    return utterance
  }})
}

// Define a pragmatic listener
var pragmaticListener = function(utterance) {
  Infer({model: function(){
    var state = statePrior()
    var valence = valencePrior(state)
    var arousal = arousalPrior(state)
    var goal = goalPrior()
    observe(speaker(state, valence, arousal, goal),utterance)
    return {state, valence, arousal}
  }})
}

display("pragmaticListener(\"terrible\")")
viz.table(pragmaticListener("terrible"))

// display("\n\nviz(marginalize(pragmaticListener(\"terrible\"), \"state\"))")
// viz(marginalize(pragmaticListener("terrible"), "state"))
// display("\nviz(marginalize(pragmaticListener(\"terrible\"), \"valence\"))")
// viz(marginalize(pragmaticListener("terrible"), "valence"))
// display("\nviz(marginalize(pragmaticListener(\"terrible\"), \"arousal\"))")
// viz(marginalize(pragmaticListener("terrible"), "arousal"))
~~~~

When the pragmatic listener hears ```"terrible"```, they account for the miniscule likelihood of ```"terrible"``` state, high likelihood of ```"amazing"``` state, and the high likelihood for ```"high"``` arousal common for both states. They then infer that the speaker most likely wants to convey how strongly they feel about the weather (```"goalArousal"```), the weather is actually ```"amazing"```, and the speaker has positive valence. This would be an ironic interpretation because the utterance ```"terrible"``` is the opposite of the inferred state ```"amazing"```.  
The two inferred full states of ```"ok"``` state, which have negative valence, and either ```"high"``` or ```"low"``` arousal are inferred to be the next most probable. These represent hyperbolic interpretations of the utterance ```"terrible"```. These two full states can be inferred by the pragmatic listener to be more probable in the model if we reduce the probability of ```"goalArousal"``` in the arousal prior, increase the probability of ```"goalValence"``` in the arousal prior, or remove arousal altogether.

### Arousal Removed
The previous model handled irony expressions by adding the dimension of arousal to the hyperbole model. The model below still simulates people talking about California weather, but has arousal removed. 

~~~~
// There are three possible states the weather could be in: 
// terrible, ok, or amazing
var states = ['terrible', 'ok', 'amazing']

// Since we are in California, the prior over these states
// are the following. One could also imagine this being 
// the prior in a certain context, e.g. when it's clearly
// sunny and nice out.
var statePrior = function() {
  categorical([1, 50, 50], states)
}

// Valence prior defined in terms of negative valence. 
// If the current state is terrible, it's extremely likely
// that the valence associated is negative. If it's ok, then 
// the valence could be negative or positive with equal 
// probability.
var valencePrior = function(state) {
  state === "terrible" ? flip(0.99) ? -1 : 1 :
  state === "ok" ? flip(0.5) ? -1 : 1 :
  state === "amazing" ? flip(0.01) ? -1 : 1 :
  true
}

// Define goals and goal priors. Could want to communicate state of the world,    
// valence about it.
var goals = ["goalState", "goalValence"]

var goalPrior = function() {
  categorical([1, 1], goals)
}

// Assume possible utterances are identical to possible states
var utterances = states

// Assume cost of utterances is uniform.
var utterancePrior = function() {
  uniformDraw(utterances)
}

// Literal interpretation is just whether utterance equals state
var literalInterpretation = function(utterance, state) {
  utterance === state
}

// A speaker's goal is satisfied if the listener infers the correct 
// and relevant information.
var goalState = function(goal, state, valence, arousal) {
  goal === "goalState" ? state :
  goal === "goalValence" ? valence :
  true
}

// Define a literal listener
var literalListener = function(utterance, goal) {
  Infer({model: function(){
    var state = uniformDraw(states)
    var valence = valencePrior(state)
    condition(literalInterpretation(utterance,state))
    return goalState(goal, state, valence)
  }})
}

// Define a speaker
var speaker = function(state, valence, goal) {
  Infer({model: function(){
    var utterance = utterancePrior()
    factor(1 * literalListener(utterance, 
                    goal).score(goalState(goal, 
                                          state, 
                                          valence)))
    return utterance
  }})
}

// Define a pragmatic listener
var pragmaticListener = function(utterance) {
  Infer({model: function(){
    var state = statePrior()
    var valence = valencePrior(state)
    var goal = goalPrior()
    observe(speaker(state, valence, goal),utterance)
    return {state, valence}
  }})
}

display("viz.table(pragmaticListener('terrible'))")
viz(pragmaticListener('terrible'))

display("\n\nviz(marginalize(pragmaticListener(\"terrible\"), \"state\"))")
viz(marginalize(pragmaticListener("terrible"), "state"))
display("\nviz(marginalize(pragmaticListener(\"terrible\"), \"valence\"))")
viz(marginalize(pragmaticListener("terrible"), "valence"))
~~~~

Without arousal to account for, the pragmatic listener accounts for the miniscule likelihood of ```"terrible"``` state, high likelihood of ```"ok"```state, and the high likelihood for ```"terrible"```state along with the moderate likelihood for ```"ok"``` state to produce negative valance. They then infer that the speaker most likely wants to convey whether they feel positively or negatively about the weather (```"goalValence"```) and infer the weather is likely ```"ok"``` and the speaker has negative valence. This would be a hyperbolic interpretation because the utterance ```"terrible"``` and the inferred state ```"ok"``` both have high likelihoods of negative valence (compared to the ```"amazing"``` state).

# References
G. Scontras, M. H. Tessler, and M. Franke. Probabilistic language understanding: An introduction to the Rational Speech Act framework. Retrieved 2023-6-14 from https://www.problang.org