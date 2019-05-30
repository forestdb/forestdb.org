---
layout: model
title: Jimenez, Marquez, Rosales Irony Extension
model-language: webppl
---

### An extension of the Kao and Goodman (2015) Irony model

*Authors: Abimael Hernandez Jimenez, Paula Aruby Marquez, and Cesar Manuel Rosales Jr.*

~~~~
/old
var states = ['terrible', 'ok', 'amazing']

//new
var states = ['terrible','bad','ok','good','amazing']
~~~~

In order to account for our 'continuous' arousal, we need to add additional states. In the original model, there are 3 states. We modified this in our model to 5 states.

~~~~
//old
var statePrior = function() {
  categorical([1, 50, 50], states)
}

//new
var statePrior = function() {
  categorical([1,20,50,50,50], states)
}
~~~~

We expanded the number of priors corresponding to the states.

~~~~
//old
var valencePrior = function(state) {
  state === "terrible" ? flip(0.99) ? -1 : 1 :
  state === "ok" ? flip(0.5) ? -1 : 1 :
  state === "amazing" ? flip(0.01) ? -1 : 1 :
  true
}

//new
var valencePrior = function(state) {
  state === "terrible" ? flip(0.99) ? -1 : 1 :
  state === "bad" ? flip(0.90) ? -1 : 1 :
  state === "ok" ? flip(0.5) ? -1 : 1 :
  state === "good" ? flip(0.09) ? -1 : 1 :
  state === "amazing" ? flip(0.01) ? -1 : 1 :
  true
}
~~~~

Due to adding new states, we added more valencePriors. Each state was assigned a probability of having a negative valence.

~~~~

//old
var arousals = ["low", "high"]

//new
var arousals = [.1,.3,.5,.7,.9]
~~~~

The original computed arousal in a binary manner. We modified arousal to be "continuous." The intensity of arousal increases as the percentage increases.

~~~~
//old
var arousalPrior = function(state) {
  state === "terrible" ? categorical([0.1, 0.9], arousals) :
  state === "ok" ? categorical([0.9, 0.1], arousals) :
  state === "amazing" ? categorical([0.1, 0.9], arousals) :
  true
}

//new
var arousalPrior = function(state) {
  state === "terrible" ? categorical([1,10,30,45,50], arousals) :
  state === "bad" ? categorical([1,5,25,40,45], arousals) :
  state === "ok" ? categorical([50,45,30,10,1], arousals) :
  state === "good" ? categorical([1,5,25,40,45], arousals) :
  state === "amazing" ? categorical([1,10,30,45,50], arousals) :
  true
}
~~~~

In the arousalPrior function we expanded the number of probabilities that map onto the state. For 'terrible' there is a greater chance of being assigned a higher level of arousal. In contrast, 'ok' has a lower chance of being assigned a higher level of arousal.

~~~~
// There are three possible states the weather could be in: 
// terrible, ok, or amazing
var states = ['terrible','bad','ok','good','amazing']

// Since we are in California, the prior over these states
// are the following. Once could also imagine this being 
// the prior in a certain context, e.g. when it's clearly
// sunny and nice out.
var statePrior = function() {
  categorical([1,5,40,40,40], states)
}

// Valence prior defined in terms of negative valence. 
// If the current state is terrible, it's extremely likely
// that the valence associated is negative. If it's ok, then 
// the valence could be negative or positive with equal 
// probability.
var valencePrior = function(state) {
  state === "terrible" ? flip(0.99) ? -1 : 1 :
  state === "bad" ? flip(0.90) ? -1 : 1 :
  state === "ok" ? flip(0.5) ? -1 : 1 :
  state === "good" ? flip(0.09) ? -1 : 1 :
  state === "amazing" ? flip(0.01) ? -1 : 1 :
  true
}

// Define binary arousals (could model as continuous).
// var arousals = ["low", "high"]
var arousals = [.1,.3,.5,.7,.9]

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
  state === "terrible" ? categorical([1,10,30,45,50], arousals) :
  state === "bad" ? categorical([1,5,25,40,45], arousals) :
  state === "ok" ? categorical([50,45,30,10,1], arousals) :
  state === "good" ? categorical([1,5,25,40,45], arousals) :
  state === "amazing" ? categorical([1,10,30,45,50], arousals) :
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

//The speaker takes in a state, valence, arousal, and a goal and returns an utterance 
//based on the probability of the literalListener arriving at the correct
//state given a goalState
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
    return {state,valence, arousal}
  }})
}

viz.table(pragmaticListener('terrible'))
~~~~