---
layout: model
title: Gonzalez & Zhang Irony S2
model-language: webppl
---

~~~~
var states = ['so pretty','okay','so ugly']

// the prior over these states

var statePrior = function() {
  categorical([1, 50, 50], states)
}

// Valence prior defined in terms of negative valence. 
// If the current state is terrible, it's extremely likely
// that the valence associated is negative. If it's ok, then 
// the valence could be negative or positive with equal 
// probability.
var valencePrior = function(state) {
  state === "so pretty" ? flip(0.99) ? -1 : 1 :
  state === "okay" ? flip(0.5) ? -1 : 1 :
  state === "so ugly" ? flip(0.01) ? -1 : 1 :
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

// Assume cost of utterances is categorical.
var utterancePrior = function() {
  categorical({vs:["so pretty","okay","so ugly"],
              ps: [0.2,0.02,0.9]})
}

// Sample arousal given a state.
var arousalPrior = function(state) {
  state === "so pretty" ? categorical([0.1, 0.9], arousals) :
  state === "ok" ? categorical([0.9, 0.1], arousals) :
  state === "so ugly" ? categorical([0.1, 0.9], arousals) :
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
    var state = statePrior()
    var valence = valencePrior(state)
    var arousal = arousalPrior(state)
    condition(literalInterpretation(utterance,state))
    return goalState(goal, state, valence, arousal)
  }})
}

// Define a speaker
var speaker1 = function(state, valence, arousal, goal) {
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
    observe(speaker1(state, valence, arousal, goal),utterance)
//     return {state, valence, arousal}
    return goalState(goal, state, valence,arousal)
  }})
}

var speaker2 = function(state) {
  Infer({model: function(){
    var valence = valencePrior(state)
    var arousal = arousalPrior(state)
    var utterance = utterancePrior()
    var goal = goalPrior()
    var L1 = pragmaticListener(utterance)
    factor(1 * pragmaticListener(utterance).score(goalState(goal, 
                                          state, 
                                          valence, 
                                         arousal )))
    return utterance
  }})
}

var speaker2va = function(state,valence,arousal) {
  Infer({model: function(){
    var utterance = utterancePrior()
    var goal = goalPrior()
    var L1 = pragmaticListener(utterance)
    factor(1 * pragmaticListener(utterance).score(goalState(goal, 
                                          state, 
                                          valence, 
                                         arousal )))
    return utterance
  }})
}
~~~~
