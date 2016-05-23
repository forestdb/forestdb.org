---
layout: model
title: Politeness
model-language: webppl
---
<script src="http://web.stanford.edu/~erindb/webppl-viz/webppl.min.js"></script>  
<link rel="stylesheet" href="http://web.stanford.edu/~erindb/webppl-viz/viz.css">


## Polite RSA
~~~~
///fold:
var marginalizeERP = function(myERP, label){
  Enumerate(function(){
    var x = sample(myERP)
    return x[label]
  })
}

var discretizeBeta = function(gamma, delta){
  var shape_alpha =  gamma * delta
  var shape_beta = (1-gamma) * delta
  var betaPDF = function(x){
    return Math.pow(x,shape_alpha-1)*Math.pow((1-x),shape_beta-1)
  }
  return map(betaPDF, [0.1,0.3,0.5,0.7,0.9])
}

var fillArray = function(n, val){
  return repeat(n, function(x){return val})
}
///
//////////////////////////////////////////////////////////
// Model Parameters
var speakerOptimality = 3
// easy toggle for testing out stochastic vs. deterministic word meanings (above)
var stochasticWords = false

// parameters for discretizeBeta are mean and concentratation
// 0.5, 2 for uniform ...
// pseudocount parameterization 
// alpha = gamma*delta
// beta = (1-gamma)*delta
var honestyPriorWeights = discretizeBeta(0.5, 2)
var kindnessPriorWeights = discretizeBeta(0.5, 2)
//////////////////////////////////////////////////////////

// a world has both a state and a valence
var worlds = [
  {state: "terrible", valence: 0.01},
  {state: "bad", valence: 0.25},
  {state: "okay", valence: 0.5},
  {state: "good", valence: 0.75},
  {state: "amazing", valence: 0.99}
]


var statePrior = function(){
  return uniformDraw(worlds)
}

// array of the state values (also the utterances)
var stateValues = _.pluck(worlds, "state")

var utterancePrior = function(){
  return uniformDraw(stateValues)
}

// array of the valence values
var worldValues = _.pluck(worlds, "valence")
var meanWorldValues = _.pluck(worlds, "valence").reverse()

// var beingNiceOrMean = function(nice){
//   var weights = nice ? worldValues : meanWorldValues
//  return worldValues[discrete(weights)]
// }

var beingNiceOrMean = function(nice, valence){
  var valenceValues = nice ? worldValues : meanWorldValues
  // what is the value of the true state?
  var i = valenceValues.indexOf(valence)
  // maintain probabilities for values >= true state (for nice)
  var k = nice ? valenceValues.slice(i) : valenceValues.slice(i).reverse()
  // any value < true state gets low probability (for nice)
  var weights = append(fillArray(i, 0.0001), k)
  return valenceValues[discrete(weights)]
}

var qudFunction = function(speakerGoals){
  return (speakerGoals.honest && speakerGoals.kind) ? 
                               function(w){return w} :
         speakerGoals.honest ? function(w){return w.state} :
                               function(w){return w.valence}
}


var meaning = stochasticWords ? 
    function(words, world){
      return words=="terrible" ? world.state == stateValues[discrete([10,1,0.1,0.01,0.001])] : 
      words=="bad" ? world.state == stateValues[discrete([1,10,1,0.1,0.01])] : 
      words=="okay" ? world.state == stateValues[discrete([0.1,1,10,1,0.1])] : 
      words=="good" ? world.state == stateValues[discrete([0.01,0.1,1,10,1])] : 
      words=="amazing" ? world.state == stateValues[discrete([0.001,0.01,0.1,1,10])] :
      true
    } : 
	function(words, world){
	  return words==world.state
	}

var listener0 = cache(function(utterance, goals) {
  Enumerate(function(){
    var world = statePrior()

    var m = meaning(utterance, world)

    condition(m)

    return qudFunction(goals)(world)
  })
})


var speaker1 = cache(function(world, speakerGoals) {
  Enumerate(function(){

    var utterance = utterancePrior()

    var goals = {
      honest: flip(speakerGoals.honesty),
      kind: flip(speakerGoals.kindness)
    }

    // if goal is NOT honesty, then choose world in proportion to valence
    var valence = goals.honest ? world.valence : 
                                 beingNiceOrMean(goals.kind, world.valence)

    // qud either returns true state, or valence, which may or may not be true
    var qudVal = qudFunction(goals)({"state":world.state, "valence":valence})

    var L0 = listener0(utterance, goals)

    factor(speakerOptimality*L0.score([],qudVal))

    return utterance
  })
})


var listener1 = function(utterance, knowledge) {
  Enumerate(function(){
    var world = statePrior()

    var speakerGoals = {
      honesty: [0.1, 0.3, 0.5, 0.7, 0.9][discrete(honestyPriorWeights)],
      kindness: [0.1, 0.3, 0.5, 0.7, 0.9][discrete(kindnessPriorWeights)]
    }

    condition(knowledge ? knowledge == world.state : true)

    var S1 = speaker1(world, speakerGoals)

    factor(S1.score([],utterance))

    return speakerGoals
  })
}

var utterance = "good"
var knowledge = "okay"
var posterior =  listener1(utterance, knowledge)

print("expected honesty " + expectation(marginalizeERP(posterior, "honesty")))
print(marginalizeERP(posterior, "honesty"))


print("expected kindness " + expectation(marginalizeERP(posterior, "kindness")))
print(marginalizeERP(posterior, "kindness"))
~~~~

## Kind utility RSA

RSA with a speaker utility that incorporates both informativeness (epistemic utility) and kindness (social utility)


~~~~
var marginalizeERP = function(myERP, label){
  Enumerate(function(){
    var x = sample(myERP)
    return x[label]
  })
}

var states = [1,2,3,4,5]
var utterances = ["terrible","bad","okay","good","amazing"]

var statePrior = function(){
  return uniformDraw(states)
}

var utterancePrior = function(){
  return uniformDraw(utterances)
}

var speakerOptimality = 5

var honestyWeights = [1,1,1,1,1]
// var kindnessWeights = [1,1,1,1,1,1,1,1,1,1]
var kindnessWeights = [1,1,1,1,1]

var literalSemantics = {
  "terrible":[.95,.30,.02,.02,.02],
  "bad":[.85,.85,.02,.02,.02],
  "okay":[0.01,0.25,1,.65,.35],
  "good":[.02,.05,.55,.95,.93],
  "amazing":[.02,.02,.02,.65,1]
}

var meaning = function(words, state){
    return words=="sayNothing" ? true : flip(literalSemantics[words][state-1])
} 


var listener0 = cache(function(utterance) {
  Enumerate(function(){
  var state = statePrior()
  var m = meaning(utterance, state)
  condition(m)
  return state
  })
})


var speaker1 = cache(function(state, speakerGoals) {
  Enumerate(function(){
    var utterance = utterancePrior()

    var L0 = listener0(utterance)
    var epistemicUtility = L0.score([],state)
    var niceUtility = expectation(L0)

    var jointUtility = speakerGoals.honesty*epistemicUtility + 
                       speakerGoals.kindness*niceUtility

    factor(speakerOptimality*jointUtility)

    return utterance
  })
})


var listener1 = function(utterance, knowledge) {
  Enumerate(function(){
    var state = statePrior()

    var speakerGoals = {
      honesty: [0.1, 0.3, 0.5, 0.7, 0.9][discrete(honestyWeights)],
//      kindness: [-0.9,-0.7,-0.5,-0.3,-0.1,0.1, 0.3, 0.5, 0.7, 0.9][discrete(kindnessWeights)]
      kindness: [0.1, 0.3, 0.5, 0.7, 0.9][discrete(kindnessWeights)]
      }

    condition(knowledge ? knowledge == state : true)

    var S1 = speaker1(state, speakerGoals)

    factor(S1.score([],utterance))

    return speakerGoals
  })
}

var experimentalCondition1 = {
  utterance : "amazing",
  knowledge: 1
}

var experimentalCondition2 = {
  utterance : "amazing",
  knowledge: 3
}

var posterior1 = listener1(experimentalCondition1.utterance, 
                        experimentalCondition1.knowledge)

print("expected honesty " + expectation(marginalizeERP(posterior1, "honesty")))
print("expected kindness " + expectation(marginalizeERP(posterior1, "kindness")))

vizPrint(posterior1)

var posterior2 = listener1(experimentalCondition2.utterance, 
                        experimentalCondition2.knowledge)

print("expected honesty " + expectation(marginalizeERP(posterior2, "honesty")))
print("expected kindness " + expectation(marginalizeERP(posterior2, "kindness")))

vizPrint(posterior2)

~~~~


# Speaker models

### Speaker who communicates his goals and the state

Here, communicating goals can be thought of as trying to appear as the kind of speaker who would say such a thing.

~~~~
var marginalizeERP = function(myERP, label){
  Enumerate(function(){
    var x = sample(myERP)
    return x[label]
  })
}

var states = [1,2,3]
var utterances = ["terrible","okay","amazing",
                  "not terrible", "not okay","not amazing"]

var statePrior = function(){
  return uniformDraw(states)
}

var utterancePrior = function(){
  return uniformDraw(utterances)
}

var speakerOptimality = 5

var honestyWeights = [1,1,1,1,1]
// var kindnessWeights = [1,1,1,1,1,1,1,1,1,1]
var kindnessWeights = [1,1,1,1,1]

var literalSemantics = {
  "terrible":    [0.95,0.02,0.02],
  "okay":        [0.02,0.95,0.02],
  "amazing":     [0.02,0.02,0.95],
  "not terrible":[0.02,0.95,0.95],
  // "not okay":[0.95,0.02,0.95], // "i can't say enough good things about this person"
  "not okay":    [0.95,0.02,0.02],
  "not amazing": [0.95,0.95,0.02]
}

var meaning = function(words, state){
    return words=="sayNothing" ? true : flip(literalSemantics[words][state-1])
} 

var alpha = 1.25

var listener0 = cache(function(utterance) {
  Enumerate(function(){
  var state = statePrior()
  var m = meaning(utterance, state)
  condition(m)
  return state
  })
})


var speaker1 = cache(function(state, speakerGoals) {
  Enumerate(function(){
    var utterance = utterancePrior()

    var L0 = listener0(utterance)
    var epistemicUtility = L0.score([],state)
    var niceUtility = expectation(L0, function(s){return alpha*s})

    var jointUtility = speakerGoals.honesty*epistemicUtility + 
                       speakerGoals.kindness*niceUtility

    factor(speakerOptimality*jointUtility)

    return utterance
  })
})


var listener1 = function(utterance) {
  Enumerate(function(){
    var state = statePrior()

    var speakerGoals = {
      honesty: [0.1, 0.3, 0.5, 0.7, 0.9][discrete(honestyWeights)],
//      kindness: [-0.9,-0.7,-0.5,-0.3,-0.1,0.1, 0.3, 0.5, 0.7, 0.9][discrete(kindnessWeights)]
      kindness: [0.1, 0.3, 0.5, 0.7, 0.9][discrete(kindnessWeights)]
      }

    var S1 = speaker1(state, speakerGoals)

    factor(S1.score([],utterance))

    return {
      state: state,
      goals: speakerGoals
    }
  })
}


var speaker2 = function(state, intendedGoals) {
  Enumerate(function(){

    var utterance = utterancePrior()

    var L1 = listener1(utterance)

    factor(L1.score([], {"state":state, "goals":intendedGoals}))

    return utterance

  })
}

print(speaker2(1, {honesty:0.9, kindness: 0.9}))

~~~~

#### Open questions 

+ Is there a difference between communicating the goal to be honest vs. being honest?
++ Right now, honesty in both cases is informativity. Maybe it should be just truth-functional.
