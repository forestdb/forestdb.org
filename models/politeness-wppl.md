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
    var valence = goals.honest ? world.valence : beingNiceOrMean(goals.kind)

    // qud either returns true state, or valence, which may or may not be true
    var qudVal = qudFunction(goals)({"state":world.state, "valence":valence})

    var L0 = listener0(utterance, goals)

    factor(L0.score([],qudVal))

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

    factor(speakerOptimality*S1.score([],utterance))

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
