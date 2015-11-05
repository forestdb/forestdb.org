---
layout: model
title: Generics
model-status: code
model-category: Reasoning about Reasoning
model-tags: language, generics, vagueness, pragmatics
model-language: webppl
---

<script src="http://web.stanford.edu/~erindb/webppl-viz/webppl.min.js"></script>  
<link rel="stylesheet" href="http://web.stanford.edu/~erindb/webppl-viz/viz.css">

This is a model of generic language used in Ref:tesslerGenerics.

The model takes the generic [[K has F]] to mean the prevalence of 
property F within kind K --- i.e., P(F | K) --- is above some threshold.
This threshold --- `tau` --- is thought to be in general unknown 
(`tau~uniform(0,1)`) and must be inferred in context. 

Context here takes the form of the listener and speakers shared beliefs
about the property in question. The shape of this distribution
affects model predictions, because the threshold must be calibrated to make utterances 
truthful and informative. The shape of this distribution varies significantly 
among different properties (e.g. *lays eggs*, *carries malaria*), and may 
be the result of a deeper conceptual model of the world. For instance,
if speakers and listeners believe that some kinds have a causal mechanism that
could give rise to the property, while others do not, then we would expect
teh prior to be structured as a mixture distribution 
(Cf. Griffiths & Tenenbaum, 2005). 

## Prior model

The following model `structuredPriorModel` instantiates this idea.
`theta` is the potential of a property F to be present in a kind.
This can also be thought of the prevalence of the property at a 
category levels (what % of kinds have this property present within the kind?).
For example, "lays eggs" is present in ducks, swans, fish, but not kangaroos or giraffes.
"Are female" is present in almost all kinds.
"Carries malaria" is present in almost no kinds.
`gamma` is the *mean prevelence when the property is present*.
Knowing that the property is present in a kind, what % of the kind do you 
expect to have it? 
For example, about 50% of a kind "is female"; 100% has wings; malaria is a rare property within a kind.
Finally, `delta` is the concentration (conceptually, the inverse variance) of that mean.
It is high for properties that present in almost every kind in exactly the same proportion (e.g. "is female"). 
It is lower when there is more uncertainty about exactly how many within a kind are expected to have the property.



~~~~
// discretized range between 0 - 1
var bins = [0.01,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.99]

// function returns a discretized beta PDF
var discretizeBeta = function(gamma, delta){
  var shape_alpha =  gamma * delta
  var shape_beta = (1-gamma) * delta
  var betaPDF = function(x){
    return Math.pow(x,shape_alpha-1)*Math.pow((1-x),shape_beta-1)
  }
  return map(betaPDF, bins)
}

var structuredPriorModel = function(params){
  Enumerate(function(){
    var theta = params["theta"]
    var g = params["gamma"]
    var d = params["delta"]
    var propertyIsPresent = flip(theta)
    var prevalence = propertyIsPresent ? 
        bins[discrete(discretizeBeta(g,d))] : 
    0

    return prevalence
  })
}

// e.g. "Has Wings"
var hasWings = structuredPriorModel({theta: 0.5,
                                     gamma: 0.99,
                                     delta: 10})

// e.g. "Lays eggs"
var laysEggs = structuredPriorModel({theta: 0.5,
                                     gamma: 0.5,
                                     delta: 10})
// e.g. "Are female"
var areFemale = structuredPriorModel({theta: 0.99,
                                      gamma: 0.5,
                                      delta: 50})

// e.g. "Carries Malaria"
var carriesMalaria = structuredPriorModel({theta: 0.1,
                                           gamma: 0.01,
                                           delta: 2})


vizPrint({
  "has wings": hasWings,
  "lays eggs": laysEggs,
  "are female": areFemale,
  "carries malaria": carriesMalaria
})

~~~~

## Generics model

~~~~
///fold:
// discretized range between 0 - 1
var bins = [0.01,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.99]

// function returns a discretized beta PDF
var discretizeBeta = function(gamma, delta){
  var shape_alpha =  gamma * delta
  var shape_beta = (1-gamma) * delta
  var betaPDF = function(x){
    return Math.pow(x,shape_alpha-1)*Math.pow((1-x),shape_beta-1)
  }
  return map(betaPDF, bins)
}

var structuredPriorModel = function(params){
  Enumerate(function(){
    var theta = params["theta"]
    var g = params["gamma"]
    var d = params["delta"]
    var propertyIsPresent = flip(theta)
    var prevalence = propertyIsPresent ? 
        bins[discrete(discretizeBeta(g,d))] : 
    0

    return prevalence
  })
}
///

var s1optimality = 5
var thresholdBins = [0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]
var thresholdPrior = function() {
  var threshold = uniformDraw(thresholdBins)
  return threshold
}

var utterancePrior = function() {
  var utterances = ["generic is true", "mu"]  
  //    var utterances = ["generic is true",
  //                 "generic is false"]  
  return flip(0.5) ? utterances[0] : utterances[1]
}

var meaning = function(utt,state, threshold) {
  return _.isNumber(utt) ? state == utt :
  utt=="generic is true"? state>threshold :
  utt=="generic is false"? state<=threshold :
  utt=='mu'? true:
  utt=='some'? state>0:
  utt=='most'? state>= 0.5:
  utt=='all'? state >= 0.99:
  true
}

var listener0 = cache(function(utterance, threshold, prior) {
  Enumerate(function(){
    var state = sample(prior)
    var m = meaning(utterance, state, threshold)
    condition(m)
    return state
  })
})

var speaker1 = cache(function(state, threshold, prior) {
  Enumerate(function(){
    var utterance = utterancePrior()
    var L0 = listener0(utterance, threshold, prior)
    factor(L0.score([],state))
    return utterance
  })
})


var listener1 = function(utterance, prior) {
  Enumerate(function(){
    var state = sample(prior)
    var threshold = thresholdPrior()
    var S1 = speaker1(state, threshold, prior)
    factor(s1optimality*S1.score([],utterance))
    return state
  })
}


var speaker2 = function(prevalence, prior){
  Enumerate(function(){
    var utterance = utterancePrior()
    var wL1 = listener1(utterance, prior)
    factor(wL1.score([], prevalence))
    return utterance
  })
}

// example priors
var hasWingsERP = structuredPriorModel({theta: 0.5,
                                        gamma: 0.99,
                                        delta: 10})
var laysEggsERP = structuredPriorModel({theta: 0.5,
                                        gamma: 0.5,
                                        delta: 10})
var carriesMalariaERP = structuredPriorModel({theta: 0.1,
                                              gamma: 0.01,
                                              delta: 2})
var areFemaleERP = structuredPriorModel({theta: 0.99,
                                         gamma: 0.5,
                                         delta: 50})


var malariaPosterior = listener1("generic is true", carriesMalariaERP)
var eggsPosterior = listener1("generic is true", laysEggsERP)
var femalePosterior = listener1("generic is true", areFemaleERP)

print("Listener interpretation of generics")

vizPrint({
  "X carries malaria": malariaPosterior,
  "X lays eggs": eggsPosterior,
  "X are female": femalePosterior
})

// truth judgment task assumes the subjective prevalence of 
// F within K is known to the speaker
// we measure these values empirically

var malariaSpeaker = speaker2(0.1, carriesMalariaERP)
var eggSpeaker = speaker2(0.6, laysEggsERP)
var femaleSpeaker = speaker2(0.5, areFemaleERP)
var lionSpeaker = speaker2(0.01, laysEggsERP)

print("Truth judgments")

print("Mosquitos carry malaria")
print(malariaSpeaker)

print("Ducks lay eggs")
print(eggSpeaker)

print("Ducks are female")
print(femaleSpeaker)

print("Lions lay eggs")
print(lionSpeaker)
~~~~

References:

- Cite:tesslerGenerics
