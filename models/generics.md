---
layout: model
title: Generics
model-status: code
model-category: Reasoning about Reasoning
model-tags: language, generics, vagueness, pragmatics
model-language: webppl
---

This is a model of generic language used in Ref:tesslerGenerics.

The model takes the generic [[K has F]] to mean the prevalence of 
property F within kind K --- i.e., P(F | K) --- is above some threshold.
This threshold --- `tau` --- is thought to be in general unknown 
(`tau~uniform(0,1)`) and must be inferred in context. 

Context here takes the form of the listener and speakers shared beliefs
about the property in question --- `statePrior`. The shape of this distribution
affects model predictions, because the threshold must be calibrated to make utterances 
truthful and informative. The shape of this distribution varies significantly 
among different properties (e.g. *lays eggs*, *carries malaria*), and may 
be the result of a deeper conceptual model of the world. For instance,
if speakers and listeners believe that some kinds have a causal mechanism that
could give rise to the property, while others do not, then we would expect
`statePrior` to be structured as a mixture distribution 
(Cf. Griffiths & Tenenbaum, 2005).

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

// function returns the PDF from a webppl ERP objects
var getProbsFromERP = function(myERP, orderedSupport){
  return map(function(s){
    Math.exp(myERP.score([], s))
  }, orderedSupport)
}

var structuredPriorModel = function(params){
  var theta = params["theta"]
  var g = params["gamma"]
  var d = params["delta"]
  var prevalencePrior = 
    Enumerate(function(){
      var propertyIsPresent = flip(theta)
      var prevalence = propertyIsPresent ? 
                  bins[discrete(discretizeBeta(g,d))] : 
                  0

      return prevalence
    })
  return getProbsFromERP(prevalencePrior, 
        [0, 0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.99]
    )
}

// e.g. "Has Wings"
structuredPriorModel({theta: 0.5,
                      gamma: 0.99,
                      delta: 10})

// e.g. "Lays eggs"
structuredPriorModel({theta: 0.5,
                      gamma: 0.5,
                      delta: 10})

// e.g. "Carries Malaria"
structuredPriorModel({theta: 0.1,
                      gamma: 0.01,
                      delta: 2})
~~~~



~~~~
var s1optimality = 5

var stateBins = [0,0.01,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.99]

var thresholdBins = [0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]

var thresholdPrior = function() {
  var threshold = uniformDraw(thresholdBins)
  return threshold
}

var statePrior = function(prior) {
  var state = stateBins[discrete(prior)]
  return state
}

var utterancePrior = function() {
   // var utterances = ["generic is true",
   //                  "generic is false"]
  var utterances = ["generic is true", "mu"]  
  var cost = 1      
  var cst = [1,cost]       
  return utterances[discrete(cst)]
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
    var state = statePrior(prior)
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
    var state = statePrior(prior)
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

~~~~

References:

- Cite:tesslerGenerics
