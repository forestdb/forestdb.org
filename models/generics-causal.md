---
layout: model
title: Generics with causal background knowledge
model-status: code
model-language: webppl
---

Contrasting "Butterflies have dust on their wings" vs. "This butterfly has dust on its wings"


Here, the specific utterance ("This butterfly...") is operationlized as the smallest value on the prevalence scale. Perhaps that is too much of a simplification.


~~~~
///fold:
// discretized range between 0 - 1
var bins = [0.001,0.01,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.99]

// function returns a discretized beta PDF
var discretizeBeta = function(gamma, delta){
  var shape_alpha =  gamma * delta
  var shape_beta = (1-gamma) * delta
  var betaPDF = function(x){
    return Math.pow(x,shape_alpha-1)*Math.pow((1-x),shape_beta-1)
  }
  return map(betaPDF, bins)
}
///

var s1optimality = 5
var thresholdBins = [0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]
var thresholdPrior = function() {
  var threshold = uniformDraw(thresholdBins)
  return threshold
}

var utterancePrior = function() {
  var utterances = ["generic", "specific", "mu"]  
  //    var utterances = ["generic is true",
  //                 "generic is false"]  
  return uniformDraw(utterances)
}

var meaning = function(utt,state, threshold) {
  return _.isNumber(utt) ? state == utt :
  utt=="generic"? state>threshold :
  utt=="specific"? state==0.001 :
  utt=="generic is false"? state<=threshold :
  utt=='mu'? true:
  utt=='some'? state>0:
  utt=='most'? state>= 0.5:
  utt=='all'? state >= 0.99:
  true
}

var listener0 = cache(function(utterance, threshold, params) {
  Enumerate(function(){
    var causeIsPresent = flip(params.theta)
    var state = causeIsPresent ? 
        bins[discrete(discretizeBeta(params.g1,params.d1))] : 
        bins[discrete(discretizeBeta(params.g0,params.d0))]

    var m = meaning(utterance, state, threshold)
    condition(m)
    return state
  })
})

var speaker1 = cache(function(state, threshold, params) {
  Enumerate(function(){
    var utterance = utterancePrior()
    var L0 = listener0(utterance, threshold, params)
    factor(L0.score([],state))
    return utterance
  })
})


var listener1 = function(utterance, params) {
  Enumerate(function(){
    var causeIsPresent = flip(params.theta)
    var state = causeIsPresent ? 
        bins[discrete(discretizeBeta(params.g1,params.d1))] : 
    bins[discrete(discretizeBeta(params.g0,params.d0))]

    var threshold = thresholdPrior()
    var S1 = speaker1(state, threshold, params)
    factor(s1optimality*S1.score([],utterance))
    return causeIsPresent ? "primary cause" : "background cause"
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



print(
  listener1(
    "specific", 
    {
      theta: 0.5,
      g1: 0.99,
      d1: 10,
      g0: 0.05,
      d0: 2,
    }
  )
)
~~~~