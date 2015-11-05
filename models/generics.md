
---
layout: model
title: Generics
model-status: code
model-category: Reasoning about Reasoning
model-tags: language, generics, vagueness, pragmatics
model-language: webppl
---


~~~~
var s1optimality = 5

var stateBins = [0,0.01,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.99]

var thetaBins = [0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]

var thetaPrior = function() {
  var threshold = uniformDraw(thetaBins)
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

var meaning = function(utt,state, theta) {
  return _.isNumber(utt) ? state == utt :
  		   utt=="generic is true"? state>theta :
         utt=="generic is false"? state<=theta :
         utt=='mu'? true:
         utt=='some'? state>0:
         utt=='most'? state>= 0.5:
         utt=='all'? state >= 0.99:
         true
}

var listener0 = cache(function(utterance, theta, prior) {
  Enumerate(function(){
    var state = statePrior(prior)
    var m = meaning(utterance, state, theta)
    condition(m)
    return state
  })
})

var speaker1 = cache(function(state, theta, prior) {
  Enumerate(function(){
    var utterance = utterancePrior()
    var L0 = listener0(utterance, theta, prior)
    factor(L0.score([],state))
    return utterance
  })
})


var listener1 = function(utterance, prior) {
  Enumerate(function(){
    var state = statePrior(prior)
    var theta = thetaPrior()
    var S1 = speaker1(state, theta, prior)
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