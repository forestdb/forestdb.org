---
layout: model
title: RSA prior inference
model-language: webppl
---


First, the vanilla RSA model. But rather than a single `objectPrior`, we relativize inference to a specific `preference`: which objects the listener thinks are most salient (i.e., most likely to get referenced).

~~~~
// Frank and Goodman (2012) RSA model from problang.org

// set of states (here: objects of reference)
// we represent objects as JavaScript objects to demarcate them from utterances
// internally we treat objects as strings nonetheless
var objects = [{color: "blue", shape: "square", string: "blue square"},
               {color: "blue", shape: "circle", string: "blue circle"},
               {color: "green", shape: "square", string: "green square"}]

// set of utterances
var utterances = ["blue", "green", "square", "circle"]

var preferences = ["blue_things", "green_things", "squares", "circles","none"]

var preferenceTable = {
  blue_things : [4,4,2],
  green_things : [1,1,8],
  squares : [4,2,4],
  circles : [1,8,1],
  none : [1,1,1]
}

var preferencePrior = function() {
  return uniformDraw(preferences)
}

// prior over world states
var objectPrior = function(preference) {
  var obj = categorical(preferenceTable[preference],objects)
  return obj.string 
}

// meaning function to interpret the utterances
var meaning = function(utterance, obj){
  _.includes(obj, utterance)
}

// literal listener
var literalListener = function(utterance,preference){
  Infer({model: function(){
    var obj = uniformDraw(objects).string // L0 has no preference
    condition(meaning(utterance, obj))
    return obj
  }})
}

// set speaker optimality
var alpha = 1

// pragmatic speaker
var speaker = function(obj,preference){
  Infer({model: function(){
    var utterance = uniformDraw(utterances)
    factor(alpha * literalListener(utterance,preference).score(obj))
    return utterance
  }})
}

// pragmatic listener
var pragmaticListener = function(utterance,preference){
  Infer({model: function(){
    var obj = objectPrior(preference)
    observe(speaker(obj,preference),utterance)
    return obj
  }})
}

print("the listener hears 'square' and has a preference for blue things")
viz(pragmaticListener("square","blue_things"))

print("the listener hears 'square' and has a preference for green things")
viz(pragmaticListener("square","green_things"))

print("the listener hears 'square' and has a preference for squares")
viz(pragmaticListener("square","squares"))
~~~~

Next, we add in a `pragmaticSpeaker` who chooses an utterance, observes the listener's behavior (i.e., the object they think is being referenced), and infers the `preference` that listener used in making their choice.

~~~~
///fold:

// set of states (here: objects of reference)
// we represent objects as JavaScript objects to demarcate them from utterances
// internally we treat objects as strings nonetheless
var objects = [{color: "blue", shape: "square", string: "blue square"},
               {color: "blue", shape: "circle", string: "blue circle"},
               {color: "green", shape: "square", string: "green square"}]

// set of utterances
var utterances = ["blue", "green", "square", "circle"]

var preferences = ["blue_things", "green_things", "squares", "circles","none"]

var preferenceTable = {
  blue_things : [4,4,2],
  green_things : [1,1,8],
  squares : [4,2,4],
  circles : [1,8,1],
  none : [1,1,1]
}

var preferencePrior = function() {
  return uniformDraw(preferences)
}

// prior over world states
var objectPrior = function(preference) {
  var obj = categorical(preferenceTable[preference],objects)
  return obj.string 
}

// meaning function to interpret the utterances
var meaning = function(utterance, obj){
  _.includes(obj, utterance)
}

// literal listener
var literalListener = function(utterance,preference){
  Infer({model: function(){
    var obj = uniformDraw(objects).string
    condition(meaning(utterance, obj))
    return obj
  }})
}

// set speaker optimality
var alpha = 1

// pragmatic speaker
var speaker = function(obj,preference){
  Infer({model: function(){
    var utterance = uniformDraw(utterances)
    factor(alpha * literalListener(utterance,preference).score(obj))
    return utterance
  }})
}

// pragmatic listener
var pragmaticListener = function(utterance,preference){
  Infer({model: function(){
    var obj = objectPrior(preference)
    observe(speaker(obj,preference),utterance)
    return obj
  }})
}

///

var pragmaticSpeaker = function(utterance,observation) {
  Infer({model: function(){
    var preference = preferencePrior()
    var L1posterior = pragmaticListener(utterance,preference)
    factor(L1posterior.score(observation))
    return preference
  }})
}


print("observing the listener chose the blue square after hearing 'square'")
viz(pragmaticSpeaker("square","blue square"))

print("observing the listener chose the green square after hearing 'square'")
viz(pragmaticSpeaker("square","green square"))

print("observing the listener chose the green square after hearing 'green'")
viz(pragmaticSpeaker("green","green square"))
~~~~

Finally, we wrap the model above in an additional layer of inference that calculates the best utterance to inform the `pragmaticSpeaker` about the `pragmaticListener`'s `preference`. This inference finds the utterance that maximizes the KL divergence between the `pragmaticListener` posterior and the flat `preferencePrior`.

~~~~
///fold:

// get probabilities from a distribution
var distProbs = function(dist, supp) {
  return map(function(s) {
    return Math.exp(dist.score(s))
  }, supp)
}

// calculate KL divergence between two distributions
var KL = function(p, q) {
  var supp = sort(p.support());
  var P = distProbs(p, supp), Q = distProbs(q, supp);
  var diverge = function(xp,xq) {
    return xp == 0 ? 0 : (xp * Math.log(xp / xq) );
  };
  return sum(map2(diverge,P,Q));
};

// Frank and Goodman (2012) RSA model

// set of states (here: objects of reference)
// we represent objects as JavaScript objects to demarcate them from utterances
// internally we treat objects as strings nonetheless
var objects = [{color: "blue", shape: "square", string: "blue square"},
               {color: "blue", shape: "circle", string: "blue circle"},
               {color: "green", shape: "square", string: "green square"}]

// set of utterances
var utterances = ["blue", "green", "square", "circle"]

var preferences = ["blue_things", "green_things", "squares", "circles","none"]

var preferenceTable = {
  blue_things : [4,4,2],
  green_things : [1,1,8],
  squares : [4,2,4],
  circles : [1,8,1],
  none : [1,1,1]
}

var preferencePrior = function() {
  return uniformDraw(preferences)
}

// prior over world states
var objectPrior = function(preference) {
  var obj = categorical(preferenceTable[preference],objects)
  return obj.string 
}

// meaning function to interpret the utterances
var meaning = function(utterance, obj){
  _.includes(obj, utterance)
}

// literal listener
var literalListener = function(utterance,preference){
  Infer({model: function(){
    var obj = uniformDraw(objects).string
    condition(meaning(utterance, obj))
    return obj
  }})
}

// set speaker optimality
var alpha = 1

// pragmatic speaker
var speaker = function(obj,preference){
  Infer({model: function(){
    var utterance = uniformDraw(utterances)
    factor(alpha * literalListener(utterance,preference).score(obj))
    return utterance
  }})
}

// pragmatic listener
var pragmaticListener = function(utterance,preference){
  Infer({model: function(){
    var obj = objectPrior(preference)
    observe(speaker(obj,preference),utterance)
    return obj
  }})
}

var pragmaticSpeaker = function(utterance,observation) {
  Infer({model: function(){
    var preference = preferencePrior()
    var L1posterior = pragmaticListener(utterance,preference)
    factor(L1posterior.score(observation))
    return preference
  }})
}

///

var bestUtterance = function() {
  Infer({model: function() {
    var utterance = uniformDraw(utterances)
    var preference = preferencePrior()
    var observation = objectPrior(preference)
    condition(meaning(utterance, observation))
    var S2posterior = pragmaticSpeaker(utterance, observation)
    factor(KL(Infer({model: preferencePrior}),S2posterior))
    return utterance
  }})
}

bestUtterance()
~~~~