---
layout: model
title: Rationally redundant, not "overinformative" referring expressions
model-status: code
model-category: Reasoning about Reasoning
model-tags: language, pragmatics, overinformativeness
model-language: webppl
model-language-version: v0.9.7
---

This is a model of the production of referring expressions, based on the vanilla model of Frank & Goodman 2012, but equipped with a relaxed semantics. It is described further in [Degen et al (under review)](https://arxiv.org/abs/1903.08237). 


## Model

### Vanilla model

We start with a model that has a Boolean semantics for color and size terms.

~~~~
var alpha = 1
var c = 0

var states = ["big_blue","small_blue","small_red"]

var utterances = ["big", "small", "blue", "red", "big_blue", "small_blue", "small_red"]     	

var statePrior = function() {
	return uniformDraw(states)
};

var utterancePrior = function() {
  return uniformDraw(utterances)
};

var meaning = function(utterance, obj){
  _.includes(obj, utterance)
}

var cost = {
	big: 0,
	small: 0,
	blue: 0,
	red: 0,
	big_blue: c,
	small_blue: c,
	small_red: c
}

// literal listener
var literalListener = cache(function(utt) {
  return Infer({method:"enumerate"},
               function(){
    var state = statePrior()
    condition(meaning(utt,state))
    return state
  })
});

// pragmatic speaker
var speaker = cache(function(state) {
  return Infer({method:"enumerate"},
               function(){
    var utt = utterancePrior()
    factor(alpha * (literalListener(utt).score(state)-cost[utt]))
    return utt
  })
});

speaker("big_blue")
~~~~

Change the cost. Convince yourself that increasing the cost of complex utterances quickly drives their probability to 0, i.e., "overinformative" expressions won't be produced.


## Relaxed semantics model

~~~~
var alpha = 30
var size_noise = 0.2
var color_noise = 0.01
var size_cost = 0
var color_cost = 0

var states = [
	{size: "big", color: "blue"},
	{size: "small", color: "blue"},
	{size: "small", color: "red"}]

var utterances = ["big", "small", "blue", "red", "big_blue", "small_blue", "small_red"]     

var colors = ["red", "blue"]
var sizes = ["big", "small"]	

var statePrior = function() {
	return uniformDraw(states)
};

var utterancePrior = function() {
  return uniformDraw(utterances)
};

// assumes that 2-word utterances consist of SIZE_COLOR, in that order
var meaning = function(utt, obj) {
  var splitWords = utt.split('_')
  if (splitWords.length == 1) {
    var word = splitWords[0]
    if(_.includes(colors, word))
      return word == obj.color ? 1-color_noise : color_noise;
    else if (_.includes(sizes, word))
      return word == obj.size ? 1-size_noise : size_noise;
  } else if (splitWords.length == 2) {
    var size_value = splitWords[0] == obj.size ? 1-size_noise : size_noise;
    var color_value = splitWords[1] == obj.color ? 1-color_noise : color_noise;
    return size_value*color_value
  } else 
    console.error("bad utterance length: "+splitWords.length)
};

var cost = {
	big: size_cost,
	small: size_cost,
	blue: color_cost,
	red: color_cost,
	big_blue: size_cost+color_cost,
	small_blue: size_cost+color_cost,
	small_red: size_cost+color_cost
}

// literal listener
var literalListener = cache(function(utt) {
  return Infer({method:"enumerate"},
               function(){
    var state = statePrior()
    factor(meaning(utt,state))
    return state
  })
});

// pragmatic speaker
var speaker = cache(function(state) {
  return Infer({method:"enumerate"},
               function(){
    var utt = utterancePrior()
    factor(alpha * (literalListener(utt).score(state)-cost[utt]))
    return utt
  })
});


viz(speaker({"size": "big", "color": "blue"}))
~~~~

1. Play around with size and color noise. For what parameter values can you generate the reported size/color asymmetry in the probability of producing an "overinformative" expression? When does the asymmetry disappear or reverse?

2. How does increasing size and color cost change speaker behavior?

3. Add additional objects to the context. How does this affect speaker behavior?