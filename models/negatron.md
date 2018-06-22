---
layout: model
title: Negatron (in prep)
model-language: webppl
model-language-version: v0.9.9
---

Current issues:
- Issue with meaning fn + utterance prior: Speaker can say utterance that has no referent (e.g., "has oranges" when nobody has oranges). This makes literal listener crash.

To do:
- Add QUDs / QUD inference

~~~~
// based off: Frank and Goodman (2012) RSA model

// set of states (here: objects of reference)
// we represent referents as JavaScript objects to demarcate them from utterances
var distractors =[
  {fruit: false, shirt: "red"},
  {fruit: "apples", shirt: "yellow"},
  {fruit: "apples", shirt: "green"}
]

// var referent = {fruit: false, shirt: "blue"}
var referent = {fruit: "oranges", shirt: "blue"}

// set of utterances
var utterances = ["apples", "oranges", "no apples", "no oranges"]

// prior over world states
var objectPrior = function() {
  var objects = distractors.concat(referent)
  var obj = uniformDraw(objects)
  return obj
}

// meaning function to interpret the utterances
var meaning = function(utterance, obj){
  var negation = utterance.indexOf("no") > -1; // utterance contains negation?
  // if utt has negation, split off the positive aspect of utterance
  var u = negation ? utterance.split("no ")[1] : utterance;
  // if there's negation, multiply truth value by -1
  ((negation ? -1 : 1) * ((obj.fruit == u) ? 1 : - 1)) == 1
}

// literal listener
var literalListener = function(utterance){
  Infer({model: function(){
    var obj = objectPrior();
    condition(meaning(utterance, obj))
    return obj.fruit
  }})
}

// set speaker optimality
var alpha = 1

// pragmatic speaker
var speaker = function(obj){
  Infer({model: function(){
    var utterance = uniformDraw(utterances)
    factor(alpha * literalListener(utterance).score(obj.fruit))
    return utterance
  }})
}

viz.table(speaker(referent))
// viz.table(literalListener("apples"))
~~~~


