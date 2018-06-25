---
layout: model
title: Negatron (in prep)
model-language: webppl
model-language-version: v0.9.9
---

Past issues:
- Issue with meaning fn + utterance prior: Speaker can say utterance that has no referent (e.g., "has oranges" when nobody has oranges). This makes literal listener crash. (Temporary-ish solution: noisy semantics + speaker conditions on saying true things)

To add:
- shirt support (speaker can talk about shirt color)
- revisit: some QUD uncertainty (listener might not know it... look to context, but speaker's utterance might trigger the qud as well)



~~~~
var shirt_colors = ["red shirt", "blue shirt", "green shirt", "yellow shirt"]

var makeSingleContext = function(alternative_fruit, n_with_apples, n_total) {
  map(function(i){
    if (i < n_with_apples) {
      return {fruit: "apples", shirt: shirt_colors[i]}
    } else {
      return {fruit: alternative_fruit, shirt: shirt_colors[i]}
    }
  }, _.range(0, n_total))
}

var makeQuadrantContexts =  function(alternative_fruit){
  return  map(
    function(n_w_apples){ 
      makeSingleContext(alternative_fruit, 3-n_w_apples, n_w_apples) 
    }, _.range(0, 4))
}

var allContexts = {
  nonexistence: makeQuadrantContexts(false),
  alternative:  makeQuadrantContexts("oranges")
}

var allReferents = {
  nonexistence: {fruit: false, shirt: "yellow shirt"},
  alternative: {fruit: "oranges", shirt: "yellow shirt"}
}

var isNegation = function(utt){
  return (utt.indexOf("no") > -1)
};

// set of utterances
var utterances = [
  "apples", "oranges", "no apples", "no oranges",
  "red shirt", "blue shirt", "green shirt", "yellow shirt"
]

var cost_per_word = 1; // cost of saying 2 words
var cost_neg = 0; // cost of saying negation (above and beyond cost of 2nd word)

var uttProbs = map(function(u) {
  var n_words = u.split(' ').length
  var uttCost = (n_words - 1)*cost_per_word + isNegation(u)*cost_neg
  return Math.exp(-uttCost)
}, utterances)

var utterancePrior = Categorical({
  vs: utterances,
  ps: uttProbs
})

// prior over world states
var objectPrior = function(objects) {
  var obj = uniformDraw(objects)
  return obj
}

// meaning function to interpret the utterances
var meaning = function(utterance, obj){
  // if utt has negation, split off the positive aspect of utterance
  var u = isNegation(utterance) ? utterance.split("no ")[1] : utterance;
  // check to see if utt is about shirt or fruit
  var referent_property = u.indexOf("shirt") > - 1 ? "shirt" : "fruit"
  // does object have property?
  var obj_property_val = obj[referent_property] == u? 1 : -1
  // if there's negation, multiply truth value by -1
  var neg_val = isNegation(utterance) ? -1 : 1
  return ((neg_val * obj_property_val) == 1) ? 0.999 : 0.001
}

var qudFns = {
  "apples?": function(obj){ return obj.fruit == "apples"},
  "which fruit?": function(obj){ return obj.fruit },
  "which referent?": function(obj){ return obj }
}

// literal listener
var literalListener = function(utterance, qud, allObjects){
  Infer({model: function(){
    var obj = objectPrior(allObjects);
    var qudFn = qudFns[qud]
    condition(flip(meaning(utterance, obj)))
    //     return [obj, qudFn(obj)]
    return qudFn(obj)
  }})
}

// set speaker optimality
var alpha = 1

// pragmatic speaker
var speaker = function(obj, context, qud){
  Infer({model: function(){
    var allObjects = context.concat(obj) // to make the listener's state prior
    var utterance = sample(utterancePrior)
    var L0 = literalListener(utterance, qud, allObjects)
    var qudFn = qudFns[qud]
    condition(flip(meaning(utterance, obj))) // strongly prefer to say true things
    factor(alpha * L0.score(qudFn(obj))) // informativity
    return utterance
  }})
}

display("===Quadrant 1: nonexistence context, nonexistence referent, apples? qud===")
var allResults = mapIndexed(
  function(i, ctxt){
    display("----" + i + " out of 3 with apples")
    var S1 = speaker(allReferents.nonexistence, ctxt, "apples?")
    display('P(speaker says "no apples") = ' + Math.exp(S1.score("no apples")))
//     viz.table(S1) // full model predictions
  }, allContexts.nonexistence
)

display("===Quadrant 2: nonexistence context, alternative referent, apples? qud===")
var allResults = mapIndexed(
  function(i, ctxt){
    display("----" + i + " out of 3 with apples")
    var S1 = speaker(allReferents.alternative, ctxt, "apples?")
    display('P(speaker says "no apples") = ' + Math.exp(S1.score("no apples")))
//     viz.table(S1) // full model predictions
  }, allContexts.nonexistence
)

display("===Quadrant 3: alternative context, nonexistence referent, which fruit? qud===")
var allResults = mapIndexed(
  function(i, ctxt){
    display("----" + i + " out of 3 with apples")
    var S1 = speaker(allReferents.nonexistence, ctxt, "which fruit?")
    display('P(speaker says "no apples") = ' + Math.exp(S1.score("no apples")))
//     viz.table(S1) // full model predictions
  }, allContexts.alternative
)

display("===Quadrant 4: alternative context, alternative referent, which fruit? qud===")
var allResults = mapIndexed(
  function(i, ctxt){
    display("----" + i + " out of 3 with apples")
    var S1 = speaker(allReferents.alternative, ctxt, "which fruit?")
    display('P(speaker says "no apples") = ' + Math.exp(S1.score("no apples")))
//     viz.table(S1) // full model predictions
  }, allContexts.alternative
)

''

~~~~


