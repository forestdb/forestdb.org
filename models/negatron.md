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
var makeSingleContext = function(alternative_fruit, n_alternatives, n_with_apples) {
  // alternative_fruit = {"oranges", false}
  repeat(n_with_apples, function(){
    return {fruit: "apples", shirt: "red"}
  }).concat(
    repeat(n_alternatives, function(){
      return {fruit: alternative_fruit, shirt: "red"}
    })
  )
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
  nonexistence: {fruit: false, shirt: "blue"},
  alternative: {fruit: "oranges", shirt: "blue"}
}

var isNegation = function(utt){
  return (utt.indexOf("no") > -1)
};

// set of utterances
var utterances = ["apples", "oranges", "no apples", "no oranges"]
var cost_yes = 0;
var cost_neg = 2;

var uttProbs = map(function(u) {
  return isNegation(u) ? Math.exp(-cost_neg) : Math.exp(-cost_yes)
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
  var negation = utterance.indexOf("no") > -1; // utterance contains negation?
  // if utt has negation, split off the positive aspect of utterance
  var u = negation ? utterance.split("no ")[1] : utterance;
  // if there's negation, multiply truth value by -1
  ((negation ? -1 : 1) * ((obj.fruit == u) ? 1 : -1)) == 1 ? 0.999 : 0.001
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


display("--Quadrant 1: nonexistence context, nonexistence referent--")
var allResults = mapIndexed(
  function(i, ctxt){
    display(i + " out of 3 with apples")
    viz.table(
      speaker(allReferents.nonexistence, ctxt, "apples?")
      )
  }, allContexts.nonexistence
)

// display("--Quadrant 2: nonexistence context, alternative referent--")
// var allResults = mapIndexed(
//   function(i, ctxt){
//     display(i + " out of 3 with apples")
//     viz.table(
//       speaker(allReferents.alternative, ctxt, "apples?")
//       )
//   }, allContexts.nonexistence
// )

// display("--Quadrant 3: alternative context, nonexistence referent--")
// var allResults = mapIndexed(
//   function(i, ctxt){
//     display(i + " out of 3 with apples")
//     viz.table(
//       speaker(allReferents.nonexistence, ctxt, "apples?")
//       )
//   }, allContexts.alternative
// )

// display("--Quadrant 4: alternative context, alternative referent--")
// var allResults = mapIndexed(
//   function(i, ctxt){
//     display(i + " out of 3 with apples")
//     viz.table(
//       speaker(allReferents.alternative, ctxt, "apples?")
//       )
//   }, allContexts.alternative
// )

''

~~~~


