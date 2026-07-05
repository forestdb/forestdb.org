---
layout: model
title: Scoring Rules
model-status: code
model-category: Agents, Games, and Social Reasoning
model-tags: mechanism design, game theory
model-language: webppl
model-language-version: pre-v0.7
---

A model comparing scoring rules used to elicit an agent's belief distribution over three events, where the agent chooses what to report in order to maximize its expected score under a linear, logarithmic, or quadratic rule. Only the logarithmic and quadratic rules turn out to be incentive-compatible, inducing the agent to report its true belief.

~~~~
///fold:
var Infer = function(thunk){
  return Enumerate(thunk);
}

var Expectation = function(thunk){
  return expectation(Infer(thunk));
}

var makeCategorical = function(vs, ps){
  return window.makeCategoricalERP(ps, vs);
};

var prob = function(erp, val) {
  return Math.exp(erp.score([], val))
}

var squared = function(x) {
  return Math.pow(x, 2)
}
///

var distPrior = Enumerate(function(){
  var potentialPs = [
    [.5, .3, .2],
    [.3, .5, .2],
    [.2, .3, .5],
    [0.005, .99, 0.005]
  ];
  var ps = uniformDraw(potentialPs);
  return makeCategorical(['A', 'B', 'C'], ps);
});

var alpha = 100;


var linearScore = function(dist, event) {
  // not incentive-compatible!
  return prob(dist, event);
}

var logScore = function(dist, event) {
  return Math.log(prob(dist, event));
}

var quadraticScore = function(dist, event) {
  var squares = map(function(x){return squared(prob(dist, x))}, dist.support());
  return 2 * prob(dist, event) - sum(squares);
}

var beliefDist = makeCategorical(['A', 'B', 'C'], [.3, .5, .2]);

var agent = function(scoreFn){
  return Infer(function(){
    var reportDist = sample(distPrior);
    var expectedScore = Expectation(function(){
      var event = sample(beliefDist);
      return scoreFn(reportDist, event);
    });
    console.log(expectedScore)
    factor(alpha * expectedScore);
    return reportDist;
  });
}

print('Actual belief distribution:');
print(beliefDist)

print('Reported distribution under LOG score:');
print(agent(logScore).MAP().val)

print('Reported distribution under QUADRATIC score:');
print(agent(quadraticScore).MAP().val)

print('Reported distribution under LINEAR score:');
print(agent(linearScore).MAP().val)
~~~~
