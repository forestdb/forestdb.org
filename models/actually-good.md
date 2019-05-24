---
layout: model
title: Microaggressive gradable adjectives
model-language: webppl
---



Data to eventually model (?): people's politeness judgments on sentences like  "You are actually good at math".

Collected also: other demographic info (like political party  affiliation) to predict what types of people are more likely to derive the "microagression" interpretation i.e., finding the use of "actually" to be offensive".


From Ciyang:

> I think one can start with a very simplified assumption: "you are actually good at math" is truth-conditionally equivalent to "you are good at math" except that it has a higher cost. This should be enough for the listener in a wonky world model to infer that the speaker's prior expectation that the listener is good at math must be low to justify the higher cost. Roughly speaking, we would hope to capture the following mapping between prior expectation and the utterance.

Prior expectation  |   Utterance
High               |          Say nothing
Mid                |           you are good at math
Low                |          you are actually good at math


~~~~
// helper functions

var round = function(x){
  return Math.round(x*100)/100
}

var isNegation = function(utt){
  return (utt.split("_")[0] == "not")
};

var avoidEnds = function(x){
  return x >= 1 ? 0.99 : x == 0 ? 0.01 : x
}

var lb = -3, ub = 3, diff = 0.2;
var bins = _.range(lb, ub + diff, diff)

var DiscreteGaussian = function(distParams){
  Infer({model: function(){
    categorical({
      vs:bins,
      ps:map(function(x){Math.exp(Gaussian(distParams).score(x))}, bins)
    })
  }})
}

var DiscreteBeta = function(a, b){
  Infer({model: function(){
    categorical({
      vs:bins,
      ps:map(function(x){
        Math.exp(Beta({a, b}).score(avoidEnds(x)))
      }, bins)
    })
  }})
}
////


var utterances = [
  "good",
  "actually good",
  "silence"
];

var cost_word = 10;

var numWords = function(utt){
  utt.split(' ').length
}

var uttProbs = map(function(u) {
  var cost = u == "silence" ? 0 : (cost_word * numWords(u))
  return Math.exp(-cost)
}, utterances)

var utterancePrior = Categorical({vs:utterances, ps: uttProbs})

var meaning = function(words, state, thresholds){
  return words == "good" ? state > thresholds.good :
  words == "actually good" ? state > thresholds.good :
  true
};

var speakerOptimality = 10;

var listener0 = cache(function(utterance, thresholds, priorParams) {
  Infer({model: function(){
    var state = sample(DiscreteGaussian(priorParams));
    var m = meaning(utterance, state, thresholds);
    condition(m);
    return state;
  },  method: "enumerate"})
}, 10000);

var speaker1 = cache(function(state, thresholds, priorParams) {
  Infer({model: function(){
    var utterance = sample(utterancePrior);
    var L0 = listener0(utterance, thresholds, priorParams);
    factor(speakerOptimality*L0.score(state));
    return utterance;
  },  method: "enumerate"})
}, 10000);

var lower_bound_semantic_threshold_bins = _.range(lb, ub, diff)
var upper_bound_semantic_threshold_bins = _.range(lb+diff, ub+diff, diff)

var hypothesisSpaceOfPriors = [
  {mu: -1, sigma: 1},
  {mu: 0, sigma: 1},
  {mu: 1, sigma: 1}
]

// Lassiter adjectives model
var listener1 = cache(function(utterance, priorParams) {
  Infer({model: function(){
    // var priorParams = uniformDraw(hypothesisSpaceOfPriors)
    var thresholds = {
      good: uniformDraw(lower_bound_semantic_threshold_bins)
    }
    // display(priorParams)
    var state = sample(DiscreteGaussian(priorParams));
    var S1 = speaker1(state, thresholds, priorParams)
    observe(S1, utterance)
    return state  
  }, method: "enumerate"})
}, 10000);

var speakerOptimality2 = 2
var speaker2 = cache(function(state, priorParams) {
  Infer({model: function(){
    var utterance = sample(utterancePrior);
    var L1 = listener1(utterance, priorParams)
    factor(speakerOptimality2 * L1.score(state) )
    return utterance
  }})
}, 10000)
//
var listener2 = function(utterance) {
  Infer({model: function(){
    var priorParams = uniformDraw(hypothesisSpaceOfPriors)
    var state = sample(DiscreteGaussian({mu:0, sigma: 1}))
    // var state = sample(DiscreteGaussian(priorParams))
    var S2 = speaker2(state, priorParams);
    observe(S2, utterance);
    return priorParams.mu
  }})
}

// var priorParams = hypothesisSpaceOfPriors[0]
// var stateprior = DiscreteGaussian(priorParams)
// map(function(s){
//   display(JSON.stringify(speaker2(s, priorParams)))
// }, sort(stateprior.support()))


display(expectation(listener2("good")))
display(expectation(listener2("actually good")))
~~~~
