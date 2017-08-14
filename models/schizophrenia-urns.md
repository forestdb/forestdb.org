---
layout: model
title: Urn model 
model-language: webppl
model-language-version: v0.9.9
---

~~~~
// total number of marbles drawn from urn every time
var nMarbles = 8;
var threshold = .6;

// example data point for self
var selfData = 4;

// example data point for others
var otherData = [{prediction: 'red', confidence: 'high'},
                 {prediction: 'red', confidence: 'high'},
                 {prediction: 'blue', confidence: 'low'},
                 {prediction: 'blue', confidence: 'low'}];

// (discretized) uniform distribution over actual proportion of red in urn
var rednessPrior = Categorical({vs: [0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1]});

/* 
  Generative model of other agents

  Assumes they are *also* doing inference about actual proportion 
  based on their data and responding according to their best guess... 
 */
var otherOutput = cache(function(kRed, threshold) {
  return Infer({method: 'enumerate', model: function() {
    var pRed = sample(rednessPrior);
    var prediction = flip(pRed) ? 'red' : 'blue';
    var highConf = prediction === 'red' ? pRed >= threshold : pRed <= 1 - threshold;

    observe(Binomial({p: pRed, n: nMarbles}), kRed);
    return {prediction: prediction, confidence: highConf ? 'high' : 'low'}
  }});
})

/* 
  Model of participant's inference on a given trial
*/ 
var trialModel = function() {
  // participant is trying to infer latent distribution in urn
  var pRed = sample(rednessPrior);
  
  // first, take into account own data (i.e. a draw of balls from urn)
  observe(Binomial({p: pRed, n: nMarbles}), selfData);
  
  // next, take into account social information 
  // assume their sample was drawn from sample population but not sure of exact data
  mapData({data: otherData}, function(datum) {
    var kSeenPrior = Binomial({p: pRed, n : nMarbles});
    var likelihood = expectation(kSeenPrior, function(k) {
      return otherOutput(k, threshold).score(datum)
    })
    factor(likelihood);
  })
  
  // ugly js convert to string for pretty plots
  return pRed + "";
}

viz.hist(Infer({method: 'enumerate', model: trialModel}))
~~~~