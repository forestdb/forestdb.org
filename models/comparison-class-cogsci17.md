---
layout: model
title: Comparison class understanding (CogSci 2017)
model-status: hidden
model-category: Miscellaneous
model-language: webppl
model-language-version: v0.9.6
---

This page shows the models used in Ref:tesslerComparisonClassCogSci.


### Listener model with comparison class uncertainty

~~~~
///fold:
////// HELPER FUNCTIONS
var round = function(x){
  return Math.round(x*10)/10
}

var distProbs = function(dist, supp) {
  return map(function(s) {
    return Math.exp(dist.score(s))
  }, supp)
}

var KL = function(p, q, supp) {
  var P = distProbs(p, supp), Q = distProbs(q, supp);
  var diverge = function(xp,xq) {
    return xp == 0 ? 0 : (xp * Math.log(xp / xq) );
  };
  return sum(map2(diverge,P,Q));
};

var exp = function(x){return Math.exp(x)}
///////


////// STATE PRIOR CONSTRUCTION (discretized Gaussians)
var binParam = 5;

var stateParams = {
  sub: {mu: 1, sigma: 0.5},
  super: {mu: 0, sigma: 1}
};

var stateVals = map(
  round,
  _.range(stateParams.super.mu - 3 * stateParams.super.sigma,
          stateParams.super.mu + 3 * stateParams.super.sigma,
          stateParams.super.sigma/binParam)
);

var stateProbs = {
  sub: map(function(s){
    Math.exp(Gaussian(stateParams.sub).score(s))+
    Number.EPSILON
  }, stateVals),
  super: map(function(s){
    Math.exp(Gaussian(stateParams.super).score(s))+
    Number.EPSILON
  }, stateVals)
};

var statePrior = {
  sub: Infer({
    model: function(){ return categorical({vs: stateVals, ps: stateProbs.sub}) }
  }),
  super: Infer({
    model: function(){ return categorical({ vs: stateVals, ps: stateProbs.super}) }
  })
};
//////


var thresholdBins ={
  positive: map(function(x){
    return  x - (1/(binParam*2));
  }, sort(statePrior.super.support())),
  negative: map(function(x){
    return  x + (1/(binParam*2));
  }, sort(statePrior.super.support()))
};

// uninformed prior over the semantic threshold
var thresholdPrior = cache(function(form){
  return Infer({
    model: function() { return uniformDraw(thresholdBins[form]) }
  });
});

// alternative utterances depend on the adjective form
var utterances = {
  positive: ["positive_null",
             "positive_sub",
             "positive_super"],
  negative: ["negative_null",
             "negative_sub",
             "negative_super"]
};

var utteranceProbs = [1, 1, 1];
var utterancePrior = cache(function(form){
  return Infer({
    model: function() {
      return categorical({
        vs: utterances[form],
        ps: utteranceProbs
      })
    }
  })
});

var meaning = function(utterance, state, thresholds) {
  utterance == "positive" ? state > thresholds.positive ? flip(0.9999) : flip(0.0001) :
  utterance == "negative" ? state < thresholds.negative ? flip(0.9999) : flip(0.0001) :
  true
}

// assume a uniform prior over comparison classes
var classPrior = Infer({
  model: function(){return uniformDraw(["sub", "super"])}
});
///
var alphas = {s1: 3, s2: 1};

var literalListener = cache(function(u, thresholds, comparisonClass) {
  Infer({model: function(){
    // if the comparison class is explicit in the utterance, use that
    // otherwise, use whatever the pragmaticListener model passes in
    var cc = u.split("_")[1] == "null" ?
        comparisonClass :
    u.split("_")[1] == "silence" ?
        comparisonClass :
    u.split("_")[1]    

    var state = sample(statePrior[cc]);
    var utterance = u.split("_")[0]
    var m = meaning(utterance, state, thresholds);
    condition(m);
    return state;
  }})
}, 10000)

var speaker1 = cache(function(state, thresholds, comparisonClass, form) {
  Infer({model: function(){
    var utterance = sample(utterancePrior(form))
    var L0 = literalListener(utterance, thresholds, comparisonClass)
    factor( alphas.s1 * L0.score(state) )
    return utterance
  }})
}, 10000)

var pragmaticListener = function(form) {
  Infer({model: function(){
    var utterance = form + "_null";
    var comparisonClass = sample(classPrior);
    var state = sample(statePrior["sub"]);
    var thresholds = form == "positive" ? {
      positive: sample(thresholdPrior("positive"))
    } : {
      negative: sample(thresholdPrior("negative"))
    }
    var S1 = speaker1(state, thresholds, comparisonClass, form);
    observe(S1, utterance);
    return comparisonClass
  }})
}

viz(pragmaticListener("positive"))
~~~~
