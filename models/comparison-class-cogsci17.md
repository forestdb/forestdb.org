---
layout: model
title: Comparison classes (CogSci 2017)
model-status: hidden
model-category: Miscellaneous
model-language: webppl
model-language-version: v0.9.6
---

This page shows the models used in Ref:tesslerComparisonClassCogSci.


### Listener model with comparison class uncertainty

~~~~
///fold:
var exp = function(x){return Math.exp(x)}

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

var binParam = 5; // for discretization

var standardNormal = {mu: 0, sigma: 1}

var stateVals = map(
  round,
  _.range(standardNormal.mu - 3 * standardNormal.sigma,
          standardNormal.mu + 3 * standardNormal.sigma,
          standardNormal.sigma/binParam)
);

var stateProbs = cache(function(stateParams){
  return map(function(s){
    exp(Gaussian(stateParams).score(s))+ Number.EPSILON
  }, stateVals)
});

var generateStatePrior = cache(function(stateParams) {
  return Infer({
    model: function(){ return categorical({vs: stateVals, ps: stateProbs(stateParams)}) }
  })
});

var thresholdBins = cache(function(form, stateSupport){
  return map(function(x){
    return form == "positive" ? x - (1/(binParam*2)) : x + (1/(binParam*2));
  }, sort(stateSupport))
})

var thresholdPrior = cache(function(form, stateSupport){
  return Infer({
    model: function() { return uniformDraw(thresholdBins(form, stateSupport)) }
  });
}); // uninformed prior over the semantic threshold


var utterances = {
  positive: ["positive_null",
             "positive_sub",
             "positive_super"],
  negative: ["negative_null",
             "negative_sub",
             "negative_super"]
}; // alternative utterances depend on the adjective form

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

var classPrior = Infer({
  model: function(){return uniformDraw(["sub", "super"])}
}); // assume a uniform prior over comparison classes

///
var alphas = {s1: 3, s2: 1};

var literalListener = cache(function(u, thresholds, comparisonClass, subordinateParams) {
  Infer({model: function(){
    // if the comparison class is explicit in the utterance, use that
    // otherwise, use whatever the pragmaticListener model passes in
    var cc = u.split("_")[1] == "null" ?
        comparisonClass :
    u.split("_")[1] == "silence" ?
        comparisonClass :
    u.split("_")[1]

    var stateParams = cc == "super" ? standardNormal : subordinateParams

    var state = sample(generateStatePrior(stateParams));
    var utterance = u.split("_")[0]
    var m = meaning(utterance, state, thresholds);
    condition(m);
    return state;
  }})
})

var speaker1 = cache(function(state, thresholds, comparisonClass, form, subordinateParams) {
  Infer({model: function(){
    var utterance = sample(utterancePrior(form))
    var L0 = literalListener(utterance, thresholds, comparisonClass, subordinateParams)
    factor( alphas.s1 * L0.score(state) )
    return utterance
  }})
})

var pragmaticListener = function(form, subordinateParams) {
  Infer({model: function(){
    var utterance = form + "_null";
    var comparisonClass = sample(classPrior);
    var statePrior = generateStatePrior(subordinateParams);
    var state = sample(statePrior);
    var thresholds = form == "positive" ? {
      positive: sample(thresholdPrior("positive", statePrior.support()))
    } : {
      negative: sample(thresholdPrior("negative", statePrior.support()))
    }
    var S1 = speaker1(state, thresholds, comparisonClass, form, subordinateParams);
    observe(S1, utterance);
    return comparisonClass
  }})
}

var subParams = {
  low: {mu: -1, sigma: 0.5},
  middle: {mu: 0, sigma: 0.5},
  high: {mu: 1, sigma: 0.5}
}

viz(pragmaticListener("positive", subParams))
~~~~
