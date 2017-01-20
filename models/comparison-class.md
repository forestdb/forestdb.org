---
layout: model
title: Comparison classes
model-status: hidden
model-category: Miscellaneous
model-language: webppl
model-language-version: v0.9.6
---

### Listener 2 model

+ `L2` has uncertainty about `cc` but knows the degree is coming from the `"sub` prior
+ `S2` is trying to communicate belief distribution (the `"sub"` distribution), but is assuming some comparison class
+ `L1` and below is straight-up adjectives

~~~~
///fold:
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

var binParam = 4;

var stateParams = {
    sub: {mu: 1, sigma: 0.25},
    super: {mu: 0, sigma: 0.5}
}

var stateVals = map(
  function(x){return round(x) },
  _.range(stateParams.super.mu - 3 * stateParams.super.sigma,
          stateParams.super.mu + 3 * stateParams.super.sigma,
          stateParams.super.sigma/binParam)
);

var stateProbs = {
  sub: map(function(s){
    exp(Gaussian(stateParams.sub).score(s))+
    Number.EPSILON
  }, stateVals),
  super: map(function(s){
    exp(Gaussian(stateParams.super).score(s))+
    Number.EPSILON
  }, stateVals)
};

var statePrior = {
  sub: Infer({
//     model: function(){ return categorical({vs: map(exp, stateVals), ps: stateProbs.sub}) }
    model: function(){ return categorical({vs: stateVals, ps: stateProbs.sub}) }
  }),
  super: Infer({
    model: function(){ return categorical({ vs: stateVals, ps: stateProbs.super}) }
//     model: function(){ return categorical({ vs: map(exp, stateVals), ps: stateProbs.super}) }
  })
};

var thresholdBins ={
  positive: map(function(x){
    return  x - (1/(binParam*2));
  }, sort(statePrior.super.support())),
  negative: map(function(x){
    return  x + (1/(binParam*2));
  }, sort(statePrior.super.support()))
};

var thresholdPrior = cache(function(form){
  return Infer({
    model: function() { return uniformDraw(thresholdBins[form]) }
  });
});
///

var alphas = {s1: 15, s2: 5};

var utterances = {
//  positive: ["positiveAdjective", "silence"],
  positive: ["positiveAdjective","negativeAdjective", "silence"],
  negative: ["negativeAdjective", "positiveAdjective","silence"]
};

var utterancePrior = cache(function(form){
  return Infer({
    model: function() { return uniformDraw(utterances[form]) }
  })
});

var meaning = function(utterance, state, threshold) {
  utterance == "positiveAdjective" ? state > threshold ? flip(0.9999) : flip(0.0001) :
  utterance == "negativeAdjective" ? state < threshold ? flip(0.9999) : flip(0.0001) :
  true
}

var classPrior = Infer({
  model: function(){return flip(0.5) ? "sub" : "super"}
});

var literalListener = cache(function(utterance, threshold, comparisonClass) {
  Infer({model: function(){
    var state = sample(statePrior[comparisonClass]);
    var m = meaning(utterance, state, threshold);
    condition(m);
    return state;
  }})
}, 10000)

var speaker1 = cache(function(state, threshold, comparisonClass, form) {
  Infer({model: function(){
    var utterance = sample(utterancePrior(form))
    var L0 = literalListener(utterance, threshold, comparisonClass)
    factor( alphas.s1 * L0.score(state) )
    return utterance
  }})
}, 10000)

var pragmaticListener = function(utterance, form, comparisonClass) {
  Infer({model: function(){
    var formForThreshold = {
      "positiveAdjective": "positive", "negativeAdjective": "negative"
    }
    var state = sample(statePrior[comparisonClass]);
    var threshold = utterance == "silence" ? -99 :
        sample(thresholdPrior(formForThreshold[utterance]));

    var S1 = speaker1(state, threshold, comparisonClass, form);
    observe(S1, utterance);
    return state
  }})
}

var speaker2 = function(form, cc){
  Infer({model: function(){
    var speakerBeliefs = statePrior["sub"];
    var utterance = sample(utterancePrior(form));
    var L1 = pragmaticListener(utterance, form, cc);
    var _kl = KL(speakerBeliefs, L1, speakerBeliefs.support());
    factor(alphas.s2 * -1 * _kl)
    return utterance
    }})
}

var listener2 = function(form) {
  Infer({model: function(){
    var utterance = form + "Adjective";
    var comparisonClass = sample(classPrior)
    var state = sample(statePrior["sub"]);
    var S2 = speaker2(form, comparisonClass);
    observe(S2, utterance);
    return comparisonClass
  }})
}

display("superordinate degree prior")
viz.hist(statePrior.super);
display("subordinate degree prior")
viz.hist(statePrior.sub);

display("tall (basketball player) --> for a person = " + Math.exp(listener2("positive").score("super")))
display("short (basketball player) --> for a person =  " + Math.exp(listener2("negative").score("super")))
~~~~
