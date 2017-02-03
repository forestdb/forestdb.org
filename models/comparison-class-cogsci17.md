---
layout: model
title: Comparison classes (CogSci 2017)
model-status: hidden
model-category: Miscellaneous
model-language: webppl
model-language-version: v0.9.6
---

This page shows the models used in Ref:tesslerComparisonClassCogSci.


## Rational Speech Act model

This model gives the listener the ability to infer the comparison class when it is not specified in the utterance.
When the utterance does use an explicit comparison class (e.g., "He's short for a basketball player"), the listener uses the explicitly mentioned comparison class.

### Experiment 1: Comparison class inference.

In this task, participants are told that the target is a member of the subordinate category (e.g., "John sees a basketball player").
The listener then hears the ambiguous utterance "He's tall".
Listener reasons about the likely comparison class.
We model this with the `pragmaticListener` given the underspecified utterance.

### Experiment 2: Adjective production

In this task, participants are told that the target is a member of the subordinate category (e.g., "John sees a basketball player").
They are asked to endorse the adjective sentence with the comparison class explicitly the superordinate category (e.g., "He's tall relative to other people").
We model this as the `speaker2` who can say either the adjective sentence with an explicit comparison class or remain silent.

Links to experiments and project repository can be found [here](https://mhtess.github.io).

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

var KL = function(p, q) {
  var supp = sort(p.support());
  var P = distProbs(p, supp), Q = distProbs(q, supp);
  var diverge = function(xp,xq) {
    return xp == 0 ? 0 : (xp * Math.log(xp / xq) );
  };
  return sum(map2(diverge,P,Q));
};

var marginalize = function(dist, key){
  return Infer({model: function(){sample(dist)[key]}})
}
var binParam = 5; // for discretization

var superordinate = {mu: 0, sigma: 1};

var stateVals = map(
  round,
  _.range(superordinate.mu - 3 * superordinate.sigma,
          superordinate.mu + 3 * superordinate.sigma,
          superordinate.sigma/binParam)
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

// alternative utterances depend on expt
// expt 1 (comparison class inferences) uses explicit paraphrases
// expt 2 (adjective endorsement) is a standard truth judgment alternative set
var utterances = {
  1: {
    positive: ["positive_null", "positive_sub", "positive_super"],
    negative: ["negative_null", "negative_sub", "negative_super"]
  },
  2: {
    positive: ["positive_super", "silence_super"],
    negative: ["negative_super", "silence_super"]
  }
}

var meaning = function(utterance, state, threshold) {
  utterance == "positive" ? state > threshold ? flip(0.9999) : flip(0.0001) :
  utterance == "negative" ? state < threshold ? flip(0.9999) : flip(0.0001) :
  true
}

var classPrior = Infer({
  model: function(){return uniformDraw(["sub", "super"])}
}); // assume a uniform prior over comparison classes

var alphas = {s1: 3, s2: 1};

var literalListener = cache(
  function(u, threshold, comparisonClass, subordinate) {
    Infer({model: function(){
      var utterance =  u.split("_")[0], explicitCC =  u.split("_")[1]
      // if the comparison class is explicit in the utterance, use that
      // otherwise, use whatever the pragmaticListener model passes in
      var cc = explicitCC == "null" ?  comparisonClass :
      explicitCC == "silence" ? comparisonClass : explicitCC

      var state = sample(generateStatePrior(cc === "super" ? superordinate : subordinate));
      var m = meaning(utterance, state, threshold);
      condition(m);
      return state;
    }})
  }
)

var speaker1 = cache(
  function(state, threshold, comparisonClass, form, subordinate, task) {
    Infer({model: function(){
      var utterance = uniformDraw(utterances[task][form]);
      var L0 = literalListener(utterance, threshold, comparisonClass, subordinate);
      factor( alphas.s1 * L0.score(state) );
      return utterance;
    }})
  }
)

var pragmaticListener = function(utterance, form, subordinate, task) {
  Infer({model: function(){
    var explicitCC = utterance.split("_")[1];
    // listener knowledge: expt 1 === "sub"; expt 2 === "super"
    var statePrior = generateStatePrior(
      task === 1 ? subordinate : superordinate
    );
    var state = sample(statePrior);
    var threshold = sample(thresholdPrior(form, statePrior.support()))
    // comparison class is implicit in expt 1 (therefore, sample)
    // explicit in expt 2 ("super")
    var c = task === 1 ? sample(classPrior) : explicitCC;

    var S1 = speaker1(state, threshold, c, form, subordinate, task);
    observe(S1, utterance);
    return { comparisonClass: c, state: state }
  }})
}

var speaker2 = function(form, subordinate){
  Infer({model: function(){
    var speakerBeliefs = generateStatePrior(subordinate);

    var utterance = uniformDraw(utterances[2][form])
    var L1 = pragmaticListener(utterance, form, subordinate, 2);

    var beliefDistance = KL(speakerBeliefs, marginalize(L1, "state"));

    factor(-1 * alphas.s2* beliefDistance)

    return utterance == form + "_super"
  }})
}

var subParams = {
  low: {mu: -1, sigma: 0.5},
  middle: {mu: 0, sigma: 0.5},
  high: {mu: 1, sigma: 0.5}
}

var exptConditions = [
  {utt: "positive_null", form: "positive", sub: "high"},
  {utt: "negative_null", form: "negative", sub: "high"},
  {utt: "positive_null", form: "positive", sub: "middle"},
  {utt: "negative_null", form: "negative", sub: "middle"},
  {utt: "positive_null", form: "positive", sub: "low"},
  {utt: "negative_null", form: "negative", sub: "low"}
];

var L1predictions = map(function(stim){
  var L1posterior = pragmaticListener(stim.utt,stim.form, subParams[stim.sub], 1)
  return {
    y: exp(marginalize(L1posterior, "comparisonClass").score("super")),
    x: stim.form,
    sub: stim.sub,
    model: "L1"
  }
}, exptConditions)

viz.table(L1predictions)

var S2predictions = map(function(stim){
  var S2posterior = speaker2(stim.form, subParams[stim.sub])
  return {
    y: expectation(S2posterior),
    x: stim.form,
    sub: stim.sub,
    model: "S2"
  }
}, exptConditions)

viz.table(S2predictions)
~~~~

## Bayesian data analysis model

~~~~norun
var dataAnalysis = function(){
  var alphas = {
    expt1 : { s1: uniform(0, 20) },
    expt2 : {
      s1: uniform(0, 20),
      s2: uniform(0, 5)
    }
  };

  var frequencyScale = uniform(0, 3);

  foreach(subcategories, function(subCat){
    var mu = uniform(-3, 3);
    var sigma = uniform(0, 5);

    var subCatPrior = Gaussian({mu: mu, sigma: sigma})

    var subCatWeight = exp(
      frequencyScale * empiricalFrequency[subCat]
    )
    var superCatWeight = exp(
      frequencyScale * empiricalFrequency[superCat]
    )

    var classPrior = Categorical({
      vs: ["sub", "super"], ps: [subCatWeight, superCatWeight]
    })

    foreach(["positive","negative"], function(utterance){

      var expt1predictions = pragmaticListener(utterance, subCatPrior, classPrior, ...)

      mapData({data: data.expt1.subCat}, function(d){
        observe(expt1predictions, d)
        })

      var expt2predictions = speaker2(utterance, subCatPrior, ...)

      mapData({data: data.expt2.subCat}, function(d){
        observe(expt2predictions, d)
      })
    })
  })
  return [expt1predictions, expt2predictions, alphas, ...]
}
~~~~
