---
layout: model
title: Conjunctive generics
model-status: hidden
model-category: Miscellaneous
model-language: webppl
model-language-version: v0.9.7
---

### Elephants live in Africa and Asia

Nickel (2008) poses the following challenge to majority based theories of generics
- "Elephants live in Africa and Asia"

There are two possible readings:
- Gen(x): Elephant(x) --> Lives in Africa and Asia(x)
- Gen(x): Elephant(x) --> Lives in Africa && Gen(x): Elephant(x) --> Lives in Asia

The first is troublesome because we have to commit to international elephants.
The second is troubling (for the "majority based" views) because it cannot be the case that most elephants live in Africa and most elephants live in Asia.

I think the uncertain threshold view can deal with this okay, provided we extend the model to interpret multiple generic statements.
After hearing the first utterance, "Elephants live in Africa", the listener forms some posterior belief distribution over the threshold.
For the second utterance, we use a different threshold.
The interesting inference comes out of the fact that living in Africa and living in Asia are mutually exclusive properties.
The inference should be that at least that:
(a) some live in Africa and some live in Asia, and more elaborately:
(b) possibly most live in Africa, and if so, fewer live in Asia, or
(c) possibly most in Asia, and if so, fewer live in Africa  
[Note: (b) and (c) are statements about the joint-posterior]



~~~~
// structure of state prior:
// "mainPlace" denotes place where elephants are
// --> unlikely to be true for both africa and asia

var probability = function(Dist, x) {
  return Math.exp(Dist.score(x));
}

var bins = [
  0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,
  0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99
];

var theta = 1, priorParams = {a: 1, b: 1}, nullParams = {a:1,b:10};

var mainComponent = Categorical({
  vs: bins,
  ps: map(function(b) {
    probability(Beta(priorParams), b) + Number.EPSILON
  }, bins )
})

var nullComponent = Categorical({
  vs: bins,
  ps: map(function(b) {
    probability(Beta(nullParams), b) + Number.EPSILON
  }, bins )
})

var statePrior = Infer({model: function(){
  var mainPlace = categorical({
    ps:[1, 1, 1, 0.001],
    vs:[
      {asia: true, africa: false}, {asia: false, africa: true},
      {asia: false, africa: false}, {asia: true, africa: true}
    ]
  });
  return {
    asia: sample(mainPlace.asia ? mainComponent : nullComponent),
    africa: sample(mainPlace.africa ? mainComponent : nullComponent)
  }
}})

viz(statePrior)
viz.marginals(statePrior)
~~~~

This is an uncertain threshold, literal listener model that can update sequentially upon hearing generic conjunctions.
Here, I explore "Elephants live in Asia", and then "Elephants live in Africa".
This is meant to correspond to "Elephants live in Asia and Africa".

~~~~
var probability = function(Dist, x) {
  return Math.exp(Dist.score(x));
}
var marginalize = function(dist, key){
  return Infer({model: function(){sample(dist)[key]}})
}

var bins = [
  0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,
  0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99
];

var theta = 1, priorParams = {a: 1, b: 1}, nullParams = {a:1,b:100};

var mainComponent = Categorical({
  vs: bins,
  ps: map(function(b) {
    probability(Beta(priorParams), b) + Number.EPSILON
  }, bins )
})

var nullComponent = Categorical({
  vs: bins,
  ps: map(function(b) {
    probability(Beta(nullParams), b) + Number.EPSILON
  }, bins )
})

var statePrior = Infer({model: function(){
  var mainPlace = categorical({
    ps:[1, 1, 1, 0.001],
    vs:[
      {asia: true, africa: false}, {asia: false, africa: true},
      {asia: false, africa: false}, {asia: true, africa: true}
    ]
  });
  return {
    asia: sample(mainPlace.asia ? mainComponent : nullComponent),
    africa: sample(mainPlace.africa ? mainComponent : nullComponent)
  }
}})

var thetaPrior = Infer({model: function(){
  return {
    asia: uniformDraw(
      [0.01, 0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,
       0.5, 0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95]
    ),
    africa: uniformDraw(
      [0.01, 0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,
       0.5, 0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95]
    )
  }
}});

var meaning = function(utt, state, theta) {
  state[utt] > theta[utt]
}

var listener0 = function(utterance, worldPriors, thresholdPriors) {
  Infer({model: function(){
    var state = sample(worldPriors)
    var theta = sample(thresholdPriors)
    var m = meaning(utterance, state, theta)
    condition(m)
    return {state, theta}
  }})
}

display("state priors'")
viz(statePrior)
viz.marginals(statePrior)

var elephantsLiveInAsia = listener0("asia", statePrior, thetaPrior);

var asiaStatePosterior = marginalize(elephantsLiveInAsia, "state");

var asiaThresholdPosterior =  marginalize(elephantsLiveInAsia, "theta");

display("state posteriors after hearing 'Elephants live in Asia'")
viz(asiaStatePosterior);
viz.marginals(asiaStatePosterior);

display("threshold posteriors after hearing 'Elephants live in Asia'")
viz(asiaThresholdPosterior);
viz.marginals(asiaThresholdPosterior)

var elephantsLiveinAfricaAndAsia = listener0("africa", asiaStatePosterior, asiaThresholdPosterior)

var AfricaAndAsiaStatePosterior = marginalize(elephantsLiveinAfricaAndAsia, "state");

var AfricaAndAsiaThresholdPosterior =  marginalize(elephantsLiveinAfricaAndAsia, "theta");

display("state posteriors after hearing 'Elephants live in Africa and Asia'")
viz(AfricaAndAsiaStatePosterior);
viz.marginals(AfricaAndAsiaStatePosterior);

display("threshold posteriors after hearing 'Elephants live in Africa and Asia'")
viz(AfricaAndAsiaThresholdPosterior);
viz.marginals(AfricaAndAsiaThresholdPosterior);
~~~~
