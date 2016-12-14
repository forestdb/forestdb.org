---
layout: model
title: Generics across generations
model-status: hidden
model-category: Miscellaneous
model-language: webppl
model-language-version: v0.9.6
---

# Question

Do mosquitos carry malaria? 
How do you know?
You probably didn't learn this through trial-and-error, nor did you conduct laboratory experiments on the topic.
Probably, somebody told you.
You learned this through language.

But then how did they learn that?
Through language as well, and up through the generations, until somebody (or a collective of people over time) gathered data and drew the generalization.

This model explores what happens when an observer gathers data (positive and negative examples of something), makes some infernece about the prevalence of the positive examples (e.g., the prevalence of the feature in the kind), and then decides whether or not to say a generic statement to a listener who shares his prior beliefs.

Then, a population of listeners, having been exposed to the population of speakers represented by the probability distribution over producing the generic, updates their beliefs about the prevalence, and decides whether or not to say the generic. And on and on.

The prior here is a hypothetical prior over some rare property (e.g., carrying malaria). 
The question is: can intergenerational transmission retain the belief in the generic utterance (e.g., "Mosquitos carry malaria") while still appreciating that not-all (or even, not many) mosquitos carry malaria.

~~~~
///fold:
var speakerParams = {
  typesOfInidividuals: {
    ps: [0.95, 0.05], vs: ["x0", "x1"]
  },
  rates: {
    x0: { a: 0.1, b: 100 } ,
    x1: { a: 1, b: 4  }
  }
};

var snap = function(x){
  return Math.round(x*20)/20
}
var marginalize = function(dist, key){
  return Infer({method: "enumerate"}, function(){ return sample(dist)[key] })
}

var acrossKind = function(typesOfIndividuals, rates){
  var individualType = categorical({ps: typesOfIndividuals.ps, 
                                    vs: typesOfIndividuals.vs});
  var rate = beta(rates[individualType])

  //   return  snap(rate)
  return {component: individualType, prevalence: snap(rate)}
}

var statePrior = Infer({method: "forward", samples: 10000}, 
                       function(){ acrossKind(speakerParams.typesOfInidividuals, speakerParams.rates) })


var alpha_1 = 5;
var alpha_2 = 1;

var utterances = ["generic", "silence"];

var thresholdBins = map(function(x){
  return x > (1/40) ? x - (1/40) : x;
}, sort(marginalize(statePrior, "prevalence").support()));

var thresholdPrior = function() { return uniformDraw(thresholdBins) };
var utterancePrior = function() { return uniformDraw(utterances) }

var meaning = function(utterance, prevalence, threshold) {
  return (utterance == "generic") ? prevalence > threshold : true
}

var literalListener = cache(function(utterance, threshold) {
  Infer({method: "enumerate"}, function(){
    var state = sample(statePrior)
    var prevalence = state.prevalence;
    var m = meaning(utterance, prevalence, threshold);
    condition(m)
    return prevalence
  })
})

var speaker1 = cache(function(prevalence, threshold) {
  Infer({method: "enumerate"}, function(){
    var utterance = utterancePrior()
    var L0 = literalListener(utterance, threshold)
    factor( alpha_1*L0.score(prevalence) )
    return utterance
  })
})
///

var learner = cache(function(utterance, obs) {
  Infer({method: "enumerate"}, function(){
    var state = sample(statePrior)
    var prevalence = state.prevalence;
    var threshold = thresholdPrior();
    var S1 = speaker1(prevalence, threshold)
    utterance ? observe(S1, utterance) : null
    obs ? observe(Binomial({
    p: prevalence, n: (obs.positive + obs.negative)}), 
    obs.positive) : null
  return state
})
})

var learnerPopulation = function(speakerDist){
  Infer({model: function(){
    var utt = sample(speakerDist);
    return sample(learner(utt, false));
  }})
}

var speaker = function(prevalenceBeliefs){
  Infer({model: function(){
    var state = sample(prevalenceBeliefs);

    // try me:  condition(state.prevalence > 0)
    // alternatively: var state = {prevalence: snap(expectation(prevalenceBeliefs,
    //     function(x){return x.prevalence}))}
    
    // generic or silence
    var utterance = utterancePrior();    
    var L1 = marginalize(learner(utterance), "prevalence");

    factor(alpha_2 * L1.score(state.prevalence));

    return utterance
  }})
}

var initObs = {positive: 1, negative:100};

// speaker distribution based on the prior
var s2_0 = speaker(statePrior);

// prevalence distribution after observations
var obsPosterior = learner(false, initObs)

// speaker distribution based on posterior after observations
var s2_1 = speaker(obsPosterior);

// prevalence distribution after population of generic speakers
var L1_1 = learnerPopulation(s2_1);

// speaker distribution based on posterior after generics
var s2_2 = speaker(L1_1);

var L1_2 = learnerPopulation(s2_2);
var s2_3 = speaker(L1_2)
var L1_3 = learnerPopulation(s2_3);
var s2_4 = speaker(L1_3)
var L1_4 = learnerPopulation(s2_4);
var s2_5 = speaker(L1_4)
var L1_5 = learnerPopulation(s2_5);
var s2_6 = speaker(L1_5)

viz.line(
  [0,1,2,3,4,5,6],
  map(function(b){
    return expectation(b, function(c){return c.prevalence})
  }, [statePrior, obsPosterior, L1_1, L1_2, L1_3, L1_4, L1_5])
)

viz.line(
  [0,1,2,3,4,5,6],
  map(function(a){
    return Math.exp(a.score("generic"))
  }, [s2_0, s2_1, s2_2, s2_3, s2_4, s2_5, s2_6])
)

display('prior on prevalence')
viz.marginals(statePrior)
display('prevalence dist after observations')
viz.marginals(obsPosterior)
display('prevalence dist after generation of generic speakers')
viz.marginals(L1_1)
display('gen 2')
viz.marginals(L1_2)
display('gen 3')
viz.marginals(L1_3)
display('gen 4')
viz.marginals(L1_4)
display('gen 5')
viz.marginals(L1_5)

~~~~
