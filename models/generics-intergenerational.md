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


~~~~
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
  return Math.floor(x*20)/20
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

var observer = cache(function(obs) {
  Infer({method: "enumerate"}, function(){
    var state = sample(statePrior)
    var prevalence = state.prevalence;
    observe(
      Binomial({p: prevalence, n: (obs.positive + obs.negative)}), 
      obs.positive)
    return state
  })
})

var learner = cache(function(utterance, obs) {
  Infer({method: "enumerate"}, function(){
    var state = sample(statePrior)
    var prevalence = state.prevalence;
    var threshold = thresholdPrior();
    var S1 = speaker1(prevalence, threshold)
    utterance ? observe(S1, utterance) : null
    obs ? observe(
    Binomial({p: prevalence, n: (obs.positive + obs.negative)}), 
    obs.positive) : null
  return state
})
})

var speaker = function(utterance, obs){
  Infer({method: "enumerate"}, function(){
    var prior = learner(utterance, obs);
//     var prevalence = snap(expectation(marginalize(prior, "prevalence")));

        var state = sample(prior);
        var prevalence = state.prevalence;;    

    // alterative generic utterances vs. individual statements (maybe quanitifers)
    var utterance = utterancePrior();    

    var L1 = marginalize(learner(utterance), "prevalence");
    factor(alpha_2 * L1.score(prevalence));

    return utterance
  })
}

var initObs = {positive: 1, negative:100};
display('listener prior expectation = ' + expectation(statePrior, function(x){return x.prevalence}))
viz.marginals(statePrior)

var s2_0 = speaker(false, false);
display('speaker prior of saying generic = ' + Math.exp(s2_0.score("generic")))

var obsPosterior = observer(initObs)
display('observer posterior expectation = ' + expectation(obsPosterior, function(x){return x.prevalence}))
viz.marginals(obsPosterior)
viz.table(marginalize(obsPosterior, "prevalence"))

var s2_1 = speaker(false, initObs);
display('speaker gen 1 = ' + Math.exp(s2_1.score("generic")))

var L1_1 = Infer({model: function(){
  var u = sample(s2_1);
  return sample(learner(u, false));
}})

viz.marginals(L1_1)
viz.table(marginalize(L1_1, "prevalence"))

display('listener gen 1 = ' + expectation(L1_1, function(x){return x.prevalence}))

var s2_2 =  Infer({model: function(){
    var state = sample(L1_1);
    var prevalence = state.prevalence;
//   condition(prevalence > 0)
//   var prevalence = snap(expectation(marginalize(L1_1, "prevalence")));

  var utterance = utterancePrior();    
  var L1 = marginalize(learner(utterance), "prevalence");
  factor(alpha_2 * L1.score(prevalence));
  return utterance
} })

display('speaker gen 2 = ' + Math.exp(s2_2.score("generic")))

var L1_2 = Infer({model: function(){
  var u = sample(s2_2);
  return sample(learner(u, false))
}})
viz.marginals(L1_2)
display('listener gen 2 = ' + expectation(L1_2, function(x){return x.prevalence}))

var s2_3 =  Infer({model: function(){
    var state = sample(L1_1);
    var prevalence = state.prevalence;
//   var prevalence = snap(expectation(marginalize(L1_2, "prevalence")));
//   condition(prevalence > 0)

  var utterance = utterancePrior();    
  var L1 = marginalize(learner(utterance), "prevalence");
  factor(alpha_2 * L1.score(prevalence));
  return utterance
} })

display('speaker gen 3 = ' + Math.exp(s2_3.score("generic")))

~~~~
