---
layout: model
title: Cushman & Hii Generics Extension
model-language: webppl
---

Extension of Generic Model: Turning the knob of threshold prior

In the original model:
Generics were defined using a simple threshold semantics.
The goal was to justify that generics are sensitive to context (captured as a prior). For simplicity, they did not include any adjustments of what could be a powerful 'dial' on the model: manipulating the threshold prior, θ.

~~~~
var thresholdPrior = function() { return uniformDraw(thresholdBins) };
~~~~

"In principle, thresholds could be learned over time for different contexts, but here we assume the listener has no informative knowledge about the semantic variable: θ ∼ Uniform([0, 1])." (Tessler & Goodman, 2011). 

We are going to turn that dial.

What does the original thresholdPrior sample? What is the output? 

~~~~
// discretized range between 0 - 1
var bins = map(function(x){
  _.round(x, 2);
},  _.range(0.01, 1, 0.02));

var thresholdBins = map2(function(x,y){
  var d = (y - x)/ 2;
  return x + d
}, bins.slice(0, bins.length - 1), bins.slice(1, bins.length))

var thresholdPrior = function() { return uniformDraw(thresholdBins) };

var runThreshold = function(){
  Infer({model: thresholdPrior})
}

print ("Sample space for threshold: " + thresholdBins)

print("threshold prior")
viz.hist(runThreshold())
~~~~

For simplicity, we are going to reduce the space of possible thresholds from 49 down to 10. The thresholds change in increments of 0.1. 

~~~~
var bins = map(function(x){
  _.round(x, 2);
},  _.range(0.01, 1, 0.1));

var Thresholds = bins
print(Thresholds)
~~~~

Now we have a manageable size for threshold manipulation.

~~~~
var thresholdPrior = function(){
  var prob = [1,2,3,4,5,6,7,8,9,10]
  return categorical(prob, Thresholds)
}
~~~~

~~~~
///fold:
// discretized range between 0 - 1
var bins = map(function(x){
  _.round(x, 2);
},  _.range(0.01, 1., 0.1));

// var thresholdBins = map2(function(x,y){
//   var d = (y - x)/ 2;
//   return x + d
// }, bins.slice(0, bins.length - 1), bins.slice(1, bins.length))

// function returns a discretized Beta distribution
var DiscreteBeta = cache(function(g, d){
  var a =  g * d, b = (1-g) * d;
  var betaPDF = function(x){
    return Math.pow(x, a-1)*Math.pow((1-x), b-1)
  }
  var probs = map(betaPDF, bins);
  return Categorical({vs: bins, ps: probs})
})

var priorModel = function(params){
  Infer({model: function(){

    var potential = params["potential"]
    var g = params["prevalenceWhenPresent"]
    var d = params["concentrationWhenPresent"]

    var StableDistribution = DiscreteBeta(g, d)
    var UnstableDistribution = DiscreteBeta(0.01, 100)

    var prevalence = flip(potential) ?
        sample(StableDistribution) :
    sample(UnstableDistribution)

    return prevalence
  }})
}
///

var alpha_1 = 1;
var alpha_2 = 1;
var utterances = ["generic", "silence"];

var utterancePrior = function() { return uniformDraw(utterances) }

var sigPrior = function(){
  return categorical([1,1,1],["matters-if-any", "majority", "matters-that-most"])
}

var sigs = {
  "matters-if-any": [10,9,8,7,6,5,4,3,2,1],
  "majority": [1,2,3,4,5,5,4,3,2,1],
  "matters-that-most": [1,2,3,4,5,6,7,8,9,10]
}

var Thresholds = bins

var thresholdPrior = function(sig){
  return categorical(sig, Thresholds)
}

var meaning = function(utterance, prevalence, threshold) {
  return (utterance == 'generic') ? prevalence >= threshold: true
}

var literalListener = cache(function(utterance, threshold, statePrior) {
  Infer({model: function(){
    var prevalence = sample(statePrior)
    var m = meaning(utterance, prevalence, threshold)
    condition(m)
    return prevalence
  }})
})

var speaker1 = cache(function(prevalence, threshold, statePrior) {
  Infer({model: function(){
    var utterance = utterancePrior()
    var L0 = literalListener(utterance, threshold, statePrior)
    factor( alpha_1 * L0.score(prevalence) )
    return utterance
  }})
})

var pragmaticListener = cache(function(utterance, statePrior) {
  Infer({model: function(){
    var prevalence = sample(statePrior)
    var sig = sigPrior()
    var threshold = thresholdPrior(sigs[sig])
    var S1 = speaker1(prevalence, threshold, statePrior)
    observe(S1, utterance)
    return {prevalence: prevalence, threshold: threshold, sig: sig}
  }})
})

var speaker2 = function(prevalence, statePrior){
  Infer({model: function(){
    var utterance = utterancePrior();
    var L1Results = pragmaticListener(utterance, statePrior);
    var L1 = marginalize(L1Results,"prevalence")
    factor( alpha_2 * L1.score(prevalence) )
    return utterance
  }})
}

var speakerExpectation = function(statePrior,bins) {
  Infer({model: function() {
    var prevalence = uniformDraw(bins)
    var speaker_dist = speaker2(prevalence, statePrior)
    var probGeneric = (Math.exp(speaker_dist.score('generic')))
    var keep = flip(probGeneric)
    condition(keep)
    return prevalence
  }})
}

// malaria
var prior = priorModel({
  potential: 0.01,
  prevalenceWhenPresent: 0.01,
  concentrationWhenPresent: 5
})

// female
// var prior = priorModel({
//   potential: 1,
//   prevalenceWhenPresent: 0.5,
//   concentrationWhenPresent: 5
// })

print("L1 upon hearing generic")
 var L1Results = pragmaticListener("generic", prior)
 print('implied prevalence')
print (expectation(marginalize(L1Results, "prevalence")))

viz.marginals(L1Results)

print("L1 upon hearing silence")
 var L1Results = pragmaticListener("silence", prior)
 print('implied prevalence')
print (expectation(marginalize(L1Results, "prevalence")))

viz.marginals(L1Results)

print('assent prevalence')
print(expectation(speakerExpectation(prior,bins)))
~~~~