---
layout: model
title: Cushman & Hii Generics Extension
model-language: webppl
---

# Extension of Generic Model: Turning the knob of threshold prior 
###### Model Extension: Isaiah Cushman and Doreen Hii
###### Author: Doreen Hii
___
Generics are utterances that express generalization on a category (Carlson, 1977; Leslie, 2008). Generics have been considered the special cases of utterance due to their amazing flexibility when it comes to truth judgement. For example, people endorse generics with varying prevalences. Generally, both generic sentences "Swans are white" and "Mosquitoes carry malaria" are endorsed although their conjecture prevalences are θ ≈ .5 and θ ≈ .01 respectively. However, generics are not as flexible at other times as in the case of sentence pair "Robins lay eggs" (θ ≈ .5) and "Robins are females" (θ ≈ .5) where the underlying prevalences are comparable. People generally endorse the former but not the latter. 

Tessler & Goodman (2016) suggested that generics are not unique from other language components. Just as the other language components, modeling generics can be successful given we take into consideration the context a generic sentence refers. Defining generics as a simple threshold semantics, Tessler & Goodman (2016) captured endorsement behaviors of generic sentences by inferring the sentences in context. The authors incorporated context by introducing different prevalence priors (i.e. listener's knowledge of the world) upon hearing a generic sentence.

~~~~
// prior distribution parameters resembling shared belief 
// for context "carry malaria"
var prior = priorModel({
  potential: 0.01,
  prevalenceWhenPresent: 0.02,
  concentrationWhenPresent: 5
})

// prior distribution parameters resembling shared belief 
// for context "are females"
var prior = priorModel({
  potential: .8,
  prevalenceWhenPresent: 0.5,
  concentrationWhenPresent: 5
})
~~~~
The incorporation of relevant priors coupled with an underspecified threshold semantics successfully modeled human prevalence posterior beliefs and endorsement behaviors upon hearing generics. Since the goal was to justify that generics are sensitive to context (captured as prevalence priors), the authors did not include any adjustments of what could be a powerful "dial" on the model: manipulating the threshold prior, θ. For simplicity, they kept threshold prior, θ uniform. 

>"In principle, thresholds could be learned over time for different contexts, but here we assume the listener has no informative knowledge about the semantic variable: θ ∼ Uniform([0, 1])." (Tessler & Goodman, 2016). 

We investigated the independent contribution of manipulating threshold priors as well as the interaction between threshold priors and prevalence priors. 

***
Cognitively, threshold prior represents the shared prior knowledge on the genre of the generics under discussion. For example, accidental or striking generics (such as "Ks eat people") may assume a lower threshold value θ than majority characteristics or body parts generics (such as "Ks have wings"). In an extreme case of striking generic, it will be informative for a speaker to utter a generic sentence even if there exist only 1 K that eats people. On the other extreme, a generic sentence about body parts will be informative only if almost all Ks have wings. Different genres of generics assume different threshold priors.

Without loss of generality, we can simplify the space of possible thresholds to 10 bins where the thresholds change in increments of 0.1. We reduced the number of possible bins to obtain a more intuitive and manageable size for threshold manipulation.
~~~~
var bins = map(function(x){
  _.round(x, 2);
},  _.range(0.01, 1, 0.1));

var Thresholds = bins
print(Thresholds)
~~~~
Knowing that there are 10 bins of threshold, we manipulated different threshold priors using a multinomial distribution with probabilities *prob* evaluated at every threshold bin. 

~~~~
var thresholdPrior = function(){
  // example of prob for constructing threshold prior
  var prob = [1,2,3,4,5,6,7,8,9,10]
  return categorical(prob, Thresholds)
}
~~~~
We suggested 3 plausible threshold priors:
~~~
var thresholdPrior1 = function(){
  // may be interpreted as "matters-if-any"
  // may be applied for striking or accidental properties
  var prob1 = [10,9,8,7,6,5,4,3,2,1]
  return categorical(prob1, Thresholds)
}
var thresholdPrior2 = function(){
  // may be interpreted as "about-half"
  // serves as a midpoint between 
  // matters-if-any and matters-if-most
  var prob2 = [1,2,3,4,5,5,4,3,2,1]
  return categorical(prob2, Thresholds)
}
var thresholdPrior3 = function(){
  // may be interpreted as "matters-if-most"
  // may be applied for genetically related properties
  var prob3 = [1,2,3,4,5,6,7,8,9,10]
  return categorical(prob3, Thresholds)
}
~~~
To independently examine the effect of manipulating threshold prior, we assumed a uniform prevalence prior while varying the threshold prior. 

We defined generics by a simple threshold semantics as implemented by the original model. Literal listener (L0), speaker (S1), pragmatic listener (L1), and pragmatic speaker (S2) perform the same computations with that of the original model except pragmatic listener (L1) samples a threshold prior from informative threshold prior instead of a uniform threshold prior. 

~~~~
///fold:
// discretized range between 0 - 1
var bins = map(function(x){
  _.round(x, 2);
},  _.range(0.01, 1., 0.1));

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

var alpha_1 = 1;
var alpha_2 = 1;
///

var utterances = ["generic", "silence"];
var utterancePrior = function() { return uniformDraw(utterances) }

var Thresholds = bins
var thresholdPrior_baseline = function(){
  var prob_base = [1,1,1,1,1,1,1,1,1,1]
  return categorical(prob_base, Thresholds)
}
var thresholdPrior1 = function(){
  var prob1 = [10,9,8,7,6,5,4,3,2,1]
  return categorical(prob1, Thresholds)
}
var thresholdPrior2 = function(){
  var prob2 = [1,2,3,4,5,5,4,3,2,1]
  return categorical(prob2, Thresholds)
}
var thresholdPrior3 = function(){
  var prob3 = [1,2,3,4,5,6,7,8,9,10]
  return categorical(prob3, Thresholds)
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
    var threshold = thresholdPrior_baseline()
    // var threshold = thresholdPrior1()
    // var threshold = thresholdPrior2()
    // var threshold = thresholdPrior3()
    var S1 = speaker1(prevalence, threshold, statePrior)
    observe(S1, utterance)
    return {prevalence: prevalence, threshold: threshold}
  }})
})

// uniform
var prior = priorModel({
  potential: 1,
  prevalenceWhenPresent: .5,
  concentrationWhenPresent: 2
})

print("L1 upon hearing generic")
var L1Results = pragmaticListener("generic", prior)
print('implied prevalence')
print (expectation(marginalize(L1Results, "prevalence")))
print('implied threshold')
print (expectation(marginalize(L1Results, "threshold")))

print("L1 upon hearing silence")
var L1Results = pragmaticListener("silence", prior)
print('implied prevalence')
print (expectation(marginalize(L1Results, "prevalence")))
print('implied threshold')
print (expectation(marginalize(L1Results, "threshold")))
~~~~
Manipulating threshold prior from *thresholdPrior_baseline* to *thresholdPrior1*, *thresholdPrior2* and *thresholdPrior3* while keeping prevalence prior constant results in different prevalence posterior. We verified the effect of manipulating threshold prior since it is able to affect prevalence posterior. 

Specifically, comparing prevalence posterior of *thresholdPrior1* or "matters-if-any" threshold with the baseline uniform threshold prior, prevalence posterior and assenting prevalence upon hearing a generic decreased. Listener with the "matters-if-any" threshold prior arrived at a lower prevalence posterior and endorsed generics at a lower implied prevalence upon hearing a generic because of the shared knowledge on how the speaker chooses her utterance. Generics will be uttered at lower threshold levels. For prevalence prior of "about-half" and "matters-if-most," prevalence posterior and assenting prevalence upon hearing a generic are higher when compared to that of uniform threshold prior. The listener arrived at a higher prevalence posterior and endorsed generics at a higher implied prevalence if generics are uttered to describe majority or most Ks having the property F.   

For ease of manipulation, we wrapped all three threshold priors into a helper function, which further allows combinations of threshold prior.
~~~
var sigPrior = function(){
  return categorical([1,1,1],["matters-if-any", "about-half", "matters-that-most"])
}

var sigs = {
  "matters-if-any": [10,9,8,7,6,5,4,3,2,1],
  "about-half": [1,2,3,4,5,5,4,3,2,1],
  "matters-that-most": [1,2,3,4,5,6,7,8,9,10]
}

var Thresholds = bins

var thresholdPrior = function(sig){
  return categorical(sig, Thresholds)
}
~~~

Different threshold priors may interact with prevalence priors to model more specific instances of generics. For example, hypothetical prevalence priors for generic sentences "Ks have wings" and "Ks pose a threat" may share a similar bimodal prevalence prior.

~~~
// bimodal
var prior = priorModel({
  potential: .5,
  prevalenceWhenPresent: 1,
  concentrationWhenPresent: 5
})
~~~
However, when property "have wings" is concerned, the threshold prior associated is intuitively higher than that for "pose a threat," since it will be more informative for a listener to know if Ks harm people at a lower threshold level. 

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

var alpha_1 = 1;
var alpha_2 = 1;
///

var utterances = ["generic", "silence"];
var utterancePrior = function() { return uniformDraw(utterances) }

var sigPrior = function(){
  // hypothetical sigPrior for property "have wings"
  // most people would want to know if about-half 
  // or most of Ks have wings
  return categorical([0,10,10],["matters-if-any", "about-half", "matters-that-most"])
  
  // hypothetical sigPrior for property "pose a threat"
  // most people would want to know if any Ks pose a threat
  // return categorical([10,0,0],["matters-if-any", "about-half", "matters-that-most"])
}

var sigs = {
  "matters-if-any": [10,9,8,7,6,5,4,3,2,1],
  "about-half": [1,2,3,4,5,5,4,3,2,1],
  "matters-that-most": [1,2,3,4,5,6,7,8,9,10]
}

///fold:
var Thresholds = bins

var thresholdPrior = function(sig){
  return categorical(sig, Thresholds)
  // return categorical([1,1,1,1,1,1,1,1,1,1], Thresholds) //original model threshold prior

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
///

// bimodal
var prior = priorModel({
  potential: .5,
  prevalenceWhenPresent: 1,
  concentrationWhenPresent: 5
})

viz.density(prior)
print("expectation of prior: " + expectation(prior))

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

* * * 
##### Possibility offered by manipulating threshold priors: New perspective to approach divergent threshold priors

In the original model, Tessler & Goodman (2016) modeled generics in the format "Ks have F," where Ks were restricted to animals and Fs were properties of animals. It was consequential to restrict the space as it removed possible complication for cases when a shared prevalence prior was not obvious, as in the case of "human activities cause global warming." 

However, a possible workaround to the divergent prevalence prior is to manipulate threshold prior. The question for threshold prior focuses on the usefulness of generics under different circumstances rather than the correctness of generics under different prevalence levels. It may be that people have stable threshold priors regarding different contexts (e.g. a left-skewed distribution generally resembles striking properties). The divergence of prevalence priors emerge because of different categorization of the property (some categorize "cause global warming" as matters-if-any or striking and others categorized it as matters-if-most). 

To gather empirical data for threshold priors, a hypothetical experiment will be to ask "From a scale of 1-10, would you rather generic sentence on 'K cause global warming' be uttered under circumstance: (A) matters if any (B) about half (C\) matters if most?" Threshold priors may be modeled as multinomial distribution to capture the different threshold priors across participants.

~~~
// A reasonable threshold priors for "cause global warming" 
// There are some weights on either end of the spectrum 
// (extreme environmentalists and extreme anti-environmentalists) 
// while the majority falls in the middle of the spectrum
var sigPrior = function(){
  return categorical([5,10,5],["matters-if-any", "about-half", "matters-that-most"])
}
~~~
Comparing the above threshold distribution with the original model's uniform threshold prior (and assuming an uninformative prevalence prior), `var thresholdPrior = function() { return uniformDraw(thresholdBins) };` individuals modeled with the multinomial distributed threshold prior arrived at a higher prevalence posterior and assenting prevalence upon hearing a generic sentence. 

In this world, listeners are more reluctant to endorse the generic at a lower prevalence level. However, if the world concerns only extreme environmentalists, (i.e. sigPrior distribution is `return categorical([10,0,0],["matters-if-any", "about-half", "matters-that-most"])` listeners are more willing to endorse generic at lower prevalence level.

Manipulating threshold prior independently affects prevalence posterior and generic endorsement. For cases where informative prevalence priors are unavailable, manipulating threshold prior (if collecting threshold priors are reasonable) brings us closer to the picture. On the other hand, for cases where informative prevalence priors are available, manipulating threshold prior distinguishes properties that may share similar prevalence priors.

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
  return categorical([5,10,5],["matters-if-any", "about-half", "matters-that-most"])
}

var sigs = {
  "matters-if-any": [10,9,8,7,6,5,4,3,2,1],
  "about-half": [1,2,3,4,5,5,4,3,2,1],
  "matters-that-most": [1,2,3,4,5,6,7,8,9,10]
}

var Thresholds = bins

var thresholdPrior = function(sig){
  return categorical(sig, Thresholds)
  // return categorical([1,1,1,1,1,1,1,1,1,1], Thresholds) //original model threshold prior

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

// uniform
var prior = priorModel({
  potential: 1,
  prevalenceWhenPresent: .5,
  concentrationWhenPresent: 2
})

viz.density(prior)
print("expectation of prior: " + expectation(prior))

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