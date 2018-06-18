---
layout: model
title: Cushman & Hii Generics Extension
model-language: webppl
---

# Introducing variable threshold priors to the generics model
---

### Generics, generally
A generic utterance is a statement of the form “Category (K) have Feature (F),” absent any quantifier. Examples include “fish have gills,” or “flamingoes stand on one leg.”

We can use a relatively simple prevalence and threshold model to describe how generics may be used between a listener and a speaker. Under such a model, if the prevalence of F among K exceeds some threshold, the utterance is true.

This view is complicated by the fact that the apparent threshold for acceptable use in conversational English varies substantially depending on the categories and the features in question— it seems just as acceptable to say “mosquitoes carry malaria” as to say “fish have gills,” even though wildly different proportions of these groups have those respective properties.

Furthermore, it may also be that the context in which a generic statement is made impacts its acceptability. If I believe it matters if even a very small fraction of K have F, that could reasonably impact at what prevalence I think it’s correct to use the generic. For example, “apple pie has walnuts in it” may receive higher endorsement in a context where it’s known that one of the parties is allergic to walnuts, without anything changing about the global or within-pie prevalence of walnuts.
 
Tessler and Goodman (cited below, manuscript available [here](https://arxiv.org/abs/1608.02926)) built a Rational Speech Act model to describe and predict the differing endorsement of generic statements across categories and features, work which forms the entire basis for this project. In this model, they built in a potential handle by which a notion of ‘context’ could be introduced to the model, in the form of prior beliefs over threshold levels, but left it unexplored. This project investigates what happens when that model feature is adjusted.

### RSA, generally

The Rational Speech Act (RSA) framework is, at its core, a tool for modeling communication between agents who consider not only what was said, but what could have been said instead.

It encodes a notion of shared communicative objectives, in that higher layers include a model of lower layers, and a notion of shared prior knowledge, in that different model layers draw samples from common probability distributions.

Exploiting common knowledge and mutual informativity allow Tessler and Goodman, and consequently our own adapted model, to produce predictions for generics endorsement which take into account existing prior beliefs, as we have determined must happen to account for the irregular interpretation of generic utterances. The particulars of this model diverge in some notable ways from the 'vanilla' RSA model.

### The generics model’s special ingredients:

The generics model is built around two key ingredients: a state space consisting of possible prevalence levels, and a threshold semantics.

A given feature F could exist in a variable proportion of the population of K, ranging from 0.01 to 1. The possible values that proportion could take is our prevalence state space, which we segment into increments of  0.1

~~~~
var bins = map(function(x){
  _.round(x, 2);
},  _.range(0.01, 1., 0.1));
~~~~

Now, we may wish to say that “K have F” is true only if that proportion exceeds some threshold value. Under a threshold semantics, the literal truth of a generic utterance is determined by comparing a prevalence level from out state space against a particular threshold value. If the threshold is exceeded, the generic statement is regarded as true. In this model, this meaning function is evaluated only when the utterance is a generic.

~~~~
var meaning = function(utterance, prevalence, threshold) {
  return (utterance == 'generic') ? prevalence >= threshold: true
}
~~~~

These two features are assembled into the generics model, as detailed in the following overview.

### Generics model outline:

To interpret a generic utterance, we step through the stages of the RSA model:
First, a common prior distribution over our state space (prevalences from 0 to 1) is established, reflecting the likelihood that a given kind has that proportion of its members possessing a given property. The values of this prior will condition the frequency with which given states are returned in all listener layers of this model.

In this model, those prior distributions are built from a mixture of two beta distributions— the first places all probability mass on 0.01 prevalence, while the second places a distribution of probability mass around some other target prevalence. The ratio of the mixture indicates “potential for the property to exist,” while the prevalence about which the second distribution is centered indicates the “property prevalence, when present.” The code for generating such a distribution follows.

~~~~

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
~~~~

The L1 layer accepts an utterance, samples a threshold value from a uniform prior and a prevalence value from its prevalence prior, then generates a prediction (in the form of a distribution over utterances) about what S1 should say, given that prevalence and threshold. To the extent that what S1 should say matches what L1 did hear, L1 assigns a higher or lower level of probability to the true prevalence value being what it sampled on that cycle.

~~~~
var pragmaticListener = cache(function(utterance, statePrior) {
  Infer({model: function(){
    var prevalence = sample(statePrior)
    var threshold = uniformDraw(bins)
    var S1 = speaker1(prevalence, threshold, statePrior)
    observe(S1, utterance)
    return {prevalence: prevalence, threshold: threshold, sig: sig}
  }})
})
~~~~

To determine what S1 ‘should say,’ the S1 layer accepts a prevalence and a threshold, samples an utterance from a uniform prior, and generates a prediction about what L0 would believe (this time, a distribution over states) if it heard that threshold and utterance. S1 returns a distribution over utterances, with each weighted in proportion to how likely L0 is to believe the correct thing after hearing it.

~~~~
var speaker1 = cache(function(prevalence, threshold, statePrior) {
  Infer({model: function(){
    var utterance = utterancePrior()
    var L0 = literalListener(utterance, threshold, statePrior)
    factor( alpha_1 * L0.score(prevalence) )
    return utterance
  }})
})
~~~~

To determine what L0 ‘would believe,’ the L0 layer accepts an utterance and a threshold value, then samples a prevalence level from the common prevalence prior. If the utterance is ‘generic’ and the sampled value falls below the given threshold, L0 will not return any prevalence value. In all other cases, it will return the prevalence value it sampled from the prior.

~~~~
var literalListener = cache(function(utterance, threshold, statePrior) {
  Infer({model: function(){
    var prevalence = sample(statePrior)
    var m = meaning(utterance, prevalence, threshold)
    condition(m)
    return prevalence
  }})
})
~~~~

Above all of this exists an S2 layer, which accepts a specific prevalence, and compares which utterance would cause L1 to produce assign higher probability to that prevalence. This final step is what we can think of as “endorsement” — ultimately, does the model believe that the generic is a good way to describe the state of the world to a savvy listener.

~~~~
var speaker2 = function(prevalence, statePrior){
  Infer({model: function(){
    var utterance = utterancePrior();
    var L1Results = pragmaticListener(utterance, statePrior);
    var L1 = marginalize(L1Results,"prevalence")
    factor( alpha_2 * L1.score(prevalence) )
    return utterance
  }})
}
~~~~
### Our addition: variable threshold priors

We attempt to capture the notion that agents could mutually understand the standards for generics usage to be biased towards primarily high or primarily low prevalences by considering alternative distributions over the threshold prior. These biases are notated as “sigs” in this code, to stand for ‘the significance of the utterance.’ The bias categories are termed “matters if any,” “majority,” and “matters that most,” to denote situations where a speaker would think it important to mention that ‘K has F’ if even a very few possessed F, a majority did, or if nearly all of K would need F before it bore mentioning. For our model, the particular values were generated manually, and bear no special significance besides the general skewedness that they represent.

~~~~
var sigWeights = [1,1,1]

var sigPrior = function(){
  return categorical(sigWeights, ["matters-if-any", "majority", "matters-that-most"])
}

var sigs = {
  "matters-if-any": [10,9,8,7,6,5,4,3,2,1],
  "majority": [1,2,3,4,5,5,4,3,2,1],
  "matters-that-most": [1,2,3,4,5,6,7,8,9,10]
}
~~~~

Now, instead of drawing a threshold uniformly from the state space, we can draw them in proportion to the distributions detailed above.

We also explore the possibility that upon hearing the generic, a listener could consider what it believes about the distribution of prevalences and predict what bias a speaker most likely has. To capture this, we let L1 sample from possible sigs, and compare which threshold prior distributions would best account for the observed utterance.

~~~~
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
~~~~

### A summary of the results

When all of these components are compiled together, we get a model that lets us show a few simple intuitions, now capturable in the model:

1. Fixing all beliefs about the actual prevalence of features among kinds, generics endorsement about those features and kinds can be increased by biasing the threshold priors lower or higher. If, say, having poisonous barbs and having prehensile toes had the same estimated prevalence distributions, yet distinct endorsement rates this bias could capture that.
2. Given observed usage of the generic, our model can infer not only that the threshold for generics usage was lower, but that perhaps the threshold was biased to be lower. If the model observed “K have poisonous barbs” being endorsed at higher rates, it could now infer that even low rates of poisonous barbs are noteworthy.

The following code will illustrate these two points when executed.
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

var utterances = ["generic", "silence"];

var utterancePrior = function() { return uniformDraw(utterances) }

var sigWeights = [1,1,1]

var sigPrior = function(){
  return categorical(sigWeights,["matters-if-any", "majority", "matters-that-most"])
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
    factor(L0.score(prevalence) )
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
    factor( L1.score(prevalence) )
    return utterance
  }})
}

// malaria
var malariaPrior = priorModel({
  potential: 0.01,
  prevalenceWhenPresent: 0.01,
  concentrationWhenPresent: 5
})

// female
var femalePrior = priorModel({
  potential: .9,
  prevalenceWhenPresent: 0.5,
  concentrationWhenPresent: 5
})

var prior = femalePrior

print('Section I: Comparing endorsement rates across threshold priors.')

print("S2's distribution over utterances (endorsement):")
viz(speaker2(.61,prior))

//Comparing Sig predictions across priors:

print('Section II: Comparing "sig" predictions across prevalence priors.')
var prior = femalePrior
print("L1's distribution over states upon hearing a generic, given 'female' prior:")
var L1Results = pragmaticListener("generic", prior)

viz(marginalize(L1Results,'sig'))
print("'female' prior:")
viz(prior)

var prior = malariaPrior
print("L1's distribution over states upon hearing a generic, given 'malaria' prior:")
var L1Results = pragmaticListener("generic", prior)

viz(marginalize(L1Results,'sig'))
print("'malaria' prior:")
viz(prior)

///
~~~~

Running the code above, we see points 1 and 2 from above illustrated. For point 1, one must adjust the values of “sigWeights,” for example to [10,1,1]. When this is done, it is clear that endorsement rates change independently of state priors, in the direction one would predict.

For point 2, the contrast is present in the code’s output— usage of the generic in situations where typical prevalence is very low points much more strongly to the “matters-if-any” distribution over thresholds, in which low thresholds are more likely, corresponding with the idea that "it must be worthy of my attention that this group has this feature."

### In conclusion

While the modifications we were able to make to the model without breaking it beyond our ability to repair it turned out to be quite modest relative to my ambitions, we were able to illustrate that the threshold prior feature of the generics model does impact its predictions in a way which can track with reasonable intuitions regarding what drives generics acceptance and interpretation.

### Referenced work
- A Pragmatic Theory of Generic Language. Tessler, Michael Henry and Goodman, Noah D. (Manuscript). arXiv:1608.02926.
- https://gscontras.github.io/probLang/




 
 




