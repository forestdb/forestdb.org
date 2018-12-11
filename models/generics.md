---
layout: model
title: Generics
model-status: code
model-category: Reasoning about Reasoning
model-tags: language, generics, vagueness, pragmatics
model-language: webppl
model-language-version: v0.9.9
---

This page is taken from the chapter on generic language from the online web-book [Probabilistic Language Understanding](http://www.problang.org).

Consider the following sentences:

1. All swans are white.
2. Most swans are white.
3. Some swans are white.
4. *Swans are white.*

Is there some relation between I - III & IV?

~~~~
var literalMeanings = {
  some: function(x){ x > 0 },
  most: function(x){ x > 0.5 },
  all: function(x){ x == 1 },
  generic: function(x, theta){ x > theta }
}
var meaningFn = literalMeanings["generic"]
meaningFn(0.6, 0.5)
~~~~

> **Exercise**: Try out the different meaning functions with different inputs to make sure you understand threshold-semantics. What should `theta` be?

### The logical problem

Generic language (e.g., *Swans are white.*) is a simple and ubiquitous way to communicate generalizations about categories.  Linguists, philosophers, and psychologists have scratched their collective heads for decades, trying to figure out what makes a generic sentence true or false. At first glance, generics feel like universally-quantified statements, as in *All swans are white*.  Unlike universals, however, generics are resilient to counter-examples (e.g., *Swans are white* even though there are black swans).  Our intuitions then fall back to something more vague like *Swans, in general, are white* because indeed most swans are white. But mosquitos, in general, do not carry malaria, yet everyone agrees *Mosquitos carry malaria*.

Indeed, it appears that any truth conditions stated in terms of how common the property is within the kind violates intuitions. Consider the birds: for a bird, being female practically implies you will lay eggs (the properties are present in the same proportion), yet we say things like *Birds lay eggs* and we do not say things like *Birds are female*.

~~~~
var theta = 0.49
var generic = function(x){ x > theta }
var number_of_birds_that_lay_eggs = 0.5;
var number_of_birds_that_are_female = 0.5;
var number_of_mosquitos_that_carry_malaria = 0.02;

display("Birds lay eggs is true ? " + generic(number_of_birds_that_lay_eggs))
display("Birds are female is true ? " + generic(number_of_birds_that_are_female))
display("Mosquitos carry malaria is true ? " + generic(number_of_mosquitos_that_carry_malaria))
''
~~~~

reft:tessler2016manuscript propose that the core meaning of a generic statement is in fact a threshold as in `generic` above, but underspecified (listener has uncertainty about `theta`).
Then we can use a Bayesian model to resolve a more precise meaning in context.

### Bayesian generic language interpretation

The model takes the generic $$[\![\text{K has F}]\!]$$ to mean the prevalence of property F within kind K is above some threshold: $$P(F \mid K) > \theta$$ (cf., Cohen 1999).
But for the generic, no fixed value of the $$\theta$$ would suffice.
Instead, we leave the threshold underspecified in the semantics ($$\theta \sim \text{Uniform}(0, 1)$$) and infer it in context.

We could write this as the following:

~~~~
var listener = function(utterance) {
  Infer({model: function(){
    var prevalence = sample(prevalencePrior)
    var theta = uniform(0, 1)
    condition( prevalence > theta)
    return prevalence
  }})
}
~~~~

Here, we have a uniform prior over `theta`.
What is `prevalence` though (and what is the `prevalencePrior`)?
Given that we've posited that the semantics of the generic are about the prevalence $$P(F \mid K)$$ (i.e., the `meaning()` function in the `literalListener` conditions on `prevalence > theta`) then what the listener updates her beliefs about is the prevalence $$P(F \mid K)$$.
<!-- So `x` is prevalence. -->

The listener samples `prevalence` from some prior `prevalencePrior`, which is a prior distribution over the prevalence of the feature.
Let's try to understand that.

### Prior model

Think of your favorite kind of animal.
Got one in mind?
What percentage of that kind of animal *is female*?
Probably roughly 50%, regardless of the kind of animal you thought of.
What percentage of that kind of animal *lays eggs*?
Well, it probably depends on the kind of animal you thought. If you thought of a falcon, then roughly 50% (recall, only the females lay eggs).
But if you thought of a bear, then 0% of them lay eggs.

We can conceive of the prior distribution over the prevalence of a feature with a kind $$P(F\mid K)$$ as a distribution over kinds $$P(K)$$ and then the prevalence of the feature within the kind.

~~~~
var allKinds = [
  {kind: "dog", family: "mammal"},
  {kind: "falcon", family: "bird"},
  {kind: "cat", family: "mammal"},
  {kind: "gorilla", family: "mammal"},
  {kind: "robin", family: "bird"},
  {kind: "alligator", family: "reptile"},
  {kind: "giraffe", family: "mammal"},
]

var kindPrior = function(){
  uniformDraw(allKinds)
}

var prevalencePrior = Infer({model:
  function(){
    var k = kindPrior()
    var prevalence =
        k.family == "bird" ? 0.5 :  // half of birds lay eggs
        k.family == "reptile" ? 0.2 : // i'm not really sure if reptiles lay eggs
        0 // no other thing lays eggs;

    return prevalence
  }
})

prevalencePrior
~~~~

> **Exercise**: What if you didn't know that exactly 50% of birds lay eggs? Generalize the above code to sample the prevalence of laying eggs for birds, reptiles, etc. from a distribution.

<!-- (Hint: The [Beta distribution](http://docs.webppl.org/en/master/distributions.html#Beta) is a distribution over numbers between 0 and 1.) -->

#### A generalization of the prior model

In the above model, we encoded the fact that people have knowledge about different types of categories (e.g., reptiles, mammals) and that this knowledge should give rise to different beliefs about the prevalence of the feature for a given kind.
More generally, if speakers and listeners believe that some kinds have a causal mechanism that *stably* gives rise to the property, while others do not, then we would expect the prior to be structured as a mixture distribution (cf., Griffiths & Tenenbaum, 2005).

For convenience, let us denote the relevant probability $$P(F \mid K)$$ as $$x$$.
The categories that have a stable causal mechanism produce the feature with some probability $$x_{stable}$$.
The categories that do not have a stable causal mechanism produce the feature with some probability $$x_{transient}$$ (perhaps this unstable mechanism is an external, environmental cause).
We would expect $$x_{transient}$$ to be small (even zero), as certain features are completely absent in many categories (e.g., the number of lions that lay eggs).
$$x_{stable}$$, on the other hand, could be large, giving rise to features that are often common in a kind (e.g., *has four legs*), but might also be substantially less than 1 for features that are non-universal in a category (e.g., *has brown fur*).

We formalize this idea by drawing $$x_{stable}$$ and $$x_{transient}$$ from Beta distributions (which has support between 0 - 1; thus samples from a Beta are numbers between 0 - 1 i.e., probabilities) with different parameters.
We fix the distribution for the transient cause: $$ x_{transient} \sim Beta(0.01, 100)$$.
(Here we use the mean--concentration parameterization of the Beta distribution rather than the canonical pseudocount parameterization. The first parameter is the mean of the distribution while the second is the concentration --- or inverse-variance --- of the distribution.)

What we plausibly can vary between contexts is the distribution for $$x_{stable} \sim Beta(\gamma, \delta)$$.
We also can vary how prevalent each component or sub-distribution is, by a parameter $$\phi$$.

Thus, the prior over $$x$$ is a mixture distribution:

$$
x \sim \phi \cdot \text{Beta}(\gamma, \delta) + (1 - \phi) \cdot \text{Beta}(0.01, 100)
$$

where $$\gamma$$ is the mean of the stable cause distribution and $$\delta$$ is the "concentration" (or, inverse-variance) of this distribution. $$\delta$$ is high for properties that present in almost every kind in exactly the same proportion (e.g. "is female"). It is lower when there is more uncertainty about exactly how many within a kind are expected to have the property.


<!-- $$\phi$$ is a parameter that governs mixture between these two components.
 For example, "is female" has a high `phi` to be present in a kind; while "lays eggs" has less potential (owing to the fact that a lot of animals do not have any members who lay eggs). "Carries malaria" has a very low potential to be present. `prevalenceWhenPresent` is the *mean prevalence when the property is present*. Knowing that the property is present in a kind, what % of the kind do you expect to have it? -->

These two components of the prior can be probed from human intuitions through two questions:

> We just discovered an animal on a far away island called a fep.
> 1. How likely is it that there is a *fep* that has wings? ($$\rightarrow \phi$$)
> 2. Suppose there is a fep that has wings, what % of feps do you think have wings? ($$\rightarrow \gamma; \rightarrow \delta$$)

(Run through your intuitions with other properties like "is female", or "lays eggs".)

The following model `priorModel` formalizes the above ideas computationally:

~~~~
///fold:
// discretized range between 0 - 1
var bins = _.range(0.01, 1, 0.025);

// function returns a discretized Beta distribution
var DiscreteBeta = cache(function(g, d){
  var a =  g * d, b = (1-g) * d;
  var betaPDF = function(x){
    return Math.pow(x, a-1)*Math.pow((1-x), b-1)
  }
  var probs = map(betaPDF, bins);
  return Categorical({vs: bins, ps: probs})
})
///
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

    return {prevalence}

  }})
}

// e.g. "Lays eggs"
viz(priorModel({
  potential: 0.3,
  prevalenceWhenPresent: 0.5, // how prevalent under the stable cause
  concentrationWhenPresent: 10   // the inverse-variance of the stable cause
}))
~~~~

> **Exercises:**
> 1. What does this picture represent? If you drew a sample from this distribution, what would that correspond to?
> 2. Try to think up a property for which the three parameters above are not able to give even a remotely plausible distribution. (If you succeed, let us know; the idea is that this parameterization is sufficient to capture---in approximation---any case of relevance.)

## Generic interpretation model

The model assumes a simple (the simplest?) meaning for a generic statement: a threshold on the probability (prevalence).

~~~~
///fold:
// discretized range between 0 - 1
var bins = map(function(x){
  _.round(x, 2);
},  _.range(0.01, 1, 0.02));

var thresholdBins = map2(function(x,y){
  var d = (y - x)/ 2;
  return x + d
}, bins.slice(0, bins.length - 1), bins.slice(1, bins.length))

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
var meaning = function(utterance, prevalence, threshold) {
  return (utterance == 'generic') ? prevalence > threshold : true
}
var thresholdPrior = function() { return uniformDraw(thresholdBins) };

var statePrior = priorModel({
  potential: 0.3,
  prevalenceWhenPresent: 0.5, // how prevalent under the stable cause
  concentrationWhenPresent: 10   // the inverse-variance of the stable cause
})

display("prevalence prior")
viz(statePrior)

var listener = cache(function(utterance) {
  Infer({model: function(){
    var prevalence = sample(statePrior)
    var threshold = thresholdPrior()
    var m = meaning(utterance, prevalence, threshold)
    condition(m)
    return prevalence
  }})
})

display("listener posterior")
listener("generic")
~~~~


> **Exercises:**
> 1. Come up with parameters for the prior that represent the *carries malaria* distribution. Test the  listener's interpretation of a generic (*Wugs carry malaria*).
> 1. Come up with parameters for the prior that represent the *lays eggs* distribution. Test the listener's interpretation of a generic (*Wugs lay eggs*).
> 1. Come up with parameters for the prior that represent the *are female* distribution. Test the listener's interpretation of a generic (*Wugs are female*).

So we have a model that can interpret generic language (with a very simple semantics). We can now imagine a speaker who thinks about this type of listener, and decides if a generic utterance is a good thing to say. Speaker models are interpreted as models of utterance production, or endorsement (reft:DegenGoodman2014Cogsci; reft:Franke2014). If we specify the alternative utterance to be a *null* utterance (or, *silence*), we model the choice between uttering the generic (i.e., endorsing its truth) or nothing at all (i.e., not endorsing its truth). (Note: You could also think about truth judgments with the alternative of saying the negation, e.g., it's not the case that Ks have F. Model behavior is very similar using that alternative in this case.)

~~~~
///fold:
// discretized range between 0 - 1
var bins = map(function(x){
  _.round(x, 2);
},  _.range(0.01, 1, 0.02));

var thresholdBins = map2(function(x,y){
  var d = (y - x)/ 2;
  return x + d
}, bins.slice(0, bins.length - 1), bins.slice(1, bins.length))

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

var alpha = 2;
var utterances = ["generic", "silence"];

var thresholdPrior = function() { return uniformDraw(thresholdBins) };
var utterancePrior = function() { return uniformDraw(utterances) }

var meaning = function(utterance, prevalence, threshold) {
  return (utterance == 'generic') ? prevalence > threshold : true
}


var listener = function(utterance, statePrior) {
  Infer({model: function(){
    var prevalence = sample(statePrior)
    var threshold = thresholdPrior()
    var m = meaning(utterance, prevalence, threshold)
    condition(m)
    return prevalence
  }})
}

var speaker = function(prevalence, statePrior){
  Infer({model: function(){
    var utterance = utterancePrior();
    var L = listener(utterance, statePrior);
    factor( alpha * L.score(prevalence) )
    return utterance
  }})
}

var target_prevalence = 0.03

var prior = priorModel({
  potential: 0.01,
  prevalenceWhenPresent: 0.01,
  concentrationWhenPresent: 5
})

viz.density(prior)

viz(speaker(target_prevalence, prior))
~~~~

> **Exercises:**
> 1. Test *Birds lay eggs* vs. *Birds are female*. (Technical note: Due to the discretization of the state space, `target_x` must take odd-numbered values such as 0.03, 0.05, 0.09, ... )
> 2. Come up with other generic sentences. Hypothesize what the prior might be, and what the prevalence might be, and test the model on it.


References:

- Cite:tesslerGenerics
