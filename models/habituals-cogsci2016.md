---
layout: model
title: Habituals (CogSci 2016)
model-language: webppl
---

This is a model of habitual language used in Ref:tesslerHabitualsCogSci.

The model takes a habitual sentence (e.g. *John smokes*) [[I does E]] to mean the propensity of 
doing E for individual I is greater than some threshold (i.e. *John smokes more than `tau`*).
This threshold --- `tau` --- is thought to be in general unknown 
(`tau~uniform(lower_bound, upper_bound)`) and must be inferred in context. 

Context here takes the form of the listener and speakers shared beliefs
about the event in question. The shape of this distribution
affects model predictions, because the threshold must be calibrated to make utterances 
truthful and informative. The shape of this distribution varies significantly 
among different properties (e.g. *smokes cigarettes*, *writes novels*), and may 
be the result of a deeper conceptual model of the world. For instance,
if speakers and listeners believe that some individuals are the kind of person to 
never do X, while others will do X then we would expect
the prior to be structured as a mixture distribution 
(Cf. Griffiths & Tenenbaum, 2005). 

## Prior model

The following model `structuredPriorModel` instantiates this idea.
`theta` is measure of how many people actually do the action.
This can also be thought of the popularity of the action at the level of individuals 
(what % of people have done this before?).
For example, "goes to the movies" is very popular, while
"climbs mountains" is not.
`mu` is the *mean frequency for people who do the action*.
Knowing that the person has done the action before, how often do you think the person does the action?
For example, "wears socks" tends to happen everyday, whereas "watches space launches" tends to happen very infrequently. 
Finally, `sigma` is the variance around that mean.
It is high for actions that almost everyone does the same (e.g. "wears socks").
It is lower for actions that people have more uncertainty about (e.g. "wears a suit").


~~~~
// discretized range between -1 and 8 (scale is log times in 5 years)
var binWidth = 0.5
var minBin = -1
var maxBin = 8
var bins = _.range(minBin, maxBin,binWidth)


var gaussianPMF = function(mu, sigma){
  return map(function(b){return Math.exp(gaussianERP.score([mu, sigma], b))}, bins)
}

// var structuredPriorModel = function(params){
//   repeat(1000, function(){
//     var theta = params["theta"]
//     var mu = params["mu"]
//     var sigma = params["sigma"]
//     var hasDoneAction = flip(theta)
//     var frequency = hasDoneAction ? 
//         gaussian(mu, sigma) : 
//         minBin
//     return frequency
//   })
// }

var structuredPriorModel = function(params){
  Enumerate(function(){
    var theta = params["theta"]
    var mu = params["mu"]
    var sigma = params["sigma"]
    var hasDoneAction = flip(theta)
    var frequency = hasDoneAction ? 
        bins[discrete(gaussianPMF(mu, sigma))] : 
        minBin
    return frequency
  })
}

// numbers drawn from data in Expt. 1 
// e.g. "runs"
var runs = structuredPriorModel(
{
  theta: 0.75,
  mu: 5.5,
  sigma: 0.5
})

// e.g. "hikes"
var hikes = structuredPriorModel(
{
  theta: 0.4,
  mu: 3.3,
  sigma: 0.5
})

// e.g. "climbs mountains"
var climbsMountains = structuredPriorModel(
{
  theta: 0.1,
  mu: 1.5,
  sigma: 1
})

// e.g. "drinks coffee"
var drinksCoffee = structuredPriorModel(
{
  theta: 0.85,
  mu: 6.5,
  sigma: 0.3
})


vizPrint({
  "runs": runs,
  "hikes": hikes,
  "climbs mountains": climbsMountains,
  "drinks coffee": drinksCoffee
})

~~~~

## Habituals model

~~~~
var binWidth = 0.5
var minBin = -1
var maxBin = 8
var statebins = _.range(minBin, maxBin, binWidth)


// get intermediate points (rounded to nearest 0.1)
var thresholdBins = map(function(x){
      return Math.round(10*(x - (binWidth / 2))) / 10 
},statebins)

var nearestPriorBin = function(x){
  return x > _.max(statebins) ? 
      _.max(statebins) :
      x < _.min(statebins) ? 
      _.min(statebins) :
      statebins[Math.round(((x - _.min(statebins))/(_.max(statebins) - _.min(statebins)))*(statebins.length-1))]
}

var logFrequency = function(rate){
  var interval = rate.interval
  var instances = rate.instances
  var extendTo5years = {
  "week": 5*52,
  "month": 5*12,
  "year": 5,
  "5 years": 1
  }
  var logFreq = Math.log(instances*extendTo5years[interval])
  return nearestPriorBin(logFreq)
}

var gaussianPMF = function(mu, sigma){
  return map(function(b){return Math.exp(gaussianERP.score([mu, sigma], b))}, statebins)
}

var structuredPriorModel = function(params){
  Enumerate(function(){
    var theta = params["theta"]
    var mu = params["mu"]
    var sigma = params["sigma"]
    var hasDoneAction = flip(theta)
    var frequency = hasDoneAction ? 
        statebins[discrete(gaussianPMF(mu, sigma))] : 
        minBin
    return frequency
  })
}

///

// "speaker optimality" parameters
var alpha_1 = 5
var alpha_1 = 1.25

var thresholdPrior = function() {
  var threshold = uniformDraw(thresholdBins)
  return threshold
}

var utterancePrior = function() {
  return flip(0.5) ? "habitual" : "stay silent"
}

var meaning = function(utt,state, threshold) {
  return  utt=="habitual"? state>threshold :
          utt=='stay silent' ? true:
          utt=="habitual is false"? state<=threshold :
  true
}


var listener0 = cache(function(utterance, threshold, prior) {
  Enumerate(function(){
    var state = sample(prior)
    var m = meaning(utterance, state, threshold)
    condition(m)
    return state
  })
})

var speaker1 = cache(function(state, threshold, prior) {
  Enumerate(function(){
    var utterance = utterancePrior()
    var L0 = listener0(utterance, threshold, prior)
    factor(alpha_1*L0.score([],state))
    return utterance
  })
})

var listener1 = cache(function(utterance, prior) {
  Enumerate(function(){
    var state = sample(prior)
    var threshold = thresholdPrior()
    var S1 = speaker1(state, threshold, prior)
    factor(S1.score([],utterance))
    return state
  })
})

var speaker2 = function(frequency, prior){
  Enumerate(function(){
    var utterance = utterancePrior()
    var wL1 = listener1(utterance, prior)
    factor(alpha_2*wL1.score([], logFrequency(frequency)))
    return utterance
  })
}

// numbers drawn from data in Expt. 1 
// e.g. "runs"
var runsPrior = structuredPriorModel(
{
  theta: 0.75,
  mu: 5.5,
  sigma: 0.5
})

// e.g. "hikes"
var hikesPrior = structuredPriorModel(
{
  theta: 0.5,
  mu: 3.3,
  sigma: 0.5
})

// e.g. "climbs mountains"
var climbsMountainsPrior = structuredPriorModel(
{
  theta: 0.1,
  mu: 1.5,
  sigma: 1
})

// e.g. "drinks coffee"
var drinksCoffeePrior = structuredPriorModel(
{
  theta: 0.85,
  mu: 7,
  sigma: 0.4
})
// truth judgment task


var runs = speaker2({instances: 3, interval: "year"}, runsPrior)
print("Runs 3 times a year")
print(runs)

var hikes = speaker2({instances: 3, interval: "year"}, hikesPrior)
print("Hikes 3 times a year")
print(hikes)

var climbs = speaker2({instances: 3, interval: "year"}, climbsMountainsPrior)
print("Climbs mountains 3 times a year")
print(climbs)

var coffee = speaker2({instances: 7, interval: "week"}, drinksCoffeePrior)
print("Drinks coffee 7 times a week")
print(coffee)

~~~~

References:

- Cite:tesslerHabitualsCogSci
