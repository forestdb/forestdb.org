---
layout: model
title: Wonky worlds
model-status: code
model-language: webppl
model-language-version: v0.9.7
---

<!-- <script src="http://web.stanford.edu/~erindb/webppl-viz/webppl.min.js"></script>  
<link rel="stylesheet" href="http://web.stanford.edu/~erindb/webppl-viz/viz.css"> -->

## Regular RSA, v.1

~~~~
///fold:
var numObjs = 15

var worldPrior = function() {
  var marbles = discrete(numObjs+1) //15 marbles... 0-15 can sink
  return marbles
}

var utterancePrior = function() {
  var utterances = [
    "Some",
    "All",
    "None"
  ]
  return uniformDraw(utterances)
}

var meaning = function(utt,world) {
  return utt=="Some" ? world > 0 :
  utt=="All" ? world == numObjs :
  utt=="None" ? world == 0 :
  utt=="mu" ? true :
  true
}
///
var binomialMarbles = function(theta){
  return map(function(x){
    return Math.exp(binomialERP.score([theta, 15], x))
  },_.range(0,16))
}

var literalListener = cache(function(utterance, priorParams) {
  Enumerate(function(){
    var primary = flip(priorParams["mix"])
    var prior =  primary ?
        binomialMarbles(priorParams["theta1"]) :
        binomialMarbles(priorParams["theta2"])
    var world = discrete(prior)
    var m = meaning(utterance, world)
    condition(m)
    return world
  })
})

var speaker = cache(function(world, priorParams) {
  Enumerate(function(){
    var utterance = utterancePrior()
    var L = literalListener(utterance, priorParams)
    factor(L.score([],world))
    return utterance
  })
})

var listener= function(utterance,speakerOptimality, priorParams) {
  Enumerate(function(){
    var primary = flip(priorParams["mix"])
    var prior =  primary ?
        binomialMarbles(priorParams["theta1"]) :
        binomialMarbles(priorParams["theta2"])
    var world = discrete(prior)

    var S = speaker(world, priorParams)

    factor(speakerOptimality*S.score([],utterance))

    var queryStatement = {"world":world,
                          "wonky":1-primary,
                          "nextWorld": discrete(prior)}
    return queryStatement
  })
}

var posterior = listener("Some", 5, {mix: 0.9, theta1:0.99, theta2:0.5})

print("expected value of world state = "+expectation(marginalize(posterior, "world")))
print("expected value of next world state = "+expectation(marginalize(posterior, "nextWorld")))
print("expected value of wonkiness = "+expectation(marginalize(posterior, "wonky")))
vizPrint(posterior)
~~~~

## Regular RSA, v.2

~~~~
///fold:
var marginalize = function(myERP, index){
  Enumerate(function(){
    var x = sample(myERP)
    return x[index]
  })
}

var numObjs = 15

var worldPrior = function() {
  var marbles = randomInteger(numObjs+1) //15 marbles... 0-15 can sink
  return marbles
}

var utterancePrior = function() {
  var utterances = [
                    "Some",
                    "All",
                    "None"
                    // "mu"
                    ]
 return uniformDraw(utterances)
}

var meaning = function(utt,world) {
  return utt=="Some"? world>0 :
  utt=="All"? world==numObjs :
  utt=="None"? world==0 :
  utt=="mu"? true :
  true
}
///
var doubleBinomialMarbles = function(theta1, theta2, mix){
  return map(
    function(x){
      return mix*Math.exp(binomialERP.score([theta1, 15], x)) +
      	 (1-mix)*Math.exp(binomialERP.score([theta2, 15], x))
    },
    _.range(0,16))
}

var literalListener = cache(function(utterance, prior) {
    Enumerate(function(){
              var world = discrete(prior)
              var m = meaning(utterance, world)
              condition(m)
              return world
  })
})

var speaker = cache(function(world, prior) {
  Enumerate(function(){
            var utterance = utterancePrior()
            var L = literalListener(utterance, prior)
            factor(L.score([],world))
            return utterance
            })
})

var listener= function(utterance,speakerOptimality, prior) {
        Enumerate(function(){
                  var world = discrete(prior)

                  var S = speaker(world, prior)

                  factor(speakerOptimality*S.score([],utterance))

                  var queryStatement = {"world":world,
                                        "nextWorld": discrete(prior)}
                  return queryStatement
      })
}
var prior = doubleBinomialMarbles(0.99, 0.5, 0.9)
var posterior = listener("Some", 5, prior)

print("expected value of world state = "+expectation(marginalize(posterior, "world")))
print("expected value of next world state = "+expectation(marginalize(posterior, "nextWorld")))
vizPrint(posterior)
~~~~

## Wonky RSA
~~~~
///fold:
var numObjs = 15

var worldPrior = function() {
  var marbles = randomInteger(numObjs+1) //15 marbles... 0-15 can sink
  return marbles
}

var utterancePrior = function() {
  var utterances = [
                    "Some",
                    "All",
                    "None"
                    ]
 return uniformDraw(utterances)
}

var meaning = function(utt,world) {
  return utt=="Some"? world>0 :
  utt=="All"? world==numObjs :
  utt=="None"? world==0 :
  utt=="mu"? true :
  true
}

var binomialMarbles = function(theta){
  return map(function(x){
    return Math.exp(Binomial({n:15, p:theta}).score(x))
  },_.range(0,16))
}

var speakerOptimality = 1;

///
var wonkyLiteralListener = cache(function(utterance, prior) {
  Infer({model: function(){
            var world = discrete(prior)
            var m = meaning(utterance, world)
            condition(m)
            return world
  }})
})

var wonkySpeaker = cache(function(world, prior) {
  Infer({model: function(){
            var utterance = utterancePrior()
            var L0 = wonkyLiteralListener(utterance, prior)
            factor(speakerOptimality * L0.score(world))
            return utterance
            }
  })
})

var wonkyRSA = function(utterance,speakerOptimality, priorParams) {
  Infer({model: function(){
    var wonky = flip(priorParams["wonkyPrior"])
    var prior =  wonky ?
                  binomialMarbles(priorParams["backoffTheta"]) :
                  binomialMarbles(priorParams["normalTheta"])
    var n_marbles = discrete(prior)

    var S1 = wonkySpeaker(n_marbles, prior)
    observe(S1, utterance)

    var posteriorPredictive = discrete(prior)

    return {n_marbles, wonky, posteriorPredictive}

  }})
}

var posterior = wonkyRSA("Some", 5, {wonkyPrior: 0.5, normalTheta:0.99, backoffTheta:0.5})

print("expected value of world state = "+expectation(marginalize(posterior, "n_marbles")))
print("expected value of next world state = "+expectation(marginalize(posterior, "posteriorPredictive")))
print("expected value of wonkiness = "+expectation(marginalize(posterior, "wonky")))
viz.marginals(posterior)
~~~~
