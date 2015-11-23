---
layout: model
title: Wonky worlds
model-status: code
model-language: webppl
---

<script src="http://web.stanford.edu/~erindb/webppl-viz/webppl.min.js"></script>  
<link rel="stylesheet" href="http://web.stanford.edu/~erindb/webppl-viz/viz.css">

## Regular RSA, v.1

~~~~
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
  // return utterances[discrete([1,1,1,10])]
}

var meaning = function(utt,world) {
  return utt=="Some"? world>0 :
  utt=="All"? world==numObjs :
  utt=="None"? world==0 :
  utt=="mu"? true :
  true
}

var binomialMarbles = function(theta){
  return map(function(x){return Math.exp(binomialERP.score([theta, 15], x))},_.range(0,16))
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
vizPrint(posterior)
~~~~

# Wonky RSA
~~~~
~~~~