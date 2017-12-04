---
layout: model
title: Progressive shift
model-language: webppl
---

A model of progressive vs. imperfective aspect by Becky Jarvis and Gunnar Lund:

~~~~
//Generate the state space given a number of events
var stateGen = function(numberBins) {
  var stateSoFar = []
  var binSize = 1 / numberBins
  var binArray = _.range(0, numberBins)
  var eventPop = map(function(x){return Math.round(((binSize/2)+(binSize*x))*100)/100}, binArray)
  var newEvents = stateSoFar.concat(eventPop)
  return newEvents
} 

//Power set helper function, from lexical uncertainty model.
var powerset = function(set){
  if(set.length==0){
    return [set]
  }
  var r = powerset(set.slice(1)) // exlude first element
  var element = [set[0]] // first element
  var new_r = r.concat(map(function(x){ element.concat(x) }, r))
  return new_r
}

//Generate the states and then apply the powerset (removing empty set)
var allstates = stateGen(5)
var powersetStates = filter(function(x){return x.length>0},powerset(allstates))

//Return uniform draw of the powerset of states.
var statePrior = function() {
  return uniformDraw(powersetStates)
} 

//utterances and utterancePrior function; requires null utt due to threshold semantics
var utterances = ["prog", "impf",""]
var cost = {
  "prog": 5,
  "impf": 1,
  "": 100
}

var utterancePrior = function() {
  var uttProbs = map(function(u) {return Math.exp(-cost[u]) }, utterances);
  return categorical(uttProbs, utterances);
};


//List of possibe thetas 
var possibleThetas = [0.4,0.5,0.6,0.7,0.8,0.9,1]

//Generate ordered pair <thetaR, thetaImpf> s.t. thetaImpf is greater than or equal to thetaR
var thetaGen = function(number, stateSoFar) {
  var stateSoFar = stateSoFar == undefined ? [] : stateSoFar
  if (number != -1) {
    var newThetaN = map(function(x){if (x >= possibleThetas[number]){return [possibleThetas[number], x]}}, possibleThetas)
    var newThetas = stateSoFar.concat(newThetaN)
    return thetaGen(number-1, newThetas)
  }
  else {
    return remove(null, stateSoFar)
  }
}

//the possibleThetas.length argument is essentially an index for the recursive function.
var thetas = thetaGen(possibleThetas.length)

var thetasPrior = function(){
  return uniformDraw(thetas)
}

//Generates the bins from the different thetas
var thetaBins = function(numberBins, theta) {
  var newBins = [0]
  var binSize = theta / numberBins
  var binArray = _.range(0, numberBins)
  var binPop = map(function(x){return (binSize)+(binSize*x)}, binArray)
  var newEvents = newBins.concat(binPop)
  return newEvents
}

//meaning fxn: checks to make sure at least one event is contained in every bin.
var meaningFn = function(state, bins, index, stateSoFar){
  var stateSoFar = stateSoFar == undefined ? [] : stateSoFar
  if (index != bins.length-1){
    var inBin = any(function(x){return x>bins[index] && x<=bins[index+1]}, state)
    var eventsInBins = stateSoFar.concat(inBin)
    return meaningFn(state, bins, index+1, eventsInBins)
  }
  else {
    return all(function(x){return x==true}, stateSoFar)
  }
}

//Actually apply the meaning function for the utterances
var meaning = function(utterance, binsR, binsT, state) {
  if (utterance == "prog") {
    return meaningFn(state, binsR, 0)
  }
  else if (utterance == "impf") {
    return meaningFn(state, binsT, 0)
  }
  else {
    return true
  }
}

//Alphas and bins
var alpha = 1
var nBins = 2

//our actors:
var literalListener = cache(function(utterance, thetaR, thetaT) {
  return Infer({model: function() {
    var state = statePrior();
    var binsR = thetaBins(nBins, thetaR);
    var binsT = thetaBins(nBins, thetaT);
    condition(meaning(utterance, binsR, binsT, state))
    return state;
  }});
});

var speaker = cache(function(state, thetaR, thetaT) {
  return Infer({method: "enumerate"}, function() {
    var utterance = utterancePrior();
    factor(alpha * literalListener(utterance, thetaR, thetaT).score(state));
    return utterance;
  });
});

var pragmaticListener = function(utterance) {
  return Infer({method: "enumerate"}, function() {
    var state = statePrior();
    var thetas = thetasPrior();
    var thetaR = thetas[0]
    var thetaT = thetas[1]
    factor(speaker(state, thetaR, thetaT).score(utterance));
    return {state: state, thetaR: thetaR, thetaT: thetaT};
  });
};

viz.marginals(pragmaticListener("prog"))
~~~~