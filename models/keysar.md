---
layout: model
title: RSA with Context Uncertainty (Keysar)
model-language: webppl
model-language-version: v0.9.6
---

~~~~
var possibleUtterances = _.flatten(map(function(modifier) {
  map(function(type) {
    return [modifier, type].join(' ').trim();
  }, ['apple', 'fish', 'cup']);
}, ['red', 'blue', 'green', '']));

var possibleObjects = [
  {type: 'apple', color: 'red'}, {type: 'apple', color: 'blue'}, {type: 'apple', color: 'green'},
  {type: 'fish', color: 'red'}, {type: 'fish', color: 'blue'}, {type: 'fish', color: 'green'},
  {type: 'cup', color: 'red'}, {type: 'cup', color: 'blue'}, {type: 'cup', color: 'green'}]

var exampleContext = {
  shared: [  
    {type: 'apple', color: 'red'},
    {type: 'fish', color: 'blue'},
    {type: 'cup', color: 'green'}
  ],
  occluded: [
    {type: 'fish', color: 'red'}
  ]
};

var alpha = 1
var deceptionPrior = .1

var uttCost = function(utt) {
  return utt.split(' ').length;
}

var uttFitness = function(utt, object) {
  var descriptors = utt.split(' ');
  if(descriptors.length > 1) {
    return (object.color === descriptors[0] && 
            object.type === descriptors[1]) ? 0 : -100;
  } else {
    return object.type === descriptors[0] ? 0 : -100;
  }
};

var L0 = cache(function(utt, perceivedContext) {
  return Infer({method: 'enumerate'}, function() {
    var object = uniformDraw(perceivedContext);
    
    factor(uttFitness(utt, object))
    return object;
  })
})

// If experimenter is deceptive, speaker can see everything; 
// if not, speaker has uncertainty over what's behind occluded square
var S1 = cache(function(target, knownContext) {
  return Infer({method: 'enumerate'}, function() {
    var possibleListenerView = (knownContext.length < 4 ? 
                                knownContext.concat(uniformDraw(possibleObjects)) : 
                                knownContext);
    var utt = uniformDraw(possibleUtterances);
    
    factor(alpha * L0(utt, possibleListenerView).score(target)
           - uttCost(utt))
    return utt;
  }); 
});

// Listener assigns small prior probability to deceptive experimenter
var L2 = cache(function(utt, perceivedContext) {
  var fullObjSet = perceivedContext.shared.concat(perceivedContext.occluded);
  return Infer({method: 'enumerate'}, function() {
    var deception = flip(deceptionPrior);
    var object = uniformDraw(fullObjSet);
    
    var speakerContext = (deception ? fullObjSet : perceivedContext.shared);
    observe(S1(object, speakerContext), utt);
    return {object: object.color + " " + object.type, deception};
  })
})

console.log("listener response after hearing (underinformative) 'fish'")
viz.marginals(L2('fish', exampleContext))
console.log("listener response after hearing 'blue fish'")
viz.marginals(L2('blue fish', exampleContext))
~~~~
