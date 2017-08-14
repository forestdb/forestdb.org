---
layout: model
title: RSA with Context Uncertainty (Keysar)
model-language: webppl
model-language-version: v0.9.6
---

Suppose the speaker has no uncertainty about the environment; they pretend as though the things they see are all the things.

Then speakers prefer to say the shorter utterance (i.e. 'fish') because it's sufficiently information but less costly, and listeners also pick the thing in shared context because it's the only possible referent. 

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

var alpha = 3;

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
    factor(uttFitness(utt, object));
    return object;
  });
});

var S1 = cache(function(target, knownContext) {
  return Infer({method: 'enumerate'}, function() {
    var utt = uniformDraw(possibleUtterances);   
    factor(alpha * L0(utt, knownContext).score(target) - uttCost(utt));
    return utt;
  }); 
});

// Listener only considers objects speaker can see (model Keysar is arguing against)
var L2 = cache(function(utt, perceivedContext) {
  var sharedContext = perceivedContext.shared;
  var fullObjSet = sharedContext.concat(perceivedContext.occluded);
  return Infer({method: 'enumerate'}, function() {
    var object = uniformDraw(sharedContext);    
    observe(S1(object, sharedContext), utt);
    return object.color + " " + object.type;
  });
});

console.log("speaker utterance to refer to blue fish");
viz.table(S1({type: 'fish', color: 'blue'}, exampleContext.shared));

console.log("listener response after hearing (underinformative) 'fish'");
viz.table(L2('fish', exampleContext));
~~~~

Now we introduce uncertainty over the context: the speaker reasons that there are additional objects of uncertain identity in the listener's field of view and incorporates this uncertainty when considering the informativity of an utterance. This consequently makes the pragmatic speaker S_1 prefer more specific utterances (hedging against possible hidden distractors) and, in turn, leads the pragmatic listener to occasionally select the hidden object when they hear an underinformative utterance.

~~~~
var possibleUtterances = _.flatten(map(function(modifier) {
  return map(function(type) {
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

var alpha = 4;

var uttCost = function(utt) {
  return utt.split(' ').length/4;
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

// speaker has uncertainty over what's behind occluded square
// marginalizes over all possibilities
var S1 = cache(function(target, perceivedContext) {
  return Infer({method: 'enumerate'}, function() {
    var utt = uniformDraw(possibleUtterances);
    var listener = Infer({method: 'enumerate', model: function() {
      var context = perceivedContext.concat(uniformDraw(possibleObjects));
      return sample(L0(utt, context));
    }})
    factor(alpha * listener.score(target) - uttCost(utt))
    return utt;
  });
});

// Listener reasons about S1; assumes they only see what's in shared context 
// but could be trying to refer to any of the objects
var L2 = cache(function(utt, perceivedContext) {
  var sharedContext = perceivedContext.shared;
  var fullContext = sharedContext.concat(perceivedContext.occluded);
  return Infer({method: 'enumerate', model: function() {
    var object = uniformDraw(fullContext);
    observe(S1(object, sharedContext), utt);
    return object.color + " " + object.type;
  }});
})

console.log("speaker utterance to refer to blue fish")
viz.table(S1({type: 'fish', color: 'blue'}, exampleContext.shared))

console.log("listener response after hearing (underinformative) 'fish'")
viz.table(L2('fish', exampleContext));
~~~~

Finally, recent work by Rubio-Fernandez (2016) has shown that upon hearing over-informative utterances, players make inferences about deception on the part of the experimenter. We incorporate this into the model as a joint inference on the part of the pragmatic listener about what the *speaker* can see, and what they are trying to refer to (note that this is more experimental!)

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