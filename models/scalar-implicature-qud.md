---
layout: model
title: Scalar Implicature with Varying QUD
model-status: code
model-category: Reasoning about Reasoning
model-tags: linguistics, pragmatics, theory of mind
model-language: webppl
---


Let's enrich the basic scalar implicature model such that the speaker is addressing one of two different QUDs, where the QUD is known to the listener. The scenario is the apple scenario from Goodman & Stuhlmueller 2013; the potential QUD's are "Are all of the apples red?" and "Are any of the apples red?".

~~~~
// possible states of the world
var statePrior = function() {
  return uniformDraw([0, 1, 2, 3])
};

// possible utterances
var utterancePrior = function() {
  return uniformDraw(['all', 'some', 'none']);
};

// possible quds
var quds = ['all?','any?']

// prior over quds (only relevant for qud inference)
var qudPrior = function() {
  return uniformDraw(quds);
};

// meaning funtion to interpret the utterances
var literalMeanings = {
  all: function(state) { return state === 3; },
  some: function(state) { return state > 0; },
  none: function(state) { return state === 0; }
};

// projection function
var qudFn = function(qud, state) {
  var qudAdressed = qud === "all?" ? state === 3 : state > 0
  return qudAdressed
}

// literal listener
var literalListener = cache(function(utt,qud) {
  return Infer({model: function(){
    var state = statePrior()
    var meaning = literalMeanings[utt]
    condition(meaning(state))
    return qudFn(qud,state)
  }})
});

// set speaker optimality
var alpha = 1

// pragmatic speaker
var speaker = cache(function(state,qud) {
  return Infer({model: function(){
    var utt = utterancePrior()
    factor(alpha * literalListener(utt,qud).score(qudFn(qud,state)))
    return utt
  }})
});

// pragmatic listener
var pragmaticListener = cache(function(utt,qud) {
  return Infer({model: function(){
    var state = statePrior()
    observe(speaker(state,qud),utt)
    return state
  }})
});

// print("pragmatic listener's interpretation of 'some':")
viz(pragmaticListener('some','any?'));

~~~~

1. Run the pragmatic listener with different QUDs. How does the output differ? Why? (Hint: Run the literal listener and the speaker forward with different QUDs.)