---
layout: model
title: Politeness
model-status: code
model-category: Reasoning about Reasoning
model-tags: language, pragmatics
model-language: webppl
model-language-version: v0.9.7
---

## Polite RSA

This is the model used in Ref:yoon_tessler_fg_2018.

~~~~
///fold:
var utterances = [
  "yes_terrible","yes_bad","yes_good","yes_amazing",
  "not_terrible","not_bad","not_good","not_amazing"
];

var states = [0, 1,2,3,];

var isNegation = function(utt){
  return (utt.split("_")[0] == "not")
};

var marginalize = function(dist, key){
  return Infer({model: function(){ sample(dist)[key] }})
}

var cost_yes = 1;
var cost_neg = 2.5;
var speakerOptimality = 4.5;
var speakerOptimality2 = 2;

var round = function(x){
  return Math.round(x * 100) / 100
}

var weightBins = map(round, _.range(0,1, 0.05))
var phiWeights = repeat(weightBins.length, function(){1})

var uttCosts = map(function(u) {
  return isNegation(u) ? Math.exp(-cost_neg) : Math.exp(-cost_yes)
}, utterances)

var utterancePrior = Infer({model: function(){
  return utterances[discrete(uttCosts)]
}});

// Parameter values = Maximum A-Posteriori values inferred from full data set
var literalSemantics = {
  "state": [0, 1, 2, 3],
  "not_amazing": [0.9652,0.9857,0.7873,0.0018],
  "not_bad": [0.0967,0.365,0.7597,0.9174],
  "not_good": [0.9909,0.736,0.2552,0.2228],
  "not_terrible": [0.2749,0.5285,0.728,0.9203],
  "yes_amazing": [4e-04,2e-04,0.1048,0.9788 ],
  "yes_bad": [0.9999,0.8777,0.1759,0.005],
  "yes_good": [0.0145,0.1126,0.9893,0.9999],
  "yes_terrible": [0.9999,0.3142,0.0708,0.0198]
};

var meaning = function(words, state){
  return flip(literalSemantics[words][state]);
};
///

var listener0 = cache(function(utterance) {
  Infer({model: function(){
    var state = uniformDraw(states);
    var m = meaning(utterance, state);
    condition(m);
    return state;
  }})
}, 10000);

var speaker1 = cache(function(state, phi) {
  Infer({model: function(){

    var utterance = sample(utterancePrior);
    var L0 = listener0(utterance);

    var utilities = {
      inf: L0.score(state), // log P(s | u)
      soc: expectation(L0) // E[s]
    }
    var speakerUtility = phi * utilities.inf +
        (1-phi) * utilities.soc;

    factor(speakerOptimality*speakerUtility);

    return utterance;
  }})
}, 10000);

var listener1 = cache(function(utterance) {
  Infer({model: function(){

    var phi = categorical({vs: weightBins, ps: phiWeights})
    var state = uniformDraw(states);
    var S1 = speaker1(state, phi);

    observe(S1, utterance)

    return {
      state: state,
      phi: phi
    }

  }})
}, 10000);

var speaker2 = function(state, phi, weights) {
  Infer({model: function(){

    var utterance = sample(utterancePrior);
    var L1 = listener1(utterance);
    var L1_state = marginalize(L1, "state");
    var L1_goals = marginalize(L1, "phi");

    var utilities = {
      inf: L1_state.score(state), // log P(s | u)
      soc: expectation(L1_state), // E [s]
      pres: L1_goals.score(phi) // // log P(phi | u)
    }

    var totalUtility = weights.soc * utilities.soc +
        weights.pres * utilities.pres +
        weights.inf * utilities.inf;

    factor(speakerOptimality * totalUtility)

    var utt = utterance.split("_")
    return {
      "utterance particle": utt[0], utterance: utt[1]
    }

  }})
};

// Parameter values = Maximum A-Posteriori values inferred from full data set for each condition

// informational
display("Speaker wants to give Listener accurate and informative feedback")
viz(speaker2(0, 0.5, {soc: 0.05, pres: 0.60, inf: 0.35}))

// social
display("Speaker wants to make Listener feel good")
viz(speaker2(0, 0.35, {soc: 0.30, pres: 0.45, inf: 0.25}))

// both
display("Speaker wants to make Listener feel good AND give accurate and informative feedback")
viz(speaker2(0, 0.35, {soc: 0.10, pres: 0.55, inf: 0.35}))
~~~~

## Polite RSA (CogSci 2017)

This is the model used in Ref:yoonTessler2017cogsci.

~~~~
///fold:
var utterances = [
  "yes_terrible","yes_bad","yes_okay","yes_good","yes_amazing",
  "not_terrible","not_bad","not_okay","not_good","not_amazing"
  ];

var states = [1,2,3,4,5];

var isNegation = function(utt){
  return (utt.split("_")[0] == "not")
};

var marginalize = function(dist, key){
	return Infer({model: function(){ sample(dist)[key] }})
}

var cost_yes = 1;
var cost_neg = 2;
var speakerOptimality = 5;
var speakerOptimality2 = 2;
var alpha = 1;
var s = 1;
var w = 0.1;

var round = function(x){
	return Math.round(x * 100) / 100
}

var weightBins = map(round, _.range(0,1, 0.05))
var phiWeights = repeat(weightBins.length, function(){1})

var uttCosts = map(function(u) {
	return isNegation(u) ? Math.exp(-cost_neg) : Math.exp(-cost_yes)
}, utterances)

var utterancePrior = Infer({model: function(){
  return utterances[discrete(uttCosts)]
}});

// taken from literal semantics expt
var literalSemantics = {
  "state": [1, 2, 3, 4, 5],
  "not_amazing": [0.9925, 0.9186, 0.7876, 0.2321, 0.042],
  "not_bad": [0.0075, 0.2897, 0.8514, 0.8694, 0.8483],
  "not_good": [0.9926, 0.8871, 0.1582, 0.0073, 0.0081],
  "not_okay": [0.9198, 0.7652, 0.1063, 0.0074, 0.1192],
  "not_terrible": [0.0415, 0.4363, 0.9588, 0.9225, 0.9116],
  "yes_amazing": [0.0077, 0.0077, 0.0426, 0.675, 0.9919],
  "yes_bad": [0.9921, 0.9574, 0.0078, 0.0078, 0.0079],
  "yes_good": [0.008, 0.0408, 0.8279, 0.9914, 0.993],
  "yes_okay": [0.0078, 0.286, 0.9619, 0.7776, 0.6122],
  "yes_terrible": [0.9593, 0.5217, 0.0753, 0.008, 0.044]
};

var meaning = function(words, state){
  return flip(literalSemantics[words][state - 1]);
};
///

var listener0 = cache(function(utterance) {
  Infer({model: function(){
		var state = uniformDraw(states);
    var m = meaning(utterance, state);
    condition(m);
    return state;
	}})
}, 10000);

var speaker1 = cache(function(state, speakerGoals) {
	Infer({model: function(){

    var utterance = sample(utterancePrior);
    var L0 = listener0(utterance);

    var epistemicUtility = L0.score(state);
    var socialUtility = expectation(L0, function(s){return alpha*s});
    var eUtility = speakerGoals.phi*epistemicUtility;
    var sUtility = (1-speakerGoals.phi)*socialUtility;
    var speakerUtility = eUtility+sUtility;

    factor(speakerOptimality*speakerUtility);

    return utterance;
	}})
}, 10000);

var listener1 = cache(function(utterance) {
	Infer({model: function(){

   var speakerGoals = {
     phi: categorical ({vs: weightBins, ps: phiWeights})
   }
   var state = uniformDraw(states);

   var S1 = speaker1(state, speakerGoals)
   observe(S1, utterance)

   return {
     state: state,
     goals: speakerGoals
   }

 }})
}, 10000);

var speaker2 = cache(function(state, informativeness) {
	Infer({model: function(){

	 var utterance = sample(utterancePrior);
	 var intendedGoals = {phi: informativeness}

   var L1 = listener1(utterance)
	 var L1_state = marginalize(L1, "state");

   factor(speakerOptimality2 * L1.score({"state":state, "goals":intendedGoals}))
	//  factor(speakerOptimality2 * L1_state.score(state))

   return isNegation(utterance) ? "negation" : "direct"

 }})
}, 10000);

display("listener hears 'it wasn't amazing'; how bad was it?")
var l1 = listener1("not_amazing")
viz(marginalize(l1, "state"))
display("how much was the speaker trying to be informative (vs kind)?")
viz(marginalize(l1, "goals"))

display("listener hears 'it was terrible'; how bad was it?")
var l1a = listener1("yes_terrible")
viz(marginalize(l1a, "state"))

display("how much was the speaker trying to be informative (vs kind)?")
viz(marginalize(l1a, "goals"))

display("speaker thinks it's terrible, tries to be nice")
var s2_nice  = speaker2(1, 0.05)
viz(s2_nice)

display("speaker thinks it's terrible, tries to be informative")
var s2_informative  = speaker2(1, 0.95)
viz(s2_informative)

display("speaker thinks it's terrible, tries to be both nice and informative")
var s2_both  = speaker2(1, 0.5)
viz(s2_both)
~~~~

References:

- Cite:yoon_tessler_fg_2018
- Cite:yoonTessler2017cogsci
