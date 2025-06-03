---
layout: model
title: Politeness - Qiyan, Mackenzie, Caitlyn
model-language: webppl
---

# Politeness - White Lies

## The Problem

Why do we often choose to speak indirectly, especially when being direct would be more informative? If, when this presentation is over, I ask, “How was our talk?” and you didn’t think it was very good, you might say, *“It wasn’t terrible,”* instead of the more honest *“It was bad.”* This kind of polite language is puzzling from the perspective of classical models of communication where the goal is to convey information efficiently and truthfully. Indirect or vague utterances seem suboptimal yet, we use them all the time.

Politeness is one of the clearest examples of how human communication goes beyond informativity. Speakers often balance competing goals: to tell the truth, to be kind, and to manage how they are perceived. The key question is: How do speakers decide what to say when these goals are in tension?

To address this, [Yoon et al. (2017)](https://drive.google.com/file/d/1VV6bV6lNjKHyyjPjjgRRSZMC13Z0NlZ5/view?usp=sharing) and [Yoon et al. (2018)](https://psyarxiv.com/67ne8) extend the RSA framework to account for polite indirect speech. As we know, in the RSA framework, speakers and listeners are modeled as rational agents who recursively reason about each other. Speakers choose utterances based on how they expect listeners to interpret them, and listeners interpret utterances by inferring the speaker’s goals and beliefs.

If you recall, the early versions of the model proposed that polite speech arises from a tradeoff between two utilities:
* *Epistemic* utility: the desire to be informative.
* *Social* utility: the desire to preserve the listener’s feelings.

This version captures phenomena like white lies, where speakers sacrifice some truth to be kind.
But to fully explain indirectness a third dimension is needed. That’s where the self-presentational utility comes in. In the extended model, speakers don’t just want to be kind and informative, they want to appear that way. This higher-order goal accounts for situations where being fully honest would be rude, and being overly nice would feel dishonest. An utterance like “It wasn’t terrible” communicates that the speaker is trying to balance these goals, and importantly, that they want the listener to see them as someone who is trying to balance these goals.

## New Utterances and Recap

Similar to the previous politeness model study we discussed in class, Yoon et al. (2018) constructs a scenario where the speaker gives feedback (0, 1, 2, or 3 stars) to the listener’s creative work (e.g. a poem). In the model, there are 4 states of the world, corresponding to the star rating, where 3 stars is the best and 0 stars is the worst. 

A new addition to this model is the *self-presentational utility* – a way that speakers signal to the listeners their conversation goals. In other words, how do the speakers want to present their *epistemic/informational* and *social* goals, even if those are not their real intentions? This new utility allows the speaker to signal to the listener that they care about both *epistemic* and *social* goals though it’s not possible in reality. 

Prioritizing the *presentational utility* leads speakers to use **indirect speech**, realized as negated adjectival phrases in this model. There are 8 possible utterances: 4 without negation (“terrible”, “bad”, “good”, “amazing”) and 4 with negation (“not terrible”, “not bad”, “not good”, “not amazing”). The negated utterances are more difficult to comprehend than the unnegated utterances, so they have a higher cost of 0.35 while the unnegated utterances have a cost of 0.

Authors obtained literal semantics of the utterances through an experiment probing participant judgments. They were presented with a state (0, 1, 2, 3) and an utterance (“Do you think Ann thought the presentation was / wasn’t [1 of 8 utterances]?”) and responded with “yes” or “no”.

Literal listener (**L0**) is the same from the vanilla RSA model. They only interpret the literal semantics of the utterances according to the meaning function.

Pragmatic speaker 1 (**S1**) stays the same as the previous politeness model we covered in class. The speaker considers their informational(being truthful) and social (being kind) goals, and *φ* determines which goal they prioritize.

<img src="https://cdn.discordapp.com/attachments/700550857913270273/1379153139373969573/image.png?ex=683f33f3&is=683de273&hm=c7509c3d5a2e91545ffc9f6240e542cb253d728ff9a27401a9f3ace34d16a091&" alt="Alt Text" style="width:70%; height:auto;">

~~~~
var utterances = [
  "yes_terrible","yes_bad","yes_good","yes_amazing",
  "not_terrible","not_bad","not_good","not_amazing"
];

var states = [0,1,2,3];

var isNegation = function(utt){
  return (utt.split("_")[0] == "not")
};

var cost_yes = 0;
var cost_neg = 0.35;
// in Yoon, Tessler, et al. (2020), the speaker optimality parameters were set to be the same value
var speakerOptimality = 4;
var speakerOptimality2 = 4;

var round = function(x){
  return Math.round(x * 100) / 100
}

var weightBins = map(round, _.range(0,1, 0.05))
var phiWeights = repeat(weightBins.length, function(){1})

var cost = function(utterance){
  return isNegation(utterance) ? cost_neg : cost_yes
}
// var uttCosts = map(function(u) {
//   return isNegation(u) ? Math.exp(-cost_neg) : Math.exp(-cost_yes)
// }, utterances)
//
// var utterancePrior = Infer({model: function(){
//   return utterances[discrete(uttCosts)]
// }});

// Parameter values = Maximum A-Posteriori values from Yoon, Tessler et al., (2018)
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

    var utterance = uniformDraw(utterances);
    var L0 = listener0(utterance);

    var utilities = {
      inf: L0.score(state), // log P(s | u)
      soc: expectation(L0) // E[s]
    }
    var speakerUtility = phi * utilities.inf +
        (1-phi) * utilities.soc - cost(utterance);

    factor(speakerOptimality * speakerUtility);

    return utterance;
  }})
}, 10000);
~~~~

Pragmatic listener 1 (**L1**) also stays the same. The listener takes in an utterance and returns a joint probability distribution over the most likely state and *φ* values. 

~~~~
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
~~~~

## Pragmatic Speaker 2 (**S2**)

This is where the new parameter *ω* comes in. Unlike *φ*, which controls how much the speaker cares about being honest versus being nice, *ω* is a vector that contains the weights assigned to three utlities: epistemic (truth), social (kindness), and presentational (how **S2** wants to be seen), showing how much the speaker values each when choosing what to say. 

<img src="https://cdn.discordapp.com/attachments/700550857913270273/1379175454371610694/image.png?ex=683f48bc&is=683df73c&hm=9a40ea22e370a5094b1732db5eec6d3166d8e91f25003757460504045cb696b9&" alt="Alt Text" style="width:100%; height:auto;">

*u* is the utterance (what the speaker says); *s* is the state of the world (how good or bad things really are); *φ* represents the speaker’s internal values (how much they personally care about truth vs. kindness); *ω* are the weights attibuted to the three utilites.

In this updated model, function *speaker2* takes in a state (0, 1, 2, 3), a *φ* value (0-1), and a vector which contains weights (*ω*) for the utilities and returns a probability distribution of utterances which can best convey these information to the listener.

Utilities calculations are based on how the listener interprets the utterance: **S2** runs **L1** and marginalizes over "state" (listener’s beliefs about the true state) and "*φ*" (listener’s beliefs about the speaker’s goals).Calculating the informational and social utilities uses state marginals, and calculating the presentational utility uses *φ* marginals. Then, Each utility is multiplied by its corresponding weight – determined by the input vector (*ω*). The total utility is then the sum of these weighted values minus the cost of the utterance – negated utterances are more costly. This total helps the speaker decide which utterance best suits the speaker’s goals.

~~~
///fold:
var utterances = [
  "yes_terrible","yes_bad","yes_good","yes_amazing",
  "not_terrible","not_bad","not_good","not_amazing"
];

var states = [0,1,2,3];

var isNegation = function(utt){
  return (utt.split("_")[0] == "not")
};

var cost_yes = 0;
var cost_neg = 0.35;
// in Yoon, Tessler, et al. (2020), the speaker optimality parameters were set to be the same value
var speakerOptimality = 4;
var speakerOptimality2 = 4;

var round = function(x){
  return Math.round(x * 100) / 100
}

var weightBins = map(round, _.range(0,1, 0.05))
var phiWeights = repeat(weightBins.length, function(){1})

var cost = function(utterance){
  return isNegation(utterance) ? cost_neg : cost_yes
}
// var uttCosts = map(function(u) {
//   return isNegation(u) ? Math.exp(-cost_neg) : Math.exp(-cost_yes)
// }, utterances)
//
// var utterancePrior = Infer({model: function(){
//   return utterances[discrete(uttCosts)]
// }});

// Parameter values = Maximum A-Posteriori values from Yoon, Tessler et al., (2018)
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

    var utterance = uniformDraw(utterances);
    var L0 = listener0(utterance);

    var utilities = {
      inf: L0.score(state), // log P(s | u)
      soc: expectation(L0) // E[s]
    }
    var speakerUtility = phi * utilities.inf +
        (1-phi) * utilities.soc - cost(utterance);

    factor(speakerOptimality * speakerUtility);

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
///

var speaker2 = function(state, phi, weights) {
  Infer({model: function(){

    var utterance = uniformDraw(utterances);
    var L1 = listener1(utterance);
    var L1_state = marginalize(L1, "state");
    var L1_phi = marginalize(L1, "phi");

    var utilities = {
      inf: L1_state.score(state), // log P(s | u)
      soc: expectation(L1_state), // E [s]
      pres: L1_phi.score(phi) // // log P(phi | u)
    }

    var totalUtility = weights.soc * utilities.soc +
        weights.pres * utilities.pres +
        weights.inf * utilities.inf - cost(utterance);

    factor(speakerOptimality2 * totalUtility)

    var utt = utterance.split("_")
    return {
      "utterance particle": utt[0], utterance: utt[1]
    }

  }})
};

// Parameter values = Maximum A-Posteriori values from Yoon, Tessler et al., (2018)
display('Listener gives presentation, worthy of 0 out of 3 hearts ("truly terrible")...')

// informational
display("Speaker wants to give Listener accurate and informative feedback")
viz(speaker2(0, 0.5, {soc: 0.05, pres: 0.60, inf: 0.35}))

// social
display("Speaker wants to make Listener feel good")
viz(speaker2(0, 0.35, {soc: 0.30, pres: 0.45, inf: 0.25}))

// both
display("Speaker wants to make Listener feel good AND give accurate and informative feedback")
viz(speaker2(0, 0.35, {soc: 0.10, pres: 0.55, inf: 0.35}))

// Condescending - speaker wants to convey only truth but appear to be social
display("Speaker wants Listener to view them as nice while they are actually rude")
viz(speaker2(1, 0.40, {soc: 0.02, pres: 0.90, inf: 0.8}))

// Friend - Your friend wants to be as balanced as possible, but internally does not want to hurt your feelings
display("Speaker wants to appear as balanced as possible but not hurt a friends feelings")
viz(speaker2(1, 0.5, {soc: 0.6, pres: 0.7, inf: 0.5}))

// Comparing presentational weights
display("What happens as a speaker’s desire to save face increases?")
viz(speaker2(1, 0.50, {soc: 0.5, pres: 0.0, inf: 0.5}))
viz(speaker2(1, 0.50, {soc: 0.5, pres: 0.1, inf: 0.5}))
~~~