---
layout: model
title: Politeness - Qiyan, Mackenzie, Caitlyn
model-language: webppl
---

Similar to the previous politeness model study we discussed in class, Yoon et al. (2018) constructs a scenario where the speaker gives feedback (0, 1, 2, or 3 stars) to the listener’s creative work (e.g. a poem). In the model, there are 4 states of the world, corresponding to the star rating, where 3 stars is the best and 0 stars is the worst. 

A new addition to this model is the self-presentational utility – a way that speakers signal to the listeners their conversation goals. In other words, how do the speakers want to present their epistemic/informational and social goals, even if those are not their real intentions? This new utility allows the speaker to signal to the listener that they care both about epistemic and social goals though it’s not possible in reality. 

Prioritizing the presentational utility leads speakers to use indirect speech, realized as negated adjectival phrases in this model. There are 8 possible utterances: 4 without negation (“terrible”, “bad”, “good”, “amazing”) and 4 with negation (“not terrible”, “not bad”, “not good”, “not amazing”). The negated utterances are more difficult to comprehend than the unnegated utterances, so they have a higher cost of 0.35 while the unnegated utterances have a cost of 0.

Authors obtained literal semantics values through an experiment probing participant judgments. They were presented with a state (0 to 3) and an utterance (“Do you think Ann thought the
presentation was / wasn’t [1 of 8 utterances]?”) and responded with “yes” or “no”.

L0 stays the same as the vanilla RSA model. They only interpret the literal semantics of the utterances according to the meaning function.

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

Infer(function(){
  return listener0
})
~~~~

S1 stays the same as the previous politeness model we covered in class. The speaker considers their informational(being truthful) and social (being kind) goals, and the phi φ value determines which goal they prioritize. 

~~~~
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

image?

<img src="https://cdn.discordapp.com/attachments/700550857913270273/1379153139373969573/image.png?ex=683f33f3&is=683de273&hm=c7509c3d5a2e91545ffc9f6240e542cb253d728ff9a27401a9f3ace34d16a091&" alt="Alt Text" style="width:70%; height:auto;">


some Equation

<img src="https://cdn.discordapp.com/attachments/700550857913270273/1379175454371610694/image.png?ex=683f48bc&is=683df73c&hm=9a40ea22e370a5094b1732db5eec6d3166d8e91f25003757460504045cb696b9&" alt="Alt Text" style="width:70%; height:auto;">
