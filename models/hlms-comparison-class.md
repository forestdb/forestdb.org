---
layout: model
title: Huang, Liu, Moreno, Scott - Comparison class
model-language: webppl
---

LSCI 107M Final Project Write-Up - Inferring the Comparison Class
------------------------------------------------------------------
Emily Kurihara Scott
Ann Liu
Sandra Moreno
Shenyan Huang
------------------------------------------------------------------
Professor Scontras
7 June 2022
------------------------------------------------------------------


The Empirical Phenomenon of Interest
------------------------------------------------------------------
Let’s take the adjective ‘tall’ for example. If one says “John is a basketball player and is tall”, your intuitions would lead you to believe that you are comparing John to “all people” (the superordinate category). If one also says that “John is a gymnast and is tall”, your intuitions would tell you that John is short compared to “all people”. If one says “John is a soccer player and is tall/short”, it might just be that John is tall/short within the group of soccer players. 

This adjective model reasons about these inferences that hold uncertainty about the relevant comparison classes: the superordinate (all people) or the subordinate (e.g., gymnasts, soccer players, basketball players) categories. 


The RSA Framework
------------------------------------------------------------------
The Bayesian Rational Speech Act framework outlines that speakers and listeners reason about each other’s reasoning about the literal interpretations of utterances. Language heavily relies on context–this is why the RSA framework puts semantics at the forefront of calculating meaning and understanding. Being able to reason about likely interpretations, provides an explanation to something like our model of specifying the thresholds in degree semantics.


Similarities between this Model and Others
------------------------------------------------------------------
Compared to the gradable adjectives model (Chapter 5 Application 1), in the first model we test to see what the word expensive means in between two different contexts. We take the adjective expensive for a sweater vs expensive for a laptop, however, these two phrases hold a different meaning depending on their context. (Expensive means more than the given price.) In our model we use the adjective tall: “tall for a basket player” and “tall for a gymnast.” In these models the adjectives (expensive and tall)  hold different meanings depending on context. Additionally, both of these models use prior knowledge like knowing the range of prices of items and the height of people. Compared to Application 1 in this chapter, both speakers could choose the utterance “ ” = “silence”, as remaining in silence is never wrong.


Differences between this Model and Others
------------------------------------------------------------------
As for differences, in this model we have uncertainty surrounding the comparison class. In the previous application, we knew what “expensive for a watch/laptop/headphones…etc.” meant. It meant that the price for said item was higher than what you would normally expect. In this model, we do not know if the speaker is referring to a superordinate category (all people), or to the more specific group of people mentioned (basketball players, gymnasts, soccer players). Our model can also take in 2 adjectives (in the form of the ‘utterances’ section) and utilizes the “threshold” variable that we have not seen in other models we have covered in class.


The Model - Simulating the Heights of Each Comparison Class
------------------------------------------------------------------
Math.exp(x) will return a number representing e^x, where e is Euler's number and x is the argument. This function just makes numbers bigger and later cancels out a log() answer in var stateProbs. We can check this value and what it does by running it exp(some number).

The graph is continuous, binParam just helps us with visualizing it. Discretization is the process through which we can transform continuous variables, models or functions into a discrete form. We do this by creating a set of contiguous intervals (or bins) that go across the range of our desired variable/model/function.


~~~~
///fold: 
// helper function
var exp = function(x){return Math.exp(x)}

// for discretization
var binParam = 3;
~~~~


In the structured object superordinate_params, mu corresponds to the mean of the distribution of heights for the subordinate category of all people, and sigma to the standard deviation of that distribution.

stateVals is assigned to a list created by the _.range function. This function takes in 3 integer arguments. The first number is the start -3. The second number is the stop 3. The last number is the step. The returned list includes all the numbers from the start to the end with an internal of the step size 1/6.


~~~~
// information about the superordinate category prior
// e.g., the height distribution for all people
var superordinate_params = {mu: 0, sigma: 1};

// calculate the range in pre-defined steps;
// these values correspond to possible heights
var stateVals = _.range(superordinate_params.mu - 3 * superordinate_params.sigma,
                        superordinate_params.mu + 3 * superordinate_params.sigma,
                        superordinate_params.sigma/binParam)
~~~~


In the stateProbs function, the map function takes a function and an array and will run the function for each value in the array. .score(s) results in a log() answer, the exp function cancels it out so it just returns a probability between 0 and 1. Gaussian means a normal distribution or a normal bell shaped curve. In totality, stateProbs looks up the probability associated with s (each state in stateVals) in the normal distribution with the mu/sigma specified by parameter stateParams.

In the generateStatePrior function, stateParams is also taken as an argument. The function returns a categorical distribution using the probabilities from stateProbs(stateParams) and applies it to all the states generated by stateVals.


~~~~
// for each possible height, calculate its probability of occurrence
var stateProbs = cache(function(stateParams){
  return map(function(s){
    exp(Gaussian(stateParams).score(s))
  }, stateVals)
});

// generate a statePrior using the possible heights and their probabilities
var generateStatePrior = cache(function(stateParams) {
  return Infer({
    model: function(){
      return categorical({vs: stateVals, ps: stateProbs(stateParams)})
    }
  })
});
~~~~


This next part handles simulating the thresholds of each adjective, which are unique to this model. The form parameter in the thresholdPrior function could be seen as the locus of what distinguishes this model from the model in Application 1 of Chapter 5. Used as the name of a property inside thresholdBins, form can be either "positive" or "negative", with the former corresponding to the "tall" adjective and the latter to "short". 

The value obtained by thresholdBins[form] is a list of the given adjective's thresholds corresponding to each state in stateVals. map() takes in a state value and subtracts 1/6 from the positive state value and adds ⅙ from the negative state value. Ultimately, to assign a ‘tall’ or ‘short’ characteristic to the state (height), we need some kind of boundary or marker to characterize that state. To have a state be considered ‘tall’, the threshold value should be below that actual state so that anything above the threshold is considered tall; the same goes for a state being considered ‘short’--you would need a threshold value above the actual state so anything below that is considered short.

Finally, the threshold prior will return a list that is uniformly drawn from the thresholdBins created in the previous chunk of code.


~~~~
// generate the uniform threshold prior
var thresholdBins ={
  positive: map(function(x){
    return  x - (1/(binParam*2));
  }, sort(stateVals)),
  negative: map(function(x){
    return  x + (1/(binParam*2));
  }, sort(stateVals))
};

var thresholdPrior = cache(function(form){
  return Infer({
    model: function() { return uniformDraw(thresholdBins[form]) }
  });
});
~~~~


The values inside subParams indicate that gymnasts have a lower height on average compared to regular people, soccer players are the same but their distribution is different (more centrated at the mean), and basketball players are on average taller than regular people– “regular or all” people refers to the superordinate category.

Inside the list of possible utterances, the null utterance is “silence” which is not saying anything at all.


~~~~
// information about the superordinate category priors
var subParams = {
  gymnasts: {mu: -1, sigma: 0.5}, // gymnast heights
  soccerPlayers: {mu: 0, sigma: 0.5}, // soccer player heights
  basketballPlayers: {mu: 1, sigma: 0.5} // basketball player heights
}

// possible utterances can be either positive (tall) or negative (short) or a null utterance
var utterances = ["tall", "short", "silence"]
~~~~

Meaning Function & classPrior
------------------------------------------------------------------
The meaning function takes in an utterance, state, and the parameter thresholds. It will check that the state is greater than or less than the threshold for the utterance passed into the function. If the utterance is neither "tall" nor "short", then the utterance is a null utterance and the meaning function will return true.

The classPrior function returns either the "subordinate" or "superordinate" comparison class with equal probability.

~~~~
// meaning function for utterances
var meaning = function(utterance, state, thresholds) {
  utterance == "tall" ? state > thresholds.tall :
  utterance == "short" ? state < thresholds.short :
  true
}

// assume a uniform prior over comparison classes
var classPrior = Infer({
  model: function(){return uniformDraw(["subordinate", "superordinate"])}
});
~~~~

Literal Listener (L0)
------------------------------------------------------------------
The literal listener is a function that takes in an utterance, thresholds, and a parameter called comparisonClass. comparisonClass takes the form of a structured object containing mu and sigma values which correspond to the distribution of heights for any given comparison class. The utterance comes from the speaker function, while the thresholds and comparisonClass come from the pragmatic listener.

Inside the literal listener, comparisonClass is passed into the generateStatePrior function, which returns the prior probabilities for each state. This result is our state prior, from which the literal listener samples a state. The literal listener then calls the meaning function with utterance, state, and thresholds as arguments. It will return the state if the meaning function returns true.

~~~~
var literalListener = cache(
  function(utterance, thresholds, comparisonClass) {
    Infer({model: function(){
      var StatePrior = generateStatePrior(comparisonClass)
      var state = sample(StatePrior);
      var m = meaning(utterance, state, thresholds);
      condition(m);
      return state;
    }})
  }, 10000 // limit cache size
)
~~~~

Speaker (S1)
------------------------------------------------------------------
The speaker is a function that takes in a state, thresholds, and comparisonClass. These are all obtained from inside the pragmaticListener function. 

The speaker first takes an utterance from a flat distribution of the possible utterances. It'll then run literalListener with the newly obtained utterance, alongside the thresholds and comparisonClass parameters. Depending on the speaker optimality variable and how likely the utterance is to convey the state, that utterance may be returned.

~~~~
// set speaker optimality
var alpha = 5;

var speaker1 = cache(
  function(state, thresholds, comparisonClass) {
    Infer({model: function(){
      var utterance = uniformDraw(utterances);
      var L0 = literalListener(utterance, thresholds, comparisonClass);
      factor( alpha * L0.score(state) );
      return utterance;
    }})
  }, 10000 // limit cache size
)
~~~~

Pragmatic Listener (L1)
------------------------------------------------------------------
The pragmatic listener is a function that has the parameters utterance and subordinate_params. subordinate_params, like the comparisonClass parameter from the literal listener, contains mu/sigma information but only specifically for the heights of a given subordinate class. It is passed into the generateStatePrior function, which returns the prior probabilities for each state. This result is our state prior, from which the pragmatic listener samples a state.
 
The variable thresholds generates the "tall" or "short" threshold depending on the adjective passed into thresholdPrior. 

A comparison class ("subordinate" or "superordinate") is then sampled from the classPrior. The pragmatic listener checks to see if this comparison class (var c) is "subordinate" or not, in order to properly assign the comparisonClass variable to the appropriate mu/sigma values. 

The speaker1 function is run with the newly obtained state, thresholds, and comparisonClass variables. The pragmatic listener observes the return value of the speaker function to see if it matches L1's given utterance. If they match, then the comparison class, c, is returned along with the state.

~~~~
var pragmaticListener = cache(function(utterance, subordinate_params) {
  Infer({model: function(){

    var statePrior = generateStatePrior(subordinate_params);
    var state = sample(statePrior);
    // separate thresholds for positive adjective and negative adjective
    var thresholds = {
      tall: sample(thresholdPrior("positive")),
      short: sample(thresholdPrior("negative"))
    }

    // uncertainty about the comparison class (superordinate vs. subordinate)
    var c = sample(classPrior)
    var comparisonClass = c == "subordinate" ? subordinate_params : superordinate_params

    var S1 = speaker1(state, thresholds, comparisonClass);
    observe(S1, utterance);

    return { comparisonClass: c, state : state }
  }})
}, 10000 // limit cache size
                             )
~~~~

Pragmatic Listener's (L1) Predictions
------------------------------------------------------------------
L1predictions generates a set of prediction using map(), which takes a list over exptConditions, run the stim function over the first object  {utt: "tall", sub: "basketballPlayers"}, second object {utt: "short", sub: "basketballPlayers"} to the last object, then visualize them together in a plot. stim refers to each structured object in exptConditions.

In the pragmatic listener posterior, pragmatic listener takes into an utterance and the mu/sigma structured object associated with the a sub category from exptConditions. The L1predictions function returns that utterance and prints out “P(superordinate comparison class)”, it will jointly infer the L1 posterior and marginalize out the comparison class variable from the L1 posterior distribution. 
It looks up and returns the probability associated with superordinate class in the marginalized L1 posterior distribution, along with the subordinate category (stim.sub), and the name of the model ("L1").

The first four display statements print the pragmatic listener's predicted heights of a basketball player when they are said to be either "tall" or "short".

The final line shows the graph of the probabilities that the superordinate class is the comparison class in describing each subordinate category as either "tall" or "short".

~~~~
// the possible experiment conditions:
// you hear that someone is a member of a subordinate category
// then you are told that they are tall/short;
// the task is to figure out the implicit comparison class
var exptConditions = [
  {utt: "tall", sub: "basketballPlayers"},
  {utt: "short", sub: "basketballPlayers"},
  {utt: "tall", sub: "soccerPlayers"},
  {utt: "short", sub: "soccerPlayers"},
  {utt: "tall",  sub: "gymnasts"},
  {utt: "short", sub: "gymnasts"}
];

// generate structure predictions by mapping through the experiment conditions
var L1predictions = map(function(stim){
  var L1posterior = pragmaticListener(stim.utt, subParams[stim.sub])
  return {
    utterance: stim.utt,
    "P(superordinate comparison class)": exp(marginalize(L1posterior, "comparisonClass").score("superordinate")),
    "subordinate category": stim.sub,
    model: "L1"
  }
}, exptConditions)

display("the basketball player is tall")
display("--> height = " + expectation(marginalize(pragmaticListener("tall",{mu: 1, sigma: 0.5}), "state")))
display("the basketball player is short")
display("--> height = " + expectation(marginalize(pragmaticListener("short",{mu: 1, sigma: 0.5}), "state")))

display("probability of superordinate comparison class (i.e., tall for all people)")
viz.bar(L1predictions, {groupBy: "subordinate category"})
~~~~

Final model
------------------------------------------------------------------

~~~~
///fold:
// helper function
var exp = function(x){return Math.exp(x)}

// for discretization
var binParam = 3;

// information about the superordinate category prior
// e.g., the height distribution for all people
var superordinate_params = {mu: 0, sigma: 1};

// calculate the range in pre-defined steps;
// these values correspond to possible heights
var stateVals = _.range(superordinate_params.mu - 3 * superordinate_params.sigma,
                        superordinate_params.mu + 3 * superordinate_params.sigma,
                        superordinate_params.sigma/binParam)

// for each possible height, calculate its probability of occurrence
var stateProbs = cache(function(stateParams){
  return map(function(s){
    exp(Gaussian(stateParams).score(s))
  }, stateVals)
});

// generate a statePrior using the possible heights and their probabilities
var generateStatePrior = cache(function(stateParams) {
  return Infer({
    model: function(){
      return categorical({vs: stateVals, ps: stateProbs(stateParams)})
    }
  })
});

// generate the uniform threshold prior
var thresholdBins ={
  positive: map(function(x){
    return  x - (1/(binParam*2));
  }, sort(stateVals)),
  negative: map(function(x){
    return  x + (1/(binParam*2));
  }, sort(stateVals))
};

var thresholdPrior = cache(function(form){
  return Infer({
    model: function() { return uniformDraw(thresholdBins[form]) }
  });
});
///

// information about the superordinate category priors
var subParams = {
  gymnasts: {mu: -1, sigma: 0.5}, // gymnast heights
  soccerPlayers: {mu: 0, sigma: 0.5}, // soccer player heights
  basketballPlayers: {mu: 1, sigma: 0.5} // basketball player heights
}

// possible utterances can be either positive (tall) or negative (short) or a null utterance
var utterances = ["tall", "short", "silence"]

// meaning function for utterances
var meaning = function(utterance, state, thresholds) {
  utterance == "tall" ? state > thresholds.tall :
  utterance == "short" ? state < thresholds.short :
  true
}

// assume a uniform prior over comparison classes
var classPrior = Infer({
  model: function(){return uniformDraw(["subordinate", "superordinate"])}
});

// set speaker optimality
var alpha = 5;

var literalListener = cache(
  function(utterance, thresholds, comparisonClass) {
    Infer({model: function(){
      var StatePrior = generateStatePrior(comparisonClass)
      var state = sample(StatePrior);
      var m = meaning(utterance, state, thresholds);
      condition(m);
      return state;
    }})
  }, 10000 // limit cache size
)

var speaker1 = cache(
  function(state, thresholds, comparisonClass) {
    Infer({model: function(){
      var utterance = uniformDraw(utterances);
      var L0 = literalListener(utterance, thresholds, comparisonClass);
      factor( alpha * L0.score(state) );
      return utterance;
    }})
  }, 10000 // limit cache size
)

var pragmaticListener = cache(function(utterance, subordinate_params) {
  Infer({model: function(){

    var statePrior = generateStatePrior(subordinate_params);
    var state = sample(statePrior);
    // separate thresholds for positive adjective and negative adjective
    var thresholds = {
      tall: sample(thresholdPrior("positive")),
      short: sample(thresholdPrior("negative"))
    }

    // uncertainty about the comparison class (superordinate vs. subordinate)
    var c = sample(classPrior)
    var comparisonClass = c == "subordinate" ? subordinate_params : superordinate_params

    var S1 = speaker1(state, thresholds, comparisonClass);
    observe(S1, utterance);

    return { comparisonClass: c, state : state }
  }})
}, 10000 // limit cache size
                             )

// the possible experiment conditions:
// you hear that someone is a member of a subordinate category
// then you are told that they are tall/short;
// the task is to figure out the implicit comparison class
var exptConditions = [
  {utt: "tall", sub: "basketballPlayers"},
  {utt: "short", sub: "basketballPlayers"},
  {utt: "tall", sub: "soccerPlayers"},
  {utt: "short", sub: "soccerPlayers"},
  {utt: "tall",  sub: "gymnasts"},
  {utt: "short", sub: "gymnasts"}
];

// generate structure predictions by mapping through the experiment conditions
var L1predictions = map(function(stim){
  var L1posterior = pragmaticListener(stim.utt, subParams[stim.sub])
  return {
    utterance: stim.utt,
    "P(superordinate comparison class)": exp(marginalize(L1posterior, "comparisonClass").score("superordinate")),
    "subordinate category": stim.sub,
    model: "L1"
  }
}, exptConditions)

display("the basketball player is tall")
display("--> height = " + expectation(marginalize(pragmaticListener("tall",{mu: 1, sigma: 0.5}), "state")))
display("the basketball player is short")
display("--> height = " + expectation(marginalize(pragmaticListener("short",{mu: 1, sigma: 0.5}), "state")))

display("probability of superordinate comparison class (i.e., tall for all people)")
viz.bar(L1predictions, {groupBy: "subordinate category"})
~~~~

Discussion of Results
------------------------------------------------------------------
According to the visualization, when the pragmatic listener hears “The basketball player is tall”, the probability that the comparison class is the superordinate category (of all people) is higher than if they had heard “The basketball player is short”. The opposite is the same for when “The gymnast is tall” is said; the probability that the height is being compared to the superordinate category is lower than if the //pragmaticListener were to hear “The gymnast is short”. For “The soccer player is short/tall” – there is an equal probability that the comparison class is either the superordinate or subordinate. If we take a look at the visualization as a table instead of a bar graph (commented out within the code), there is an ever-so-slightly higher probability for the “short” utterance. “The soccer player is short” would have a slightly higher chance of the comparison class being the superordinate instead of the subordinate.
