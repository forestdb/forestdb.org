---
layout: model
title: Comparison Class - Kiara, Noah, Kiley
model-language: webppl
---


***Comparison Class Model:***

As previously seen in the adjectives model, when we hear descriptors such as *tall* or *expensive*, we interpret them relative to a comparison class. For example, we have different comparison classes for an expensive watch as opposed to an expensive sweater.

But, what if we don't know the comparison class?

For this model, we are considering the adjective *tall*:
- "John is a basketball player, and he is tall."
- "John is a gymnast, and he is tall".
- "John is a soccer player, and he is tall."

You likely have different ideas of which comparison class is used for each statement. For John, being a basketball player, we're likely to think that he is being compared to all people or the general population. When John is a gymnast, his being tall is likely a comparison to gymnasts specifically, so he would be short relative to all people. And when John is a soccer player, we might infer that it is just for soccer players and not the general population. 

These different comparison classes can be defined as:
- *Superordinate* or the general population 
- *Subordinate* or the specific group (basketball players, gymnasts, soccer players)

By capturing the uncertainty over which comparison class is being used (superordinate vs. subordinate), Tessler et. al (2017) extend the adjective model to account for this ambiguity. We will point out the similarities and differences between the two models as we go through.  







**Helper Function**
- A simple function to calculate the exponential of a number.
- Used later for converting log-probabilities to probabilities.


~~~~
var exp = function(x){ return Math.exp(x) }
~~~~



**Height Space Discretization**
- Controls how finely we discretize, or split up, the range of possible heights.
- Defines the distribution for the general population (the "superordinate" comparison class). We have a mean of 0 and a standard deviation of 1, which is just a normal distribution. 
- Heights are not used in a literal way***

~~~~
// discretization
var binParam = 3;

var superordinate_params = {mu: 0, sigma: 1};
~~~~



**Possible Height Values**
- Create a list of possible heights that an individual might have 
- Instead of looking at every number, we pick a range and break it into steps (binParam) 
- The range is 3 standard deviations below and above the average. We use 3 standard deviations because in a normal curve, most values fall within 3 standard deviations from the mean. So, this range covers almost all the realistic heights someone could have.


~~~~
///fold:
// helper function
var exp = function(x){return Math.exp(x)}

// discretization
var binParam = 3;

// information about the superordinate category prior
var superordinate_params = {mu: 0, sigma: 1};
///


var stateVals = _.range(superordinate_params.mu - 3 * superordinate_params.sigma,
                        superordinate_params.mu + 3 * superordinate_params.sigma,
                        superordinate_params.sigma / binParam)
stateVals
~~~~



**Probabilities for Each Height**
- For each possible height, we calculate how likely it is using a normal distribution (Gaussian).
- The model first gets the log-probability for each height value.
- We then apply the exponential function to convert those log values into regular probabilities.
- This results in a probability distribution over all the possible heights we’re considering.

~~~~
///fold:
// helper function
var exp = function(x){return Math.exp(x)}

// for discretization
var binParam = 3;

// information about the superordinate category prior
var superordinate_params = {mu: 0, sigma: 1};

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

stateProbs({mu: 0, sigma: 1})
~~~~



**Generate State Prior**
- Here we create a pior distribution over possible heights using the values (vs) and their probabilities (ps).
- What we believe about someone's height before hearing they are tall.



~~~~
///fold:
// helper function
var exp = function(x){return Math.exp(x)}

// discretization
var binParam = 3;

// information about the superordinate category prior
var superordinate_params = {mu: 0, sigma: 1};

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
///


var generateStatePrior = cache(function(stateParams) {
  return Infer({
    model: function(){
      return categorical({vs: stateVals, ps: stateProbs(stateParams)})
    }
  })
});

generateStatePrior({mu: 0, sigma: 1})
~~~~



**Thresholds for Tall**
- To decide if someone is tall, we need a threshold.
- The bins are slightly adjusted from the height values to compute these thresholds.
- Positive and negative thresholds model different assumptions (e.g., tall vs. short).

~~~~
///fold:
// helper function
var exp = function(x){return Math.exp(x)}

// for discretization
var binParam = 3;

// information about the superordinate category prior
var superordinate_params = {mu: 0, sigma: 1};

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
/// 

var thresholdBins = {
  positive: map(function(x){ return x - (1/(binParam*2)); }, sort(stateVals)), //tall
  negative: map(function(x){ return x + (1/(binParam*2)); }, sort(stateVals)) //short
};

display(thresholdBins.positive)
display(thresholdBins.negative)
~~~~



**Threshold Prior**
- Samples a threshold uniformly from the defined bins.
- Uncertainty about what "tall" actually means numerically.

~~~~
///fold:
// helper function
var exp = function(x){return Math.exp(x)}

// for discretization
var binParam = 3;

// information about the superordinate category prior
var superordinate_params = {mu: 0, sigma: 1};

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
///

var thresholdPrior = cache(function(form){
  return Infer({
    model: function() { return uniformDraw(thresholdBins[form]) }
  });
});

viz.density(thresholdBins.positive)
viz.density(thresholdBins.negative)
~~~~



**Subordinate Category Priors**
- Specific group priors over height.
- Each group (gymnasts, soccer players, basketball players) has its own mean and standard deviation.
- Just to point out, the specific athletic groups have smaller standard deviations, compared to the whole population, because of the fact that they are all doing the same activity.
- Used when assuming the comparison class is the specific group (subordinate).

~~~~
///fold:
// helper function
var exp = function(x){return Math.exp(x)}

// for discretization
var binParam = 3;

// information about the superordinate category prior
var superordinate_params = {mu: 0, sigma: 1};

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

var subParams = {
  gymnasts: {mu: -1, sigma: 0.5}, // gymnast heights
  soccerPlayers: {mu: 0, sigma: 0.5}, // soccer player heights
  basketballPlayers: {mu: 1, sigma: 0.5} // basketball player heights
}
~~~~

Let's start with the utterance and meaning functions, adapted from the basic adjective model. 

For the utterance model, there are three possible utterances, characterized as positive ("tall"), negative ("short"), and null ("silence").

For the meaning function, the goal is to return a boolean (*true* or *false*) that reflects whether the given utterance correctly describes the given state's relationship to the given threshold. *In the basic adjective model, our meaning function was similar, where it took in utterance, price, and theta. In our model, price is similar to state and theta provided a relevant threshold, like our thresholds object*

~~~~
var utterances = ["tall", "short", "silence"]

var meaning = function(utterance, state, thresholds) {
  utterance == "tall" ? state > thresholds.tall :
  utterance == "short" ? state < thresholds.short :
  true
}



display(meaning("tall", 2, {tall: 1, short:-1}))
display(meaning("short", 2, {tall: 1, short:-1}))
display(meaning("null", 2, {tall: 1, short:-1}))
~~~~

This model turns on the idea of superordinate and subordinate comparison classes. We have defined priors about these subordinate classes (**subParams**), but now let's create a uniform prior over these classes.

~~~~
var classPrior = Infer({
  model: function(){return uniformDraw(["subordinate", "superordinate"])}
})



viz(classPrior)
~~~~

So to get into the model itself, we start with the L₀. This model takes in an utterance, a set of thresholds, and the **comparison class**. 

~~~~
//remember:
// the superordinate category has a prior as follows (e.g. the height distribution for all people):
var superordinate_params = {mu: 0, sigma: 1}

// also, the subordinate categories have priors as follows:
var subParams = {
  gymnasts: {mu: -1, sigma: 0.5},
  soccerPlayers: {mu: 0, sigma: 0.5},
  basketballPlayers: {mu: 1, sigma: 0.5}
}



var literalListener = cache(
  function(utterance, thresholds, comparisonClass) {
    Infer({model: function(){
      var StatePrior = generateStatePrior(comparisonClass)
      var state = sample(StatePrior);
      var m = meaning(utterance, state, thresholds);
      condition(m);
      return state;
    }})
  }, 10000
)
~~~~

Let's break down exactly what the comparison class argument is. In the model, it is defined as an object that is passed into the L₀ from the S₁, through the L₁.

We have prior knowledge about the distribution of heights that various classes have (i.e. gymnasts are usually shorter than basketball players, and the probability distributions of their heights reflects that knowledge). The reason behind including this **comparison class** object is to code in the idea that a listener may be uncertain about whether a statement like "John is tall" means that John is tall *compared to all people* or *compared to basketball players*. 

This object is defined as follows:

~~~~
/// fold:
var exp = function(x){return Math.exp(x)}

var binParam = 3;

var superordinate_params = {mu: 0, sigma: 1};

var stateVals = _.range(superordinate_params.mu - 3 * superordinate_params.sigma,
                        superordinate_params.mu + 3 * superordinate_params.sigma,
                        superordinate_params.sigma/binParam)

var stateProbs = cache(function(stateParams){
  return map(function(s){
    exp(Gaussian(stateParams).score(s))
  }, stateVals)
});

var generateStatePrior = cache(function(stateParams) {
  return Infer({
    model: function(){
      return categorical({vs: stateVals, ps: stateProbs(stateParams)})
    }
  })
});

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

var subParams = {
  gymnasts: {mu: -1, sigma: 0.5},
  soccerPlayers: {mu: 0, sigma: 0.5},
  basketballPlayers: {mu: 1, sigma: 0.5}
}

var utterances = ["tall", "short", "silence"]

var meaning = function(utterance, state, thresholds) {
  utterance == "tall" ? state > thresholds.tall :
  utterance == "short" ? state < thresholds.short :
  true
}

var classPrior = Infer({
  model: function(){return uniformDraw(["subordinate", "superordinate"])}
});
///

// for the sake of running the L0, 
// i have defined the subordinate_params with the gymnast parameters
var subordinate_params = {mu: -1, sigma: 0.5}

// and i have defined the thresholds in the variable below
// the full model defines these within the L1, which we will see later
var thresholds = {
  tall: -2,
  short: -2.5
  }

var literalListener = cache(
  function(utterance, thresholds, comparisonClass) {
    Infer({model: function(){
      var StatePrior = generateStatePrior(comparisonClass)
      var state = sample(StatePrior);
      var m = meaning(utterance, state, thresholds);
      condition(m);
      return state;
    }})
  }, 10000
)



// var c = sample(classPrior)
// var comparisonClass = c == "subordinate" ? subordinate_params : superordinate_params
      
viz(literalListener("tall", thresholds, subordinate_params))

viz(literalListener("tall", thresholds, superordinate_params))
~~~~

Next we move on to the S₁.

This piece is pretty straightforward. The S₁ is a function that takes in the **state** the speaker wishes to communicate, the thresholds, and the comparison class. Like our other models we have investigated this quarter, it returns a probability distribution over utterances.

In order to calculate this probability distribution, it samples an utterance then scores the L0 (using the same threshold and comparison class criteria).

~~~~
/// fold:
var exp = function(x){return Math.exp(x)}

var binParam = 3;

var superordinate_params = {mu: 0, sigma: 1};

var stateVals = _.range(superordinate_params.mu - 3 * superordinate_params.sigma,
                        superordinate_params.mu + 3 * superordinate_params.sigma,
                        superordinate_params.sigma/binParam)

var stateProbs = cache(function(stateParams){
  return map(function(s){
    exp(Gaussian(stateParams).score(s))
  }, stateVals)
});

var generateStatePrior = cache(function(stateParams) {
  return Infer({
    model: function(){
      return categorical({vs: stateVals, ps: stateProbs(stateParams)})
    }
  })
});

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

var subParams = {
  gymnasts: {mu: -1, sigma: 0.5},
  soccerPlayers: {mu: 0, sigma: 0.5},
  basketballPlayers: {mu: 1, sigma: 0.5}
}

var utterances = ["tall", "short", "silence"]

var meaning = function(utterance, state, thresholds) {
  utterance == "tall" ? state > thresholds.tall :
  utterance == "short" ? state < thresholds.short :
  true
}

var classPrior = Infer({
  model: function(){return uniformDraw(["subordinate", "superordinate"])}
});

var alpha = 5;
///

var literalListener = cache(
  function(utterance, thresholds, comparisonClass) {
    Infer({model: function(){
      var StatePrior = generateStatePrior(comparisonClass)
      var state = sample(StatePrior);
      var m = meaning(utterance, state, thresholds);
      condition(m);
      return state;
    }})
  }, 10000 
)

var speaker = cache(
  function(state, thresholds, comparisonClass) {
    Infer({model: function(){
      var utterance = uniformDraw(utterances);
      var L0 = literalListener(utterance, thresholds, comparisonClass);
      factor( alpha * L0.score(state) );
      return utterance;
    }})
  }, 10000 
)



//test function with basketball player subordinate params
var subSpeaker = function() {
    Infer({model: function(){
      var subordinate_params = {mu: -1, sigma: 0.5}
      var thresholds = {
        tall: -2,
        short: -2.5
      }
      var c = "subordinate"
      var comparisonClass = c == "subordinate" ? subordinate_params : superordinate_params
      var StatePrior = generateStatePrior(comparisonClass)
      var state = sample(StatePrior)
      var utterance = uniformDraw(utterances);
      var L0 = literalListener(utterance, thresholds, comparisonClass);
      factor( alpha * L0.score(state) );
      return utterance;
    }})
  }
  
var superSpeaker = function() {
    Infer({model: function(){
      var subordinate_params = {mu: -1, sigma: 0.5}
      var thresholds = {
        tall: -2,
        short: -2.5
      }
      var c = "superordinate"
      var comparisonClass = c == "subordinate" ? subordinate_params : superordinate_params
      var StatePrior = generateStatePrior(comparisonClass)
      var state = sample(StatePrior)
      var utterance = uniformDraw(utterances);
      var L0 = literalListener(utterance, thresholds, comparisonClass);
      factor( alpha * L0.score(state) );
      return utterance;
    }})
  }
  
viz(subSpeaker())
viz(superSpeaker())
~~~~

Finally, the L₁. 

Similar to many other models we've looked at, the L₁'s goal is to infer the state that the speaker is trying to communicate. In our model, the L₁ is also attempting to infer the comparison class (e.g. is the speaker saying that John is tall compared to all people, or just to gymnasts?) In doing so, it runs **Infer** over a function that returns a structured object with both the comparison class and state.

The L₁ takes in two arguments: the **utterance** and the **subordinate category** (N.B. the L₁ is told the subordinate category and **not** the comparison class. It then generates a state prior from the given subordinate category, sampling a state from said prior. The thresholds are sampled like in the previous speaker and listener layers. Finally, the **comparisonClass** function is run, sampling either the subordinate or superordinate class.

With these variables defined, the L₁ function runs the speaker function with the sampled state, the sampled thresholds, and the sampled comparison class. The speaker function returns a distribution (since it runs **Infer** over a function) which it then runs through the **observe** function alongside the original utterance: this reweights the world state and comparison class hypothesis based on how likely a speaker with those parameters would have produced the observed utterance.

*The L₁ uses **observe**, unlike the rest of the model which uses **score** and **factor**.
~~~~
var pragmaticListener = cache(function(utterance, subordinate_params) {
  Infer({model: function(){

    var statePrior = generateStatePrior(subordinate_params);
    var state = sample(statePrior);
    var thresholds = {
      tall: sample(thresholdPrior("positive")),
      short: sample(thresholdPrior("negative"))
    }
    var c = sample(classPrior)
    var comparisonClass = c == "subordinate" ? subordinate_params : superordinate_params
    var S = speaker(state, thresholds, comparisonClass);
    observe(S, utterance);

    return { comparisonClass: c, state : state }
  }})
}, 10000)


/// fold:
var exp = function(x){return Math.exp(x)}

var binParam = 3;

var superordinate_params = {mu: 0, sigma: 1};

var stateVals = _.range(superordinate_params.mu - 3 * superordinate_params.sigma,
                        superordinate_params.mu + 3 * superordinate_params.sigma,
                        superordinate_params.sigma/binParam)

var stateProbs = cache(function(stateParams){
  return map(function(s){
    exp(Gaussian(stateParams).score(s))
  }, stateVals)
});

var generateStatePrior = cache(function(stateParams) {
  return Infer({
    model: function(){
      return categorical({vs: stateVals, ps: stateProbs(stateParams)})
    }
  })
});

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

var subParams = {
  gymnasts: {mu: -1, sigma: 0.5},
  soccerPlayers: {mu: 0, sigma: 0.5},
  basketballPlayers: {mu: 1, sigma: 0.5}
}

var utterances = ["tall", "short", "silence"]

var meaning = function(utterance, state, thresholds) {
  utterance == "tall" ? state > thresholds.tall :
  utterance == "short" ? state < thresholds.short :
  true
}

var classPrior = Infer({
  model: function(){return uniformDraw(["subordinate", "superordinate"])}
});

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
  }, 10000 
)
///

var speaker = cache(
  function(state, thresholds, comparisonClass) {
    Infer({model: function(){
      var utterance = uniformDraw(utterances);
      var L0 = literalListener(utterance, thresholds, comparisonClass);
      factor( alpha * L0.score(state) );
      return utterance;
    }})
  }, 10000 
)

var pragmaticListener = cache(function(utterance, subordinate_params) {
  Infer({model: function(){
    var statePrior = generateStatePrior(subordinate_params);
    var state = sample(statePrior);
    var thresholds = {
      tall: sample(thresholdPrior("positive")),
      short: sample(thresholdPrior("negative"))
    }
    var c = sample(classPrior)
    var comparisonClass = c == "subordinate" ? subordinate_params : superordinate_params
    var S = speaker(state, thresholds, comparisonClass);
    observe(S, utterance);

    return { comparisonClass: c, state : state }
  }})
}, 10000)



display("pragmatic listener")
viz(pragmaticListener("tall", {mu: -1, sigma: 0.5}))
~~~~

~~~~
///
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

///
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

Pragmatic Listener 
- takes two arguments: an utterance of tall or short and the subordinate params (height distributions) and returns a posterior distribution over possible state and comparison class used by the speaker. 

State Prior
- Generates a state prior of heights based on the subordinate params, or the range of heights for sports and samples a state from this state prior. 

thresholds for tall and short:  
- var thresholds is a structured object with two properties: tall which samples from the threshold prior of positive and short which samples from the threshold prior of negative. These thresholds are drawn from a uniform distribution. 

Uncertainty About Comparison Class:
- If c, or a sample of classPrior, is subordinate then subordinate params is used if not then superordinate params are used 
Simulates the behavior of speaker 1, modeling how likely the speaker is to choose an utterance based on how likely that utterance would make the lister arrive at the correct understanding. 

Simulating Speaker 1
- chooses an utterance based on state, thresholds, and comparison class
-Observe builds in the .score syntax from factor to keep track of what utterances are especially likely for speaker 1 to use, allowing the pragmatic listener to reason about the speaker's choice of utterance

Return

- The pragmatic listener returns the posterior distribution over comparison class (subordinate or superordinate) and state, or height.

Comparison to Price Estimate Model

 - In contrast to the price estimate model which uses factor, this model uses observe to condition on the utterance. Observe functions as factor when alpha is 1. This model contains uncertainty about comparison class and height while the price uncertainty model only has uncertainty about the price. There is also 2 thresholds in this model. One for short and one for tall while the price uncertainty model only has a threshold for expensive. 

~~~~
///
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
///
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
~~~~

exptConditions creates the experimental conditions. This is an array with 6 elements. Each element is a structured object representing an experimental condition with two properties: utterance of tall or short and the subordinate param of each sport 

~~~~
///
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

///

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
~~~~

- creates a prediction for how the pragmatic listener interprets tall or short based on what category they are in and what comparison class the speaker is likely referring to
- Within L1predictions map applies the function stim to each condition within exptConditions, a structured object with an utterance and a subordinate paramater. For example tall gymnast.

- L1 posterior runs the pragmatic listener over stim, taking in an utterance and the subordinate paramaters for a group 

returns:
- the utterance. 


- the probability of the superordinate comparison class being used by taking the marginal distribution of comparison classes and the .score("superordinate") 

- Within L1predictions map applies the function stim to each condition within exptConditions, a structured object with an utterance and a subordinate paramater. 

- what subordinate category was used and that the model is L1.

~~~~
///
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

///
//probability the pragmatic listener thinks the basketball player is tall or short
display("the basketball player is short")
display("--> height = " + expectation(marginalize(pragmaticListener("short",{mu: 1, sigma: 0.5}), "state")))
display("the basketball player is tall")
display("--> height = " + expectation(marginalize(pragmaticListener("tall",{mu: 1, sigma: 0.5}), "state")))
viz(pragmaticListener("tall",{mu: 1, sigma: 0.5}))
//probability the pragmatic listener thinks the soccer player is tall or short
display("the soccer player is short")
display("--> height = " + expectation(marginalize(pragmaticListener("short",{mu: 0, sigma: 0.5}), "state")))
display("the soccer player is tall")
display("--> height = " + expectation(marginalize(pragmaticListener("tall",{mu: 0, sigma: 0.5}), "state")))
viz(pragmaticListener("tall",{mu: 0, sigma: 0.5}))
//probability the gymnast is tall or short short
display("the gymnast is short")
display("--> height = " + expectation(marginalize(pragmaticListener("short",{mu: -1, sigma: 0.5}), "state")))
display("the gymnast is tall")
display("--> height = " + expectation(marginalize(pragmaticListener("tall",{mu: -1, sigma: 0.5}), "state")))
viz(pragmaticListener("tall",{mu: -1, sigma: 0.5}))
~~~~



Above is the pragmatic listener's beliefs about the height of different sports. They hear that they are tall or short with the range of heights for that sport. 

~~~~
display("probability of superordinate comparison class (i.e., tall for all people)")
viz.bar(L1predictions, {groupBy: "subordinate category"})
~~~~

This displays the probability the pragmatic listener infers the speaker is referring to tall for the superordinate comparison class for each of the supordinate categories. Basketball players have the highest probability for being tall in the superordinate class and lowest probability for short due to their distribution of heights in subParams.

