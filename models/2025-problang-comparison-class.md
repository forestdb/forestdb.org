---
layout: model
title: Comparison Class Inference for 'Tall'
model-language: webppl
model-category: Language and Pragmatics
model-status: code
---

*By Kiara, Noah, Kiley*

***Comparison Class Model:***

As previously seen in the adjectives model, when we hear descriptors such as *tall* or *expensive*, we interpret them relative to a comparison class. For example, we have different comparison classes for an expensive watch as opposed to an expensive sweater.

But, what if we don't know the comparison class?

For this model, we are considering the adjective *tall*:
- "John is a basketball player, and he is tall."
- "John is a gymnast, and he is tall."
- "John is a soccer player, and he is tall."

You likely have different ideas of which comparison class is used for each statement. For John, being a basketball player, we're likely to think that he is being compared to all people or the general population. When John is a gymnast, his being tall is likely a comparison to gymnasts specifically, so he would be short relative to all people. And when John is a soccer player, we might infer that it is just for soccer players and not the general population. 

These different comparison classes can be defined as:
- *Superordinate* or the general population 
- *Subordinate* or the specific group (basketball players, gymnasts, soccer players)

By capturing the uncertainty over which comparison class is being used (superordinate vs. subordinate), Tessler et al. (2017) extend the adjective model to account for this ambiguity. We will point out the similarities and differences between the two models as we go through.  



**Rational Speech Act Background** 
- This model uses the Rational Speech Act (RSA) framework, which treats communication as a kind of recursive reasoning between speakers and listeners. This recursive structure means that a listener (L1) interprets an utterance by simulating a speaker (S1), who in turn is assumed to reason about a literal listener (L0). In this case, when someone says “John is tall,” the listener doesn't just guess how tall John is - they also think about who John is being compared to (like gymnasts or the general population). The model captures this by letting the listener infer both John’s height and the comparison group at the same time. This dual inference process is important in modeling the kind of nuanced interpretation humans perform effortlessly in everyday language use.

**Helper Function**
- First, the model defines a simple function that is used to convert log-probabilities (that are returned by Gaussian/normal distributions) into regular probabilities. This is done by calculating the exponential of a number.

~~~~
var exp = function(x){ return Math.exp(x) }
~~~~

**Height Space Discretization**
- Next, the binParam controls how finely we discretize, or split up, the range of possible heights. The reason the continuous variables are split up is to make the computation easier for WebPPL. 
- The superordinate parameters are also introduced here. This defines the distribution for the general population (the "superordinate" comparison class). We have a mean of 0 and a standard deviation of 1, which is just a normal distribution. 
- It's important to note that in this model, heights are not used in a literal way; instead, we have standardized values (or z-scores). This helps the model be more generalizable and not limited to height values only.

~~~~
// discretization
var binParam = 3;

var superordinate_params = {mu: 0, sigma: 1};
~~~~

**Possible Height Values & Probabilities**
- The model begins by generating a list of possible height values that an individual might have. Rather than considering every real number, it selects a range spanning three standard deviations above and below the mean, capturing nearly all realistic values under a normal distribution, and divides this range into discrete steps, controlled by the binParam setting.
- For each of these discretized height values, the model calculates the likelihood of occurrence using a Gaussian distribution. It first computes the log-probability for each height, then applies the exponential function to convert these into standard probabilities, resulting in a full probability distribution over the possible height values.


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

var stateProbs = cache(function(stateParams){
  return map(function(s){
    exp(Gaussian(stateParams).score(s))
  }, stateVals)
});

stateVals
stateProbs({mu: 0, sigma: 1})
~~~~

**Generate State Prior**
- This function creates a full prior distribution over height values based on the probabilities computed above. Using Infer, it samples from a categorical distribution where the possible values are the discretized heights (stateVals) and the probabilities come from the Gaussian model (stateProbs). This gives us the prior belief about a person's height before hearing the adjective "tall."
- *In both the adjectives model and this comparison class model, listeners infer both the state of the world (like a price or height) and some hidden parameter (such as a threshold or comparison class) based on the interpretation of terms like “expensive” or “tall.” They also both capture how listeners interpret adjectives depending on context.*

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

**Thresholds Bins & Priors**
- To interpret an adjective like "tall," we need to define a threshold: a height above which a person is considered tall. The model does this by creating a range of possible threshold values. For "tall" (positive), it shifts each height bin slightly left; for "short" (negative), it shifts right. This adjustment prevents overlap between height values and threshold bins and allows for smooth probabilistic inference.
- If we don’t shift the thresholds and just use the same values for height and threshold, then we might run into cases where the height is exactly equal to the threshold. Since the model checks if the height is greater than the threshold, not equal to it, these cases wouldn’t count, even though they’re right on the edge. That can make the model behave in weird or unrealistic ways. We could try using >= or <= in the meaning function to fix that, but then heights that are exactly on the boundary would be treated as clearly “tall” or “short,” which also isn’t quite right. Shifting the thresholds a little helps the model avoid these edge cases and gives more natural results.
- The threshold prior then samples a threshold uniformly from the defined bins. This represents an uncertainty about what "tall" actually means numerically.

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

var thresholdPrior = cache(function(form){
  return Infer({
    model: function() { return uniformDraw(thresholdBins[form]) }
  });
});

display(thresholdBins.positive)
display(thresholdBins.negative)
viz.density(thresholdBins.positive)
viz.density(thresholdBins.negative)
~~~~

**Subordinate Category Priors**
- Specific group priors over height.
- Each group (gymnasts, soccer players, basketball players) has its own mean and standard deviation.
- Just to point out, the specific athletic groups have smaller standard deviations, compared to the whole population, because of the fact that they are all doing the same activity.
- Used when assuming the comparison class is the specific group (subordinate).
- *The adjectives model uses empirical price priors based on real-world data for specific items, while this comparison model uses predefined priors not based on experimental data.* 

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

Let's move on to the utterance and meaning functions, adapted from the basic adjective model. 

For the utterance model, there are three possible utterances, characterized as positive ("tall"), negative ("short"), and null ("silence").

For the meaning function, the goal is to return a boolean (*true* or *false*) that reflects whether the given utterance correctly describes the given state's relationship to the given threshold. *In the basic adjective model, our meaning function was similar, where it took in utterance, price, and theta. In our model, price is similar to state and theta provided a relevant threshold, like our thresholds object.*

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

The **classPrior** function merely creates a 50/50 distribution of both comparison classes that the model can draw upon.

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

Let's break down exactly what the comparison class argument is. In the model, it is defined as an object that is passed into the L₀ from the L₁, through the S₁.

We have prior knowledge about the distribution of heights that various classes have (i.e. gymnasts are usually shorter than basketball players, and the probability distributions of their heights reflect that knowledge). The reason behind including this **comparisonClass** object is to code in the idea that a listener may be uncertain about whether a statement like "John is tall" means that John is tall *compared to all people* or *compared to basketball players*. 

This object is defined as follows:

~~~~ norun
/// fold:
var classPrior = Infer({
  model: function(){return uniformDraw(["subordinate", "superordinate"])}
});
///

//remember:
// the superordinate category has a prior as follows (e.g. the height distribution for all people):
var superordinate_params = {mu: 0, sigma: 1}

// also, the subordinate categories have priors as follows:
var subParams = {
  gymnasts: {mu: -1, sigma: 0.5},
  soccerPlayers: {mu: 0, sigma: 0.5},
  basketballPlayers: {mu: 1, sigma: 0.5}
}



var c = sample(classPrior)
var comparisonClass = c == "subordinate" ? subordinate_params : superordinate_params
~~~~

So now let's apply the **comparisonClass** to the L₀.



~~~~
///fold:
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
var thresholds = 
    {tall: 0.16666666666666655, 
     short: 0.4999999999999999}

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


display("subordinate class")
viz(literalListener("tall", thresholds, subordinate_params))

display("superordinate class")
viz(literalListener("tall", thresholds, superordinate_params))
~~~~

Next we move on to the S₁.

The S₁ is a function that takes in the **state** the speaker wishes to communicate, the **thresholds**, and the **comparison class**. Like our other models we have investigated this quarter, it returns a probability distribution over utterances (using **Infer**).

In order to calculate this probability distribution, it samples an utterance then scores the L0 (using the same threshold and comparison class criteria). Then it scores the probability of that utterance against the pre-defined state. Using **Infer**, it returns this utterance probability distribution.

Below, the speaker function is called and returns a probability distribution over utterances, considering both a) whether the state is tall or short and b) how tall or short the state is relative to the prior knowledge about the comparison class in question.

~~~~
///fold:
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

viz(speaker
    (0.3333333333333332, // state
    {tall: 0.16666666666666655, short: 0.4999999999999999}, // thresholds
    {mu: 0, sigma: 1}) // parameters (in this case, superordinate parameters)
   )
~~~~

Let's run a test case on the speaker.

Below, the speaker function is run with fixed state (0.33333...) and subparameters (gymnasts). The thresholds are changed comparing the effect of comparison class selection on differing thresholds.

The **Tall Threshold** implements thresholds where the state would be considered tall. The **Lower Short Threshold** implements thresholds where the state is short, but closer to the threshold compared to the **Higher Short Threshold**. 

Of course, the thresholds determine which informative utterance ("short"/"tall") is chosen, but here you see that the subordinate vs. superordinate parameter distinction is important in determining whether the speaker would choose an informative utterance or silence. 

For example, within the **Lower Short Threshold** distributions, a speaker would be less inclined to mention a person is short if they are considering their height relative to gymnasts (subordinate comparison), but there is much higher probability that the speaker would use the informative utterance if they are comparing to all people (superordinate comparison).


~~~~
///fold:
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
///

var testState = 0.3333333333333332
var testSubParams = {mu: -1, sigma: 0.5}
display("TALL threshold")
viz(speaker(testState, {tall: 0.16666666666666655, short: 0.4999999999999999}, testSubParams))
viz(speaker(testState, {tall: 0.16666666666666655, short: 0.4999999999999999}, superordinate_params))

display("lower SHORT threshold")
viz(speaker(testState, {tall: 0.4999999999999999, short: 0.8333333333333331}, testSubParams))
viz(speaker(testState, {tall: 0.4999999999999999, short: 0.8333333333333331}, superordinate_params))

display("higher SHORT threshold")
viz(speaker(testState, {tall: 0.8333333333333331, short: 1.1666666666666665}, testSubParams))
viz(speaker(testState, {tall: 0.8333333333333331, short: 1.1666666666666665}, superordinate_params))
~~~~

One question that arises from the speaker model, whose goal is to be informative and truthful: **why choose silence?**

Generally, you would think that most people are either short or tall. If someone were to fall in the unique case that they are exactly not tall or short, silence may be warranted. However, there are state/threshold/parameter combinations in our test cases that warrant the speaker to consider silence.

In our test cases we are looking at the subparameters for gymnasts (who are generally shorter than the superordinate population). If we look at our "lower SHORT threshold" speaker (listed below), where the state is lower than both the tall and the short threshold, the speaker is much more likely to consider uttering "short" when factoring in the superordinate parameters. This is because the state is much more likely to be interpreted as short (using the meaning function) compared to when subordinate parameters are used (i.e. you are more likely to consider someone who is short to be short when comparing them to all people than when comparing them to gymnasts). 

So that begs the larger question: why, when comparing a short person to gymnasts, does the speaker think that silence is just as informative as uttering "short"? In this case, when the comparison class is gymnasts, and not all people, calling a person short may be uninformative, since compared to gymnasts they may be average, which leads the speaker to consider silence.

~~~~
///fold:
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
///

var testState = 0.3333333333333332
var testSubParams = {mu: -1, sigma: 0.5}

display("lower SHORT threshold")
viz(speaker(testState, {tall: 0.4999999999999999, short: 0.8333333333333331}, testSubParams))
viz(speaker(testState, {tall: 0.4999999999999999, short: 0.8333333333333331}, superordinate_params))
~~~~

Finally, the L₁. 

Similar to many other models we've looked at, the L₁'s goal is to infer the state that the speaker is trying to communicate. In our model, the L₁ is also attempting to infer the comparison class (e.g. is the speaker saying that John is tall compared to all people, or just to gymnasts?) In doing so, it runs **Infer** over a function that returns a structured object with both the comparison class and state.

The L₁ takes in two arguments: the **utterance** and the **subordinate category** (N.B. the L₁ is told the subordinate category and **not** the comparison class). It then generates a state prior from the given subordinate category, sampling a state from said prior. The thresholds are sampled like in the previous speaker and listener layers. Finally, the **comparisonClass** function is run, sampling either the subordinate or superordinate class.

With these variables defined, the L₁ function runs the speaker function with the sampled state, the sampled thresholds, and the sampled comparison class. The speaker function returns a distribution (since it runs **Infer** over a function) which it then runs through the **observe** function alongside the original utterance: this reweights the world state and comparison class hypothesis based on how likely a speaker with those parameters would have produced the observed utterance.

N.B. The L₁ uses **observe**, unlike the rest of the model (and unlike the adjective model) which uses **score** and **factor**. 

~~~~
///fold:
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
///

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
- If c, or a sample of classPrior, is subordinate then subordinate params is used; if not, then superordinate params are used 


Simulating Speaker 1
- Simulates the behavior of speaker 1, modeling how likely the speaker is to choose an utterance based on how likely that utterance would make the listener arrive at the correct understanding. 

- Chooses an utterance based on state, thresholds, and comparison class

- Observe builds in the .score syntax from factor to keep track of what utterances are especially likely for speaker 1 to use, allowing the pragmatic listener to reason about the speaker's choice of utterance

Return

- The pragmatic listener returns the posterior distribution over comparison class (subordinate or superordinate) and state, or height.

Comparison to Price Estimate Model

 - In contrast to the price estimate model which uses factor, this model uses observe to condition on the utterance. Observe functions as factor when alpha is 1. This model contains uncertainty about comparison class and height while the price uncertainty model only has uncertainty about the price. There are also 2 thresholds in this model. One for short and one for tall while the price uncertainty model only has a threshold for expensive. 

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

exptConditions creates the experimental conditions. This is an array with 6 elements. Each element is a structured object representing an experimental condition with two properties: utterance of tall or short and the subordinate param of each sport. 

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
- Within L1predictions map applies the function stim to each condition within exptConditions, a structured object with an utterance and a subordinate parameter. For example, tall gymnast.

- L1 posterior runs the pragmatic listener over stim, taking in an utterance and the subordinate parameters for a group 

returns:
- the utterance. 


- the probability of the superordinate comparison class being used by taking the marginal distribution of comparison classes and then scoring the probability of "superordinate" 


- what subordinate category was used and that the model is L1.

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

//probability the pragmatic listener thinks the soccer player is tall or short
display("the soccer player is short")
display("--> height = " + expectation(marginalize(pragmaticListener("short",{mu: 0, sigma: 0.5}), "state")))
display("the soccer player is tall")
display("--> height = " + expectation(marginalize(pragmaticListener("tall",{mu: 0, sigma: 0.5}), "state")))

//probability the gymnast is tall or short short
display("the gymnast is short")
display("--> height = " + expectation(marginalize(pragmaticListener("short",{mu: -1, sigma: 0.5}), "state")))
display("the gymnast is tall")
display("--> height = " + expectation(marginalize(pragmaticListener("tall",{mu: -1, sigma: 0.5}), "state")))
~~~~



[basketball players](https://ibb.co/xqhsvyfC)

[soccer players](https://ibb.co/Zk4km17) 

[gymnasts](https://ibb.co/nMS5Z2JN) 

Above are the pragmatic listener's beliefs about the height of different sports. They hear that they are tall or short with the range of heights for that sport. The line plots display the probabilities of which comparison class the pragmatic listener interprets when they hear that someone who plays a specific sport is tall as well as the height they interpret this to be. For each chart, the highest probability of height is at the upper end of the height ranges for a sport. For example, the highest probability of height for gymnasts is concentrated around -0.78, above average gymnast height of -1. In comparison to gymnasts and soccer players, basketball players have an expected height that is closer to their mean height. This is because saying tall to refer to a basketball player is less informative than it would be to call a gymnast or a soccer player tall, so the interpreted height for "tall" is closer to that of the average population.  

So what does **L1predictions** do exactly? 

The goal of the function is to return the probability that, when hearing the given utterance with the given comparison class, the speaker was comparing them to the superordinate class.

In order to do so, it itemizes each utterance/subordinate-group combination, and then runs the pragmatic listener over each combination (which leads us to run the L₁ over 6 different combinations). Next, over each combination, it extracts the marginal probability that the L₁ listener assigns to the speaker using the superordinate comparison class.

In running the function, it will return a probability distribution marginalized over the states “tall” and “short,” showing the probability that a certain subordinate class would be considered tall or short compared to the superordinate population.

~~~~ norun
var exptConditions = [
  {utt: "tall", sub: "basketballPlayers"},
  {utt: "short", sub: "basketballPlayers"},
  {utt: "tall", sub: "soccerPlayers"},
  {utt: "short", sub: "soccerPlayers"},
  {utt: "tall",  sub: "gymnasts"},
  {utt: "short", sub: "gymnasts"}
];

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

display("probability of superordinate comparison class (i.e., tall for all people)")
viz.bar(L1predictions, {groupBy: "subordinate category"})
~~~~

This histogram shows the probability the pragmatic listener infers the speaker is referring to tall for the superordinate comparison class for each of the superordinate categories. Basketball players have the highest probability for being tall in the superordinate class and lowest probability for short due to their distribution of heights in subParams. This means a basketball player would be more likely to be tall in the general population, so it is likely that someone is referring to them in the superordinate category. In contrast, gymnasts are very short so they are unlikely to be tall for the general population. As a result, it is likely the speaker is referring to the subordinate comparison class, or "tall for a gymnast". Soccer players have the same average height as the general population, so it is equally likely they will be short for all people, or tall for all people.

