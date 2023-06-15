---
layout: model
title: Cortes-Gress, Nguyen, Quinteros, Romano-Kwan - Comparison Class
model-language: webppl
---

Inferring the Comparison Class (Write-Up)


RSA modeling approach.

The Rational Speech Act (RSA) is a linguistic framework that looks at communication through the lens of social and pragmatic reasoning between a listener and a speaker. In the RSA framework, speakers are assumed to be rational agents who choose utterances to maximize their listeners' understanding of their intended meaning. Listeners are also assumed to be rational agents who make an inference based on the utterance by reasoning about the speaker's intended meaning and the context in which the utterance was spoken. This framework allows us to computationally simulate the behavior of the speaker and the listener by using probabilistic modeling to capture their prior beliefs about each other. The RSA model has proven useful in studying aspects of pragmatic language, such as non-literal language and generic language, and the many factors that may possibly influence an agent’s reasoning behind their lexical choices.

When we try to describe the world around us, sometimes we use words that may be unclear and may have different meanings depending on the context in which they are used. For example, the gradable adjective “warm” may imply a different temperature depending on whether it is summer or winter. This example highlights how the context in which an utterance is spoken can alter the threshold in which something is considered “warm.” The RSA framework is ideal for studying vague language because it allows us to consider how ambiguity about the context can influence the recursive reasoning between a speaker and a listener. 

The empirical phenomenon of interest.

Imagine this scenario: let’s say, a speaker is telling you about their friend named John, and they tell you that he is a basketball player. On top of the information that John is a basketball player, they tell you that he is also tall. The main phenomenon that we’re interested in for this RSA model is the ambiguity that comes from the vagueness of the statement “John is tall.” The ambiguity is that the listener must determine whether or not they are saying he is tall compared to all people or whether he is tall for a basketball player. In this model, this will be called the “comparison class,” which is the point of comparison that we are using when we are talking about “tall”. We’ll call the comparison class for all people the “superordinate category” and the comparison class for basketball players to be the “subordinate category”. This phenomenon is interesting because we can assume that someone who is tall for a basketball player will be much taller than someone who is just taller for the general population since basketball players already tend to be taller. 

If a speaker told someone that John is a basketball player and he is tall, they will more likely infer that we are comparing him to all people, since basketball players tend to already be taller than the average person. However, if a speaker told someone that John is a gymnast and he is tall, they would more likely infer that the speaker is saying that he is tall compared to gymnasts since gymnasts tend to be shorter than the average person. Furthermore, if a speaker were to tell someone that John is a soccer player and he was tall, they would be equally likely to assume that they are talking about the superordinate category (all people) or the subordinate category (soccer players) since soccer players are typically average height.

The following RSA model by Tessler et al. (2017) is trying to simulate what a listener is likely to infer when they hear a certain gradable adjective (e.g. tall) when there is uncertainty about the relevant comparison class (all people, gymnasts, basketball players, soccer players). 

Comparison Class Model.

The helper function is one that will calculate the math in the model for us. It turns log probabilities into real probabilities. This will be useful in the State probs portion of the model. You can use the helper function by writing exp(x) and substitute x in with any number you want to turn into a real probability. In this case, logs will be put into x.

~~~~
// helper function
var exp = function(x){return Math.exp(x)}
~~~~

For this model, as stated in the introduction, we have three bin parameters that we will be analyzing. The parameters are gymnasts, soccer players, and basketball players which refer to the number 3 in binParams. By changing the value of binParam, it will change the scaling of the graphs in the functions of generateStatePrior. BinParam is working similarly to Alpha which is set at 3 to or maximum optimality alongside the three subgroups. It works by setting how big or small the intervals will be when making the finite sets from a string of continuum. 

~~~~
// for discretization
var binParam = 3;
~~~~

The superordinate parameters refer to the overarching average of everyone’s height inside the world of the model. This is helpful when establishing who is tall or short when compared to the average person. Mu stands for mean and Sigma refers to standard deviation. Standard deviation helps us to determine how far off they are from the mean and the variability. In this case, the mean is 0 and the standard deviation is 1. It's important to note that the scale for height is arbitrary. We will be using the value 0 to represent the average height of all people. This will be helpful in understanding the context of generating a state prior.  

~~~~
// information about the superordinate category prior
// e.g., the height distribution for all people
var superordinate_params = {mu: 0, sigma: 1};
~~~~

The variable stateVals gives us a list of numbers that refers to all the possible heights for the three categories of basketball, soccer, and gymnasts. The smallest value is referring to the superordinate parameter average -3 multiplied by 1 and the largest is the superordinate parameter mean +3 multiplied by 1. We multiply by 1 because that is our standard deviation (sigma).  

~~~~
///fold:
var binParam = 3;

var superordinate_params = {mu: 0, sigma: 1};
///

// calculate the range in predefined steps;
// these values correspond to possible heights
var stateVals = _.range(superordinate_params.mu - 3 * superordinate_params.sigma,
                        superordinate_params.mu + 3 * superordinate_params.sigma,
                        superordinate_params.sigma/binParam)
~~~~

State Probabilities is a function that will either take in the argument of superordinate params or sub params, meaning that it can run on the overall population or with the three parameters of soccer, gymnast, and basketball player. This will return a list of numbers that correspond with the probabilities of those heights. Then we use our helper function which has a Gaussian function taking in state param to return a distribution of the log probabilities of all the heights and because it is in the helper function, it will translate the value into real probabilities. We can see an example of this with the superordinate params and those are all the possible heights for the general population and the average person in the world this model created. This will return a probability distribution for each possible state depending on the relevant comparison class. This will be particularly helpful for the next function, generateStatePrior.

~~~~
///fold:
var exp = function(x){return Math.exp(x)}

var binParam = 3;

var superordinate_params = {mu: 0, sigma: 1};

var stateVals = _.range(superordinate_params.mu - 3 * superordinate_params.sigma,
                        superordinate_params.mu + 3 * superordinate_params.sigma,
                        superordinate_params.sigma/binParam)
///

// for each possible height, calculate its probability of occurrence
var stateProbs = cache(function(stateParams){
  return map(function(s){
    exp(Gaussian(stateParams).score(s))
  }, stateVals)
});

stateProbs(superordinate_params)
~~~~

Generating a statePrior is a good checkpoint to see if the model is matching your own intuitions, with basketball players being tall and gymnasts being short. generateStatePrior is a function that uses cache to help the computer remember all the data and takes in another function that uses the argument stateParams. generateStatePrior will then return a model because we used the infer function and it will be categorical over the states and its probabilities of being real. generateStatePrior uses the probabilities given by the stateProbs and returns a distribution. 

You can see this with when we input "superordinate_params" into the generateStatePrior function. The output is a probability distribution over all the average heights as an even curve centered at 0. When compared to the input of subParams["gymnasts"], we see a probability distribution which is skewed to the left near the negatives on the x-axis, which represents them being shorter on average. Likewise, running basketball players on this function will produce a model that is skewed to the right because of they are typically taller.

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
///

// generate a statePrior using the possible heights and their probabilities
var generateStatePrior = cache(function(stateParams) {
  return Infer({
    model: function(){
      return categorical({vs: stateVals, ps: stateProbs(stateParams)})
    }
  })
});

display("hypothetical height prior for all people")
viz.density(generateStatePrior(superordinate_params))
~~~~

Given the two possible states of the model (tall or short), thresholds need to be set to determine if any given state (height) should be considered tall or short. In this model, a uniform threshold prior will help both the speaker determine which utterance to say and the listeners to correctly infer upon hearing the utterances "tall" or "short". To set a uniform threshold prior, the model defines a variable thresholdBins. This object returns a value that is slightly below or above each possible state. The positive list represents thresholds that allow the state to be considered tall; these are values that are slightly below each state. In contrast, the negative list represents the thresholds that allow the actual state to be considered short; these would be values slightly above each state. The model uses thresholdPrior later on in the pragmatic listener code box.  

~~~~
///fold:
var binParam = 3;

var superordinate_params = {mu: 0, sigma: 1};

var stateVals = _.range(superordinate_params.mu - 3 * superordinate_params.sigma,
                        superordinate_params.mu + 3 * superordinate_params.sigma,
                        superordinate_params.sigma/binParam)
///

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

The main difference between this model and previous models is that it introduces the concept of comparison classes. In John's world, the comparison class would be the state to which his height was relevant to. John is a basketball player therefore when someone hears the utterance "tall" they are comparing John to other basketball players but they could very well be comparing him to the public in general. Our predictions about John's height would differ if we were to compare him to other athletes say gymnasts or soccer players. The first introduction to comparison class being added to the adjectives model is in these sub-parameters. The three different objects in subParams, correspond to structured objects that define the subordinate category priors, these are also the heights of gymnasts, soccer players, and basketball players. A similar syntax is seen above in the superordinate height distribution. Therefore, the model has now presented a prior for each class (superordinate and subordinate) and we can move on to possible utterances and the meaning that these utterances can have. 

~~~~
// information about the superordinate category priors
var subParams = {
  gymnasts: {mu: -1, sigma: 0.5}, // gymnast heights
  soccerPlayers: {mu: 0, sigma: 0.5}, // soccer player heights
  basketballPlayers: {mu: 1, sigma: 0.5} // basketball player heights
}
~~~~

The model has two possible utterances "tall" or "short" and the option to not say anything about height, "silence" the null utterance. Therefore the model has three possible items that define the variable utterances. In order to give meaning to each utterance the model uses the XYZ syntax we have seen in previous chapters. This function says that the utterance will be true for the utterance tall if the state is greater than the threshold for tall. Or similarly, for the utterance short, the line of code reads that if the state is less than the threshold for short then the utterance is true. In these lines of code, we see the dot is used to bring two items together and search within the threshold prior for the necessary information. One important thing to note about the utterances and meaning function is that it is not followed by any cost functions. This is a notable difference from previous models, where cost is usually defined after the utterances and would affect the pragmatic speaker and their behavior. 

~~~~
// possible utterances can be either positive (tall) or negative (short) or a null utterance
var utterances = ["tall", "short", "silence"]

// meaning function for utterances
var meaning = function(utterance, state, thresholds) {
  utterance == "tall" ? state > thresholds.tall :
  utterance == "short" ? state < thresholds.short :
  true
}
~~~~

The variable classPrior essentially makes a uniform draw between the superordinate category (all people) and the relevant subordinate category (basketball player, soccer player, or gymnast). We use the infer function on this uniform draw function so that we can visualize the probability distribution over the categories, which can be seen when we run classPrior. There is equal probability for the superordinate category and the subordinate category. This classPrior variable is used in the code when we run the pragmatic listener, which is meant to represent the uncertainty about the superordinate and subordinate comparison classes.

~~~~
// assume a uniform prior over comparison classes
var classPrior = Infer({
  model: function(){return uniformDraw(["subordinate", "superordinate"])}
});
classPrior
~~~~

We make a variable called alpha, which is used later in the speaker model to determine their optimality. For this model, alpha is automatically set to 5, which means that the speaker is very reliable in choosing an utterance that accurately reflects the true state. If alpha is set to any number higher than 5, you don’t see too much of a change when running the pragmatic listener. However, if alpha is set to something smaller than 5, then the speaker becomes less reliable, which causes the pragmatic listener to be unable to reliably differentiate between the subordinate and superordinate category.

~~~~
// set speaker optimality
var alpha = 5;
~~~~

Now, we can make a literal listener model using the variables that have been created. The literal listener takes in three parameters: an utterance from the speaker, the established thresholds, and the relevant comparison class. Then, the literal listener model generates a statePrior from the comparison class (by using the generateStatePrior function that was created earlier). Then, from the StatePrior, we sample a state and run the meaning function that was created on the utterance, state, and the thresholds to determine what state would be considered tall and what would be considered short, given a certain comparison class. If the literalListener returns true, then we use the cache function to let the computer save the first 10,000 runs. The literal listener model can be run to visualize the most likely state (height) given certain parameters.

If you were to run the code shown below, you would be presented with a visualization of the literal listener's inference. It essentially shows the probability distribution for possible heights. For example, the literal listener was run with the utterance “tall”, thresholds for tall at 1.167 and for short at -0.83, and the superordinate comparison class for all people, we’ll get a distribution with a peak around 1.3. The literal listener is most likely to believe that the height is 1.33 because 1.33 is bigger the tall threshold of 1.16 and, according to the state probability of the superordinate comparison class, 1.33 is more probable than the larger heights.

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
  gymnasts: {mu: -1, sigma: 0.5}, // gymnast heights
  soccerPlayers: {mu: 0, sigma: 0.5}, // soccer player heights
  basketballPlayers: {mu: 1, sigma: 0.5} // basketball player heights
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
  }, 10000 // limit cache size
)

literalListener("tall", {tall: 1.1666666666666663, short: -0.8333333333333334}, superordinate_params)
~~~~

This RSA model contains one speaker model. The speaker takes in an input containing the actual state of the person (which means they see the actual height of the person), the established thresholds, and a comparison class. The speaker samples an utterance using a uniform draw, which means that the utterances “tall”, “short”, and saying nothing would be equally likely. Then we make an L0 variable by running the literalListener model using the utterance that we sampled, and the same thresholds and comparisonClass to see what height the literal listener would assume. This is meant to represent how the speaker chooses an utterance based on their beliefs about the listener’s prior knowledge. Given all this information, the speaker then returns an utterance.

When running the speaker function with certain parameters, we may see that “silence” becomes a viable utterance like in the example below. This typically occurs when running the model with a “tall” state for a basketball player or a “short” state for a gymnast. Silence is a possible option because there is still a possibility that the literal listener will arrive at the correct state, even without anything being uttered, since basketball players are typically tall and gymnasts are typically short.

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
  gymnasts: {mu: -1, sigma: 0.5}, // gymnast heights
  soccerPlayers: {mu: 0, sigma: 0.5}, // soccer player heights
  basketballPlayers: {mu: 1, sigma: 0.5} // basketball player heights
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
  }, 10000 // limit cache size
)
///

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
speaker1(-1.9999999999999998, {tall: 0.8333333333333331, short: -0.5000000000000001}, subParams["gymnasts"])
~~~~

The pragmatic listener hears the utterance describing the height and is given the relevant subordinate category. Then, the pragmatic listener samples a state, a tall and short threshold, and a comparison class. With this information, the pragmatic listener reasons about the utterance the speaker would have chosen to say given those same conditions. Based on that, the pragmatic listener returns a joint distribution on the comparison class and the state.

One way to run the pragmatic listener would be to simply provide the pragmatic listener with an utterance and a subordinate class. Unfortunately, this does not provide a useful visualization. Instead, what we can do is find the marginal distribution for the comparison class and state separately.

In the following example, when the pragmatic listener hears that a person is both tall and a basketball player, they are likely to believe that the person is tall compared to the superordinate class (all people) rather than just the subordinate class.

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

marginalize(pragmaticListener("tall", subParams["basketballPlayers"]), "comparisonClass")
~~~~

In the next example, when the pragmatic listener hears the utterance "tall" and knows that basketball players are the subordinate class, the most likely height for the person being described is about 1.1. This distribution of states strongly resembles the generateStatePrior for basketball players, which makes sense because the heights are sampled from the relevant generateStatePrior.

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

marginalize(pragmaticListener("tall", subParams["basketballPlayers"]), "state")
~~~~

The exptConditions provides every possible combination of utterances and subordinate classes.

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

exptConditions
~~~~

L1predictions takes a function that runs the pragmatic listener on an utterance and subordinate category from the argument "stim." This function returns a structured object containing the utterance, the probability that the superordinate category is the comparison class, the subordinate category, and the model L1. 

Since this function is mapped onto the list of exptConditions, the utterance and subordinate class are actually provided by the exptConditions. Therefore, when L1predictions is run, a list of structured objects is provided in the same order of utterances and subordinate classes as they appear in the exptConditions.

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

L1predictions
~~~~

Discussion of relevant results.

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

According to the model, the pragmatic listener expects the height of a tall basketball player to be ~1.13, which is what we saw in the L1 predictions. On the other hand, the pragmatic listener believes that a short basketball player is ~0.77. 

When the pragmatic listener hears the utterance “short,” they believe that a basketball player is being compared to other basketball players rather than all people. A short gymnast, however, is likely to be short compared to all people. 

When the pragmatic listener hears the utterance “tall” being used to describe a basketball player, they believe the basketball player is being compared to all people. Considering that gymnasts tend to be on the shorter side, a gymnast being described as “tall” is likely to just be tall for a gymnast. 

Regardless of the utterance, soccer players are equally likely to be compared to all people or just other soccer players. 

~~~~
Similarities between other models.

// Similarities Comparison Class 
//similar in threshold
var thresholdBins ={
  positive: map(function(x){
    return  x - (1/(binParam*2));
  }, sort(stateVals)),
  negative: map(function(x){
    return  x + (1/(binParam*2));
  }, sort(stateVals))
}; 

//similar probabilities  
var generateStatePrior = cache(function(stateParams) {
  return Infer({
    model: function(){
      return categorical({vs: stateVals, ps: stateProbs(stateParams)})
    }
  })
});


// Similarities Vagueness Model by Lassiter and Goodman 
//similar in theta 
var thetaPrior = function() {
    return uniformDraw(book.prices);
};

//similarities in probabilities  
var statePrior_lookup = function(item) {
  var prices = data[item].prices;
  var probabilities = data[item].probabilities;
  return function() {
    return categorical(probabilities, prices);
  };
};
~~~~

The similarities of this model when compared to the vagueness model created by Lassiter and Goodman. Though the vagueness model does not have an explicit threshold like our comparison class model it does create a similar aspect by using Theta. These functions are used to answer the questions of  what is categorized as “expensive” to show that it is different from regular coffee, watches or sweaters. Similar to how comparison class uses threshold distinguish what the model refers to “tall” or “short” in reference to our parameters as being either different from the average height or for their subordinate  parameters.The generating state prior is similar to the item in the list of objects in the vagueness model of the price and probability. It works similarly to the state prior lookup function in the vagueness model by taking in a probability and a price. In the function of generating statePrior you take in both the height probabilities and build a graph which we can see where it would land in comparison to the average. These are the similarities we have seen between the models  along with the usage  uniform draw in both of the models.

The main difference between this model and previous models such as the vagueness model created by Lassiter and Goodman is the addition of comparison classes. As shown and explained throughout the model, these are sub-parameters that allow us to distinguish a target from other classes. The inclusion of comparison classes changes our normal literal listener because instead of using items such as theta for the adjective threshold, we now have thresholds that depend on the comparison class. The literal listener section of this model defines state prior by generating a state prior from the comparison class, whereas in previous models the state prior comes from a look-up table of priors that would take in the specific item to search for its probability. Additional differences are seen in the speaker part of this model where there is no cost for the utterances and we also see a uniform draw being used to select an utterance. Finally, the differences we can observe in the pragmatic listener section of the model come from having separate thresholds for possible positive and negative adjectives. This comes from the two possible utterances, which are “tall” or “short”. Previous models showed this positive and negative relationship when we had “expensive” as an utterance, however, this model differs because the goals of the comparison class adjectives model have changed from the orginal vagueness model.