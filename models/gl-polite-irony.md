---
layout: model
title: Gu, Li - Polite Irony
model-language: webppl
---

Let’s say you are a passionate baker, but your baking skills is not that sharp, so the quality of your cookies is unstable. Sometimes you bake pretty good ones, other times they are terrible. This time you baked really terrible cookies, and you invited a bunch of people to your place for a cookie tasting party. Person A tasted your cookies— you are naïve about the quality. You asked person A how she likes the cookies, and she answered "Oh they are amazing". 

According to the politeness model we covered in class, when you hear "amazing", you as the pragmatic listener (the L1) would probably expect that the quality of your cookie is about 3.5 out of 5 hearts. You would infer that person A is being polite and wants to save your face, and that they’re not being very truthful about the quality of your cookies. But other than being polite or kind, there is at least another possibility that person A said your terrible cookies are amazing: person A is being ironic! They are producing utterances whose intended meanings are opposite in polarity to the literal meaning. And so you may wonder: What exactly do they wish to communicate through the ironic language use? Are they really talking about the quality of your cookies or do they want to let you know how strongly they feel about the cookies? This leads to ambiguity in interpretation because you are not sure what QUD is: Is it about quality? Or is it about their feeling about the cookies? Hence, we want the pragmatic listener to actively reason about the QUD to capture some irony, if not all.

Based on the Politeness model from the textbook, we are going to make the following modifications. First, we transform the previous "state" to include two properties. Full states include two attributes: the quality of the cookie, e.g., 1 heart out of 5 hearts, with 1 meaning "terrible" and 5 meaning "amazing". Arousal is how strongly you feel about the quality of the cookie, with 10 meaning strong feeling (either positive or negative) and 0 meaning indifference. This is completely based on our own intuition. 


~~~~
var fullStates = [{"quality": 1, "arousal": 10},
                  {"quality": 2, "arousal": 3},
                  {"quality": 3, "arousal": 0},
                  {"quality": 4, "arousal": 3},
                  {"quality": 5, "arousal": 10},
                 ]
~~~~

Next, we add two different QUDs into the model: quality and arousal. The qudPrior function below assigns equal probability to the two QUDs. The qudFns is a look up table that takes in a state, and returns an answer according to the QUD. For instance, if the state is {"quality": 5, "arousal": 10} and the QUD is "quality", then the function will return the quality attribute of the state that is fed into the function, which is 5.

~~~~
// Prior over QUDs
var qudPrior = function(){
  categorical({
    vs: ["quality", "arousal"],
    ps: [1,1]
  })
}

// qud answer look up table
var qudFns = {
  quality : function(state) {return state.quality} ,
  arousal : function(state) {return state.arousal} ,
}
~~~~

Now we can update the literalListener function since the naming of certain variables are changed. Functionally, the literalListener still takes in an utterance, and infers the state by performing a uniform draw over the five possible states. For a state to be returned, it needs to pass the meaning function, which is the same as in the original model. 

~~~~
// literal listener
var literalListener = cache(function(utterance) {
  Infer({model: function(){
    var state = uniformDraw(fullStates)
    var m = meaning(utterance, state.quality)
    condition(m)
    return state
  }})
})
~~~~

So far, we have everything we need to run the literalListener function. Every variable that wasn't mentioned above are the same as in the original model. Let's give it a try:

~~~~
var fullStates = [{"quality": 1, "arousal": 10},
                  {"quality": 2, "arousal": 3},
                  {"quality": 3, "arousal": 0},
                  {"quality": 4, "arousal": 3},
                  {"quality": 5, "arousal": 10},
                 ]
var utterances = ["terrible","bad","okay","good","amazing"]

// correspondence of utterances to states (empirically measured)
var literalSemantics = {
  "terrible":[.95,.85,.02,.02,.02],
  "bad":[.85,.95,.02,.02,.02],
  "okay":[0.02,0.25,0.95,.65,.35],
  "good":[.02,.05,.55,.95,.93],
  "amazing":[.02,.02,.02,.65,0.95]
}

// Prior over QUDs
var qudPrior = function(){
  categorical({
    vs: ["quality", "arousal"],
    ps: [1,1]
  })
}

// qud answer look up table
var qudFns = {
  quality : function(state) {return state.quality} ,
  arousal : function(state) {return state.arousal} ,
}

// determine whether the utterance describes the state
// by flipping a coin with the literalSemantics weight
// ... state - 1 because of 0-indexing
var meaning = function(utterance, state){
  return flip(literalSemantics[utterance][state - 1]);
}

// value function scales social utility by a parameter lambda
var lambda = 1.25 // value taken from MAP estimate from Yoon, Tessler, et al. 2016
var valueFunction = function(s){
  return lambda * s
}

// literal listener
var literalListener = cache(function(utterance) {
  Infer({model: function(){
    var state = uniformDraw(fullStates)
    var m = meaning(utterance, state.quality)
    condition(m)
    return state
  }})
})

viz(literalListener("amazing"))
viz(marginalize(literalListener("amazing"), "quality"))
viz(marginalize(literalListener("amazing"), "arousal"))
~~~~

As we can see, the model prediction is the same as the prediction of the original listeralListener. The only difference is that there are now two attributes (quality and arousal) instead of one (quality). 

Now, let's move on to the speaker. The pragmatic Speaker1 picks an utterance according to three things now: state, qud and phi. Given the fact that qud is added into the model, the speaker performs in the following manner: she looks up for the answer to the QUD using the look up table we specified earlier. The speaker then reasons about the literal listener based on the epistemic and social utility. L0_posterior in our model includes both quality and arousal so when calculating the utility, we use marginalize to pick only one of the two attributes that correspond to the QUD. This will return the probability of QUD answer in the distribution using the “.score”. 

Unlike the original model, where the social utility captures the "politeness" of the speaker, our utility captures the "dramaness" of the speaker when the QUD is arousal: it incentivizes more extreme utterances which carry higher arousal score. If however the QUD is quality, then it still reflects the politeness. 

~~~~
var speaker1 = cache(function(state, qud, phi) {
  Infer({model: function(){
    var utterance = uniformDraw(utterances);
    var L0_posterior = literalListener(utterance);
    var qudFn = qudFns[qud]
    var qudAns = qudFn(state)
    var utility = {
      epistemic: marginalize(L0_posterior, qud).score(qudAns),
      social: expectation(marginalize(L0_posterior, qud),valueFunction)
    }
    var speakerUtility = phi * utility.epistemic +
        (1 - phi) * utility.social
    factor(alpha * speakerUtility)
    return utterance
  }})
})
~~~~

Let's now try to run the speaker function:

~~~~
var fullStates = [{"quality": 1, "arousal": 10},
                  {"quality": 2, "arousal": 3},
                  {"quality": 3, "arousal": 0},
                  {"quality": 4, "arousal": 3},
                  {"quality": 5, "arousal": 10},
                 ]
var utterances = ["terrible","bad","okay","good","amazing"]

// correspondence of utterances to states (empirically measured)
var literalSemantics = {
  "terrible":[.95,.85,.02,.02,.02],
  "bad":[.85,.95,.02,.02,.02],
  "okay":[0.02,0.25,0.95,.65,.35],
  "good":[.02,.05,.55,.95,.93],
  "amazing":[.02,.02,.02,.65,0.95]
}

// Prior over QUDs
var qudPrior = function(){
  categorical({
    vs: ["quality", "arousal"],
    ps: [1,1]
  })
}

// qud answer look up table
var qudFns = {
  quality : function(state) {return state.quality} ,
  arousal : function(state) {return state.arousal} ,
}

// determine whether the utterance describes the state
// by flipping a coin with the literalSemantics weight
// ... state - 1 because of 0-indexing
var meaning = function(utterance, state){
  return flip(literalSemantics[utterance][state - 1]);
}

// value function scales social utility by a parameter lambda
var lambda = 1.25 // value taken from MAP estimate from Yoon, Tessler, et al. 2016
var valueFunction = function(s){
  return lambda * s
}

// literal listener
var literalListener = cache(function(utterance) {
  Infer({model: function(){
    var state = uniformDraw(fullStates)
    var m = meaning(utterance, state.quality)
    condition(m)
    return state
  }})
})

var alpha = 10; // MAP estimate from Yoon, Tessler, et al. 2016
var speaker1 = cache(function(state, qud, phi) {
  Infer({model: function(){
    var utterance = uniformDraw(utterances);
    var L0_posterior = literalListener(utterance);
    var qudFn = qudFns[qud]
    var qudAns = qudFn(state)
    var utility = {
      epistemic: marginalize(L0_posterior, qud).score(qudAns),
      social: expectation(marginalize(L0_posterior, "quality"),valueFunction)
    }
    var speakerUtility = phi * utility.epistemic +
        (1 - phi) * utility.social
    factor(alpha * speakerUtility)
    return utterance
  }})
})

viz(speaker1({"quality": 5, "arousal": 10}, "quality", 0.99))
viz(speaker1({"quality": 5, "arousal": 10}, "arousal", 0.99))
~~~~

As we can see, when we alter the QUD, the model gives different predictions. When the QUD is "quality", the model prediction looks the same as the original model. However, when the QUD is "arousal", we can see that probabilities shift from the good utterances to the bad ones. More specifically, "terrible" receives the second highest probability. This is because that our target state has an arousal of 10, and "terrible" and "amazing" are both good at communicating this communicative target.

If we keep social utility as a complete "polite" component, i.e. we compute the marginal distribution for quality instead of qud while calculating social utility, the shape of the distribution does not change much, although slightly more probabilities are assigned to more positive utterances. This matches with our intuition -- when QUD is "arousal", although the negative utterances do not get favored through social utility, they do get favored through epistemic utility. 

Lastly, we add the pragmaticListener into the model. It looks pretty much the same as the original one, despite that she also samples a QUD uniformly among the two possible QUDs. 

~~~~
var pragmaticListener = function(utterance) {
  Infer({model: function(){
    var state = uniformDraw(fullStates)
    var qud = qudPrior()
    var phi = uniformDraw(_.range(0.05, 0.95, 0.05))
    var S1 = speaker1(state, qud, phi)
    observe(S1, utterance)
    return { state, qud, phi }
  }})
}
~~~~

Combining everything together, we have the full model:

~~~~
var fullStates = [{"quality": 1, "arousal": 10},
                  {"quality": 2, "arousal": 3},
                  {"quality": 3, "arousal": 0},
                  {"quality": 4, "arousal": 3},
                  {"quality": 5, "arousal": 10},
                 ]
var utterances = ["terrible","bad","okay","good","amazing"]

// correspondence of utterances to states (empirically measured)
var literalSemantics = {
  "terrible":[.95,.85,.02,.02,.02],
  "bad":[.85,.95,.02,.02,.02],
  "okay":[0.02,0.25,0.95,.65,.35],
  "good":[.02,.05,.55,.95,.93],
  "amazing":[.02,.02,.02,.65,0.95]
}


// Prior over QUDs
var qudPrior = function(){
  categorical({
    vs: ["quality", "arousal"],
    ps: [1,1]
  })
}

var qudFns = {
  quality : function(state) {return state.quality} ,
  arousal : function(state) {return state.arousal} ,
}

// determine whether the utterance describes the state
// by flipping a coin with the literalSemantics weight
// ... state - 1 because of 0-indexing
var meaning = function(utterance, state){
  return flip(literalSemantics[utterance][state - 1]);
}

// value function scales social utility by a parameter lambda
var lambda = 1.25 // value taken from MAP estimate from Yoon, Tessler, et al. 2016
var valueFunction = function(s){
  return lambda * s
}

// literal listener
var literalListener = cache(function(utterance) {
  Infer({model: function(){
    var state = uniformDraw(fullStates)
    var m = meaning(utterance, state.quality)
    condition(m)
    return state
  }})
})

var alpha = 10; // MAP estimate from Yoon, Tessler, et al. 2016

// proven correct
var speaker1 = cache(function(state, qud, phi) {
  Infer({model: function(){
    var utterance = uniformDraw(utterances);
    var L0_posterior = literalListener(utterance);
    var qudFn = qudFns[qud]
    var qudAns = qudFn(state)
    var utility = {
      epistemic: marginalize(L0_posterior, qud).score(qudAns),
      social: expectation(marginalize(L0_posterior, qud),valueFunction)
    }
    var speakerUtility = phi * utility.epistemic +
        (1 - phi) * utility.social
    factor(alpha * speakerUtility)
    return utterance
  }})
})
///
var pragmaticListener = function(utterance) {
  Infer({model: function(){
    var state = uniformDraw(fullStates)
    var qud = qudPrior()
    var phi = uniformDraw(_.range(0.05, 0.95, 0.05))
    var S1 = speaker1(state, qud, phi)
    observe(S1, utterance)
    return { state, qud, phi }
  }})
}

var listenerPosterior = pragmaticListener("okay")
// note in this case that visualizing the joint distribution via viz()
// produces the wrong joint distribution. this is a bug in the viz() program.
// we visualize the marginal distributions instead:

var statePosterior = marginalize(listenerPosterior, "state")
// viz(statePosterior)

display("expected state = " +
        expectation(marginalize(statePosterior, "quality")))

viz(marginalize(statePosterior, "quality"))

display("expected phi = " +
        expectation(marginalize(listenerPosterior, "phi")))

viz.density(marginalize(listenerPosterior, "phi"))

viz(marginalize(listenerPosterior, "qud"))
~~~~

First let’s look at the comparison between our model and the old model with regard to L1’s interpretation of the utterance "terrible". Here, we have this pragmatic listener who hears that the cookies are terrible and infers how well they actually did, i.e. the quality of the cookies, as well as how much the speaker values honesty, or being truthful about the quality of the cookies. The original model barely assign any probability to state 4 and 5, i.e. the good quality states. While for the predictions made by our model, probabilities are shifted from the bad quality states (1 and 2) to the good quality states (4 and 5). This would suggest that our model is able to capture some of the irony. In other words, if the speaker is being ironic by saying "terrible", and the listener is able to infer that the speaker could actually think that the cookies are good, or even amazing. We could also see that from the expected quality score, which increases from about 1.2 to about 2.1.

If we change the utterance to "amazing", the difference will be that our model assigns more probabilities to quality state 1 ("terrible"), and quality state 2 ("bad"). The expected quality score also decreases from 3.5 to 3.2. Again, irony is reflected here. 

If we compare the expected phi values for the extreme utterance "amazing" and the more mediocre, or neutral utterance "okay", the values are .39 and .66 respectively. Saying "amazing" ends up having smaller phi value makes sense here. This is because social utility always gets bumped up if the speaker goes with "amazing", regardless of QUD; and when phi gets smaller, it suggests that the speaker values more about social utility, and thus more incentivized to say "amazing".

And if we look at the probability distribution on QUD, we can see that "amazing" results in higher probability for "arousal" over "quality", meaning the L1 thinks that the speaker is more likely to communicate about how strongly they feel instead of how the cookies actually are. This is because "amazing" is even more effective at communicating arousal than quality, given the more drastic difference between arousal scores in the full states.

In contrast, if L1 hears "okay", she will think that the QUD is more likely to be "quality" rather than "arousal". Given the expected phi, we can calculate the speaker utility given the utterance of "okay" for both QUDs, and the combined epistemic and social utility for "quality" is higher than that of "arousal". Therefore when the pragmatic listener infers about the speaker, she will imagine that the speaker wished to communicate the qud that has higher utility for the same utterance.  


Our whole discussion above is based on the assumption that the state prior has a flat distribution, meaning that the quality of the cookies are of equal probability. If however, our pragmatic listener knows that she usually bakes terrible cookies (i.e. having extreme prior over states), how would that change the picture? We can update the model by replacing the uniform draw over full states within the pragmatic listener function with a categorical distribution:

~~~~
var fullStates = [{"quality": 1, "arousal": 10},
                  {"quality": 2, "arousal": 3},
                  {"quality": 3, "arousal": 0},
                  {"quality": 4, "arousal": 3},
                  {"quality": 5, "arousal": 10},
                 ]
var utterances = ["terrible","bad","okay","good","amazing"]

// correspondence of utterances to states (empirically measured)
var literalSemantics = {
  "terrible":[.95,.85,.02,.02,.02],
  "bad":[.85,.95,.02,.02,.02],
  "okay":[0.02,0.25,0.95,.65,.35],
  "good":[.02,.05,.55,.95,.93],
  "amazing":[.02,.02,.02,.65,0.95]
}


// Prior over QUDs
var qudPrior = function(){
  categorical({
    vs: ["quality", "arousal"],
    ps: [1,1]
  })
}

var qudFns = {
  quality : function(state) {return state.quality} ,
  arousal : function(state) {return state.arousal} ,
}

// determine whether the utterance describes the state
// by flipping a coin with the literalSemantics weight
// ... state - 1 because of 0-indexing
var meaning = function(utterance, state){
  return flip(literalSemantics[utterance][state - 1]);
}

// value function scales social utility by a parameter lambda
var lambda = 1.25 // value taken from MAP estimate from Yoon, Tessler, et al. 2016
var valueFunction = function(s){
  return lambda * s
}

// literal listener
var literalListener = cache(function(utterance) {
  Infer({model: function(){
    var state = uniformDraw(fullStates)
    var m = meaning(utterance, state.quality)
    condition(m)
    return state
  }})
})

var alpha = 10; // MAP estimate from Yoon, Tessler, et al. 2016

// proven correct
var speaker1 = cache(function(state, qud, phi) {
  Infer({model: function(){
    var utterance = uniformDraw(utterances);
    var L0_posterior = literalListener(utterance);
    var qudFn = qudFns[qud]
    var qudAns = qudFn(state)
    var utility = {
      epistemic: marginalize(L0_posterior, qud).score(qudAns),
      social: expectation(marginalize(L0_posterior, qud),valueFunction)
    }
    var speakerUtility = phi * utility.epistemic +
        (1 - phi) * utility.social
    factor(alpha * speakerUtility)
    return utterance
  }})
})
///
var pragmaticListener = function(utterance) {
  Infer({model: function(){
    var state = categorical([80, 15, 3, 1, 1], fullStates)
    var qud = qudPrior()
    var phi = uniformDraw(_.range(0.05, 0.95, 0.05))
    var S1 = speaker1(state, qud, phi)
    observe(S1, utterance)
    return { state, qud, phi }
  }})
}

var listenerPosterior = pragmaticListener("amazing")
// note in this case that visualizing the joint distribution via viz()
// produces the wrong joint distribution. this is a bug in the viz() program.
// we visualize the marginal distributions instead:

var statePosterior = marginalize(listenerPosterior, "state")
// viz(statePosterior)

display("expected state = " +
        expectation(marginalize(statePosterior, "quality")))

viz(marginalize(statePosterior, "quality"))

display("expected phi = " +
        expectation(marginalize(listenerPosterior, "phi")))

viz.density(marginalize(listenerPosterior, "phi"))

viz(marginalize(listenerPosterior, "qud"))
~~~~

As a result, we have a pragmatic listener, who has a strong prior belief that her cookies are generally terrible. When she hears "amazing", she will get the irony and assign most of the probabilities to quality state 1. This irony interpretation is even stronger than the previous model with the flat prior. 

To conclude, our model is able to capture some possibility of people being ironic in addition to being polite. The main message that we’re trying to get through is that: in real world communications, especially when facing people that you don’t really know well, upon hearing something, extreme comments in particular, we as pragmatic listeners would be able to infer both kindness and irony from the utterance.