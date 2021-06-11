---
layout: model
title: Torabian Politeness + QUDs
model-language: webppl
---

In this project we look into social reasoning about social reasoning, and particularly extend the politeness model by Yoon, Tessler, et al. (2016) by adding a Question-Under-Discussion (QUD). This QUD is inspired by the irony model proposed by Kao and Goodman (2015).

In the politeness model, there are cookies made by a character who we name Sajjad, and another character named Sarah tastes the cookies. Now, assume that Sarah really likes the cookies, and thinks that they deserve 5 out of 5 stars. We denote these stars as states in the model:

~~~~
var states = [1,2,3,4,5]

// We live in a world where cookies of rather okay quality
// can be baked. So the prior over these states are the following.
var statePrior = function() {
  categorical([1, 1, 2, 2, 2], states)
}
~~~~

Also, we assume that Sajjad is a rather skillful baker, therefore states of 3, 4, and 5 receive more prior weight. Note that the state prior was absent in the original model, and we add it as the pragmatic listener in the new model needs to make inferences based upon it; the pragmatic listener will be discussed later.

We, next, introduce our QUD (also referred to as "goal") which include emotions into the model. There are various ways through which emotions are modeled. One common way is to map emotions onto two dimensions of valence of arousal. While valence indicates whether a person feels positive or negative, arousal (as its name represents) shows how aroused the person is. For example, when you experience astonishment, you are highly aroused and are feeling positive valence. Or, when you are angry, you are again highly aroused, but with negative valence.

Here we use the same direction to incorporate emotions into the model, specifically by adding the dimension of arousal:

~~~~
// Define binary arousals.
var arousals = ["low", "high"]

// Define goals and goal priors. Could want to communicate state of the world,    
// or arousal (intensity of feeling) about it.
var goals = ["goalState", "goalArousal"]

var goalPrior = function() {
  categorical([1, 1], goals)
}
~~~~

When tasting the cookies, Sarah can either experience low or high arousal. We then include our QUD as a goal that can be directed towards state, or arousal. These two goals receive the same weight a priori.

Below shows the utterances that remain the same as before. The difference compared to the original model is the addition of a uniform prior distribution over utterances, which will be used later by the speaker:

~~~~
var utterances = ["terrible","bad","okay","good","amazing"]

// Assume cost of utterances is uniform.
var utterancePrior = function() {
  uniformDraw(utterances)
}
~~~~

Next, we define prior arousal probabilities conditioned on state. Under states 1 and 5, where Sarah very much dislikes the cookies and really likes them respectively, we set %90 prior weight on high arousal. With states 2 and 4 which are not as extreme as states 1 and 5, the aforementioned weight is degraded to %70, and it is set to %10 for state 3. Note that these values are based on the writer's intuition, and are not empirically derived:

~~~~
// Sample arousal given a state.
var arousalPrior = function(state) {
  state === 1 ? categorical([0.1, 0.9], arousals) :
  state === 2 ? categorical([0.3, 0.7], arousals) :
  state === 3 ? categorical([0.9, 0.1], arousals) :
  state === 4 ? categorical([0.3, 0.7], arousals) :
  state === 5 ? categorical([0.1, 0.9], arousals) :
  true
}
~~~~

Literal semantics, the meaning and value functions all remain the same, so we include them just as they appeared in the original model:

~~~~
// correspondence of utterances to states (empirically measured)
var literalSemantics = {
  "terrible":[.95,.85,.02,.02,.02],
  "bad":[.85,.95,.02,.02,.02],
  "okay":[0.02,0.25,0.95,.65,.35],
  "good":[.02,.05,.55,.95,.93],
  "amazing":[.02,.02,.02,.65,0.95]
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
~~~~

Here we take the last step before defining our literal listener. The following function returns state if the goal is state, and returns arousal in case of arousal:

~~~~
// A speaker's goal is satisfied if the listener infers the correct 
// and relevant information.
var goalState = function(goal, state, arousal) {
  goal === "goalState" ? state :
  goal === "goalArousal" ? arousal :
  true
}
~~~~

Our literal listener differs from the original version in that it receives goal in addition to utterance (so the goal is also known to them), and returns arousal in addition to state:

~~~~
// literal listener
var literalListener = cache(function(utterance, goal) {
  Infer({model: function(){
    var state = uniformDraw(states)
    var arousal = arousalPrior(state)
    condition(meaning(utterance, state))
    return {state, arousal}
  }})
})
~~~~

The speaker function is modified as follows. As we learned from the original model, this speaker can either take an epistemic stance or a social one, and that is implemented through the parameter phi. Phi varies between 0 and 1, and the higher the phi, the more epistemic the speaker will be. More epistemic values mean more directness and less politeness. For example, if Sajjad has made terrible cookies which deserve 1 out of 5 stars on Sarah's mind, then Sarah would tend more towards uttering "terrible" if phi is closer to 1. State and this phi value will be passed to the speaker as before. However, we now have arousal and goal as additional arguments.

The speaker makes inferences about the literal listener by looking particularly into its state and arousal. Then, utility is defined as a list which includes epistemic and social, as structured in the original model. We set epistemic to I) the literal listener's score over state if the goal is to report state, and to II) literal listener's score over arousal otherwise (if the goal is to show arousal). Everything else remains similar to before.

~~~~
var alpha = 10; // MAP estimate from Yoon, Tessler, et al. 2016
var speaker1 = cache(function(state, arousal, goal, phi) {
  Infer({model: function(){
    var utterance = utterancePrior()
    var L0_posterior = literalListener(utterance, goal);
    var L0_state = marginalize(L0_posterior, "state")
    var L0_arousal = marginalize(L0_posterior, "arousal")
    var utility = {
      epistemic: goal === "goalState" ? L0_state.score(state) :
      L0_arousal.score(arousal),
      social: expectation(L0_state, valueFunction)
    }
    var speakerUtility = phi * utility.epistemic +
                        (1 - phi) * utility.social
    factor(alpha * speakerUtility)
    return utterance
  }})
})
~~~~

Last but not least, we define the pragmatic listener who listens to the speaker and in addition to state and phi which were already embedded in the model, makes inferences about arousal.

~~~~
var pragmaticListener = function(utterance) {
  Infer({model: function(){
    var state = statePrior()
    var arousal = arousalPrior(state)
    var goal = goalPrior()
    var phi = uniformDraw(_.range(0.05, 0.95, 0.05))
    observe(speaker1(state, arousal, goal, phi), utterance)
    return { state, arousal, phi }
  }})
}
~~~~

We now put all the pieces together to get the following politeness model with the QUD of emotion (arousal). Towards the end of the code, we test the pragmatic listener when he hears the utterance "terrible" by looking into his inferences about state, arousal, and phi. Compared to the posterior distribution on states from the original model, this model results in slight differences which appear as rather negligible weights on states 4 and 5. This observation comes from the non-uniform prior distribution on states which we defined at the beginning. The posterior distribution on phi has also shifted slightly to the right, which indicates more frankness from the speaker.

As mentioned earlier, arousal accepts two possibilities of high and low. The posterior shows significant weight on high compared to low since uttering "terrible" indicates a rather extreme feeling from the speaker. Note that part of this rather high probability comes from the a priori distribution over arousal (conditioned on state). However, based on the literal semantics which were derived empirically, "terrible" can mean states 1 or 2 with rather high probabilities of 0.95 and 0.85, and other states receive some weight as well. Therefore, in our final results, arousal is most probably high, but not as high as defined by its prior.

~~~~
var states = [1,2,3,4,5]

// We live in a world where cookies of rather okay quality
// can be baked. So the prior over these states are the following.
var statePrior = function() {
  categorical([1, 1, 2, 2, 2], states)
}

// Define binary arousals.
var arousals = ["low", "high"]

// Define goals and goal priors. Could want to communicate state of the world,    
// or arousal (intensity of feeling) about it.
var goals = ["goalState", "goalArousal"]

var goalPrior = function() {
  categorical([1, 1], goals)
}

var utterances = ["terrible","bad","okay","good","amazing"]

// Assume cost of utterances is uniform.
var utterancePrior = function() {
  uniformDraw(utterances)
}

// Sample arousal given a state.
var arousalPrior = function(state) {
  state === 1 ? categorical([0.1, 0.9], arousals) :
  state === 2 ? categorical([0.3, 0.7], arousals) :
  state === 3 ? categorical([0.9, 0.1], arousals) :
  state === 4 ? categorical([0.3, 0.7], arousals) :
  state === 5 ? categorical([0.1, 0.9], arousals) :
  true
}

// correspondence of utterances to states (empirically measured)
var literalSemantics = {
  "terrible":[.95,.85,.02,.02,.02],
  "bad":[.85,.95,.02,.02,.02],
  "okay":[0.02,0.25,0.95,.65,.35],
  "good":[.02,.05,.55,.95,.93],
  "amazing":[.02,.02,.02,.65,0.95]
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

// A speaker's goal is satisfied if the listener infers the correct 
// and relevant information.
var goalState = function(goal, state, arousal) {
  goal === "goalState" ? state :
  goal === "goalArousal" ? arousal :
  true
}

// literal listener
var literalListener = cache(function(utterance, goal) {
  Infer({model: function(){
    var state = uniformDraw(states)
    var arousal = arousalPrior(state)
    condition(meaning(utterance, state))
    return {state, arousal}
  }})
})

var alpha = 10; // MAP estimate from Yoon, Tessler, et al. 2016
var speaker1 = cache(function(state, arousal, goal, phi) {
  Infer({model: function(){
    var utterance = utterancePrior()
    var L0_posterior = literalListener(utterance, goal);
    var L0_state = marginalize(L0_posterior, "state")
    var L0_arousal = marginalize(L0_posterior, "arousal")
    var utility = {
      epistemic: goal === "goalState" ? L0_state.score(state) :
      L0_arousal.score(arousal),
      social: expectation(L0_state, valueFunction)
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
    var state = statePrior()
    var arousal = arousalPrior(state)
    var goal = goalPrior()
    var phi = uniformDraw(_.range(0.05, 0.95, 0.05))
    observe(speaker1(state, arousal, goal, phi), utterance)
    return { state, arousal, phi }
  }})
}

var listenerPosterior = pragmaticListener("terrible")
// note in this case that visualizing the joint distribution via viz()
// produces the wrong joint distribution. this is a bug in the viz() program.
// we visualize the marginal distributions instead:

display("expected state = " +
        expectation(marginalize(listenerPosterior, "state")))
viz(marginalize(listenerPosterior, "state"))

viz(marginalize(listenerPosterior, "arousal"))

display("expected phi = " +
        expectation(marginalize(listenerPosterior, "phi")))
viz.density(marginalize(listenerPosterior, "phi"))
~~~~