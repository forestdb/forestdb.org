---
layout: model
title: Teasing and Playful Insults
model-language: webppl
model-category: Language and Pragmatics
model-status: code
---

By: Amina M. Pasha and Elina Haghighi

LSCI 107M / PSYCH 157M: Probabilistic Language Understanding

University of California, Irvine

June 12, 2025

Suppose you're working on a group project in your LSCI 107M. While typing out the code, you make a minor mistake of misspelling "states" as "stapes". One of your group project partners then tells you, "Oh, you are dumb as rocks." In the literal sense, "dumb as rocks" should be insulting towards you. However, based on your actions and the utterance itself, you would probably take it in a light-hearted manner. Now, suppose another utterance that your project partner could have said to that mistake. Instead of saying "dumb as rocks", your project partner responds with "Oh, you are a fucking idiot." For one, you are most definitely taking this to be a full-on insult, in the literal sense and pragmatic sense. Furthermore, you are interpreting your project partner to be an overall mean individual. This contrasts with the interpretation of "dumb as rocks," where literal semantics do not align with how mean the person may actually be. Additionally, consider how these utterances can be interpreted if you made a very big mistake. The model below illustrates this linguistic phenomenon of insult. 

RSA models help demonstrate ambiguous linguistic phenomena because they help to map out how pragmatic listeners reason about the world they are in and how others interpret the world as well. There are three main components to the RSA model: literal listener, pragmatic speaker, and pragmatic listener. The literal listener takes in a naive, semantic-based interpretation of utterances given to them. The pragmatic speaker reasons on what factors can most effectively communicate the intent of an utterance to a literal listener. Lastly, a pragmatic listener, which is the level we reason at, interprets the pragmatic speaker’s reasoning in order to come to a conclusion on what the speaker is trying to communicate with an utterance. Along with these components, there will often be states of the world, questions under discussion (QUD), and other factors that help us arrive at the intended, intuitive meaning of an utterance. 

In this model, the states of the world are numbers 1 through 4, correlating to how intelligently the listener is acting in that scenario. 1 would be a very objectively dumb decision, like permanently deleting your entire project, and 4 would be objectively intelligent, like successfully implementing a new QUD into your code. The list of utterances corresponds to what the speaker could communicate to the listener and goes as follows: nothing or null, “You are dumb as rocks”, “You are dumb”, and “You are a fucking idiot”. 

1. Very unintelligent (e.g., deleting your whole project)
2. Mildly unintelligent (e.g., typo like "stapes")
3. Competent action (e.g., making a comment on the project)
4. Very intelligent (e.g., implementing a QUD correctly)

Under literal semantics, we list probabilities for each utterance that corresponds to how likely it is for that utterance to be present based on the state (level of intelligence of the action). Literal semantics does not take in pragmatic reasoning, so the probability distribution is based on what we think the utterances would literally mean to the listener. For example, we correspond “dumb as rocks” to most likely communicate state 2. 

The meaning function below takes in an utterance and a state, returning true or false based on a weighted coin flip. For example, the utterance “dumb” with a state of 1 corresponds to a probability of .85, as listed under literalSemantics. The meaning function then flips a coin where true is 85% likely and false is 15% likely, and returns which side the coin landed on.


~~~~
// Level of intelligence
var states = [1,2,3,4]

// What can be said
var utterances = ["", "dumb as rocks", "dumb", "f*cking idiot"]

// Correspondence of utterances to states
var literalSemantics = {
  "" : [.25,.25,.25,.25],
  "dumb as rocks" : [0.45,0.85,0.20,.02],
  "dumb" : [.85,.95,.02,.02],
  "f*cking idiot" : [.95,.55,.02,.02]
}

// Determine whether the utterance describes the state
// Flip a coin with the literalSemantics weight
// *state - 1 because of 0-indexing*
var meaning = function(utterance, state){
  return flip(literalSemantics[utterance][state - 1]);
}

// // Check the model to see how the utterances describing states are weighted by literalSemantics
// viz.table(Infer({model: function(){
// meaning("dumb", 2)
// }}))
~~~~

The QUDs in our model have been taken from the irony model under Chapter 3: Non-literal language. The QUDs include valence, arousal, and state. QUDs are listed under goals in the code. These QUDs are what the speaker infers when deciding what intentions it is communicating to the pragmatic listener. If the speaker is trying to communicate, for instance, that they feel negatively about the action the listener took, they would be using a valence QUD in order to communicate it.

A state QUD is just communicating the literal state of the world. The speaker is not communicating an emotion, but rather the literal state of the listener. For example, if the listener is acting unintelligently, a state QUD would prioritize communicating that to the listener.

A valence QUD communicates whether or not a speaker feels good/positive or bad/negative about what the listener has done. For example, if the listener made a minor spelling mistake, a speaker could have the intent of communicating that they feel positively or negatively about that action.

For Valence Prior, we take an if-then statement. For example, if state 1 is true, we flip a weighted coin. If the coin flip returns true, then the state is good. If the coin flip returns false, then the state is bad.

~~~~
///fold:
// Level of intelligence
var states = [1,2,3,4]

// What can be said
var utterances = ["", "dumb as rocks", "dumb", "f*cking idiot"]

// Correspondence of utterances to states
var literalSemantics = {
  "" : [.25,.25,.25,.25],
  "dumb as rocks" : [0.45,0.85,0.20,.02],
  "dumb" : [.85,.95,.02,.02],
  "f*cking idiot" : [.95,.55,.02,.02]
}

// Determine whether the utterance describes the state
// Flip a coin with the literalSemantics weight
// *state - 1 because of 0-indexing*
var meaning = function(utterance, state){
  return flip(literalSemantics[utterance][state - 1]);
}

// Whether the speaker feels good or bad about the listener's action.
// Speakers’ attitudes toward the true state of the world (e.g., the valence of their affect), which is modeled simply as a binary positive/negative variable (representing whether or not the speaker is repulsed by the listener’s behavior).
var valencePrior = function(state) {
  state === 1 ? flip(0.01) ? "good" : "bad" :
  state === 2 ? flip(0.33) ? "good" : "bad" :
  state === 3 ? flip(0.66) ? "good" : "bad" :
  state === 4 ? flip(0.99) ? "good" : "bad" :
  true}

valencePrior(3)
~~~~

An arousal QUD communicates how strongly the speaker feels about the listener’s action. For example, if they feel good about the listener’s action, it can be very good (e.g., “high” arousal) or just slightly good (e.g., “low” arousal).

For the function of arousalPrior, it takes in a state and puts it into an if-then statement. If the state is 1, for instance, then the chance of the arousal being “low” is 0.1 while “high” is 0.9. 

~~~~
///fold:
// Level of intelligence
var states = [1,2,3,4]

// What can be said
var utterances = ["", "dumb as rocks", "dumb", "f*cking idiot"]

// Correspondence of utterances to states
var literalSemantics = {
  "" : [.25,.25,.25,.25],
  "dumb as rocks" : [0.45,0.85,0.20,.02],
  "dumb" : [.85,.95,.02,.02],
  "f*cking idiot" : [.95,.55,.02,.02]
}

// Determine whether the utterance describes the state
// Flip a coin with the literalSemantics weight
// *state - 1 because of 0-indexing*
var meaning = function(utterance, state){
  return flip(literalSemantics[utterance][state - 1]);
}

// Whether the speaker feels good or bad about the listener's action.
// Speakers’ attitudes toward the true state of the world (e.g., the valence of their affect), which is modeled simply as a binary positive/negative variable (representing whether or not the speaker is repulsed by the listener’s behavior).
var valencePrior = function(state) {
  state === 1 ? flip(0.01) ? "good" : "bad" :
  state === 2 ? flip(0.33) ? "good" : "bad" :
  state === 3 ? flip(0.66) ? "good" : "bad" :
  state === 4 ? flip(0.99) ? "good" : "bad" :
  true}
///

// How amplified the speaker feels about the listener being dumb to different degrees.
var arousals = ["low", "high"]

//How passionate/aroused the listener feels about how intelligent the person is
var arousalPrior = function(state) {
  state === 1 ? categorical([0.1, 0.9], arousals) :
  state === 2 ? categorical([0.7, 0.3], arousals) :
  state === 3 ? categorical([0.7, 0.3], arousals) :
  state === 4 ? categorical([0.1, 0.9], arousals) :
  true
}

arousalPrior(1)
~~~~

~~~~
///fold:
// Level of intelligence
var states = [1,2,3,4]

// What can be said
var utterances = ["", "dumb as rocks", "dumb", "f*cking idiot"]

// Correspondence of utterances to states
var literalSemantics = {
  "" : [.25,.25,.25,.25],
  "dumb as rocks" : [0.45,0.85,0.20,.02],
  "dumb" : [.85,.95,.02,.02],
  "f*cking idiot" : [.95,.55,.02,.02]
}

// Determine whether the utterance describes the state
// Flip a coin with the literalSemantics weight
// *state - 1 because of 0-indexing*
var meaning = function(utterance, state){
  return flip(literalSemantics[utterance][state - 1]);
}

// Whether the speaker feels good or bad about the listener's action.
// Speakers’ attitudes toward the true state of the world (e.g., the valence of their affect), which is modeled simply as a binary positive/negative variable (representing whether or not the speaker is repulsed by the listener’s behavior).
var valencePrior = function(state) {
  state === 1 ? flip(0.01) ? "good" : "bad" :
  state === 2 ? flip(0.33) ? "good" : "bad" :
  state === 3 ? flip(0.66) ? "good" : "bad" :
  state === 4 ? flip(0.99) ? "good" : "bad" :
  true}


// How amplified the speaker feels about the listener being dumb to different degrees.
var arousals = ["low", "high"]

//How passionate/aroused the listener feels about how intelligent the person is
var arousalPrior = function(state) {
  state === 1 ? categorical([0.1, 0.9], arousals) :
  state === 2 ? categorical([0.7, 0.3], arousals) :
  state === 3 ? categorical([0.7, 0.3], arousals) :
  state === 4 ? categorical([0.1, 0.9], arousals) :
  true
}
///

// A list of strings of QUD choices
var goals = ["goalState", "goalValence", "goalArousal"]

// There are 3 possible goals with a flat prior
var goalPrior = function() {
  categorical([1, 1, 1], goals)
}

// A speaker's goal is satisfied if the listener infers the correct
// and relevant information.
var goalState = function(goal, state, valence, arousal) {
  goal === "goalState" ? state :
  goal === "goalValence" ? valence :
  goal === "goalArousal" ? arousal :
  true
}
~~~~

In our literal listener function, it takes in an utterance and goal, returning a probability distribution over states based on the literal listener knowing the utterance and QUD. This means that if you input the utterance “dumb as rocks” and the QUD “goalValence”, it will return a distribution of the likelihood of the states if the literal listener heard the utterance “dumb as rocks” knowing that the intent was to communicate positive or negative affect. Wrapping the command in viz.marginals makes it so it returns those probability distributions of QUDanswer and state as being separate.
We run Infer over a function that takes in no arguments, in order to return our marginalized distribution over states. Additionally, the literal listener is taking the input utterance and a sampled state, inputting it into the meaning function. Only when the meaning function returns true can we pass the condition.

Under return, QUDanswer is going through the function, goalState, in order for the inputted goal to be understood as an argument and not a string.

If the goal is “goalState”, the probability distributions of QUDanswer and state are the exact same because a state QUD is communicating the actual state of the world. This demonstrates the fact that the prior for “goalState” is the same as the prior for states. If the goal is “goalValence” or “goalArousal”, the probability distribution for QUDanswer will be different as it is showing the prior for those two QUDs. 


~~~~
///fold:
// Level of intelligence
var states = [1,2,3,4]

// What can be said
var utterances = ["", "dumb as rocks", "dumb", "f*cking idiot"]

// Correspondence of utterances to states
var literalSemantics = {
  "" : [.25,.25,.25,.25],
  "dumb as rocks" : [0.45,0.85,0.20,.02],
  "dumb" : [.85,.95,.02,.02],
  "f*cking idiot" : [.95,.55,.02,.02]
}

// Determine whether the utterance describes the state
// Flip a coin with the literalSemantics weight
// *state - 1 because of 0-indexing*
var meaning = function(utterance, state){
  return flip(literalSemantics[utterance][state - 1]);
}

// Whether the speaker feels good or bad about the listener's action.
// Speakers’ attitudes toward the true state of the world (e.g., the valence of their affect), which is modeled simply as a binary positive/negative variable (representing whether or not the speaker is repulsed by the listener’s behavior).
var valencePrior = function(state) {
  state === 1 ? flip(0.01) ? "good" : "bad" :
  state === 2 ? flip(0.33) ? "good" : "bad" :
  state === 3 ? flip(0.66) ? "good" : "bad" :
  state === 4 ? flip(0.99) ? "good" : "bad" :
  true}


// How amplified the speaker feels about the listener being dumb to different degrees.
var arousals = ["low", "high"]

//How passionate/aroused the listener feels about how intelligent the person is
var arousalPrior = function(state) {
  state === 1 ? categorical([0.1, 0.9], arousals) :
  state === 2 ? categorical([0.7, 0.3], arousals) :
  state === 3 ? categorical([0.7, 0.3], arousals) :
  state === 4 ? categorical([0.1, 0.9], arousals) :
  true
}

// A list of strings of QUD choices
var goals = ["goalState", "goalValence", "goalArousal"]

// There are 3 possible goals with a flat prior
var goalPrior = function() {
  categorical([1, 1, 1], goals)
}

// A speaker's goal is satisfied if the listener infers the correct
// and relevant information.
var goalState = function(goal, state, valence, arousal) {
  goal === "goalState" ? state :
  goal === "goalValence" ? valence :
  goal === "goalArousal" ? arousal :
  true
}
///

// literal listener
var literalListener = function(utterance, goal) {
  Infer({model: function(){
    var state = uniformDraw(states)
    var valence = valencePrior(state)
    var arousal = arousalPrior(state) 
    var m = meaning(utterance, state);
    condition(m);
    return {QUDanswer: goalState(goal,state,valence,arousal), state: state}
  }})
}

print("dumb")
print("state")
viz.marginals(literalListener("dumb", "goalState"))
print("valence")
viz.marginals(literalListener("dumb", "goalValence"))
print("arousal")
viz.marginals(literalListener("dumb", "goalArousal"))

print("dumb as rocks")
print("state")
viz.marginals(literalListener("dumb as rocks", "goalState"))
print("valence")
viz.marginals(literalListener("dumb as rocks", "goalValence"))
print("arousal")
viz.marginals(literalListener("dumb as rocks", "goalArousal"))

print("")
print("state")
viz.marginals(literalListener("", "goalState"))
print("valence")
viz.marginals(literalListener("", "goalValence"))
print("arousal")
viz.marginals(literalListener("", "goalArousal"))

print("f*cking idiot")
print("state")
viz.marginals(literalListener("f*cking idiot", "goalState"))
print("valence")
viz.marginals(literalListener("f*cking idiot", "goalValence"))
print("arousal")
viz.marginals(literalListener("f*cking idiot", "goalArousal"))
~~~~

The relevant results given by the literal listener demonstrate how the results of goalState are the same as literalSemantics and the overall probabilities of goalValence and goalArousal given to us in the priors. As shown by the results, the literal listener interprets the utterance, “dumb as rocks”, to be most indicative of state 2 because in the literalSemantics lookup table, state 2 has the highest probability returning true at 0.85. Since the literal listener takes things literally, it is not pragmatically reasoning about the utterances and is based on any literal semantics that we feed into the code. Additionally, the literal listener puts more probability on a “bad” valence for the utterance, “dumb as rocks”, based on the utterance correlating to lower states like states 1 and 2. Lastly, the arousal for literal listener results is somewhat equal between “low” and “high”, a bit more probability on “low”, as states 1 and 2 in the arousalPrior correlate to a “high” and “low” arousal in that respective order.

The social utility in our model is derived from the politeness model, as presented in Chapter 9: Politeness. Just like in the politeness model, the pragmatic speaker is utilizing lambda to measure how social a person is being. However, this is different in our model because we set lambda to be negative. This has an opposite effect to social utility, making it so phi now measures how socially opposed a person is trying to be rather than how social. For example, in the original politeness model, a lower phi would correspond to the speaker trying to communicate politeness or be nice to the listener. However, in our model, a lower phi means the speaker is trying to be mean or “antisocial”. Additionally, alpha is set the same as the original politeness model.

The speaker function takes in a state, a phi, a QUD, a valence, and an arousal, then returns an utterance. Situationally, this means the speaker is aware of how dumb you are acting, how mean they want to be, the intention of their utterance, if they feel positively or negatively about your action, and how strongly they feel about the action. With all this information, the speaker then returns the probability distribution over utterances, demonstrating what utterances the speaker is most likely to say based on the inputted arguments. We also run Infer over a function that takes in no arguments, in order to return our marginalized distribution over the speaker arguments.

The speaker first samples an utterance, with uniform draw making all utterances equally likely, then gets a QUDanswer based on the inputted goal. Then, the speaker takes both the sampled utterance and inputted goal, runs them through the literal listener, to return the literal listener's posterior or the literal listener’s beliefs. The function of L0_stateMarginal and L0_qudAnswerMarginal is to return a joint marginalized distribution over states and QUDanswers. The utility portion of the speaker consists of the epistemic and antisocial values where epistemic is essentially the literal listener’s beliefs, while the antisocial is incorporating lambda, or the valueFunction, into it. The speaker utility then takes the inputted phi and multiplies it by the epistemic utility and adds that to the antisocial utility times phi’s complement. To maximize speaker utility, we multiply by alpha, returning an utterance based on the overall utility of the speaker (including epistemic and antisocial factors). 

A positive lambda uses social utility to promote politeness. 0 lambda has the speaker wanting to give information, no matter how the literal listener feels. A negative lambda uses social utility to demote politeness and encourage harm.

~~~~
///fold:
// Level of intelligence
var states = [1,2,3,4]

// What can be said
var utterances = ["", "dumb as rocks", "dumb", "f*cking idiot"]

// Correspondence of utterances to states
var literalSemantics = {
  "" : [.25,.25,.25,.25],
  "dumb as rocks" : [0.45,0.85,0.20,.02],
  "dumb" : [.85,.95,.02,.02],
  "f*cking idiot" : [.95,.55,.02,.02]
}

// Determine whether the utterance describes the state
// Flip a coin with the literalSemantics weight
// *state - 1 because of 0-indexing*
var meaning = function(utterance, state){
  return flip(literalSemantics[utterance][state - 1]);
}

// Whether the speaker feels good or bad about the listener's action.
// Speakers’ attitudes toward the true state of the world (e.g., the valence of their affect), which is modeled simply as a binary positive/negative variable (representing whether or not the speaker is repulsed by the listener’s behavior).
var valencePrior = function(state) {
  state === 1 ? flip(0.01) ? "good" : "bad" :
  state === 2 ? flip(0.33) ? "good" : "bad" :
  state === 3 ? flip(0.66) ? "good" : "bad" :
  state === 4 ? flip(0.99) ? "good" : "bad" :
  true}


// How amplified the speaker feels about the listener being dumb to different degrees.
var arousals = ["low", "high"]

//How passionate/aroused the listener feels about how intelligent the person is
var arousalPrior = function(state) {
  state === 1 ? categorical([0.1, 0.9], arousals) :
  state === 2 ? categorical([0.7, 0.3], arousals) :
  state === 3 ? categorical([0.7, 0.3], arousals) :
  state === 4 ? categorical([0.1, 0.9], arousals) :
  true
}

// A list of strings of QUD choices
var goals = ["goalState", "goalValence", "goalArousal"]

// There are 3 possible goals with a flat prior
var goalPrior = function() {
  categorical([1, 1, 1], goals)
}

// A speaker's goal is satisfied if the listener infers the correct
// and relevant information.
var goalState = function(goal, state, valence, arousal) {
  goal === "goalState" ? state :
  goal === "goalValence" ? valence :
  goal === "goalArousal" ? arousal :
  true
}

// literal listener
var literalListener = function(utterance, goal) {
  Infer({model: function(){
    var state = uniformDraw(states)
    var valence = valencePrior(state)
    var arousal = arousalPrior(state) 
    var m = meaning(utterance, state);
    condition(m);
    return {QUDanswer: goalState(goal,state,valence,arousal), state: state}
  }})
}
///

// value function scales social utility by a parameter lambda
var lambda = -1.25
var valueFunction = function(s){
  return lambda * s
};

var alpha = 10
var speaker1 = function(state, phi, goal, valence, arousal) {
  Infer({model: function(){
    var utterance = uniformDraw(utterances)
    var QUDanswer = goalState(goal,state,valence,arousal)
    var L0_posterior = literalListener(utterance,goal)
    var L0_stateMarginal = marginalize(L0_posterior,"state")
    var L0_qudAnswerMarginal = marginalize(L0_posterior,"QUDanswer")
    var utility = {
      epistemic: L0_qudAnswerMarginal.score(QUDanswer),
      antisocial: expectation(L0_stateMarginal, valueFunction)
    }
    var speakerUtility = phi * utility.epistemic +
                        (1 - phi) * utility.antisocial
    factor(alpha * speakerUtility)
    return utterance
  }})
}

// Data from pragmatic listener for each utterance
display("null: speaker1(4, 0.6965145285812476, ''goalState'', ''good'', ''high'')")
viz(speaker1(4, 0.6965145285812476, "goalState", "good", "high"))

display("dumb as rocks: speaker1(3, 0.5789691332024225, ''goalState'', ''good'', ''low'')")
viz(speaker1(3, 0.5789691332024225, "goalState", "good", "low"))

display("dumb: speaker1(2, 0.46859624229176833, ''goalArousal'', ''bad'', ''low'')")
viz(speaker1(2, 0.46859624229176833, "goalArousal", "bad", "low"))

display("f*cking idiot: speaker1(1, 0.39314615110470846, ''goalArousal'', ''bad'', ''high'')")
viz(speaker1(1, 0.39314615110470846, "goalArousal", "bad", "high"))
~~~~

Speaker is a function that takes in a state, phi, goal, valence, and arousal. We assign the state, phi, goal, and the corresponding QUD’s category from the Pragmatic Listener (mentioned later below). When the speaker passionately refers to state 1 with an epistemic phi of 0.39, the speaker is most likely referring to “fucking idiot” and slightly referring to “dumb”. When apathetically referring to state 2 with a somewhat epistemic phi of 0.47, the speaker is much more likely to refer to “dumb”, shifting distribution away from “fucking idiot”, and giving more probability to “dumb as rocks”. There is no longer any probability for “dumb” or “fucking idiot” when the speaker refers to state 3 with a more antisocial phi at 0.58, and speaking on the speaker’s state. Distribution is given more to dumb as rocks, but not as much as null. For state 4, all weight is shifted to null when the speaker wants to refer to the state of the listener at an even more antisocial phi of 0.7. With the highest phi of being intentionally mean and causing harm, the speaker would rather say nothing because the listener’s state is the highest it could be. Even when the listener is not at the highest state of intelligence and has a more antisocial utility to be mean, the speaker chooses to say nothing over teasing. The speaker avoids insult and signals intelligence passively by saying null for state 4, as it is the best utterance to convey that state with a highly positive affect. The speaker will communicate state 3 with null to convey a positive state, yet if we change phi to be more epistemic, the speaker is incredibly more likely to tease with “dumb as rocks,” while a more antisocial phi looks identical to the results of state 4. This shows that phi is very influential on what the speaker chooses to say, despite the goal and state of the world.

The pragmatic listener takes in an utterance and then returns the probability distribution of states in correspondence to phi. Wrapping the command in viz.marginals makes it so that the probability distributions of states and phi are separate from one another. We run Infer over a function that takes in no arguments, in order to return our marginalized distribution over states. The pragmatic listener is then sampling a state, a valence, an arousal, a goal, and a phi in order to input these arguments into the pragmatic speaker (speaker1) function. With observe, it then weighs how likely the speaker is to actually say an utterance to the inputted utterance.

~~~~
///fold:
// Level of intelligence
var states = [1,2,3,4]

// What can be said
var utterances = ["", "dumb as rocks", "dumb", "f*cking idiot"]

// Correspondence of utterances to states
var literalSemantics = {
  "" : [.25,.25,.25,.25],
  "dumb as rocks" : [0.45,0.85,0.20,.02],
  "dumb" : [.85,.95,.02,.02],
  "f*cking idiot" : [.95,.55,.02,.02]
}

// Determine whether the utterance describes the state
// Flip a coin with the literalSemantics weight
// *state - 1 because of 0-indexing*
var meaning = function(utterance, state){
  return flip(literalSemantics[utterance][state - 1]);
}

// Whether the speaker feels good or bad about the listener's action.
// Speakers’ attitudes toward the true state of the world (e.g., the valence of their affect), which is modeled simply as a binary positive/negative variable (representing whether or not the speaker is repulsed by the listener’s behavior).
var valencePrior = function(state) {
  state === 1 ? flip(0.01) ? "good" : "bad" :
  state === 2 ? flip(0.33) ? "good" : "bad" :
  state === 3 ? flip(0.66) ? "good" : "bad" :
  state === 4 ? flip(0.99) ? "good" : "bad" :
  true}


// How amplified the speaker feels about the listener being dumb to different degrees.
var arousals = ["low", "high"]

//How passionate/aroused the listener feels about how intelligent the person is
var arousalPrior = function(state) {
  state === 1 ? categorical([0.1, 0.9], arousals) :
  state === 2 ? categorical([0.7, 0.3], arousals) :
  state === 3 ? categorical([0.7, 0.3], arousals) :
  state === 4 ? categorical([0.1, 0.9], arousals) :
  true
}

// A list of strings of QUD choices
var goals = ["goalState", "goalValence", "goalArousal"]

// There are 3 possible goals with a flat prior
var goalPrior = function() {
  categorical([1, 1, 1], goals)
}

// A speaker's goal is satisfied if the listener infers the correct
// and relevant information.
var goalState = function(goal, state, valence, arousal) {
  goal === "goalState" ? state :
  goal === "goalValence" ? valence :
  goal === "goalArousal" ? arousal :
  true
}

// literal listener
var literalListener = function(utterance, goal) {
  Infer({model: function(){
    var state = uniformDraw(states)
    var valence = valencePrior(state)
    var arousal = arousalPrior(state) 
    var m = meaning(utterance, state);
    condition(m);
    return {QUDanswer: goalState(goal,state,valence,arousal), state: state}
  }})
}

// value function scales social utility by a parameter lambda
var lambda = -1.25
var valueFunction = function(s){
  return lambda * s
};

var alpha = 10
var speaker1 = function(state, phi, goal, valence, arousal) {
  Infer({model: function(){
    var utterance = uniformDraw(utterances)
    var QUDanswer = goalState(goal,state,valence,arousal)
    var L0_posterior = literalListener(utterance,goal)
    var L0_stateMarginal = marginalize(L0_posterior,"state")
    var L0_qudAnswerMarginal = marginalize(L0_posterior,"QUDanswer")
    var utility = {
      epistemic: L0_qudAnswerMarginal.score(QUDanswer),
      antisocial: expectation(L0_stateMarginal, valueFunction)
    }
    var speakerUtility = phi * utility.epistemic +
                        (1 - phi) * utility.antisocial
    factor(alpha * speakerUtility)
    return utterance
  }})
}
///

//pragmatic listener 
var pragmaticListener = function(utterance) {
  Infer({model: function(){
    var state = uniformDraw(states)
    var valence = valencePrior(state)
    var arousal = arousalPrior(state) 
    var goal = goalPrior()
//0.05 corresponds to the starting point of phi while 0.95 is the endpoint. The last 0.05 corresponds to the interval length
    var phi = uniformDraw(_.range(0.05, 0.95, 0.05))
    var S1 = speaker1(state, phi, goal, valence, arousal)
    observe(S1, utterance)
    return { state, phi , goal, valence, arousal }
  }})
}

//vizualize the marginal distributions for each utterance.
display("F*cking idiot")
        viz.marginals(pragmaticListener("f*cking idiot"))
display("Dumb")
        viz.marginals(pragmaticListener("dumb"))
display("Dumb as rocks")
        viz.marginals(pragmaticListener("dumb as rocks"))
display("null")
        viz.marginals(pragmaticListener(""))

//vizualize the listenerPosterior to get the exact phi for each utterance.
var listenerPosteriorA = pragmaticListener("f*cking idiot")
display("expected phi for f*cking idiot = " +
        expectation(marginalize(listenerPosteriorA, "phi")))

var listenerPosteriorB = pragmaticListener("dumb")
display("expected phi for dumb = " +
        expectation(marginalize(listenerPosteriorB, "phi")))

var listenerPosteriorC = pragmaticListener("dumb as rocks")
display("expected phi for dumb as rocks = " +
        expectation(marginalize(listenerPosteriorC, "phi")))

var listenerPosteriorD = pragmaticListener("")
display("expected phi for null = " +
        expectation(marginalize(listenerPosteriorD, "phi")))
~~~~

In terms of states, we can see a general shift from right to left as the utterance becomes more extreme or harsh. We can see the null utterance being most indicative of state 4 because it is the best utterance to communicate that state. This is because if someone has not made any mistake or error, there is no need to put out a teasing or even negative utterance about that person. This means silence, in our model, is mainly the absence of teasing or insulting. The biggest change to states we see from literal to pragmatic interpretation is from the utterance, “dumb as rocks”. Now, instead of this utterance being indicative of states 2 and even 1, it had shifted most of its probability over to state 3. This is due to the pragmatic listener’s ability to observe utterances in existence of one another, with the two forces of a null utterance and the insulting utterances pushing state 3 to be the best option for “dumb as rocks”. The pragmatic listener can recognize that the null utterance is the best option for state 4 while the more insulting utterances like “dumb” and “fucking idiot” are the best option for states 2 and 1. This leaves state 3 without an utterance if the pragmatic listener could not observe these utterances side by side to see that the only option left for state 3 has to be “dumb as rocks”. When we move into insulting utterances like “dumb” and “fucking idiot”, we see states 2 and 1, in that respective order, being the most probable states for those utterances. However, an interesting thing to note is how much probability the other states have for these utterances, even though, for example, “fucking idiot” has a 0.02 probability of returning true for state 4 in literalSemantics. The reason for this distribution in state probability has to do with phi. As we see, phi is shifting right to left as the utterance becomes more extreme because the harsher the utterance, the more mean a person is being by saying it. Phi, in a sense, trumps states where more insulting utterances like “dumb” and “fucking idiot” have to do more so with a person’s phi or character than it does with the state of the person taking an action. If a person is mean enough to say “fucking idiot”, it says more about the phi than it does about the state as a lower phi correlates to less certainty on the state a person is in. In a more anecdotal sense, if someone were to call you a “fucking idiot”, even though you have done nothing wrong, you are not questioning if the actions you took were incorrect more so than thinking this person is callous and rude. It is also recognized that probability on state 4 for “fucking idiot” can be interpreted as non-literal language based on the relationship of states and arousal seen in the marginal posterior distribution. If the pragmatic listener were to only return state and arousal, and the command pragmaticListener(“fucking idiot”) was put in, we see that what states 1 and 4 have in common for this utterance is having a higher probability on “high” arousal compared to “low” arousal. This is where the non-literal interpretation of state 4 stems from.

When moving into the results for goal, valence, and arousal, we see that many of the results make intuitive sense. The null utterance would have a goal of goalState since it is the only utterance that effectively communicates state 4 the best and has a “good” valence because you do not feel inclined to insult/tease anyone by staying silent. The reason why arousal is very high for the null utterance is also due to the fact that the alternatives are teases/insults. In order for you to “restrain” yourself from teasing/insulting someone, you have to feel really good about their actions, correlating to a “high” arousal along with that “good” valence. One thing that may not make complete sense at first comes from the results for the goal of “dumb as rocks”, which returns the highest probability on the goal of communicating a state. This is because of what was said earlier about “dumb as rocks” being the most effective, if not only, utterance to communicate state 3. Because of how indicative the utterance is to that state, the goal also returns as goalState. Additionally, “dumb as rocks” has a “good” valence and “low” arousal because, compared to the other utterances, you do not feel strongly positive to not tease the person a bit and you do not feel “bad” about a person’s actions as there are better utterances to communicate that disapproval of a “bad” valence. Lastly, the results for “dumb” and “fucking idiot” are similar in the sense that you are most likely communicating a “bad” valence if you are insulting someone with these utterances when you could have stayed silent or given a teasing remark in their place. However, they differ in arousal as “dumb” would be a lower arousal compared to “fucking idiot”. This is because “fucking idiot” is a more extreme utterance that communicates more disdain than “dumb”. Furthermore, the reason why both these utterances return the highest probability on the goal being arousal, and not valence, is that arousal is the main thing that distinguishes these two utterances from one another. One is more arousing than the other, so both are most likely communicating arousal based on which insult you are picking. 

Final compiled teasing model:

~~~~
// Level of intelligence
var states = [1,2,3,4]

// What can be said
var utterances = ["", "dumb as rocks", "dumb", "f*cking idiot"]

// Correspondence of utterances to states
var literalSemantics = {
  "" : [.25,.25,.25,.25],
  "dumb as rocks" : [0.45,0.85,0.20,.02],
  "dumb" : [.85,.95,.02,.02],
  "f*cking idiot" : [.95,.55,.02,.02]
}

// Determine whether the utterance describes the state
// Flip a coin with the literalSemantics weight
// *state - 1 because of 0-indexing*
var meaning = function(utterance, state){
  return flip(literalSemantics[utterance][state - 1]);
}

// Whether the speaker feels good or bad about the listener's action.
// Speakers’ attitudes toward the true state of the world (e.g., the valence of their affect), which is modeled simply as a binary positive/negative variable (representing whether or not the speaker is repulsed by the listener’s behavior).
var valencePrior = function(state) {
  state === 1 ? flip(0.01) ? "good" : "bad" :
  state === 2 ? flip(0.33) ? "good" : "bad" :
  state === 3 ? flip(0.66) ? "good" : "bad" :
  state === 4 ? flip(0.99) ? "good" : "bad" :
  true}

// How amplified the speaker feels about the listener being dumb to different degrees.
var arousals = ["low", "high"]

//How passionate/aroused the listener feels about how intelligent the person is
var arousalPrior = function(state) {
  state === 1 ? categorical([0.1, 0.9], arousals) :
  state === 2 ? categorical([0.7, 0.3], arousals) :
  state === 3 ? categorical([0.7, 0.3], arousals) :
  state === 4 ? categorical([0.1, 0.9], arousals) :
  true
}

// A list of strings of QUD choices
var goals = ["goalState", "goalValence", "goalArousal"]

// There are 3 possible goals with a flat prior
var goalPrior = function() {
  categorical([1, 1, 1], goals)
}

// A speaker's goal is satisfied if the listener infers the correct
// and relevant information.
var goalState = function(goal, state, valence, arousal) {
  goal === "goalState" ? state :
  goal === "goalValence" ? valence :
  goal === "goalArousal" ? arousal :
  true
}

// literal listener
var literalListener = function(utterance, goal) {
  Infer({model: function(){
    var state = uniformDraw(states)
    var valence = valencePrior(state)
    var arousal = arousalPrior(state) 
    var m = meaning(utterance, state);
    condition(m);
    return {QUDanswer: goalState(goal,state,valence,arousal), state: state}
  }})
}

// value function scales social utility by a parameter lambda
var lambda = -1.25
var valueFunction = function(s){
  return lambda * s
};

var alpha = 10
var speaker1 = function(state, phi, goal, valence, arousal) {
  Infer({model: function(){
    var utterance = uniformDraw(utterances)
    var QUDanswer = goalState(goal,state,valence,arousal)
    var L0_posterior = literalListener(utterance,goal)
    var L0_stateMarginal = marginalize(L0_posterior,"state")
    var L0_qudAnswerMarginal = marginalize(L0_posterior,"QUDanswer")
    var utility = {
      epistemic: L0_qudAnswerMarginal.score(QUDanswer),
      antisocial: expectation(L0_stateMarginal, valueFunction)
    }
    var speakerUtility = phi * utility.epistemic +
                        (1 - phi) * utility.antisocial
    factor(alpha * speakerUtility)
    return utterance
  }})
}

//pragmatic listener 
var pragmaticListener = function(utterance) {
  Infer({model: function(){
    var state = uniformDraw(states)
    var valence = valencePrior(state)
    var arousal = arousalPrior(state) 
    var goal = goalPrior()
//0.05 corresponds to the starting point of phi while 0.95 is the endpoint. 
//The last 0.05 corresponds to the interval length
    var phi = uniformDraw(_.range(0.05, 0.95, 0.05))
    var S1 = speaker1(state, phi, goal, valence, arousal)
    observe(S1, utterance)
   return { state, phi, goal, valence, arousal }
  }})
}
viz.marginals(pragmaticListener(""))
viz.marginals(pragmaticListener("dumb as rocks"))
viz.marginals(pragmaticListener("dumb"))
viz.marginals(pragmaticListener("f*cking idiot"))
~~~~