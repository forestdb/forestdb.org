---
layout: model
title: Lai - Irony
model-language: webppl
---

It is a irony model that the utterances meaning intended to describe the opposite of the real meaning. For instance, today is a beautiful day but I tell you the weather is "terrible", you will not think that I am trying the convey the information literally. You realize that I am trying to convey the information ironically, so I am actually telling you the weather is really good. The following model have three conversational goals: communicating about the true state, communicating about the speaker’s valence (i.e., whether they feel positively or negatively toward the state), and communicating about the speaker’s arousal (i.e., how strongly they feel about the state).

The RSA model used here is to model the situation that people say ironic utterance, and there are three kinds of participants here. LiteralListener represents people make reflections on what they hear, and they don't have the prior knowledge, like today is a beautiful day. Speaker have the prior knowledge and they know what LiteralListener responses, and they response an utterance, which can let the LiteralListener know the state with highest probability. Finally, we have PragmaticListener who also has the prior knowledge, and they hear what the speaker response the utterance. Then they analyze what the state, valence and arousal that the speaker want to response. 

~~~~
// There are three possible states the weather could be in: 
// terrible, ok, or amazing
var states = ['terrible', 'ok', 'amazing']

// Since we are in California, the prior over these states
// are the following. Once could also imagine this being 
// the prior in a certain context, e.g. when it's clearly
// sunny and nice out.
var statePrior = function() {
  categorical([1, 50, 50], states)
}

Infer(statePrior)
~~~~

First we get the state and statePrior. The three states to describe the weather is "terrible", "ok", and "amazing". The statePrior is categorical, which is actually because all the listener and spekaer have the prior that today is a beautiful day. Therefore we have more probaility of prior to the ok and amazing. 

~~~~
// Valence prior defined in terms of negative valence. 
// If the current state is terrible, it's extremely likely
// that the valence associated is negative. If it's ok, then 
// the valence could be negative or positive with equal 
// probability.
var valencePrior = function(state) {
  state === "terrible" ? flip(0.99) ? -1 : 1 :
  state === "ok" ? flip(0.5) ? -1 : 1 :
  state === "amazing" ? flip(0.01) ? -1 : 1 :
  true
}

Infer(valencePrior)
~~~~

valencePrior represent the negative valence that people feel. If their current state is "terrible", it is extremely like that their valence is negative, which it is equal to -1. If their current state is "ok", it is equally likely that their valence is negative or positive, so both side have 50% probability. If their current state is "amazing", it is extremely like that their valence is positive, which it is equal to 1. Then the code will be, if the state is "terrible", then flip the coin. There are 99% probability to be True. If it is True, get -1. If it is false, get 1. If the state is "ok", then flip the coin. There are 50% probability to be True. If the state is amazing, then flip the coin. There are only 1% probabilty to be True. 

~~~~
// Define binary arousals (could model as continuous).
var arousals = ["low", "high"]

var arousalPrior = function(state) {
  state === "terrible" ? categorical([0.1, 0.9], arousals) :
  state === "ok" ? categorical([0.9, 0.1], arousals) :
  state === "amazing" ? categorical([0.1, 0.9], arousals) :
  true
}

Infer(arousalPrior)
~~~~

There are arousals "low" and "high" to describe how people feel about the state. If the state is "terrible" or "amazing", it is a categorical draw that there are much more probabilities to have a strong feeling, so the probability is 0.9 for "high" and 0.1 for "low". If the state is "ok", it reverses, the probability is 0.1 for "high" and 0.9 for "low". 

~~~~
// Define goals and goal priors. Could want to communicate state of the world,    
// valence about it, or arousal (intensity of feeling) about it.
var goals = ["goalState", "goalValence", "goalArousal"]

var goalPrior = function() {
  categorical([1, 1, 1], goals)
}

// Assume possible utterances are identical to possible states
var utterances = states

// Assume cost of utterances is uniform.
var utterancePrior = function() {
  uniformDraw(utterances)
}
~~~~

goals is the QUD that a speaker is likely addressing with their utterance. There are three QUDs here, State, Valence, and Arousal. Each have equal prior probability. There are three utterances same as states, "terrible", "ok", and "amazing", but the prior will be the uniformdraw, same probability for the three utterances. 

~~~~
// Literal interpretation is just whether utterance equals state
var literalInterpretation = function(utterance, state) {
  utterance === state
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

The literalInterpretation is let the utterance equal to state, so the speaker will choose the utterances in the three states "terrible", "ok", and "amazing". goalState is the QUD that ask speaker to answer, and how the goal match the goalState. 

~~~~
// Define a literal listener
var literalListener = function(utterance, goal) {
  Infer({model: function(){
    var state = uniformDraw(states)
    var valence = valencePrior(state)
    var arousal = arousalPrior(state)
    condition(literalInterpretation(utterance,state))
    return goalState(goal, state, valence, arousal)
  }})
}
~~~~

For the literalListener, this is the function with argument utterance and goal. They hear an utterance and a goal. state is from uniformDraw, valence is from valencePrior, and arousal is take from arousalPrior. The condition of literalInterpretation let the utterance equal to state. Then literalListener return the goalState which matches the goal they hear. 

~~~~
// Define a speaker
var speaker = function(state, valence, arousal, goal) {
  Infer({model: function(){
    var utterance = utterancePrior()
    factor(1 * literalListener(utterance, 
                    goal).score(goalState(goal, 
                                          state, 
                                          valence, 
                                          arousal)))
    return utterance
  }})
}
~~~~

For the speaker, this is the function with argument state, valence, arousal, and goal. They learn the actual state, valence, and arousal, and they hear the QUD they will answer. The utterance is take from utterancePrior. 

~~~~
// Define a pragmatic listener
var pragmaticListener = function(utterance) {
  Infer({model: function(){
    var state = statePrior()
    var valence = valencePrior(state)
    var arousal = arousalPrior(state)
    var goal = goalPrior()
    observe(speaker(state, valence, arousal, goal),utterance)
    return {state, valence, arousal}
  }})
}
~~~~

The pragmaticListener is the function with argument utternace. The pragmaticListener only hear the utterance, and they take state from statePrior, valence from valencePrior, arousal from arousalPrior, and goal from goalPrior. They observe how speaker see the state, valence, arousal, goal, and how speaker use utterance to answer. Finally they return the types of state, valence, arousal with proability. 

Similarity: It is the model similar with hyperbole model that both model is under the situation that you have the probabilities for each state and valence priorly. And they both include the QUDs to answer. So this the situation for both model to predict how they will interpret the non-literal utterances. For example, in the hyperbole model, people have price prior $50 with high probability, but they hear $50000. In the irony model, people have state prior “amazing” or “ok” with high probability, but they hear “terrible”. 


Difference: The differences is that the irony model add one QUD arousal in the mode. The arousal is used to test how strongly they feel about the state. The reason to add the arousal is to make certain for the PragmaticListener that they consider the state is "amazing". If there is no arousal QUD, the return with most probabilitywhen PragmaticListener hear "terrible" will be "ok".

The following is the whole code.

~~~~
// There are three possible states the weather could be in: 
// terrible, ok, or amazing
var states = ['terrible', 'ok', 'amazing']

// Since we are in California, the prior over these states
// are the following. Once could also imagine this being 
// the prior in a certain context, e.g. when it's clearly
// sunny and nice out.
var statePrior = function() {
  categorical([1, 50, 50], states)
}

// Valence prior defined in terms of negative valence. 
// If the current state is terrible, it's extremely likely
// that the valence associated is negative. If it's ok, then 
// the valence could be negative or positive with equal 
// probability.
var valencePrior = function(state) {
  state === "terrible" ? flip(0.99) ? -1 : 1 :
  state === "ok" ? flip(0.5) ? -1 : 1 :
  state === "amazing" ? flip(0.01) ? -1 : 1 :
  true
}

// Define binary arousals (could model as continuous).
var arousals = ["low", "high"]

// Define goals and goal priors. Could want to communicate state of the world,    
// valence about it, or arousal (intensity of feeling) about it.
var goals = ["goalState", "goalValence", "goalArousal"]

var goalPrior = function() {
  categorical([1, 1, 1], goals)
}

// Assume possible utterances are identical to possible states
var utterances = states

// Assume cost of utterances is uniform.
var utterancePrior = function() {
  uniformDraw(utterances)
}

// Sample arousal given a state.
var arousalPrior = function(state) {
  state === "terrible" ? categorical([0.1, 0.9], arousals) :
  state === "ok" ? categorical([0.9, 0.1], arousals) :
  state === "amazing" ? categorical([0.1, 0.9], arousals) :
  true
}

// Literal interpretation is just whether utterance equals state
var literalInterpretation = function(utterance, state) {
  utterance === state
}

// A speaker's goal is satisfied if the listener infers the correct 
// and relevant information.
var goalState = function(goal, state, valence, arousal) {
  goal === "goalState" ? state :
  goal === "goalValence" ? valence :
  goal === "goalArousal" ? arousal :
  true
}

// Define a literal listener
var literalListener = function(utterance, goal) {
  Infer({model: function(){
    var state = uniformDraw(states)
    var valence = valencePrior(state)
    var arousal = arousalPrior(state)
    condition(literalInterpretation(utterance,state))
    return goalState(goal, state, valence, arousal)
  }})
}

// Define a speaker
var speaker = function(state, valence, arousal, goal) {
  Infer({model: function(){
    var utterance = utterancePrior()
    factor(1 * literalListener(utterance, 
                    goal).score(goalState(goal, 
                                          state, 
                                          valence, 
                                          arousal)))
    return utterance
  }})
}

// Define a pragmatic listener
var pragmaticListener = function(utterance) {
  Infer({model: function(){
    var state = statePrior()
    var valence = valencePrior(state)
    var arousal = arousalPrior(state)
    var goal = goalPrior()
    observe(speaker(state, valence, arousal, goal),utterance)
    return {state, valence, arousal, goal}
  }})
}

viz.table(literalListener("terrible", "goalState"))
viz.table(speaker("terrible", -1, "high", "goalValence"))
viz.table(pragmaticListener("terrible"))
~~~~

We can see that when LiteralListener hear "terrible", they think that the state will be "terrible". When PragmaticListener hear "terrible", the two states with most probability are "amazing", 1, and "high", and "ok", -1, "low". The state is "amazing" and "ok" is that PragmaticListener have the prior statePrior with really high probability "amazing" and "ok". When they think the state is "amazing", they think the valence is 1, and arousal is "high". When they think the state is "ok", valence is -1, and arousal is "low". The valence is -1 but not 1 for state "ok" is that they still hear the utterance "terrible" from the speaker. Therefore, they may think that the true valence is -1 but not 1. 

In addition, when we change or add the goal in the return of pragmaticListener. We can see that for the highest probability condition with state "amazing", PragmaticListener think that the speaker answer the goalArousal question, and this is because they think the arousal QUD is the most informative. "Amazing" and "terrible" have the same arousalprior.