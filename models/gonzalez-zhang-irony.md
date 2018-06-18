---
layout: model
title: Gonzalez & Zhang Irony S2
model-language: webppl
---

The Original Irony Model

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
    var state = statePrior()
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
    return {state, valence, arousal}
  }})
}

viz.marginals(pragmaticListener("amazing"))
~~~~

Introduction of the Irony model:
The irony model introduced by Kao and Goodman demonstrates the parallels in people's interpretation of irony given the states of the weather. There were nine images of the weather and a sentence that reads, "Ann says, 'The weather is ___!'" where an adjective "terrible, bad, okay, good, and amazing". The participant then had to rate how ironic the statement was, how Ann would feel rate the weather from terrible, bad, neutral, good and amazing and they also rated how likely it was for Ann to feel the seven emotions about the weather: excited, happy, content, neutral, good, or amazing. 

Results for this part of the experiment showed that participants did well when matching the intuitions of irony--utterances that have an intended meaning opposite of what the speaker intends to say. (Kao and Goodman, 2015)

 

This influenced us to build an RSA model that captures sarcasm. We wanted to demonstrate sarcasm through a conversation between two people. 

Below is the conversation between two people talking about Mary's dress:

A: Did  you see the dress that Mary was wearing?
B: Yeah, I did!
A: It had lots of tulle and was covered in pom poms.
B: It's so pretty!

The dress covered in pom poms and tulle is meant to show the dress is ugly. Speaker B says it's so pretty to show sarcasm in the model. 

Irony is thought of as saying one thing, but meaning the complete opposite.
Sarcasm is similar to irony, only it is meant to insult the person.

Below, we introduce the beginning of the model. 
There are three states the dress could be in and we created a variable.

~~~~
//We have three states the dress could be in:
// "so pretty", "okay", "so ugly"

var states = ['so pretty','okay','so ugly']
~~~~

We need a State Prior for the model so we create a function that takes in categorical. This in turn takes in the probabilities of each state. 

~~~~
//We have three states the dress could be in:
// "so pretty", "okay", "so ugly"

var states = ['so pretty','okay','so ugly']

// the prior over these states

var statePrior = function() {
  categorical([1, 50, 50], states)
}
~~~~

After the statePrior we introduce the valence Prior. This is one of the important parts of the model that will help us with the sarcasm model. The valence will be used to determine how the speaker feels about the state. We determined the valence with a weighted probability for each state. 
Positive states ==1 
Negative states == -1

The model will run "So pretty". The (0.99) probability means for that state, there is a (0.99) probability that the speaker will feel positive and (0.01) probability that the speaker will feel negative.
"Okay" has an equal probability (0.5) for positive and negative. 
"So ugly" has a (0.01) chance of the speaker feeling positive about the state and (0.99) probability of feeling negative. 

These probabilities match with the intuitions people have of these utterances and so we adjusted the probabilities to reflect that. 

~~~~
///fold:

//We have three states the dress could be in:
// "so pretty", "okay", "so ugly"

var states = ['so pretty','okay','so ugly']

// the prior over these states

var statePrior = function() {
  categorical([1, 50, 50], states)
}

///
                              
// Valence prior defined in terms of negative valence. 
// If the current state is terrible, it's extremely likely
// that the valence associated is negative. If it's ok, then 
// the valence could be negative or positive with equal 
// probability.
var valencePrior = function(state) {
  state === "so pretty" ? flip(0.99) ? 1 : -1 :
  state === "okay" ? flip(0.5) ? -1 : 1 :
  state === "so ugly" ? flip(0.01) ? 1 : -1 :
  true
}
~~~~

The next important part of our sarcasm model is the variable arousal of the speaker. This is going to determine how strongly the speaker feels about the state. 

Low arousal == does not feel strongly about state
High arousal == feels strongly about state

~~~~
///fold:

//We have three states the dress could be in:
// "so pretty", "okay", "so ugly"

var states = ['so pretty','okay','so ugly']

// the prior over these states

var statePrior = function() {
  categorical([1, 50, 50], states)
}

// Valence prior defined in terms of negative valence. 
// If the current state is terrible, it's extremely likely
// that the valence associated is negative. If it's ok, then 
// the valence could be negative or positive with equal 
// probability.
var valencePrior = function(state) {
  state === "so pretty" ? flip(0.99) ? 1 : -1 :
  state === "okay" ? flip(0.5) ? -1 : 1 :
  state === "so ugly" ? flip(0.01) ? 1 : -1 :
  true
}

///

// Define binary arousals (could model as continuous).
var arousals = ["low", "high"]
~~~~

Next, we introduce goals in our model. The purpose of having goals is to inform us of the state of the world, valence and arousal. 

~~~~
///fold:

//We have three states the dress could be in:
// "so pretty", "okay", "so ugly"

var states = ['so pretty','okay','so ugly']

// the prior over these states

var statePrior = function() {
  categorical([1, 50, 50], states)
}

// Valence prior defined in terms of negative valence. 
// If the current state is terrible, it's extremely likely
// that the valence associated is negative. If it's ok, then 
// the valence could be negative or positive with equal 
// probability.
var valencePrior = function(state) {
  state === "so pretty" ? flip(0.99) ? 1 : -1 :
  state === "okay" ? flip(0.5) ? -1 : 1 :
  state === "so ugly" ? flip(0.01) ? 1 : -1 :
  true
}

// Define binary arousals (could model as continuous).
var arousals = ["low", "high"]

///

// Define goals and goal priors. Could want to communicate state' of the world,    
// valence about it, or arousal (intensity of feeling) about it.
var goals = ["goalState", "goalValence", "goalArousal"]
~~~~

We made a goalPrior for the goals. This variable is going to run the function on categorical for the goals. We gave each of the goals equal probability in order to make sure one goal does not take preference over another. 

~~~~
///fold:

//We have three states the dress could be in:
// "so pretty", "okay", "so ugly"

var states = ['so pretty','okay','so ugly']

// the prior over these states

var statePrior = function() {
  categorical([1, 50, 50], states)
}

// Valence prior defined in terms of negative valence. 
// If the current state is terrible, it's extremely likely
// that the valence associated is negative. If it's ok, then 
// the valence could be negative or positive with equal 
// probability.
var valencePrior = function(state) {
  state === "so pretty" ? flip(0.99) ? 1 : -1 :
  state === "okay" ? flip(0.5) ? -1 : 1 :
  state === "so ugly" ? flip(0.01) ? 1 : -1 :
  true
}

// Define binary arousals (could model as continuous).
var arousals = ["low", "high"]

// Define goals and goal priors. Could want to communicate state' of the world,    
// valence about it, or arousal (intensity of feeling) about it.
var goals = ["goalState", "goalValence", "goalArousal"]

///

var goalPrior = function() {
  categorical([1, 1, 1], goals)
}
~~~~

Next, we made utterances for each of the states. We used the states we define previously and just added "The dress is ____"

~~~~
///fold:

//We have three states the dress could be in:
// "so pretty", "okay", "so ugly"

var states = ['so pretty','okay','so ugly']

// the prior over these states

var statePrior = function() {
  categorical([1, 50, 50], states)
}

// Valence prior defined in terms of negative valence. 
// If the current state is terrible, it's extremely likely
// that the valence associated is negative. If it's ok, then 
// the valence could be negative or positive with equal 
// probability.
var valencePrior = function(state) {
  state === "so pretty" ? flip(0.99) ? 1 : -1 :
  state === "okay" ? flip(0.5) ? -1 : 1 :
  state === "so ugly" ? flip(0.01) ? 1 : -1 :
  true
}

// Define binary arousals (could model as continuous).
var arousals = ["low", "high"]

// Define goals and goal priors. Could want to communicate state' of the world,    
// valence about it, or arousal (intensity of feeling) about it.
var goals = ["goalState", "goalValence", "goalArousal"]

var goalPrior = function() {
  categorical([1, 1, 1], goals)
}

///

// Assume possible utterances are identical to possible states
// var utterances = states
var utterances = ["the dress is so pretty","the dress is so ugly","the dress is okay"]
~~~~

Utterance Prior is defined and we gave each of these equal probability so no one utterance has priority over the other. 

~~~~
///fold:

//We have three states the dress could be in:
// "so pretty", "okay", "so ugly"

var states = ['so pretty','okay','so ugly']

// the prior over these states

var statePrior = function() {
  categorical([1, 50, 50], states)
}

// Valence prior defined in terms of negative valence. 
// If the current state is terrible, it's extremely likely
// that the valence associated is negative. If it's ok, then 
// the valence could be negative or positive with equal 
// probability.
var valencePrior = function(state) {
  state === "so pretty" ? flip(0.99) ? 1 : -1 :
  state === "okay" ? flip(0.5) ? -1 : 1 :
  state === "so ugly" ? flip(0.01) ? 1 : -1 :
  true
}

// Define binary arousals (could model as continuous).
var arousals = ["low", "high"]

// Define goals and goal priors. Could want to communicate state' of the world,    
// valence about it, or arousal (intensity of feeling) about it.
var goals = ["goalState", "goalValence", "goalArousal"]

var goalPrior = function() {
  categorical([1, 1, 1], goals)
}

// Assume possible utterances are identical to possible states
// var utterances = states
var utterances = ["the dress is so pretty","the dress is so ugly","the dress is okay"]

///

// Assume cost of utterances is categorical.
var utterancePrior = function() {
  categorical({vs:utterances,
              ps: [0.2,0.2,0.2]})
}
~~~~

Arousal Prior defined and we have it check the arousal for each of the states. 

~~~~
///fold:

//We have three states the dress could be in:
// "so pretty", "okay", "so ugly"

var states = ['so pretty','okay','so ugly']

// the prior over these states

var statePrior = function() {
  categorical([1, 50, 50], states)
}

// Valence prior defined in terms of negative valence. 
// If the current state is terrible, it's extremely likely
// that the valence associated is negative. If it's ok, then 
// the valence could be negative or positive with equal 
// probability.
var valencePrior = function(state) {
  state === "so pretty" ? flip(0.99) ? 1 : -1 :
  state === "okay" ? flip(0.5) ? -1 : 1 :
  state === "so ugly" ? flip(0.01) ? 1 : -1 :
  true
}

// Define binary arousals (could model as continuous).
var arousals = ["low", "high"]

// Define goals and goal priors. Could want to communicate state' of the world,    
// valence about it, or arousal (intensity of feeling) about it.
var goals = ["goalState", "goalValence", "goalArousal"]

var goalPrior = function() {
  categorical([1, 1, 1], goals)
}

// Assume possible utterances are identical to possible states
// var utterances = states
var utterances = ["the dress is so pretty","the dress is so ugly","the dress is okay"]

// Assume cost of utterances is categorical.
var utterancePrior = function() {
  categorical({vs:utterances,
              ps: [0.2,0.2,0.2]})
}

///

// Sample arousal given a state.
var arousalPrior = function(state) {
  state === "so pretty" ? categorical([0.1, 0.9], arousals) :
  state === "okay" ? categorical([0.9, 0.1], arousals) :
  state === "so ugly" ? categorical([0.1, 0.9], arousals) :
  true
}
~~~~

The Literal Interpretation is an if else statement because we want it to check if the utterance is equal to the state. It is going to check the states for all of them and if the utterance does not match the state, then it is going to return "so ugly".

~~~~
///fold:

//We have three states the dress could be in:
// "so pretty", "okay", "so ugly"

var states = ['so pretty','okay','so ugly']

// the prior over these states

var statePrior = function() {
  categorical([1, 50, 50], states)
}

// Valence prior defined in terms of negative valence. 
// If the current state is terrible, it's extremely likely
// that the valence associated is negative. If it's ok, then 
// the valence could be negative or positive with equal 
// probability.
var valencePrior = function(state) {
  state === "so pretty" ? flip(0.99) ? 1 : -1 :
  state === "okay" ? flip(0.5) ? -1 : 1 :
  state === "so ugly" ? flip(0.01) ? 1 : -1 :
  true
}

// Define binary arousals (could model as continuous).
var arousals = ["low", "high"]

// Define goals and goal priors. Could want to communicate state' of the world,    
// valence about it, or arousal (intensity of feeling) about it.
var goals = ["goalState", "goalValence", "goalArousal"]

var goalPrior = function() {
  categorical([1, 1, 1], goals)
}

// Assume possible utterances are identical to possible states
// var utterances = states
var utterances = ["the dress is so pretty","the dress is so ugly","the dress is okay"]

// Assume cost of utterances is categorical.
var utterancePrior = function() {
  categorical({vs:utterances,
              ps: [0.2,0.2,0.2]})
}

// Sample arousal given a state.
var arousalPrior = function(state) {
  state === "so pretty" ? categorical([0.1, 0.9], arousals) :
  state === "okay" ? categorical([0.9, 0.1], arousals) :
  state === "so ugly" ? categorical([0.1, 0.9], arousals) :
  true
}

///

// Literal interpretation is just whether utterance equals state
var literalInterpretation = function(utterance, state) {
  utterance == "the dress is so pretty" ? state == "so pretty" :
  utterance == "the dress is okay" ? state == "okay":
  state == "so ugly"
}
~~~~

For the variable GoalState we wanted it to take in the goal, state, valence, and arousal. This is going to check that the goalState is equal to the state, the goalValence is equal to the valence and so on. If not, then it is going to return null. 

~~~~
///fold:

//We have three states the dress could be in:
// "so pretty", "okay", "so ugly"

var states = ['so pretty','okay','so ugly']

// the prior over these states

var statePrior = function() {
  categorical([1, 50, 50], states)
}

// Valence prior defined in terms of negative valence. 
// If the current state is terrible, it's extremely likely
// that the valence associated is negative. If it's ok, then 
// the valence could be negative or positive with equal 
// probability.
var valencePrior = function(state) {
  state === "so pretty" ? flip(0.99) ? 1 : -1 :
  state === "okay" ? flip(0.5) ? -1 : 1 :
  state === "so ugly" ? flip(0.01) ? 1 : -1 :
  true
}

// Define binary arousals (could model as continuous).
var arousals = ["low", "high"]

// Define goals and goal priors. Could want to communicate state' of the world,    
// valence about it, or arousal (intensity of feeling) about it.
var goals = ["goalState", "goalValence", "goalArousal"]

var goalPrior = function() {
  categorical([1, 1, 1], goals)
}

// Assume possible utterances are identical to possible states
// var utterances = states
var utterances = ["the dress is so pretty","the dress is so ugly","the dress is okay"]

// Assume cost of utterances is categorical.
var utterancePrior = function() {
  categorical({vs:utterances,
              ps: [0.2,0.2,0.2]})
}

// Sample arousal given a state.
var arousalPrior = function(state) {
  state === "so pretty" ? categorical([0.1, 0.9], arousals) :
  state === "okay" ? categorical([0.9, 0.1], arousals) :
  state === "so ugly" ? categorical([0.1, 0.9], arousals) :
  true
}

// Literal interpretation is just whether utterance equals state
var literalInterpretation = function(utterance, state) {
  utterance == "the dress is so pretty" ? state == "so pretty" :
  utterance == "the dress is okay" ? state == "okay":
  state == "so ugly"
}

///

// A speaker's goal is satisfied if the listener infers the correct 
// and relevant information.
var goalState = function(goal, state, valence, arousal) {
  goal === "goalState" ? state :
  goal === "goalValence" ? valence :
  goal === "goalArousal" ? arousal :
  true
}
~~~~

A literal listener conditions on the literal meaning of the utterance to provide updates to the prior goal states. The goal state determines which kind of distribution will be returned.


~~~~
///fold:

//We have three states the dress could be in:
// "so pretty", "okay", "so ugly"

var states = ['so pretty','okay','so ugly']

// the prior over these states

var statePrior = function() {
  categorical([1, 50, 50], states)
}

// Valence prior defined in terms of negative valence. 
// If the current state is terrible, it's extremely likely
// that the valence associated is negative. If it's ok, then 
// the valence could be negative or positive with equal 
// probability.
var valencePrior = function(state) {
  state === "so pretty" ? flip(0.99) ? 1 : -1 :
  state === "okay" ? flip(0.5) ? -1 : 1 :
  state === "so ugly" ? flip(0.01) ? 1 : -1 :
  true
}

// Define binary arousals (could model as continuous).
var arousals = ["low", "high"]

// Define goals and goal priors. Could want to communicate state' of the world,    
// valence about it, or arousal (intensity of feeling) about it.
var goals = ["goalState", "goalValence", "goalArousal"]

var goalPrior = function() {
  categorical([1, 1, 1], goals)
}

// Assume possible utterances are identical to possible states
// var utterances = states
var utterances = ["the dress is so pretty","the dress is so ugly","the dress is okay"]

// Assume cost of utterances is categorical.
var utterancePrior = function() {
  categorical({vs:utterances,
              ps: [0.2,0.2,0.2]})
}

// Sample arousal given a state.
var arousalPrior = function(state) {
  state === "so pretty" ? categorical([0.1, 0.9], arousals) :
  state === "okay" ? categorical([0.9, 0.1], arousals) :
  state === "so ugly" ? categorical([0.1, 0.9], arousals) :
  true
}

// Literal interpretation is just whether utterance equals state
var literalInterpretation = function(utterance, state) {
  utterance == "the dress is so pretty" ? state == "so pretty" :
  utterance == "the dress is okay" ? state == "okay":
  state == "so ugly"
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

// Define a literal listener
var literalListener = function(utterance, goal) {
  Infer({model: function(){
    var state = statePrior()
    var valence = valencePrior(state)
    var arousal = arousalPrior(state)
    condition(literalInterpretation(utterance,state))
    return goalState(goal, state, valence, arousal)
  }})
}
~~~~

Here the literal speaker takes in what the literal Listener produced and infers the function based on the utterances selected from utterance Prior and factors against the literal Listener and the score of the goal state.


~~~~
///fold:

//We have three states the dress could be in:
// "so pretty", "okay", "so ugly"

var states = ['so pretty','okay','so ugly']

// the prior over these states

var statePrior = function() {
  categorical([1, 50, 50], states)
}

// Valence prior defined in terms of negative valence. 
// If the current state is terrible, it's extremely likely
// that the valence associated is negative. If it's ok, then 
// the valence could be negative or positive with equal 
// probability.
var valencePrior = function(state) {
  state === "so pretty" ? flip(0.99) ? 1 : -1 :
  state === "okay" ? flip(0.5) ? -1 : 1 :
  state === "so ugly" ? flip(0.01) ? 1 : -1 :
  true
}

// Define binary arousals (could model as continuous).
var arousals = ["low", "high"]

// Define goals and goal priors. Could want to communicate state' of the world,    
// valence about it, or arousal (intensity of feeling) about it.
var goals = ["goalState", "goalValence", "goalArousal"]

var goalPrior = function() {
  categorical([1, 1, 1], goals)
}

// Assume possible utterances are identical to possible states
// var utterances = states
var utterances = ["the dress is so pretty","the dress is so ugly","the dress is okay"]

// Assume cost of utterances is categorical.
var utterancePrior = function() {
  categorical({vs:utterances,
              ps: [0.2,0.2,0.2]})
}

// Sample arousal given a state.
var arousalPrior = function(state) {
  state === "so pretty" ? categorical([0.1, 0.9], arousals) :
  state === "okay" ? categorical([0.9, 0.1], arousals) :
  state === "so ugly" ? categorical([0.1, 0.9], arousals) :
  true
}

// Literal interpretation is just whether utterance equals state
var literalInterpretation = function(utterance, state) {
  utterance == "the dress is so pretty" ? state == "so pretty" :
  utterance == "the dress is okay" ? state == "okay":
  state == "so ugly"
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
    var state = statePrior()
    var valence = valencePrior(state)
    var arousal = arousalPrior(state)
    condition(literalInterpretation(utterance,state))
    return goalState(goal, state, valence, arousal)
  }})
}

///

// Define a speaker
var speaker1 = function(state, valence, arousal, goal) {
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

This pragmatic listener takes in the results produced by S1 and does a “cross reference” together with state, valence and arousal as defined previosuly. Then, by observing the S1, the pragmatic listener spits out a proper combination of state, valence, and arousal. 

~~~~
///fold:

//We have three states the dress could be in:
// "so pretty", "okay", "so ugly"

var states = ['so pretty','okay','so ugly']

// the prior over these states

var statePrior = function() {
  categorical([1, 50, 50], states)
}

// Valence prior defined in terms of negative valence. 
// If the current state is terrible, it's extremely likely
// that the valence associated is negative. If it's ok, then 
// the valence could be negative or positive with equal 
// probability.
var valencePrior = function(state) {
  state === "so pretty" ? flip(0.99) ? 1 : -1 :
  state === "okay" ? flip(0.5) ? -1 : 1 :
  state === "so ugly" ? flip(0.01) ? 1 : -1 :
  true
}

// Define binary arousals (could model as continuous).
var arousals = ["low", "high"]

// Define goals and goal priors. Could want to communicate state' of the world,    
// valence about it, or arousal (intensity of feeling) about it.
var goals = ["goalState", "goalValence", "goalArousal"]

var goalPrior = function() {
  categorical([1, 1, 1], goals)
}

// Assume possible utterances are identical to possible states
// var utterances = states
var utterances = ["the dress is so pretty","the dress is so ugly","the dress is okay"]

// Assume cost of utterances is categorical.
var utterancePrior = function() {
  categorical({vs:utterances,
              ps: [0.2,0.2,0.2]})
}

// Sample arousal given a state.
var arousalPrior = function(state) {
  state === "so pretty" ? categorical([0.1, 0.9], arousals) :
  state === "okay" ? categorical([0.9, 0.1], arousals) :
  state === "so ugly" ? categorical([0.1, 0.9], arousals) :
  true
}

// Literal interpretation is just whether utterance equals state
var literalInterpretation = function(utterance, state) {
  utterance == "the dress is so pretty" ? state == "so pretty" :
  utterance == "the dress is okay" ? state == "okay":
  state == "so ugly"
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
    var state = statePrior()
    var valence = valencePrior(state)
    var arousal = arousalPrior(state)
    condition(literalInterpretation(utterance,state))
    return goalState(goal, state, valence, arousal)
  }})
}

// Define a speaker
var speaker1 = function(state, valence, arousal, goal) {
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

///

// Define a pragmatic listener
var pragmaticListener = function(utterance) {
  Infer({model: function(){
    var state = statePrior()
    var valence = valencePrior(state)
    var arousal = arousalPrior(state)
    var goal = goalPrior()
    observe(speaker1(state, valence, arousal, goal),utterance)
    return {state, valence, arousal}

  }})
}
~~~~

This is the speaker 2 we eventually decided to use, as this function takes more dependent variables and runs inference. As a result, it provides a more accurate outcome that fits better to our original expectations for a sarcasm model.

~~~~
///fold:

//We have three states the dress could be in:
// "so pretty", "okay", "so ugly"

var states = ['so pretty','okay','so ugly']

// the prior over these states

var statePrior = function() {
  categorical([1, 50, 50], states)
}

// Valence prior defined in terms of negative valence. 
// If the current state is terrible, it's extremely likely
// that the valence associated is negative. If it's ok, then 
// the valence could be negative or positive with equal 
// probability.
var valencePrior = function(state) {
  state === "so pretty" ? flip(0.99) ? 1 : -1 :
  state === "okay" ? flip(0.5) ? -1 : 1 :
  state === "so ugly" ? flip(0.01) ? 1 : -1 :
  true
}

// Define binary arousals (could model as continuous).
var arousals = ["low", "high"]

// Define goals and goal priors. Could want to communicate state' of the world,    
// valence about it, or arousal (intensity of feeling) about it.
var goals = ["goalState", "goalValence", "goalArousal"]

var goalPrior = function() {
  categorical([1, 1, 1], goals)
}

// Assume possible utterances are identical to possible states
// var utterances = states
var utterances = ["the dress is so pretty","the dress is so ugly","the dress is okay"]

// Assume cost of utterances is categorical.
var utterancePrior = function() {
  categorical({vs:utterances,
              ps: [0.2,0.2,0.2]})
}

// Sample arousal given a state.
var arousalPrior = function(state) {
  state === "so pretty" ? categorical([0.1, 0.9], arousals) :
  state === "okay" ? categorical([0.9, 0.1], arousals) :
  state === "so ugly" ? categorical([0.1, 0.9], arousals) :
  true
}

// Literal interpretation is just whether utterance equals state
var literalInterpretation = function(utterance, state) {
  utterance == "the dress is so pretty" ? state == "so pretty" :
  utterance == "the dress is okay" ? state == "okay":
  state == "so ugly"
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
    var state = statePrior()
    var valence = valencePrior(state)
    var arousal = arousalPrior(state)
    condition(literalInterpretation(utterance,state))
    return goalState(goal, state, valence, arousal)
  }})
}

// Define a speaker
var speaker1 = function(state, valence, arousal, goal) {
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
    observe(speaker1(state, valence, arousal, goal),utterance)
    return {state, valence, arousal}

  }})
}

///

var speaker2va = function(state,valence,arousal) {
  Infer({model: function(){
    var utterance = utterancePrior()
//     var goal = goalPrior()
    var L1 = pragmaticListener(utterance)

        factor(1 * pragmaticListener(utterance).score({ 
                                          state, 
                                          valence, 
                                         arousal}))
    return utterance
  }})
}
~~~~

After running speaker2va the speaker endorsed "the dress is so pretty" when the state itself was "so pretty". This matches our expectations of how the model would run based on the speaker with the state "so pretty". They feel positive about it and given a high arousal, they feel strongly positive and so the outcome has higher probability for "the dress is so pretty". 


~~~~
///fold:

//We have three states the dress could be in:
// "so pretty", "okay", "so ugly"

var states = ['so pretty','okay','so ugly']

// the prior over these states

var statePrior = function() {
  categorical([1, 50, 50], states)
}

// Valence prior defined in terms of negative valence. 
// If the current state is terrible, it's extremely likely
// that the valence associated is negative. If it's ok, then 
// the valence could be negative or positive with equal 
// probability.
var valencePrior = function(state) {
  state === "so pretty" ? flip(0.99) ? 1 : -1 :
  state === "okay" ? flip(0.5) ? -1 : 1 :
  state === "so ugly" ? flip(0.01) ? 1 : -1 :
  true
}

// Define binary arousals (could model as continuous).
var arousals = ["low", "high"]

// Define goals and goal priors. Could want to communicate state' of the world,    
// valence about it, or arousal (intensity of feeling) about it.
var goals = ["goalState", "goalValence", "goalArousal"]

var goalPrior = function() {
  categorical([1, 1, 1], goals)
}

// Assume possible utterances are identical to possible states
// var utterances = states
var utterances = ["the dress is so pretty","the dress is so ugly","the dress is okay"]

// Assume cost of utterances is categorical.
var utterancePrior = function() {
  categorical({vs:utterances,
              ps: [0.2,0.2,0.2]})
}

// Sample arousal given a state.
var arousalPrior = function(state) {
  state === "so pretty" ? categorical([0.1, 0.9], arousals) :
  state === "okay" ? categorical([0.9, 0.1], arousals) :
  state === "so ugly" ? categorical([0.1, 0.9], arousals) :
  true
}

// Literal interpretation is just whether utterance equals state
var literalInterpretation = function(utterance, state) {
  utterance == "the dress is so pretty" ? state == "so pretty" :
  utterance == "the dress is okay" ? state == "okay":
  state == "so ugly"
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
    var state = statePrior()
    var valence = valencePrior(state)
    var arousal = arousalPrior(state)
    condition(literalInterpretation(utterance,state))
    return goalState(goal, state, valence, arousal)
  }})
}

// Define a speaker
var speaker1 = function(state, valence, arousal, goal) {
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
    observe(speaker1(state, valence, arousal, goal),utterance)
    return {state, valence, arousal}
  }})
}

///

var speaker2va = function(state,valence,arousal) {
  Infer({model: function(){
    var utterance = utterancePrior()
//     var goal = goalPrior()
    var L1 = pragmaticListener(utterance)

        factor(1 * pragmaticListener(utterance).score({ 
                                          state, 
                                          valence, 
                                         arousal}))
    return utterance
  }})
}



viz(speaker2va("so pretty", 1, "high"))
~~~~

Now when we run the speaker2va with "so ugly", -1, high, there is higher probability for "the dress is so ugly" and this again matches our intuitions about the model because the speaker is in the state "so ugly" and they feel strongly negative about it, so they are more likely to say "the dress is so ugly". Typically when we utter something like "so ugly" we feel negative about it because the state itself is associated with negative connotation.

~~~~
///fold:

//We have three states the dress could be in:
// "so pretty", "okay", "so ugly"

var states = ['so pretty','okay','so ugly']

// the prior over these states

var statePrior = function() {
  categorical([1, 50, 50], states)
}

// Valence prior defined in terms of negative valence. 
// If the current state is terrible, it's extremely likely
// that the valence associated is negative. If it's ok, then 
// the valence could be negative or positive with equal 
// probability.
var valencePrior = function(state) {
  state === "so pretty" ? flip(0.99) ? 1 : -1 :
  state === "okay" ? flip(0.5) ? -1 : 1 :
  state === "so ugly" ? flip(0.01) ? 1 : -1 :
  true
}

// Define binary arousals (could model as continuous).
var arousals = ["low", "high"]

// Define goals and goal priors. Could want to communicate state' of the world,    
// valence about it, or arousal (intensity of feeling) about it.
var goals = ["goalState", "goalValence", "goalArousal"]

var goalPrior = function() {
  categorical([1, 1, 1], goals)
}

// Assume possible utterances are identical to possible states
// var utterances = states
var utterances = ["the dress is so pretty","the dress is so ugly","the dress is okay"]

// Assume cost of utterances is categorical.
var utterancePrior = function() {
  categorical({vs:utterances,
              ps: [0.2,0.2,0.2]})
}

// Sample arousal given a state.
var arousalPrior = function(state) {
  state === "so pretty" ? categorical([0.1, 0.9], arousals) :
  state === "okay" ? categorical([0.9, 0.1], arousals) :
  state === "so ugly" ? categorical([0.1, 0.9], arousals) :
  true
}

// Literal interpretation is just whether utterance equals state
var literalInterpretation = function(utterance, state) {
  utterance == "the dress is so pretty" ? state == "so pretty" :
  utterance == "the dress is okay" ? state == "okay":
  state == "so ugly"
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
    var state = statePrior()
    var valence = valencePrior(state)
    var arousal = arousalPrior(state)
    condition(literalInterpretation(utterance,state))
    return goalState(goal, state, valence, arousal)
  }})
}

// Define a speaker
var speaker1 = function(state, valence, arousal, goal) {
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
    observe(speaker1(state, valence, arousal, goal),utterance)
    return {state, valence, arousal}
//     return goalState(goal, state, valence,arousal)
  }})
}

///

var speaker2va = function(state,valence,arousal) {
  Infer({model: function(){
    var utterance = utterancePrior()
//     var goal = goalPrior()
    var L1 = pragmaticListener(utterance)
//     factor(1 * pragmaticListener(utterance).score(goalState(goal, 
//                                           state, 
//                                           valence, 
//                                          arousal )))
        factor(1 * pragmaticListener(utterance).score({ 
                                          state, 
                                          valence, 
                                         arousal}))
    return utterance
  }})
}


viz(speaker2va("so ugly", -1, "high"))
~~~~

So, now when we run "okay", 1, low the graph shows that there is higher probability for "the dress is okay". This is because the speaker doesn't feel strongly positive about the state, so then the interpretation of this results in the speaker saying "the dress is okay". 

~~~~
///fold:

//We have three states the dress could be in:
// "so pretty", "okay", "so ugly"

var states = ['so pretty','okay','so ugly']

// the prior over these states

var statePrior = function() {
  categorical([1, 50, 50], states)
}

// Valence prior defined in terms of negative valence. 
// If the current state is terrible, it's extremely likely
// that the valence associated is negative. If it's ok, then 
// the valence could be negative or positive with equal 
// probability.
var valencePrior = function(state) {
  state === "so pretty" ? flip(0.99) ? 1 : -1 :
  state === "okay" ? flip(0.5) ? -1 : 1 :
  state === "so ugly" ? flip(0.01) ? 1 : -1 :
  true
}

// Define binary arousals (could model as continuous).
var arousals = ["low", "high"]

// Define goals and goal priors. Could want to communicate state' of the world,    
// valence about it, or arousal (intensity of feeling) about it.
var goals = ["goalState", "goalValence", "goalArousal"]

var goalPrior = function() {
  categorical([1, 1, 1], goals)
}

// Assume possible utterances are identical to possible states
// var utterances = states
var utterances = ["the dress is so pretty","the dress is so ugly","the dress is okay"]

// Assume cost of utterances is categorical.
var utterancePrior = function() {
  categorical({vs:utterances,
              ps: [0.2,0.2,0.2]})
}

// Sample arousal given a state.
var arousalPrior = function(state) {
  state === "so pretty" ? categorical([0.1, 0.9], arousals) :
  state === "okay" ? categorical([0.9, 0.1], arousals) :
  state === "so ugly" ? categorical([0.1, 0.9], arousals) :
  true
}

// Literal interpretation is just whether utterance equals state
var literalInterpretation = function(utterance, state) {
  utterance == "the dress is so pretty" ? state == "so pretty" :
  utterance == "the dress is okay" ? state == "okay":
  state == "so ugly"
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
    var state = statePrior()
    var valence = valencePrior(state)
    var arousal = arousalPrior(state)
    condition(literalInterpretation(utterance,state))
    return goalState(goal, state, valence, arousal)
  }})
}

// Define a speaker
var speaker1 = function(state, valence, arousal, goal) {
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
    observe(speaker1(state, valence, arousal, goal),utterance)
    return {state, valence, arousal}
  }})
}

///

var speaker2va = function(state,valence,arousal) {
  Infer({model: function(){
    var utterance = utterancePrior()
//     var goal = goalPrior()
    var L1 = pragmaticListener(utterance)

        factor(1 * pragmaticListener(utterance).score({ 
                                          state, 
                                          valence, 
                                         arousal}))
    return utterance
  }})
}


viz(speaker2va("okay", 1, "low"))
~~~~

When the pragmatic listener hears "the dress is so pretty" by our expectations  of the sarcasm model it should generate the opposite meaning of "so pretty".  This is why the states for "okay" and "so ugly"  are higher. Valence is -1 and arousal is high correctly mapping to the negative feeling associated with the state "so ugly". 

~~~~
///fold: 

//We have three states the dress could be in:
// "so pretty", "okay", "so ugly"

var states = ['so pretty','okay','so ugly']

// the prior over these states

var statePrior = function() {
  categorical([1, 50, 50], states)
}

// Valence prior defined in terms of negative valence. 
// If the current state is terrible, it's extremely likely
// that the valence associated is negative. If it's ok, then 
// the valence could be negative or positive with equal 
// probability.
var valencePrior = function(state) {
  state === "so pretty" ? flip(0.99) ? 1 : -1 :
  state === "okay" ? flip(0.5) ? -1 : 1 :
  state === "so ugly" ? flip(0.01) ? 1 : -1 :
  true
}

// Define binary arousals (could model as continuous).
var arousals = ["low", "high"]

// Define goals and goal priors. Could want to communicate state' of the world,    
// valence about it, or arousal (intensity of feeling) about it.
var goals = ["goalState", "goalValence", "goalArousal"]

var goalPrior = function() {
  categorical([1, 1, 1], goals)
}

// Assume possible utterances are identical to possible states
// var utterances = states
var utterances = ["the dress is so pretty","the dress is so ugly","the dress is okay"]

// Assume cost of utterances is categorical.
var utterancePrior = function() {
  categorical({vs:utterances,
              ps: [0.2,0.2,0.2]})
}

// Sample arousal given a state.
var arousalPrior = function(state) {
  state === "so pretty" ? categorical([0.1, 0.9], arousals) :
  state === "okay" ? categorical([0.9, 0.1], arousals) :
  state === "so ugly" ? categorical([0.1, 0.9], arousals) :
  true
}

// Literal interpretation is just whether utterance equals state
var literalInterpretation = function(utterance, state) {
  utterance == "the dress is so pretty" ? state == "so pretty" :
  utterance == "the dress is okay" ? state == "okay":
  state == "so ugly"
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
    var state = statePrior()
    var valence = valencePrior(state)
    var arousal = arousalPrior(state)
    condition(literalInterpretation(utterance,state))
    return goalState(goal, state, valence, arousal)
  }})
}

// Define a speaker
var speaker1 = function(state, valence, arousal, goal) {
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
    observe(speaker1(state, valence, arousal, goal),utterance)
    return {state, valence, arousal}

  }})
}

///

var speaker2va = function(state,valence,arousal) {
  Infer({model: function(){
    var utterance = utterancePrior()
//     var goal = goalPrior()
    var L1 = pragmaticListener(utterance)

        factor(1 * pragmaticListener(utterance).score({ 
                                          state, 
                                          valence, 
                                         arousal}))
    return utterance
  }})
}


viz.marginals(pragmaticListener("the dress is so pretty"))
~~~~

When the pragmatic Listener hears the utterance "the dress is so ugly" the graphs return the state "so ugly" having the highest probability with negative valence and high arousal. In our state Prior the probability for "so pretty" is low compared to "okay" and "so ugly" and these have a probability of ( .50). This is why when the pragmatic Listener hears "the dress is so ugly" it is easier for them to interpret it literally as the dress is so ugly because it is costlier for the state "so pretty" and the speaker is lazy and "so ugly" is the winner here.

~~~~
///fold:

//We have three states the dress could be in:
// "so pretty", "okay", "so ugly"

var states = ['so pretty','okay','so ugly']

// the prior over these states

var statePrior = function() {
  categorical([1, 50, 50], states)
}

// Valence prior defined in terms of negative valence. 
// If the current state is terrible, it's extremely likely
// that the valence associated is negative. If it's ok, then 
// the valence could be negative or positive with equal 
// probability.
var valencePrior = function(state) {
  state === "so pretty" ? flip(0.99) ? 1 : -1 :
  state === "okay" ? flip(0.5) ? -1 : 1 :
  state === "so ugly" ? flip(0.01) ? 1 : -1 :
  true
}

// Define binary arousals (could model as continuous).
var arousals = ["low", "high"]

// Define goals and goal priors. Could want to communicate state' of the world,    
// valence about it, or arousal (intensity of feeling) about it.
var goals = ["goalState", "goalValence", "goalArousal"]

var goalPrior = function() {
  categorical([1, 1, 1], goals)
}

// Assume possible utterances are identical to possible states
// var utterances = states
var utterances = ["the dress is so pretty","the dress is so ugly","the dress is okay"]

// Assume cost of utterances is categorical.
var utterancePrior = function() {
  categorical({vs:utterances,
              ps: [0.2,0.2,0.2]})
}

// Sample arousal given a state.
var arousalPrior = function(state) {
  state === "so pretty" ? categorical([0.1, 0.9], arousals) :
  state === "okay" ? categorical([0.9, 0.1], arousals) :
  state === "so ugly" ? categorical([0.1, 0.9], arousals) :
  true
}

// Literal interpretation is just whether utterance equals state
var literalInterpretation = function(utterance, state) {
  utterance == "the dress is so pretty" ? state == "so pretty" :
  utterance == "the dress is okay" ? state == "okay":
  state == "so ugly"
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
    var state = statePrior()
    var valence = valencePrior(state)
    var arousal = arousalPrior(state)
    condition(literalInterpretation(utterance,state))
    return goalState(goal, state, valence, arousal)
  }})
}

// Define a speaker
var speaker1 = function(state, valence, arousal, goal) {
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
    observe(speaker1(state, valence, arousal, goal),utterance)
    return {state, valence, arousal}

  }})
}

///

var speaker2va = function(state,valence,arousal) {
  Infer({model: function(){
    var utterance = utterancePrior()
//     var goal = goalPrior()
    var L1 = pragmaticListener(utterance)

        factor(1 * pragmaticListener(utterance).score({ 
                                          state, 
                                          valence, 
                                         arousal}))
    return utterance
  }})
}


viz.marginals(pragmaticListener("the dress is so ugly"))
~~~~

Now when the pragmatic Listener hears "the dress is okay" then there is a greater chance that the model thinks the dress is okay. Our interpretation of the sarcasm model fits in this because there is some probability for so ugly and this accounts for irony in the utterance. The pragmatic Listener hears "the dress is okay" and they are likely to not feel strongly negative about it. 

~~~~
///fold:

//We have three states the dress could be in:
// "so pretty", "okay", "so ugly"

var states = ['so pretty','okay','so ugly']

// the prior over these states

var statePrior = function() {
  categorical([1, 50, 50], states)
}

// Valence prior defined in terms of negative valence. 
// If the current state is terrible, it's extremely likely
// that the valence associated is negative. If it's ok, then 
// the valence could be negative or positive with equal 
// probability.
var valencePrior = function(state) {
  state === "so pretty" ? flip(0.99) ? 1 : -1 :
  state === "okay" ? flip(0.5) ? -1 : 1 :
  state === "so ugly" ? flip(0.01) ? 1 : -1 :
  true
}

// Define binary arousals (could model as continuous).
var arousals = ["low", "high"]

// Define goals and goal priors. Could want to communicate state' of the world,    
// valence about it, or arousal (intensity of feeling) about it.
var goals = ["goalState", "goalValence", "goalArousal"]

var goalPrior = function() {
  categorical([1, 1, 1], goals)
}

// Assume possible utterances are identical to possible states
// var utterances = states
var utterances = ["the dress is so pretty","the dress is so ugly","the dress is okay"]

// Assume cost of utterances is categorical.
var utterancePrior = function() {
  categorical({vs:utterances,
              ps: [0.2,0.2,0.2]})
}

// Sample arousal given a state.
var arousalPrior = function(state) {
  state === "so pretty" ? categorical([0.1, 0.9], arousals) :
  state === "okay" ? categorical([0.9, 0.1], arousals) :
  state === "so ugly" ? categorical([0.1, 0.9], arousals) :
  true
}

// Literal interpretation is just whether utterance equals state
var literalInterpretation = function(utterance, state) {
  utterance == "the dress is so pretty" ? state == "so pretty" :
  utterance == "the dress is okay" ? state == "okay":
  state == "so ugly"
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
    var state = statePrior()
    var valence = valencePrior(state)
    var arousal = arousalPrior(state)
    condition(literalInterpretation(utterance,state))
    return goalState(goal, state, valence, arousal)
  }})
}

// Define a speaker
var speaker1 = function(state, valence, arousal, goal) {
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
    observe(speaker1(state, valence, arousal, goal),utterance)
    return {state, valence, arousal}

  }})
}

///

var speaker2va = function(state,valence,arousal) {
  Infer({model: function(){
    var utterance = utterancePrior()
//     var goal = goalPrior()
    var L1 = pragmaticListener(utterance)

        factor(1 * pragmaticListener(utterance).score({ 
                                          state, 
                                          valence, 
                                         arousal}))
    return utterance
  }})
}


viz.marginals(pragmaticListener("the dress is okay"))
~~~~

Similarities:
The similarities in this model compared to the irony model are pretty identical. We borrowed everything from state up until the pragmatic listener. Of course, we adjusted the model to fit our sarcasm model and changed the states/ utterances accordingly. 

Differences:
Although this model is similar to the Irony model to a large extent, we were working on adding a second speaker layer; so that the result combines the 3 dependent variable (state, valence, and arousal) from the pragmatic listener, and thus creating a result that better fits to our expectation. 


The sarcasm model works better for "the dress is so pretty" because the state it captures are "okay" and "ugly"---ugly being the opposite of pretty---as predicted in the previous studies by Kao and Goodman. However, for the other utterances, our results showed that the pragmatic listener can interpret the utterances as literal for "the dress is okay", but still have some probability for the state so ugly. This small probability is what captures the sarcasm in our model. Similarly, "the dress is so ugly" is interpreted literally with a small probability of "okay". The probability for "okay" can be thought of as the pragmatic listener interpreting the meaning of the speaker's utterance as so. 

The original Irony model yielded similar results. It captured Irony perfectly when the pragmatic listener heard "terrible" (the state of the weather), but when it came to the other states, "amazing" and "ok" the pragmatic listener interpreted them as literal. There is some irony where probabilities for other states are small, but it seems there is still work to be done to capture the complete opposite meaning when given "amazing".