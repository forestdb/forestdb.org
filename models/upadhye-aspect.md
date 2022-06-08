---
layout: model
title: Upadhye - Aspect
model-language: webppl
---

## An RSA model of event interpretation

*Shiva Upadhye, University of California, Irvine*

The following utterances describe how Tom, a fastidious cat with a passion for cleaning, has been spending his waking hours. What might be a listener's expectations regarding the events described in these utterances?

a. Tom has been cleaning the attic for two minutes

b. Tom has been cleaning the attic for two years

In case of (a), we are more likely to infer that the utterance describes an event that is ongoing with respect to the reference time. In case of (b), however, we find that our expectations shift as we are now more likely to consider that Tom cleaned the attic regularly or multiple times over the course of two years. Adapting terminology from Carlson (2005), (a) describes an *episodic singular* event while (b) describes a *habitual* or *generic* event. 

Comparing these utterances, one might argue that the reason for (a) being interpreted as an episodic event is that two minutes is too short a timeframe to be able to complete cleaning a decently-sized attic. A two year timeframe, on the other hand, seems to be long enough to justify a habitual reading of the sentence. However, taking a look at (c) may complicate this reasoning:

c. Tom has been cleaning the ocean for two years 

In this case, we are more likely to interpret the event described in (c) as episodic rather than habitual. Perhaps, the difference in interpretation is merely a function of the size of the domain? One might argue, for instance, that the ocean is significantly larger than an attic, hence it is unlikely to be cleaned in two years. Interestingly, while there might be consensus about the fact that an event like cleaning the ocean will be characterized by an incomprehensibly long duration, there is implicit uncertainty about what that culmination point would look like. Furthermore, we may not often have access to the actual size of the domain in question. For instance, while we may be able to estimate the time needed to "drain a bathtub" based on experience, we cannot estimate how long it may take to "drain the (political) swamp" or "clean a company's codebase" since there is tremendous uncertainty about the scope of these events. 

#### The Rational Speech Act Framework

What is evident from the above utterances is that compositional meaning alone is insufficient to determine whether an utterance is describing an episodic or habitual event. Rather the interpretation is driven by the semantics of the verbal predicate and the interlocutors' expectations about the domain. 

The Rational Speech Act (RSA) theory (Frank and Goodman, 2012) provides a conducive probabilistic framework for dealing with the uncertainty that characterizes how speakers and listeners reason over, and communicate about, utterances describing events in the world. In the context of this framework, we have two cooperative agents, namely a speaker who observes an event and wishes to communicate about it through an utterance and a listener who attempts to decode the state of the world given the utterance produced by the speaker. In order to maximize their communicative objectives, the speaker reasons over the literal interpretations of the utterances to determine the most optimal utterance; the pragmatic listener, on the other hand, reasons over the speaker's beliefs and production process to determine the state that is most compatible with the utterance.

In the context of event interpretation, the pragmatic listener not only reasons about the state describing the event being observed and communicated by the speaker, but also attempts to infer whether the event described by the utterance is episodic or habitual.

#### Modeling the state space

We define a state, *s = (N, D)* where *N* is the number of events and *D* is the average duration of the event. As noted above, beliefs about event duration appear to vary significantly for each object given a specific timeframe; hence, we define an object-specific *Beta* distribution from which to sample the event durations. Similar to the *prevalence* parameter in the Generic language interpretation model in Chapter 7 (Tessler and Goodman, 2016) that defined the *prevalence* of a feature in a particular category of objects, we fix a lower *alpha* value to model objects with a shorter duration (e.g., *table* or *attic*) and higher *alpha* values for those with longer durations. The *beta* parameter, which controls the variability of the distribution, was initially set to 5. The probabilities generated from this distribution constitute the probability vector for a categorical distribution, which also takes a set of possible event durations, D = {1,2,3,4,5,6,7,8,9,10} as argument. 

We also model the number of events, *N*, using a similar approach. Crucially, however, we bake into this distribution the assumption that both episodic (N=1) and habitual (N > 1) events are equally likely. For habitual events, we expect event duration to be inversely related to the number of events; consequently, we use the complement of the *alpha* parameter to define a *Beta* distribution from which to sample the number of habitual events. We also define a *Beta* distribution that assigns high probability mass to a single event. In case of the first distribution, *beta* was initially set to 5 whilst in case of the second one, the *beta* parameter was assigned a higher value of 10 to concentrate the probability mass at one event. Similar to the generic language interpretation model, we also define a mixture model over both these distributions. Sampling from either distribution is controlled by a *phi* parameter, which we set to *0.5* to render both types of events equally likely.



~~~~
// helper function for discrete Beta distribution
var bins = _.range(0.1, 1.1, 0.1);
var DiscreteBetaProbs = cache(function(g, d){
  var a =  g * d, b = (1-g) * d;
  var betaPDF = function(x){
    return Math.pow(x, a-1)*Math.pow((1-x), b-1)
  }
  var probs = map(betaPDF, bins);
  return probs
})

// state distribution
var phi = 0.5
var priorModel = function(object) {
  var alpha = params[object][0]
  var beta = params[object][1]
  var gamma = params[object][2]
  // duration model
  var durationProbabilities = DiscreteBetaProbs(alpha,beta)
  var durations = _.range(1,11,1)
  // event model
  var eventProbabilities = flip(phi) ? 
      DiscreteBetaProbs(1-alpha,beta) : DiscreteBetaProbs(0.1,gamma)
  var numEvents = _.range(1,11,1)
  return {durPs:durationProbabilities,durations:durations,
          eventPs:eventProbabilities,numEvents:numEvents}
}

// state prior
var statePrior = function(object) {
  var objectPriors = priorModel(object)
  var eventDuration = categorical({ps:objectPriors.durPs,vs:objectPriors.durations})
  var numEvents = categorical({ps:objectPriors.eventPs,vs:objectPriors.numEvents})
  return {numEvents: numEvents,eventDuration: eventDuration}
}


var objects = ["table","attic","ocean","code"]
var params = {table:[0.1,7,10],attic:[0.25,7,10],
              ocean:[0.85,7,10],code:[0.5,7,10]}

var object = "attic"
Infer({model: function() {
  return statePrior(object)
}})
~~~~

The model reasons over present perfect progressive utterances of the form, "Tom has been cleaning the **[object]** for **[timeframe]**," where **object** and **timeframe** are provided as arguments. In addition to timeframes sampled uniformly from the set, *T = {t1,t2,t3,t4,t5,t6,t7,t8,t9,t10}*, we also include an utterance with a "null" timeframe, which can be interpreted as an utterance that lacks a duration adverbial i.e., "Tom has been cleaning the **[object]**."

Similar to the scope ambiguity model in Chapter 4, which involved jointly inferring the state as well as the scope interpretation, the current model also performs joint inference over the state, 
*s =  (N,D)* and the event interpretations, namely the *episodic* and *habitual* readings of the utterance. Both event interpretations are equally likely a priori.

~~~~
var objects = ["table","attic","ocean","code"]
var params = {table:[0.1,7,10],attic:[0.25,7,10],
              ocean:[0.85,7,10],code:[0.5,7,10]}
  
// Utterance prior
var utterancePrior = function(object) {
  var timeframe = uniformDraw(_.range(1,11,1))
  return flip (0.9) ? 
    {object:object,timeframe:timeframe}: {object:object,timeframe:"null"}
}

// Event type prior
var eventTypes = ["episodic","habitual"]
var eventTypePrior = function(){
  return uniformDraw(eventTypes)
}

var object = "attic"
Infer({model: function() {
  return utterancePrior(object)
}})
~~~~

#### From intuitions about event semantics to the meaning function
Intuitively, we consider an event to be ongoing or episodic when its duration either encompasses or exceeds a fixed timeframe. In contrast, we think of events being habitual when we can perceive that multiple events may have culminated within the timeframe. Given a timeframe *T* and a state, *s = (N,D)*, an event will be episodic iff *N = 1*, and *T <= N x D.* Since it is possible to have multiple events whose cumulative duration is less than the timeframe, we consider an event to be habitual if *N > 1*. We make some simplifying assumptions here: 
* In case of multiple events, we assume that all *N* events have the same duration, *D*
* We assume that all events are bound to culminate 

While the first assumption may be considered simplistic, we may interpret *D* as being the average duration of a specific type of event (e.g., cleaning an attic). The second assumption arises due to the telicity of the verbal predicate (Verkuyl, 1989). While it is possible to interrupt a telic event (Landman, 1992), we assume that a new event cannot begin until the previous one has reached the point of culmination. 

These semantic intuitions are encoded in the meaning function below:


~~~~
// meaning function
// meaning function
var meaning = function(utterance,eventType,state) {
  if (utterance.timeframe != "null") {
  var totalDuration = state.numEvents * state.eventDuration
  var length = utterance.timeframe <= totalDuration
  var number = eventType == "episodic" ? state.numEvents == 1 : state.numEvents > 1
  return length && number
  }
  else {
    return true
  }
}
~~~~

#### Literal Listener

Similar to the literal listener from scope ambiguity model, the literal listener in this model also reasons about state *s = (N,D)* by performing Bayesian inference over the literal meaning of the utterance and the event interpretation, weighted by the listener's prior beliefs about the state. However, contrary to the scope ambiguity model, which used uniform priors, the literal listener reasons over an informative prior.


~~~~
// helper function for discrete Beta distribution
var bins = _.range(0.1, 1.1, 0.1);
var DiscreteBetaProbs = cache(function(g, d){
  var a =  g * d, b = (1-g) * d;
  var betaPDF = function(x){
    return Math.pow(x, a-1)*Math.pow((1-x), b-1)
  }
  var probs = map(betaPDF, bins);
  return probs
})

// state distribution
var phi = 0.5
var priorModel = function(object) {
  var alpha = params[object][0]
  var beta = params[object][1]
  var gamma = params[object][2]
  // duration model
  var durationProbabilities = DiscreteBetaProbs(alpha,beta)
  var durations = _.range(1,11,1)
  // event model
  var eventProbabilities = flip(phi) ? 
      DiscreteBetaProbs(1-alpha,beta) : DiscreteBetaProbs(0.1,gamma)
  var numEvents = _.range(1,11,1)
  return {durPs:durationProbabilities,durations:durations,
          eventPs:eventProbabilities,numEvents:numEvents}
}

// state prior
var statePrior = function(object) {
  var objectPriors = priorModel(object)
  var eventDuration = categorical({ps:objectPriors.durPs,vs:objectPriors.durations})
  var numEvents = categorical({ps:objectPriors.eventPs,vs:objectPriors.numEvents})
  return {numEvents: numEvents,eventDuration: eventDuration}
}


var objects = ["table","attic","ocean","code"]
var params = {table:[0.1,5,10],attic:[0.25,5,10],
              ocean:[0.85,5,10],code:[0.5,5,10]}

// Utterance prior
var utterancePrior = function(object) {
  var timeframe = uniformDraw(_.range(1,11,1))
  return flip (0.95) ? 
    {object:object,timeframe:timeframe}: {object:object,timeframe:"null"}
}

// Event type prior
var eventTypes = ["episodic","habitual"]
var eventTypePrior = function(){
  return uniformDraw(eventTypes)
}

// meaning function
var meaning = function(utterance,eventType,state) {
  if (utterance.timeframe != "null") {
  var totalDuration = state.numEvents * state.eventDuration
  var length = utterance.timeframe <= totalDuration
  var number = eventType == "episodic" ? state.numEvents == 1 : state.numEvents > 1
  return length && number
  }
  else {
    return true
  }
}

// Literal Listener
var literalListener = function(utterance,eventType) {
  return Infer({model: function(){
  var object = utterance.object
  var state = statePrior(object)
  var m = meaning(utterance,eventType,state)
  condition(m)
  return state
  }})}

var object = "attic"
//var object = "ocean"
var uttr = utterancePrior(object)
var eventType = "habitual"
//var eventType = "episodic"
display("utterance object: " + object)
display("utterance timeframe: " + uttr.timeframe)
display("event type: " + eventType)
display("Literal Listener's posterior:")
literalListener(uttr,eventType)
~~~~

**Habitual interpretation**

For the habitual interpretation, we observe that when the literal listener hears an utterance with a longer timeframe, they assign greater probability to the region of the distribution with *N > 1* for both attic and ocean respectively. In case of *ocean*, we observe that a very low number of events with the highest possible duration are more likely regardless of the timeframe. In contrast, with *attic* we find that there is a tradeoff between number of events and event duration, which is based on the timeframe.

**Episodic interpretation**

In case of an episodic interpretation, we observe that the listener assigns highest probability to event durations that are closest to the timeframe in the utterance for both *attic* and *ocean*.




#### Speaker

The speaker in this model is also adapted from the speaker from the scope ambiguity model. As the agent that observes the event, the speaker has access to the state and the event interpretation. She then samples a present-perfect continuous utterance with a timeframe *T* with a probability of 0.95 or a "null" timeframe with a probability of 0.05. The speaker simulates a literal listener to determine the utterance that minimizes the cost and conveys the state optimally. Considering situations where the state, timeframe, and event interpretation may be incompatible with each other (e.g., *D* = 1, *N = 10*, *T=1*, and event interpretation is episodic), we add an additional unit of cost for producing non-null utterances, hence making it possible for the speaker to omit the timeframe by using "null" whenever appropriate. The speaker's behavior is also controlled by an optimality parameter, which determines how rationally the speaker chooses utterances to describe states (default *alpha = 1*)

~~~~
// helper function for discrete Beta distribution
var bins = _.range(0.1, 1.1, 0.1);
var DiscreteBetaProbs = cache(function(g, d){
  var a =  g * d, b = (1-g) * d;
  var betaPDF = function(x){
    return Math.pow(x, a-1)*Math.pow((1-x), b-1)
  }
  var probs = map(betaPDF, bins);
  return probs
})

// state distribution
var phi = 0.2
var priorModel = function(object) {
  var alpha = params[object][0]
  var beta = params[object][1]
  var gamma = params[object][2]
  // duration model
  var durationProbabilities = DiscreteBetaProbs(alpha,beta)
  var durations = _.range(1,11,1)
  // event model
  var eventProbabilities = flip(phi) ? 
      DiscreteBetaProbs(1-alpha,beta) : DiscreteBetaProbs(0.1,gamma)
  var numEvents = _.range(1,11,1)
  return {durPs:durationProbabilities,durations:durations,
          eventPs:eventProbabilities,numEvents:numEvents}
}

// state prior
var statePrior = function(object) {
  var objectPriors = priorModel(object)
  var eventDuration = categorical({ps:objectPriors.durPs,vs:objectPriors.durations})
  var numEvents = categorical({ps:objectPriors.eventPs,vs:objectPriors.numEvents})
  return {numEvents: numEvents,eventDuration: eventDuration}
}


var objects = ["table","attic","ocean","code"]
var params = {table:[0.1,5,10],attic:[0.25,5,10],
              ocean:[0.85,5,10],code:[0.5,5,10]}
  
// Utterance prior
var utterancePrior = function(object) {
  var timeframe = uniformDraw(_.range(1,11,1))
  return flip (0.95) ? 
    {object:object,timeframe:timeframe}: {object:object,timeframe:"null"}
}

// Event type prior
var eventTypes = ["episodic","habitual"]
var eventTypePrior = function(){
  return uniformDraw(eventTypes)
}

// meaning function
var meaning = function(utterance,eventType,state) {
  if (utterance.timeframe != "null") {
  var totalDuration = state.numEvents * state.eventDuration
  var length = utterance.timeframe <= totalDuration
  var number = eventType == "episodic" ? state.numEvents == 1 : state.numEvents > 1
  return length && number
  }
  else {
    return true
  }
}

// Literal Listener
var literalListener = function(utterance,eventType) {
  return Infer({model: function(){
  var object = utterance.object
  var state = statePrior(object)
  var m = meaning(utterance,eventType,state)
  condition(m)
  return state
  }})}

// Speaker
var alpha = 1
var cost = function(utterance){
  return utterance.timeframe == "null"? 1 : 2
}

var speaker = function(object,eventType,state) {
  return Infer({model: function(){
    var utterance = utterancePrior(object)
    var L = literalListener(utterance,eventType)
    factor(alpha * L.score(state) - cost(utterance))
    return utterance
  }})}

var object = "ocean"
var eventType = "episodic"
var state = {numEvents:1,eventDuration:2}
display("object: " + object)
display("event type: " + eventType)
display("num events (state): " + state.numEvents)
display("event duration (state): " + state.eventDuration)
speaker(object,eventType,state)
~~~~

In case of shorter duration domains like *attic*, we observe that the speaker assigns progressively higher probability to timeframes that are nearest in value to the cumulative duration (i.e., *N x D*). Hence, if the speaker were at liberty to choose a timeframe to describe the process of cleaning an attic using a present perfect progressive construction, she would be more likely to choose utterances where the timeframes are closest to the cumulative duration. In case of a longer duration domains like *ocean*, however, we observe that all timeframes that are less than or equal to the cumulative duration are equally probable. Hence, the speaker appears to have no preference for the timeframe to describe the cleaning of the ocean, so long as the timeframe does not exceed the cumulative duration.

We also observe that the "null" timeframe is assigned some probability mass when the speaker is compelled to produce the state is not compatible with the timeframe or the interpretation. 

#### Pragmatic Listener

Similar to the pragmatic listener from the scope ambiguity model, the pragmatic listener defined below infers the state (i.e., number of events and event duration) as well as the interpretation (episodic vs. habitual) by reasoning about the utterance. In particular, the listener attempts to infer the state and event interpretation most compatible with the utterance by reasoning over the speaker's beliefs (and by extension, the literal listener's beliefs).



~~~~
// helper function for discrete Beta distribution
var bins = _.range(0.1, 1.1, 0.1);
var DiscreteBetaProbs = cache(function(g, d){
  var a =  g * d, b = (1-g) * d;
  var betaPDF = function(x){
    return Math.pow(x, a-1)*Math.pow((1-x), b-1)
  }
  var probs = map(betaPDF, bins);
  return probs
})

// state distribution
var phi = 0.2
var priorModel = function(object) {
  var alpha = params[object][0]
  var beta = params[object][1]
  var gamma = params[object][2]
  // duration model
  var durationProbabilities = DiscreteBetaProbs(alpha,beta)
  var durations = _.range(1,11,1)
  // event model
  var eventProbabilities = flip(phi) ? 
      DiscreteBetaProbs(1-alpha,beta) : DiscreteBetaProbs(0.1,gamma)
  var numEvents = _.range(1,11,1)
  return {durPs:durationProbabilities,durations:durations,
          eventPs:eventProbabilities,numEvents:numEvents}
}

// state prior
var statePrior = function(object) {
  var objectPriors = priorModel(object)
  var eventDuration = categorical({ps:objectPriors.durPs,vs:objectPriors.durations})
  var numEvents = categorical({ps:objectPriors.eventPs,vs:objectPriors.numEvents})
  return {numEvents: numEvents,eventDuration: eventDuration}
}

var objects = ["table","attic","ocean","code"]
var params = {table:[0.1,7,20],attic:[0.25,7,20],
              ocean:[0.85,7,20],code:[0.5,7,20]}
  
// Utterance prior
var utterancePrior = function(object) {
  var timeframe = uniformDraw(_.range(1,11,1))
  return flip (0.95) ? 
    {object:object,timeframe:timeframe}: {object:object,timeframe:"null"}
}

// Event type prior
var eventTypes = ["episodic","habitual"]
var eventTypePrior = function(){
  return uniformDraw(eventTypes)
}
                     
// meaning function
var meaning = function(utterance,eventType,state) {
  if (utterance.timeframe != "null") {
  var totalDuration = state.numEvents * state.eventDuration
  var length = utterance.timeframe <= totalDuration
  var number = eventType == "episodic" ? state.numEvents == 1 : state.numEvents > 1
  return length && number
  }
  else {
    return true
  }
}

// Literal Listener
var literalListener = function(utterance,eventType) {
  return Infer({model: function(){
  var object = utterance.object
  var state = statePrior(object)
  var m = meaning(utterance,eventType,state)
  condition(m)
  return state
  }})}

// Speaker
var alpha = 1
var cost = function(utterance){
  return utterance.timeframe == "null"? 2 : 1
}

var speaker = function(object,eventType,state) {
  return Infer({model: function(){
    var utterance = utterancePrior(object)
    var L = literalListener(utterance,eventType)
    factor(alpha * L.score(state) - cost(utterance))
    return utterance
  }})}

// Pragmatic listener
var pragmaticListener = function(utterance){
  return Infer({model: function(){
    var object = utterance.object
    var eventType = eventTypePrior()
    var state = statePrior(object)
    observe(speaker(object,eventType,state),utterance)
    return {state: state,
            eventType: eventType}
  }})}

var object = "ocean"
//var object = "attic"
var timeframe = 10
var uttr = {object:object,timeframe:timeframe}
display("utterance timeframe: " + uttr.timeframe)
marginalize(pragmaticListener(uttr),"eventType")
~~~~

#### Predictions

A grid search for optimal state distribution parameters found that *beta = 7* and *gamma = 20* (i.e., the concentration parameters in the mixture model) produced the most reasonable predictions. Below we report the predictions for specific timeframes, ranging from the shortest to the longest:

**T=1**

We observe that the episodic interpretation is assigned higher probability than the habitual interpretation for both *attic* and *ocean* 

**T=4**

As we increase the timeframe, we observe that the habitual interpretation is assigned higher probability for *attic* whereas the episodic interpretation is assigned higher probability for *ocean*. We also observe that the posterior distributions for *ocean* when *T=1* and *T=4* appear very similar, which suggests little change in terms of beliefs about episodic vs. habitual events.

**T=8**

As we further increase the length of the timeframe to *T=8*, we observe that the probability assigned to the episodic interpretation for *attic* falls significantly, with most probability mass assigned to the habitual interpretation. In contrast, for *ocean*, we observe that the probability of the episodic interpretation is slightly lowered compared to *T=4* but still higher than the habitual interpretation.

**T=10**

As we increase the length to the longest possible timeframe, we observe that the probability assigned to the habitual interpretation is close to 1 for *attic*. In case of *ocean*, we find that the probability of the episodic interpretation decreases quite noticeably whilst still being higher than the habitual interpretation.




#### Discussion and Conclusion

Broadly speaking, we find that the pragmatic listener is more likely to interpret an utterance as being habitual if (i) the event is characterized by a shorter duration and (ii) if the timeframe in the utterance is longer. Whereas these observations align with our intuitions, the model does not exhibit the expected gradual shifts in the probabilities assigned to each of the interpretations for different objects and timeframes. 

We also observe that the model's predictions are sensitive to the distribution parameters, which were defined based on intuitions.  In addition to the *beta* and *gamma* parameters, we find that decreasing *phi*, which has the effect of decreasing the chances of sampling from the "many" distribution in the mixture model, pulls away probability from the habitual interpretation, hence making shifts more gradual. However, we still observe that for short duration domains like *attic*, the probability assigned to the habitual interpretation for the longest possible timeframe is still close to 1. 

Considering these limitations, an important next step would be the empirical estimation of the *alpha*, *beta*, *gamma*, and *phi* parameters used to characterize the state distribution. Additionally, a modification that I am currently working (based on a suggestion during presentation) is implementing a threshold semantics for habituals in the meaning function.

#### References

Carlson, G. N. (2005). Generics, habituals and iteratives.

Frank, M. C., & Goodman, N. D. (2012). Predicting pragmatic reasoning in language games. Science, 336(6084), 998-998.

Tessler, M. H., & Goodman, N. D. (2016). A pragmatic theory of generic language. arXiv preprint arXiv:1608.02926.

Landman, F. 1992. “The Progressive.” Linguistics and Philosophy, 1: 1–32.

Verkuyl, H. J. (1989). Aspectual Classes and Aspectual Composition. Linguistics and Philosophy, 12(1), 39–94. http://www.jstor.org/stable/25001331