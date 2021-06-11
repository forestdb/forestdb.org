---
layout: model
title: Dickson Speaker cost
model-language: webppl
---

Every time we decide to speak, we exert effort to convey information or engage in social behavior. We can call this effort exerted the cost of the utterance. Intuitively, we know that every utterance is spoken with some cost, but how does cost influence our reasoning about the utterances of our conversational partner? How might we reason about the cost of the utterances we hear? In this Rational Speech Act (RSA) model, I explore how a listener might reason about the cost function that the speaker is using.

We begin with an introduction to the RSA framework. The RSA framework posits that when comprehending utterances from a conversational partner, a listener performs a recursive reasoning process. This pragmatic listener reasons about a cooperative speaker who is trying to communicate a state of the world to a literal listener. This recursive reasoning relies on Bayesian inference which ultimately allows the pragmatic listener to reason about the state of the world based on an utterance. We will first set up the world of our model, then explain the behavior of the three main layers of our model (literal listener, pragmatic speaker, and pragmatic listener).

To first explore our question of reasoning about cost in our RSA framework, we will assume the world of the vanilla RSA model: the listener and speaker are only communicating about three objects.

~~~~
// set of states
var objects = [{color: "blue", shape: "square", string: "blue square"},
               {color: "blue", shape: "circle", string: "blue circle"},
               {color: "green", shape: "square", string: "green square"}]
~~~~

In this reference game, the speaker will choose an utterance to communicate which of the objects they are referring to. We will then need to specify the possible utterances the speaker chooses from.

~~~~
// set of utterances
var utterances = ["blue", "green", "square", "circle",
                  "blue square", "blue circle", "green square"]
~~~~

In the original vanilla model, the speaker chose from the four one-word utterances. The cost of these one-word utterances might vary slightly based on how frequently the speaker uses them or the length, but to better explore reasoning about a cost function, I included the highly informative two-word utterances. We can reasonably assume that these two-word utterances are more costly than the one-word utterances, but the speaker will still face some pressure to use these costlier utterances because using them would guarantee that a literal listener would arrive at the intended object. We can contrast the two-word informativity with the one-word informativity as many of the one-word utterances can reference multiple objects leaving a listener uncertain about the intended object.

Now that we have established our world of objects and possible utterances, we need to add one more piece before we introduce the three main layers. We need to specify a cost function that a pragmatic listener can ultimately reason about. We established that the two-word utterances will be more costly than the one-word utterances, but we do not know the degree of this difference. We can implement a cost function that takes in an utterance and a cost parameter such that the pragmatic listener reasons about the cost parameter to figure out the degree of the difference.

~~~~
// utterance cost function
var cost = function(utterance, costParameter) {
  var numWords = function(utterance) {
    var split = utterance.split(" ")
    return _.size(split)
  }
  return costParameter*numWords(utterance)
}
~~~~

In this cost function, we are simply counting the number of words in the utterances (with the numWords function) and returning that number multiplied by the costParameter. As the costParameter approaches 0, the difference between the cost of the one-word utterance and the cost of the two-word utterances becomes very small. This small difference between utterances means that the speaker has a similar access to both utterance groups. As the costParameter increases, the difference between the cost of the one-word utterance and the cost of the two-word utterance becomes more prominent such that the two-word utterances become more and more inaccessible to the speaker.

This additional parameter is similar to the addition of the phi parameter from the politeness model. The phi parameter from the politeness model controls how much the speaker chooses utterances based on informativity or based on social considerations. Then, when the pragmatic listener reasons about the phi value, they are reasoning about the nature of the speaker (how much weight this speaker places on social considerations). Similarly, in the current model, the pragmatic listener reasons about the cost parameter that the speaker is using and the difference between the cost of the one-word and two-word utterances.

We now move on to the three main layers of the model. We start with the literal listener who has some prior beliefs about the objects in our world, and then they update these beliefs based on the literal semantics of the utterance they hear.

~~~~
// set of states
var objects = [{color: "blue", shape: "square", string: "blue square"},
               {color: "blue", shape: "circle", string: "blue circle"},
               {color: "green", shape: "square", string: "green square"}]

// set of utterances
var utterances = ["blue", "green", "square", "circle",
                  "blue square", "blue circle", "green square"]

// prior over world states
var objectPrior = function() {
  var obj = uniformDraw(objects)
  return obj.string
}

// meaning function to interpret the utterances
var meaning = function(utterance, obj){
  _.includes(obj, utterance)
}

// literal listener
var literalListener = function(utterance){
  Infer({model: function(){
    var obj = objectPrior();
    condition(meaning(utterance, obj))
    return obj
  }})
}

literalListener("blue")
~~~~

We don't have any reason to believe that one of the objects is more likely to be referenced than another, so the prior belief specifies that the objects are equally likely. The meaning function simply checks whether an utterance can literally refer to an object. Finally, our literal listener hears an utterance and returns a distribution of utterances that the utterance can literally refer to using Bayesian inference (implemented with the Infer function).

Next, we introduce our pragmatic speaker. The pragmatic speaker knows the object they want to communicate and the cost parameter that they are using, and they return a distribution of utterances proportional to the probability that the literal listener would arrive at the intended object after hearing the utterance.

~~~~
// set of states
var objects = [{color: "blue", shape: "square", string: "blue square"},
               {color: "blue", shape: "circle", string: "blue circle"},
               {color: "green", shape: "square", string: "green square"}]

// prior over world states
var objectPrior = function() {
  var obj = uniformDraw(objects)
  return obj.string
}

// set of utterances
var utterances = ["blue", "green", "square", "circle",
                  "blue square", "blue circle", "green square"]

// utterance cost function
var cost = function(utterance, costParameter) {
  var numWords = function(utterance) {
    var split = utterance.split(" ")
    return _.size(split)
  }
  return costParameter*numWords(utterance)
};


// meaning function to interpret the utterances
var meaning = function(utterance, obj){
  _.includes(obj, utterance)
}

// literal listener
var literalListener = function(utterance){
  Infer({model: function(){
    var obj = objectPrior();
    condition(meaning(utterance, obj))
    return obj
  }})
}

// set speaker optimality
var alpha = 1

// pragmatic speaker
var speaker = function(obj,costParameter){
  Infer({model: function(){
    var utterance = uniformDraw(utterances)
    factor(alpha * (literalListener(utterance).score(obj) -
                    cost(utterance,costParameter)))
    return utterance
  }})
}

speaker("blue square", 1)
~~~~

We can see in the speaker function, the speaker returns an utterance that maximizes the probability that the literal listener would arrive at the intended object while minimizing the cost of the utterance. We can now check the speaker's behavior to illustrate how the cost parameter affects the speaker's access to the one-word and two-word utterances.

Finally, we introduce the pragmatic listener. The pragmatic listener hears an utterance and then jointly reasons about the intended object the speaker wants to reference and the cost parameter that the speaker is using.

~~~~
// prior over cost parameters
var costParameterPrior = function() {
  return uniformDraw(_.range(0.05, 5, 0.5))
}

// pragmatic listener
var pragmaticListener = function(utterance){
  Infer({model: function(){
    var obj = objectPrior()
    var costParameter = costParameterPrior()
    observe(speaker(obj,costParameter),utterance)
    return {obj, costParameter}
  }})
}
~~~~

Here, we can see the pragmatic listener is updating their beliefs from object prior and the cost parameter prior based on the utterance and the speaker's behavior. Jointly reasoning about the world state and another parameter appeared in a few other models throughout the course (namely the pragmatic listener reasoned about scope in the Quantifier Scope Ambiguity model, about theta in the vagueness model, and about phi in the politeness model), but none of the pragmatic listeners reasoned about the cost function as seen in the current model.

Now putting it all together, we present the full model.

~~~~
// set of states
var objects = [{color: "blue", shape: "square", string: "blue square"},
               {color: "blue", shape: "circle", string: "blue circle"},
               {color: "green", shape: "square", string: "green square"}]

// prior over world states
var objectPrior = function() {
  var obj = uniformDraw(objects)
  return obj.string
}

// set of utterances
var utterances = ["blue", "green", "square", "circle",
                  "blue square", "blue circle", "green square"]

// utterance cost function
var cost = function(utterance, costParameter) {
  var numWords = function(utterance) {
    var split = utterance.split(" ")
    return _.size(split)
  }
  return costParameter*numWords(utterance)
};

var costParameterPrior = function() {
  return uniformDraw(_.range(0.05, 5, 0.5))
}

// meaning function to interpret the utterances
var meaning = function(utterance, obj){
  _.includes(obj, utterance)
}

// literal listener
var literalListener = function(utterance){
  Infer({model: function(){
    var obj = objectPrior();
    condition(meaning(utterance, obj))
    return obj
  }})
}

// set speaker optimality
var alpha = 1

// pragmatic speaker
var speaker = function(obj,costParameter){
  Infer({model: function(){
    var utterance = uniformDraw(utterances)
    factor(alpha * (literalListener(utterance).score(obj) -
                    cost(utterance,costParameter)))
    return utterance
  }})
}

// pragmatic listener
var pragmaticListener = function(utterance){
  Infer({model: function(){
    var obj = objectPrior()
    var costParameter = costParameterPrior()
    observe(speaker(obj,costParameter),utterance)
    return {obj, costParameter}
  }})
}


display("cost parameter prior")
viz(Infer(costParameterPrior))

var listenerPosteriorBlue = pragmaticListener("blue")
display("pragmatic listener hears \"blue\"")
viz.table(marginalize(listenerPosteriorBlue, "obj"))
viz(marginalize(listenerPosteriorBlue, "costParameter"))

var listenerPosteriorBlueSquare = pragmaticListener("blue square")
display("pragmatic listener hears \"blue square\"")
viz.table(marginalize(listenerPosteriorBlueSquare, "obj"))
viz(marginalize(listenerPosteriorBlueSquare, "costParameter"))
~~~~

From the visualizations, we can see what the pragmatic listener learns about the cost parameter based on the utterances "blue" and "blue square." The learning can be determined by the change in beliefs from the cost parameter prior to the marginal distribution of the cost parameter posterior. After the pragmatic listener hears the one-word utterance "blue," they believe that the cost parameter is higher than the prior would suggest (evidenced by the shift in probability mass to the right). Alternatively, after the pragmatic listener hears the two-word utterance "blue square," they believe that the cost parameter is dramatically lower than the prior would suggest. Intuitively, the directions of these shifts of beliefs make sense because if you hear a one-word utterance, you can assume that the more informative two-word utterance was costly, and if you hear a two-word utterance, you can assume the two-word utterance was not much more costly than the one-word utterance. But the degree of the shift for the two-word utterance does not match my intuitions because it suggests that the listener was surprised to hear a two-word utterance. In a real reference game with informative two-word utterances, I would expect to hear the two-word utterances often. This mismatch between the results and my intuitions suggests that a simple notion of cost may not have a substantial impact on a listener's reasoning during a reference game.

We will now make a slight adjustment to the world of our model in order to investigate another intuition. Up until now, we have been implementing a slightly revised vanilla model to understand how a pragmatic listener may reason about the speaker's cost. But, I want to investigate if the ambiguity of the utterance influence's our pragmatic listener's beliefs about the cost parameter. The intuition behind this investigation is the following. If I am reasoning about the cost of the utterance and the intended object, and I hear a one-word utterance that is ambiguous (refers to more than one object) when an unambiguous two-word utterance is available. Then, I will be more likely to reason that the cost parameter is high than in the unambiguous one-word utterance case. To test if we can see this effect with this implementation of the cost function, we simplify the world to include only a "blue square" and a "green square." Now our utterances contains one ambiguous one-word utterance, "square," and one unambiguous one-word utterance, "blue."

~~~~
// set of states
var objects = [{color: "blue", shape: "square", string: "blue square"},
               {color: "green", shape: "square", string: "green square"}]

// prior over world states
var objectPrior = function() {
  var obj = uniformDraw(objects)
  return obj.string
}

// set of utterances
var utterances = ["blue", "square", "blue square", "green square"]

// utterance cost function
var cost = function(utterance, costParameter) {
  var numWords = function(utterance) {
    var split = utterance.split(" ")
    return _.size(split)
  }
  return costParameter*numWords(utterance)
};

var costParameterPrior = function() {
  return uniformDraw(_.range(0.05, 5, 0.5))
}

// meaning function to interpret the utterances
var meaning = function(utterance, obj){
  _.includes(obj, utterance)
}

// literal listener
var literalListener = function(utterance){
  Infer({model: function(){
    var obj = objectPrior();
    condition(meaning(utterance, obj))
    return obj
  }})
}

// set speaker optimality
var alpha = 1

// pragmatic speaker
var speaker = function(obj,costParameter){
  Infer({model: function(){
    var utterance = uniformDraw(utterances)
    factor(alpha * (literalListener(utterance).score(obj) -
                    cost(utterance,costParameter)))
    return utterance
  }})
}

// pragmatic listener
var pragmaticListener = function(utterance){
  Infer({model: function(){
    var obj = objectPrior()
    var costParameter = costParameterPrior()
    observe(speaker(obj,costParameter),utterance)
    return {obj, costParameter}
  }})
}


display("cost parameter prior")
viz(Infer(costParameterPrior))

var listenerPosteriorBlue = pragmaticListener("blue")
display("pragmatic listener hears \"blue\"")
viz.table(marginalize(listenerPosteriorBlue, "obj"))
viz(marginalize(listenerPosteriorBlue, "costParameter"))

var listenerPosteriorSquare = pragmaticListener("square")
display("pragmatic listener hears \"square\"")
viz.table(marginalize(listenerPosteriorSquare, "obj"))
viz(marginalize(listenerPosteriorSquare, "costParameter"))
~~~~

From the results, we see more probability mass on the higher cost parameters for the ambiguous one-word utterance (the predicted direction), but the differences between the two posterior beliefs about the cost parameter are minor.

In conclusion, this RSA model introduces a way that the pragmatic listener can infer information about the cost function that the speaker is using. This introduction is interesting because the extent to which the cost of utterances influences our reasoning process is not clear. Ultimately, the reference game scenario is limited in its application to real world language use, but the knowledge that our conversational partner is exerting effort to produce their utterances may influence the information we extract from the utterances. Extending this model may help us better understand this process.
