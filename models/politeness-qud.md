---
layout: model
title: Daniel, Huynh & Zambrano Politeness QUD
model-language: webppl
---


When asked a question to make a value judgement about the quality of the topic of conversation, one has to choose their responses carefully. There are instances in communication in which politeness comes into play dependent on the topic of conversation. For example, certain questions may elicit a more polite response for the purpose of saving face or rather a brutally honest response for the purpose of being informative. The way in which people arrive at their responses given the actual quality of the topic of conversation and given their decision to either be polite or informative is formally modeled in the social reasoning and politeness RSA model by Yoon, Tessler, et al. (2016). 

Furthermore, another aspect that must be taken into consideration is the communicative goal of the speaker. This goal is called the Question Under Discussion (QUD) that the speaker is trying to address by their utterance.

In the Politeness QUD hybrid model, the QUD adds a layer that serves as another "knob" that adjusts the speaker's decided utterance to either be polite or informative. 

In Chapter 3, Kao et al. (2014) introduces the Question Under Discussion (QUD) dimension when modeling pragmatic inference in order to resolve ambiguity of the speaker's intent. 


~~~~
// QUD component of scope model 
var QUDs = ["how many?","all red?","none red?"];
var QUDPrior = function() {
  uniformDraw(QUDs);
}
var QUDFun = function(QUD,state) {
  QUD == "all red?" ? state == 3 :
  QUD == "none red?" ? state == 0 :
  state;
};

//Returns boolean otherwise, the given state
~~~~

The QUDs in the politeness QUD hybrid model models projects of world states under relevant circumstances for each Question Under Discussion

     - "Is it edible" : quality must be either "terrible" or "bad" 
     - "Is it perfect?": quality must be "amazing" 
     - "What is it" : what is the quality of the topic 


~~~~
// QUD component of politeness QUD model 
var QUDs = ["Is edible?","Is perfect?", "What is it?"];
var QUDPrior = function() {
  uniformDraw(QUDs);
}
var QUDFun = function(QUD,state) {
  QUD == "Is edible?" ? state > 2 : 
  QUD == "Is perfect?" ? state == 5 :
  state;
};

//Samples
//QUDFun("Is edible?", 3)
//QUDFun("Is edible?", 1)
//QUDFun("What is it?", 5)
~~~~

In Chapter 9, Brown and Levinson (1987) explored the notion of a speaker who takes into consideration epistemic considerations when deciding upon an utterance so as to save one's face (reputation or self image). Yoon, Tessler, et al. (2016) formalized this idea in RSA model and also introduce social considerations when deciding upon an utterance. 

Lambda is a parameter that weighs emphasis on social versus epistemic utility: 

     - If lambda is greater than 1, higher numbered states are more valuable 
     - If lambda is lower than 1: lower numbered states are less valuable 

Illusion produced:

     - Increase lambda: more weight on social utility
     - Decrease lambda: more weight on epistemic utility

~~~~
var states = [1,2,3,4,5]
var utterances = ["terrible","bad","okay","good","amazing"]

// Empircally Measured correspondence of utterances to states
var literalSemantics = {
  "terrible":[.95,.85,.02,.02,.02],
  "bad":[.85,.95,.02,.02,.02],
  "okay":[0.02,0.25,0.95,.65,.35],
  "good":[.02,.05,.55,.95,.93],
  "amazing":[.02,.02,.02,.65,0.95]
}

// Meaning Function determine whether the utterance describes the state by 
//flipping a coin with the literalSemantics weight
var meaning = function(utterance, state){
  return flip(literalSemantics[utterance][state - 1]);
}


// Value function scales social utility by a parameter lambda
var lambda = 1.25 // value taken from MAP estimate from Yoon, Tessler, et al. 2016
var valueFunction = function(state) {
  state * lambda
}

//Value Function is calculated in social utlity in S function
~~~~

Understanding the Literal Listener:

So in accordance with RSA, we begin with the literal listener, L0  who takes in an utterance and QUD, and interprets it by mapping the state, qstate and m and conditioning on meaning.

Here the L0 is almost identical to the Ch4 model L0 that introduces the QUD. 


~~~~
// Literal listener (L0) of Politeness Model 
var listener0 = cache(function(utterance) {
  Infer({model: function(){
    var state = uniformDraw(states)
    var m = meaning(utterance, state)
    condition(m)
    return state
  }})
})

// Literal listener (L0) of QUD Quantifier Scope Model
var literalListener = cache(function(utterance, scope, QUD) {
  Infer({model: function(){
    var state = statePrior()
    var qState = QUDFun(QUD,state)
    condition(meaning(utterance, state, scope))
    return qState
  }});
});
~~~~

~~~~
///fold: 
var states = [1,2,3,4,5]
var utterances = ["terrible","bad","okay","good","amazing"]

var literalSemantics = {
  "terrible":[.95,.85,.02,.02,.02],
  "bad":[.85,.95,.02,.02,.02],
  "okay":[0.02,0.25,0.95,.65,.35],
  "good":[.02,.05,.55,.95,.93],
  "amazing":[.02,.02,.02,.65,0.95]
}

var meaning = function(utterance, state){
  return flip(literalSemantics[utterance][state - 1]);
}

var lambda = 1.25
var valueFunction = function(state) {
  state * lambda
}

var QUDs = ["Is edible?","Is perfect?", "What is it?"];
var QUDPrior = function() {
  uniformDraw(QUDs);
}

var QUDFun = function(QUD,state) {
  QUD == "Is edible?" ? state > 2 :
  QUD == "Is perfect?" ? state == 5 :
  state;
};
///

// Literal Listener (L0) of Politeness QUD Hybrid 
var listener0 = cache(function(utterance, QUD) {
  Infer({model: function(){
    var state = uniformDraw(states)
    var qState = QUDFun(QUD,state)
    var m = meaning(utterance, state)
    condition(m)
    return qState
  }})
})

//listener0("terrible", "Is edible?")
//listener0("terrible", "Is perfect?")
//listener0("amazing", "Is perfect?") //Try playing with the Literal Semantics
//listener0("okay", "What is it?")
//listener0("bad", "What is it?")
~~~~

Understanding the Speaker:

The speaker observes the world state, phi, and QUD and infers about these things with the literal listener. Since our model includes the QUD, it differs from the original Politeness model when determining the speaker’s social utility. 

Here the sUtility reflects the speaker’s thought process in responding to the QUD. When asked “Is [the cookie] edible?”, the speaker will weigh the social utility according to the state he/she observed. The lambda value will then be adjusted dependent on the variable state. By comparison, the original model only operates on the valueFunction, returning the state multiplied by lambda.


~~~~
// Speaker (S) of QUD Quantifier Scope Model
var speaker = cache(function(scope, state, QUD) {
  Infer({model: function(){
    var utterance = utterancePrior()
    var qState = QUDFun(QUD, state)
    observe(literalListener(utterance, scope, QUD), qState)
    return utterance
  }})
})

// Speaker (S) of Politeness Model
var alpha = 10; // MAP estimate from Yoon, Tessler, et al. 2016
var speaker1 = cache(function(state, phi) {
  Infer({model: function(){
    var utterance = uniformDraw(utterances);
    var L0_posterior = listener0(utterance);
    var utility = {
      epistemic: L0_posterior.score(state),
      social: expectation(L0_posterior, valueFunction)
    }
    var speakerUtility = phi * utility.epistemic +
                        (1 - phi) * utility.social
    factor(alpha * speakerUtility)
    return utterance
  }})
})
~~~~

~~~~
///fold: 
var states = [1,2,3,4,5]
var utterances = ["terrible","bad","okay","good","amazing"]

var literalSemantics = {
  "terrible":[.95,.85,.02,.02,.02],
  "bad":[.85,.95,.02,.02,.02],
  "okay":[0.02,0.25,0.95,.65,.35],
  "good":[.02,.05,.55,.95,.93],
  "amazing":[.02,.02,.02,.65,0.95]
}

var meaning = function(utterance, state){
  return flip(literalSemantics[utterance][state - 1]);
}

var lambda = 1.25
var valueFunction = function(state) {
  state * lambda
}

var QUDs = ["Is edible?","Is perfect?", "What is it?"];
var QUDPrior = function() {
  uniformDraw(QUDs);
}

var QUDFun = function(QUD,state) {
  QUD == "Is edible?" ? state < 2 :
  QUD == "Is perfect?" ? state == 5 :
  state;
};

// literal listener
var listener0 = cache(function(utterance, QUD) {
  Infer({model: function(){
    var state = uniformDraw(states)
    var qState = QUDFun(QUD,state)
    var m = meaning(utterance, state)
    condition(m)
    return qState
  }})
})
///

// Speaker (S) of Politeness QUD Hybrid 
var alpha = 10; // MAP estimate from Yoon, Tessler, et al. 2016 
var speaker1 = cache(function(state, phi, QUD) {
  Infer({model: function(){
    var utterance = uniformDraw(utterances);
    var L0_posterior = listener0(utterance, QUD);
    var qState = QUDFun(QUD,state);
    var sUtility = QUD == "What is it?" ? 
        expectation(L0_posterior, valueFunction(QUD)) :
    QUD == "Is edible?" ?  
        QUDFun("Is edible?",state) ? lambda * uniformDraw([3,4,5]) : 
    lambda * uniformDraw([1,2]) :
    QUDFun("Is perfect?",state) ? lambda * 5 : 
    lambda * uniformDraw([1,2,3,4]) 
    var utility = {
      epistemic: L0_posterior.score(qState),
      social: sUtility
    }
    var speakerUtility = phi * utility.epistemic +
        (1 - phi) * utility.social
    factor(alpha * speakerUtility);
    return utterance
  }})
})

//speaker1(3, 0.5, "Is edible?")
// speaker1(3, 0.5, "Is perfect?")
// speaker1(3, 0.5, "What is it?")
~~~~

Understanding the Pragmatic Listener:
The pragmatic listener draws samples for arguments needed in to run the speaker since it is making it's predictions on the basis of what it believes the speaker is thinking. The pragmatic listener in our politeness QUD hybrid model returns a distribution over what it believes the actual state of the cookies are, the phi value used by the speaker and the QUD answered by the speaker on the basis of the utterance given by the speaker.

~~~~
// Pragmatic listener (L1) of QUD Quantifier Scope Model
var pragmaticListener = cache(function(utterance) {
  Infer({model: function(){
    var state = statePrior()
    var scope = scopePrior()
    var QUD = QUDPrior()
    observe(speaker(scope, state, QUD), utterance)
    return {state: state,
            scope: scope}
  }})
})

//Pragmatic Listener (L1) of the Politeness Model 
var pragmaticListener = function(utterance) {
  Infer({model: function(){
    var state = uniformDraw(states)
    var phi = uniformDraw([0.1, 0.3, 0.5, 0.7, 0.9])
    var S1 = speaker1(state, phi)
    observe(S1, utterance)
    return { state, phi }
  }})
}
~~~~

~~~~
///fold: 
var states = [1,2,3,4,5]
var utterances = ["terrible","bad","okay","good","amazing"]

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

var valueFunction = function(state) {
  state * lambda
}

// QUDs
var QUDs = ["Is edible?","Is perfect?", "What is it?"];
var QUDPrior = function() {
  uniformDraw(QUDs);
}
var QUDFun = function(QUD,state) {
  QUD == "Is edible?" ? state > 2 :
  QUD == "Is perfect?" ? state == 5 :
  state;
};


// literal listener
var listener0 = cache(function(utterance, QUD) {
  Infer({model: function(){
    var state = uniformDraw(states)
    var qState = QUDFun(QUD,state)
    var m = meaning(utterance, state)
    condition(m)
    return qState
  }})
})

//
var alpha = 10; // MAP estimate from Yoon, Tessler, et al. 2016 
var speaker1 = cache(function(state, phi, QUD) {
  Infer({model: function(){
    var utterance = uniformDraw(utterances);
    var L0_posterior = listener0(utterance, QUD);
    var qState = QUDFun(QUD,state);
    var sUtility = QUD == "What is it?" ? 
        expectation(L0_posterior, valueFunction(QUD)) :
    QUD == "Is edible?" ?  
        QUDFun("Is edible?",state) ? lambda * uniformDraw([3,4,5]) : 
    lambda * uniformDraw([1,2]) :
    QUDFun("Is perfect?",state) ? lambda * 5 : 
    lambda * uniformDraw([1,2,3,4]) 
    var utility = {
      epistemic: L0_posterior.score(qState),
      social: sUtility
    }
    var speakerUtility = phi * utility.epistemic +
        (1 - phi) * utility.social
    factor(alpha * speakerUtility);
    return utterance
  }})
})

///

// Pragmatic Listener of the Politeness QUD Hybrid Model 
var pragmaticListener = function(utterance) {
  Infer({model: function(){
    var state = uniformDraw(states) 
    var QUD = QUDPrior();
    var phi = uniformDraw([0.1, 0.3, 0.5, 0.7, 0.9])
    var S1 = speaker1(state, phi, QUD)
    observe(S1, utterance)
    return { state, phi, QUD }
  }})
}

//Visualizing Pragmatic Listener
var listenerPosterior = pragmaticListener("terrible") 
var listenerPosterior = pragmaticListener("bad") 
var listenerPosterior = pragmaticListener("okay") 
var listenerPosterior = pragmaticListener("good") 
var listenerPosterior = pragmaticListener("amazing") 

//belief distribution
display("expected state = " +
        expectation(marginalize(listenerPosterior, "state")))
viz(marginalize(listenerPosterior, "state")) 

display("expected phi = " +
        expectation(marginalize(listenerPosterior, "phi")))
viz.density(marginalize(listenerPosterior, "phi"))

print("QUD")
viz(marginalize(listenerPosterior, "QUD"))
~~~~

~~~~
///fold: 
var states = [1,2,3,4,5]
var utterances = ["terrible","bad","okay","good","amazing"]

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

// literal listener
var listener0 = cache(function(utterance) {
  Infer({model: function(){
    var state = uniformDraw(states)
    var m = meaning(utterance, state)
    condition(m)
    return state
  }})
})

var alpha = 10; // MAP estimate from Yoon, Tessler, et al. 2016
var speaker1 = cache(function(state, phi) {
  Infer({model: function(){
    var utterance = uniformDraw(utterances);
    var L0_posterior = listener0(utterance);
    var utility = {
      epistemic: L0_posterior.score(state),
      social: expectation(L0_posterior, valueFunction)
    }
    var speakerUtility = phi * utility.epistemic +
                        (1 - phi) * utility.social
    factor(alpha * speakerUtility)
    return utterance
  }})
})
///
var pragmaticListener = function(utterance, state) {
  Infer({model: function(){
    //var state = uniformDraw(states)
    var phi = uniformDraw([0.1, 0.3, 0.5, 0.7, 0.9])
    //var phi = 0.99 //wants listener to feel good 
    //var phi = 0.1 //wants to convey information
    //var phi = 0.5 //wants to do both
    var S1 = speaker1(state, phi)
    observe(S1, utterance)
    return { state, phi }
  }})
}

//Visualizing Pragmatic Listener
var listenerPosterior = pragmaticListener("terrible", 5)

//belief distribution
display("expected state = " +
        expectation(marginalize(listenerPosterior, "state")))
viz(marginalize(listenerPosterior, "state")) 

display("expected phi = " +
        expectation(marginalize(listenerPosterior, "phi")))
viz.density(marginalize(listenerPosterior, "phi"))
~~~~