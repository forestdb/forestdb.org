---
layout: model
title: Ho, Huynh & Zambrano Politeness QUD
model-language: webppl
---


When asked to make a value judgement about the quality of the topic of conversation, one has to choose their responses carefully and determine the motive or purpose that their response will answer. There are instances when one tries to communicate in a way in which politeness comes into play depending upon the actual value of the topic of conversation. These instances are a result when someone recognizes that their honest and informative responses may be too harsh for their counterpart to internalize; thus, influencing said person to rely on polite implications as a resolve.  For example, certain questions may elicit an individual to provide more polite response for the purpose of saving face and/or being considerate of the other person's feelings. These polite implications have been defined as white lies when the responses do not particularly match with the true value or state of the topic of interest. This phenomenon where people are willing to resort to white lies rather than providing truth was originally investigated by Brown and Levinson (1987). This study by Brown and Levinson about the way in which people arrive at their responses given the actual quality of the topic of conversation and given their decision to either be polite or (brutally) informative is formally was later modeled in the social reasoning and politeness RSA model by Yoon, Tessler, et al. (2016). 


The original Rational Speech Act is a framework that allows us to view communication between two persons as recursive reasoning between the speaker and the listener. Recursion happens between three levels: the pragmatic listener (L1), the pragmatic speaker (S1) and the literal listener (L0). The pragmatic listener makes inferences about the state of the world by reasoning about the speaker's utterances and why they chose to make those specific utterances. Before then, the pragmatic speaker chooses it's utterances (responses) by considering the probabilistic interpretations that the Literal Listener has calculated about the world given the actual state of the world and it's priors. Where the Literal Listener is decides about it's probabilities based off of what the states of the world are, it's prior probabilities, and it's meaning.  

The Rational Speech Act models allows us to formalize and understand the ways in which people arrive at their decided utterance/responses and the way they internalize these utterances/responses upon hearing these utterances/responses under different linguistic and social circumstances. 


In a realistic scenario, there are many different factors that influence a person's decision to produce a certain response or utterance. Another linguistic circumstance that has been taken into consideration by Kao et. al (2014) is the communicative goal of the speaker. This goal is called the Question Under Discussion (QUD) that the speaker is trying to address by their utterance. Kao et al. introduces the Question Under Discussion (QUD) dimension when modeling pragmatic inference in order to resolve ambiguity of the speaker's intent. 

Conclusively, it is understood human cognition is a result of eclectic combination of many different influences and circumstances. An agent's cognizant decision to produce an utterance or interpret an utterance is therefore subject to several linguistic and social circumstances. 

This Politeness QUD Hybrid model, hopes to capture the plurality of influences in realistic human interactions by investigating the influences of politeness, informativity and questions under discussion upon people's responses and interpretation of such responses. It is expected that the QUD will add another layer to the politeness model that serves as another "knob" that adjusts the speaker's decided utterance to either be polite or informative. 

The QUD manipulation formalized by Kao et. al (2014) is demonstrated in the Scope Model, orginally constructed by Montague (1973) and later developed by May (1977), as follows:


~~~~
// QUD component of Chapter 4 scope model 
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

The Politeness QUD Hybrid model will simulate the following experience (situation) based on the politeness model by Yoon, Tessler, et. al (2016):

- Your friend has baked some cookies and asks your to rate them on a scale of 1 through 5. Where 1 would represent that the cookies are terrible, 2 represents that the cookies are bad, 3 represents that the cookies were okay, 4 represents that the cookies were good and 5 represents that the cookie were amazing. 

Questions under discussion will be added to elicit and influence the response the person being asked to rate the cookies will formulate. The QUDs in the Politeness QUD Hybrid model will model projects of world states under relevant circumstances for each Question Under Discussion:

     - "Is it edible?" : cookie quality must be "okay", "good" or "amazing"
     - "Is it perfect?": cookie quality must be "amazing" 
     - "What is it?" : what is the quality of the cookies (topic)


~~~~
// Newly Introduced QUD component of the politeness QUD Hybrid model 
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

In Chapter 9, Brown and Levinson (1987) explored the notion of a speaker who takes into account epistemic considerations when deciding upon an utterance so as to communicate an informative accounts rather than to save one's face (reputation or self image). Yoon, Tessler, et al. (2016) formalized this idea in RSA model where one has to self calculate and consider epistemic utility (informative) purposes or social utility (social considerations) when deciding upon an utterance. 

In this model, Lambda is a physical parameter that weighs emphasis on social versus epistemic utility: 

     - If lambda > 1: higher numbered states are more valuable 
     - If lambda < 1: lower numbered states are less valuable 

The Illusion that is produced by differing values of lambda: 

     - Increase lambda: more weight is put upon social utility
     - Decrease lambda: more weight is put upon epistemic utility

~~~~
//The Politeness Model by Yoon and Tessler (2016) in Chapter 9 
var states = [1,2,3,4,5]
var utterances = ["terrible","bad","okay","good","amazing"]

// Empircally Measured correspondence of utterances to states (by surveying the population)
var literalSemantics = {
  "terrible":[.95,.85,.02,.02,.02],
  "bad":[.85,.95,.02,.02,.02],
  "okay":[0.02,0.25,0.95,.65,.35],
  "good":[.02,.05,.55,.95,.93],
  "amazing":[.02,.02,.02,.65,0.95]
}

// Meaning Function determines whether the utterance describes the state by 
//flipping a coin with the literalSemantics weight at it's corresponding index
var meaning = function(utterance, state){
  return flip(literalSemantics[utterance][state - 1]);
}


// Value function scales social utility by a parameter lambda
var lambda = 1.25 // value taken from MAP estimate from Yoon, Tessler, et al. 2016
var valueFunction = function(state) {
  state * lambda
}

//Value Function is used in calculation for social utlity in the Speaker (S) function
~~~~

Understanding the Literal Listener:

In accordance with RSA, we begin with the literal listener, L0 who takes in an utterance and QUD, and interprets it by mapping the state, qstate and m and conditioning on meaning.

L0 uses the operator 'condition', which interprets binary truth values (boolean values that are true or false) and produces a result when a condition is met. Here, the L0 of the Chapter 9 Politeness Model embodies the typical structure of the L0 which takes an utterance and compares whether the utterance matches the proper meaningful state of a sampled state. This is repeated until the condition is met, and this proper state is returned. The L0 in the Chapter 9 Politeness Model is almost identical to the Chapter 4 Scope Model where instead the latter L0 introduces the scope.

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

The Politeness QUD Hybrid Model similarly to the Chapter 4 L0 will use only the utterance and QUD as arguments for interpretation of truth values and probability calculation for states. 

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

//listener0("okay", "What is it?") //return distrbution over states
//listener0("bad", "What is it?") //return distribution over states
//listener0("terrible", "Is edible?") //return distrbution over binary values (true/false)
//listener0("terrible", "Is perfect?") //return distrbution over binary values (true/false)

//Specific Example: 
//listener0("amazing", "Is perfect?") //Try playing with the Literal Semantics where
// a lower value for 4th state (3rd index) of "amazing" list of probabilities will 
//enhance truth value of this specific QUD
~~~~

Understanding the Speaker:

In accordance with RSA, the speaker observes the world states as generated by L0. The speaker also acts as a rational 'agent' that chooses an utterance depending upon it's utility for the purpose that it is trying to serve (whether that may be a certain QUD, polite/informative intentions, or both). 

In the Chapter 4 Scope Model, the speaker samples an utterance and takes in the arguments state and QUD to formulate a qstate. Furthermore, the speaker uses the sampled utterance and the arguments: scope and QUD for L0 formulate these state probabilities. The speaker uses the operator: 'observe' or 'factor' which will compared the states generated by L0 and qstate. The Speaker will return it's decided utterance upon the conditions of the L0 and the QUD by qState. To reiterate, the speaker wishes to chose an utterance that will communicate the state of the world. The inferences of L0 (the mapping of the states of the world) and the QUD are the influences of the speaker's decided utterance. 

In the Chapter 9 Politeness model, the speaker, again, samples an utterance to generate L0's probabilities. A noted difference is that the speaker now also takes the argument phi which is a parameter that weighs the speaker's decided utility. Low phi values place emphasis upon epistemic utility and low phi values place emphasis upon social utility. Alpha is a parameter that optimizes speaker utility and is established at the value 10 by Yoon and Tessler's calculations. These parameters are used to scale speaker utility which influences the speaker's decided utterance to either place emphasis on informativity or politeness. 

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

The Politeness QUD Hybrid model specifically differs from the original Politeness models when determining the speaker’s social utility. 

- Here the sUtility reflects the speaker’s thought process in responding to the QUD. When asked "What is it", the speaker will scale the state only by the predetermined lamda value of 1.25 in the valueFunction (estimated by Yoon and Tessler). 

When asked “Is [the cookie] edible?”, the speaker will weigh the social utility according to the state he/she observed. The lambda value will then be adjusted dependent on the variable state. By comparison, the original model only operates on the valueFunction, returning the state scaled (multiplied) by lambda. This new model scales the state by state values that correspond to the QUD depending whether the L0 predictions are met upon the condition of the QUD. Where certain QUD's will influence the politeness emphasis when determining the utterance the speaker will produce. 

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

var lambda = 1
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
        sample(L0_posterior) ? lambda * uniformDraw([3,4,5]) : 
    lambda * uniformDraw([1,2]) :
    sample(L0_posterior) ? lambda * 5 : 
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

//Phi values have been kept at 0.5 for ambivalence in utility to allow us to observe
// the influences of the QUD upon the decided utterance 

//speaker1(3, 0.5, "Is edible?")
//speaker1(3, 0.5, "Is perfect?")
//speaker1(5, 0.5, "Is perfect?")
// speaker1(3, 0.5, "What is it?")
~~~~

Understanding the Pragmatic Listener:

In accordance with RSA, the pragmatic listener hears an utterance and draws samples of arguments needed to run the speaker since it is making it's predictions on the basis of what it believes the speaker is thinking. This is construct is demonstrated in both the Chapter 4 Scope Model which needs a scope, state and QUD to run speaker and the Chapter 9 Politeness Model which needs a state and phi value to run speaker.  

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

Similarly, the pragmatic listener in the Politeness QUD Hybrid Model formulates samples for the state, phi, and QUD for it to run the model's speaker function. It then returns a distribution over what it believes the actual state of the cookies are, the phi value it believes was used by the speaker and the QUD answered by the speaker on the basis of the utterance given by the speaker.

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
var lambda = 1.25 // value taken from MAP estimate from Yoon, Tessler, et al. 2016
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
var listener0 = cache(function(utterance, QUD) {
  Infer({model: function(){
    var state = uniformDraw(states)
    var qState = QUDFun(QUD,state)
    var m = meaning(utterance, state)
    condition(m)
    return qState
  }})
})
var alpha = 10; // MAP estimate from Yoon, Tessler, et al. 2016
var speaker1 = cache(function(state, phi, QUD) {
  Infer({model: function(){
    var utterance = uniformDraw(utterances);
    var L0_posterior = listener0(utterance, QUD);
    var qState = QUDFun(QUD,state);
    var sUtility = QUD == "What is it?" ? 
        expectation(L0_posterior, valueFunction(QUD)) :
    QUD == "Is edible?" ?  
        sample(L0_posterior) ? lambda * uniformDraw([3,4,5]) : 
    lambda * uniformDraw([1,2]) :
    sample(L0_posterior) ? lambda * 5 : 
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
//var listenerPosterior = pragmaticListener("terrible") 
//var listenerPosterior = pragmaticListener("bad") 
//var listenerPosterior = pragmaticListener("okay") 
//var listenerPosterior = pragmaticListener("good") 
//var listenerPosterior = pragmaticListener("amazing") 

//Politeness QUD Hybrid Model: Pragmatic Listener Belief Distribution
display("expected state = " +
        expectation(marginalize(listenerPosterior, "state")))
viz(marginalize(listenerPosterior, "state")) 

display("expected phi = " +
        expectation(marginalize(listenerPosterior, "phi")))
viz.density(marginalize(listenerPosterior, "phi"))

print("QUD")
viz(marginalize(listenerPosterior, "QUD"))
~~~~

General Results for Politeness QUD Hybrid Model:
- "Terrible":    State = approx.  1.71 // Phi = approx.  0.66 // QUD: "Is edible?"
- "Bad":         State = approx.  1.8   // Phi = approx.  0.66 // QUD: "Is edible?"
- "Okay":       State = approx.  2.81 // Phi = approx.  0.49 // QUD: "Is perfect?"
- "Good":      State = approx.  3.34 // Phi = approx.  0.42 // QUD: "Is edible?"
- "Amazing": State = approx.  3.8   // Phi = approx.  0.42 // QUD: "What is it?"




~~~~
//Original Chapter 9 Politeness Model
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
var lambda = 1.25 // value taken from MAP estimate from Yoon, Tessler, et al. 2016
var valueFunction = function(s){
  return lambda * s
}
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
//Pragmatic Listener of the Chapter 9 Politeness Model
var pragmaticListener = function(utterance) {
  Infer({model: function(){
    var state = uniformDraw(states)
    var phi = uniformDraw([0.1, 0.3, 0.5, 0.7, 0.9])
    var S1 = speaker1(state, phi)
    observe(S1, utterance)
    return { state, phi }
  }})
}

//Visualizing Pragmatic Listener
// var listenerPosterior = pragmaticListener("terrible")
// var listenerPosterior = pragmaticListener("bad")
// var listenerPosterior = pragmaticListener("okay")
// var listenerPosterior = pragmaticListener("good")
// var listenerPosterior = pragmaticListener("amazing")


//Ch 9 Model: Pragmatic Listener's Belief Distribution
display("expected state = " +
        expectation(marginalize(listenerPosterior, "state")))
viz(marginalize(listenerPosterior, "state")) 

display("expected phi = " +
        expectation(marginalize(listenerPosterior, "phi")))
viz.density(marginalize(listenerPosterior, "phi"))
~~~~

General Results for Original Politeness Model:
- "Terrible":    State = approx.  1.23 // Phi = approx.  0.75 
- "Bad":         State = approx.  1.6   // Phi = approx.  0.76 
- "Okay":       State = approx.  2.69 // Phi = approx.  0.66 
- "Good":      State = approx.  3.19 // Phi = approx.  0.45 
- "Amazing": State = approx.  3.55 // Phi = approx.  0.39

Two Model Comparison: 

- Predicted states in the hybrid model is generally higher than the predicted states in the original politeness model. (Specifically: .3-.5 difference in extreme states 1 and 5, .2 difference in less extreme states 2 and 4, and .1 difference in the ambivalent state 3 between the two models) 
- Predicted phi values for lower states is higher in the original politeness model than than hybrid model except for state 5 which is higher in the hybrid model than the original politeness model 

Conclusion: 

The arguments used to run pragmatic listener is kept constant for both models; where both models only took in an utterance as an argument and both models take in utterances from the same set of utterance. Despite these controls, higher predicted state values were found in the hybrid model in combination with the lower phi values (implying more social utility) in the hybrid model. These trends in the hybrid model results demonstrates that this model must have had influences by the QUD manipulation. The QUD manipulations: "Is [the cookie] edible?" and "Is [the cookie] perfect?" demonstrates influence in potentially decreasing phi values which increases social utility and appeal to politeness. The QUD "What is it?" seems to instead increase phi values (informativity) at the highest predicted state yet the corresponding predicted state is still consistent with the overall predicted states trend. 

A comparison of these results demonstrates that the QUD manipulation serviced as a "knob" that influenced the pragmatic listener's predicted weight of different influences (Phi and QUD) upon the speaker who generating the utterance communicated.

This model specifically demonstrates the interaction between linguistic conditions (QUD) and social conditions (politeness) in influencing communication between two entities. In further research and study, different ratio adjustments of literal semantic probabilities of states and utterance mappings can be studied in junction with different combinations and values for parameters such as lambda, alpha and phi. These sorts of eclectic approaches to studying diction and inference will provide a more realistic capture of actual real life phenomenon. Studying the multifarious interaction and influences of semantic and social conditions will allow for more coherent understanding of recursive reasoning in communication between real, rational agents.