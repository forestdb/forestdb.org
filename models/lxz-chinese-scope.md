---
layout: model
title: Li, Xu, Zhi - Chinese scope
model-language: webppl
---


## Introduction
Quantifier scope ambiguity has long been the subject of study in psycholinguistics. A sentence with a quantifier and a logic operator (e.g. negation) would render two different readings depending on the underlying syntactic structures. For instance, sentence *(1)* is ambiguous between the surface scope interpretation *(1a)* and the inverse scope interpretation *(1b)*. 

*(1)* I did not feed two rabbits

*(1a)* surface scope (not > two): It is not the case that I fed two rabbits. 

*(1b)* inverse scope (two > not): There are two rabbits that I did not feed. 

Under the surface scope interpretation, negation is taking scope over the quantifier phrase and the interpretation is equivalent to 'it is not the case that I fed two rabbits'. Under the inverse scope, the interpretation could be paraphrased as 'there are two rabbits that I did not feed'. 

How do speakers and listeners understand scope ambiguous sentences? One important experimental methodology to assess meaning is through truth-value judgment tasks. In the experiment, participants are given a scenario which describes the truth state of the world. In story (2), the scenario describes a situation where the surface scope interpretation is true (i.e. it is not the case that I fed two rabbits), and the inverse scope interpretation is false (i.e. there are two rabbits that I did not feed).  After hearing the story, a puppet will make an utterance with a sentence ambiguous between surface scope and inverse scope (e.g. I did not feed two rabbits), the listener could make a judgment about whether the puppet's statement is true or false. 

*(2)* There was a white rabbit and a brown rabbit. I wanted to feed both of them, but I didn’t have enough carrots. In the end, I just fed the brown rabbit.

A tactic linking hypothesis is that the proportion of 'yes' responses given a certain scenario represents how likely the speaker will endorse the ambiguous utterance. When the story describes a situation where surface scope interpretation is true, the proportion of 'yes' responses in truth-value judgment tasks reflects speakers' endorsement of surface scope interpretation of the sentence. The experiments based on truth-value judgments allow us to precisely investigate the interpretations of the scope ambiguous sentences with negation and quantifiers phrases of 'two'.

## The empirical puzzle

Previous studies have revealed many puzzles around the scope interpretations of the negation and quantifier constructions such as 'I did not feed two rabbits'. 

### Developmental effect
In order to understand how children might interpret scope ambiguous sentences differently from adults, Lidz and Musolino (2002) conducted two experiments with the truth-value judgment tasks. 

In the first experiment, the participants were presented with a similar story as in *(2)*, where there were two rabbits, and only one rabbit was fed. They were then asked to evaluate the utterance 'I did not feed two rabbits' given the story. If they decided the ambiguous sentence was correct under such a circumstance, that would indicate that a surface scope interpretation was endorsed. We label this experiment as the *surface scope* experiment.

In the second experiment, participants were asked to do a similar task, and the story they heard would favor the inverse scope interpretation. In the story, they were presented with four rabbits, and only two rabbits were fed. The endorsement of the utternace in the second experiment would reflect an endorsement of the inverse scope interpretation. We label the second experiment as the *inverse scope* experiment.

Lidz and Musolino (2002) found that while surface-scope and inverse-scope interpretations are equally readily available to English speaking adults (surface: 97%; inverse: 93%), English-speaking children show a preference over surface scope (81%) over inverse scope (33%). They conclude that English-speaking children's reluctancy to endorse inverse scope interpretation could mainly be attributed to the fact that children are more likely to use hierarchical structure such as c-command or linear precedence to interpret sentences, instead of constructing semantic logical forms. While this explanation is very intuitive, their result has not been quantitatively tested. It is possible that other general cognitive factors (such as children's ability to count) might have an impact on the scope interpretation. 

### Cross-linguistic effect
Besides the developmental pattern in English, the picture of scope ambiguity interpretation gets more complicated with the empirical observations from Mandarin. 

Previous literature has suggested that inverse scope interpretation is not available in Mandarin (Scontras et al., 2014), however, Su (2008)'s results are seemingly contraditory with that conclusion. Su (2008) conducted experiments similarly as Lidz and Musolino (2002) with native Mandarin-speaking children and adults.

Su (2008) found that the scope interpretation in Mandarin Chinese is different from the observations surrounding English. Firstly, Chinese-speaking adults demonstrate a preference for surface scope (72%) than inverse scope (31%). It should be acknowledged that Chinese-speaking adults show a moderate endorsement rate for inverse scope, which is consistent with previous literature that suggest that Mandarin does not allow inverse scope interpretations (Scontras et al., 2014). Furthermore, Mandarin-speaking children show an interesting pattern where the inverse scope interpretation (63%) is not only available, but also preferred than surface scope interpretation (35%). 

The results present challenges to the previous literature in two ways. Firstly, the theory need to reconcile adults preference in Su (2008) with the previous observation where inverse scope is unavailable in Mandarin Chinese. Secondly, the theory need to explain Mandarin children's preference for inverse scope given English-speaking children's preference, and to reconcile with the cognitive explanations that children generally have difficulty processing inverse scope structures.

The Mandarin results are generally explained from the perspective of formal semantics. The first explanation is concerned with the nature of the numeral phrases. Semantic analysis (Lee, 1996) suggest that there are two readings of *numeral-classifier-noun* construction: (a) quantity interpretation and (b) non-quantity indefinite individual-denoting interpretation. The quantity-denoting interpretation is available when preceded by a modal or belief-contexts (as shown in *(3)*). 

*(3)* Liangzhi          maomi    chi-bu-wan     shitiao           yu. 

      Two-Classifier    cats  eat-not-finish   ten-Classifier   fish.

      Two cats cannot finish ten fish.

Unlike adults, children’s quantify denoting reading is not restricted to presence of modal or belief-contexts. Chinese-speaking children prefer quantity denoting interpretation. 

Secondly, Huang (1988) suggests that a meta-linguistic reading is prohibited when negative morphome forms an immediate constituent with verb head. Comparing *(4)* and *(5)*, the meta-linguistic reading is prohibited in *(4)* but not in *(5)*. As a result, *(4)* only has one reading where there are precisely two rabbits that were not fed. *(5)* allows a pragmatic reading where the number of rabbits I did not feed was less than two. It is felicitous to continue *(5)* with 'I just fed one of them', but not for *(4)*. It is possible that children process the stimuli in Su (2008) as if the auxilary was deleted, so the *(5)* would receive similar readings as *(4)*.

*(4)* Wo mei wei     liangzhi        tuzi.
     
      I  not feed  two-Classifier     tuzi.

      I did not feed two rabbits.

*(5)* Wo     mei-you    wei     liangzhi        tuzi.
     
      I  not-auxiliary  feed  two-Classifier     tuzi.

      I did not feed two rabbits.    


Although the formal semantic explanations are appealing, there is no experimental evidence to support the nature of children's formal analysis of Mandarin Chinese. 

## RSA model
### Related work 

While scope ambiguity resolution is often explained under the umbrella of compositional semantics, the Rational Speech Act (RSA) framework suggests that the inference over contexts, situation, conversational partner all play an role beyond linguistic factors when we derive meaning from utterances. The RSA framework assumes that listener can infer the intended meaning of speakers, and the speakers can choose their utterances based on their inferences about how listeners are going to utilize the information. More critically, the joint inference between speaker and listener is achieved by the mutual understanding of the balance between informativeness and utterance cost. The speaker is trying to be as informative as possible in delivering the message and to be understood by the listener, but the speaker is not going to be more informative than needed. Ever since the seminal work of Frank and Goodman (2012), the RSA framework has been used to address pragmatic inference such as scalar implicature (Goodman and Stuhlmuller, 2013), non-literal interpretation of language (Kao et al., 2014), scope ambiguity resolution (Scontras and Pearl, 2021), vagueness (Lassiter and Goodman, 2013), politeness (Yoon, Tessler, et al., 2016) and lexical uncertainty (Leon Bergen et al., 2016).

In this project, we will resort to a rational speech act framework to explain these puzzles. Crucially, we will explore the role of cognitive factors in scope ambiguous sentences in both Mandarin and English and the explanary power of formal semantic analysis, by explicitly tuning the relevant parameters in our computational model. We hope our model could provide a quantitative prediction for the empirical puzzle, and shed light on the cognitive mechanisms underlying developmental and cross-linguistic patterns.

### Implementation

Following other RSA models, we build up our model implementation as follows. Following the intuitions presented in Chapter 4, we define the reasoning about the possible meanings relative to surface/inverse scopes, such that the meaning function takes possible utterances (utterance), state of the world (state), and scopes (scope) as its arguments, and returns a truth value. 

~~~~
// possible world states
var states = [0,1,2,3]
var statePrior = function() {
  uniformDraw(states)
}

// possible utterances: saying nothing or asserting the ambiguous utterance
var utterances = ["null","every-not"]

// possible scopes
var scopePrior = function(){ 
  return uniformDraw(["surface", "inverse"])
}

// meaning function
var meaning = function(utterance, state, scope) {
  return utterance == "every-not" ? 
    scope == "surface" ? state == 0 :
  state < 3 : 
  true
}

meaning("every-not", 1, "surface")
~~~~

We also set up three different types of QUDs, which corresponds to 'how many rabbits have been fed?', "Have all rabbits been fed?" and "Have none rabbits been fed?". Given the QUD and a prior state, the QUD function returns a truth-value. 
~~~~
// QUDs
var QUDs = ["how many?","all?","none?"];
var QUDPrior = function() {
  uniformDraw(QUDs);
}
var QUDFun = function(QUD,state) {
  QUD == "all?" ? state == 2 :
  QUD == "none?" ? state == 0 :
  state;
};
~~~~

The literal listener has a prior of state, infers QUDs given the state. The literal listener updates the state by jointly inferring on utterance, state, scope and truth-value based on QUD and prior state: 

~~~~
// Literal listener (L0)
var literalListener = cache(function(utterance,scope,QUD) {
  Infer({model: function(){
    var state = uniformDraw(states);
    var qState = QUDFun(QUD,state)
    condition(meaning(utterance,state,scope));
    return qState;
  }});
});
~~~~

The speaker observes the state of the world, but also knows which scope of interpretation that is going to intend and knows the QUD that is trying to be addressed. The speaker will return the probability distribution of utterances and communicate the answer to the QUD to the Literal Listener. The speaker tries to maximize the probability that the literal listener will give the correct answer. Given that utterance with that scope and that QUD and minimizing cost. 

~~~~
var alpha = 1

// Speaker (S)
var speaker = cache(function(scope, state, QUD) {
  return Infer({model: function(){
    var utterance = utterancePrior()
    var qState = QUDFun(QUD, state)
    factor(alpha*(literalListener(utterance,scope,QUD).score(qState) 
                  - cost(utterance)))
    return utterance
  }})
})
~~~~

The pragmatic listener interprets the utterance by jointly inferring what the speaker is likely to convey with the given utterance, and their prior knowledge of scope, state and QUDs, and returns the inferred state of the world.

~~~~
// Pragmatic listener (L1)
var pragmaticListener = cache(function(utterance) {
  Infer({model: function(){
    var state = statePrior();
    var scope = scopePrior();
    var QUD = QUDPrior();
    observe(speaker(scope,state,QUD),utterance);
    return state
  }});
});
~~~~

Finally, our critical prediction is concerned with how a pragmatic speaker would endorse an ambiguous utternance given the state of the world. The pragmatic speaker make an utterance based on the true state of the world they want to convey, and their understanding of how a pragmatic listener is going to interpret their utternance.

~~~~
// Pragmatic speaker (S2)
var pragmaticSpeaker = cache(function(state) {
  Infer({model: function(){
    var utterance = utterancePrior();
    factor(pragmaticListener(utterance).score(state))
    return utterance
  }})
})
~~~~

Our crucial manipulation we perform is that we set up the state, scope, utterance, and meaning function differently in accord with the true experimental set-up. 

The two experiments in both Su (2008) and Lidz and Musolino (2002) are different in terms of the number of objects appearing in the story. For Experiment 1 where the scenario favoring the surface scope interpretation, two objects are presented, and the corresponding state is as follows:

~~~~
// possible world states
var states = [0,1,2]
~~~~

For Experiment 2 (inverse scope experiment), four objects are presented, and the corresponding state is as follows:

~~~~
// possible world states
var states = [0,1,2,3,4]
~~~~

We also distinguish the English model with the Mandarin model by adjusting the scope prior. Given that the inverse scope is not available in Mandarin, the scope prior is changed into a categorical distribution where the inverse scope is extremely unlikely.

~~~~
var scopePrior = function(){ 
  return categorical({vs:["surface", "inverse"],ps:[100,1]})
}
~~~~

In addition, to address the Huang (1988)'s formal analysis on Mandarin, we consider an alternative utterance 'none', which is equivalent to the reading in *(4)* where none of the two rabbits had been fed. We also tested the role of the 'none' utternance in English model.

~~~~
var utterancePrior = function() {
  categorical({vs:["null","not-two","none"],ps:[1,1,1]})
}
~~~~

#### English full model

Below are the codes of English scope ambiguity models for the simulation of both Experiment 1 and 2 in Lidz and Mosulino (2002).

~~~~
// Here is the code for English Expt 1 (surface scope)
//different qud prior, different state prior, access to alternative utterances

// Here is the code for the quantifier scope model

// possible utterances
var utterances = ["null","not-two"];

var utterancePrior = function() {
  categorical({vs:["null","not-two"],ps:[1,1]})
}

var cost = function(utterance) {
  return 1
}

// possible world states
var states = [0,1,2];
var statePrior = function() {
  categorical({vs:[0,1,2],ps:[1,1,1]})
}

// possible scopes
var scopePrior = function(){ 
  return categorical({vs:["surface", "inverse"],ps:[1,1]})
}

var meaning = function(utterance, state, scope) {
  //if utterance == none:
     //return state==0
  //else:
    //elif utternace == nottwo:
         //if scope == surface:
            //return state == 0 / state==1
          //else:
            //return state == 0
     //else:
         //return true
  
  return utterance == "not-two" ? 
    scope == "surface" ? (state == 0 | state ==1): 
  state == 0 :
  true;
};


// QUDs
var QUDs = ["how many?","all red?","none red?"];
var QUDPrior = function() {
  uniformDraw(QUDs);
}
var QUDFun = function(QUD,state) {
  QUD == "all red?" ? state == 2 :
  QUD == "none red?" ? state == 0 :
  state;
};

// Literal listener (L0)
var literalListener = cache(function(utterance,scope,QUD) {
  Infer({model: function(){
    var state = uniformDraw(states);
    var qState = QUDFun(QUD,state)
    condition(meaning(utterance,state,scope));
    return qState;
  }});
});

var alpha = 1

// Speaker (S)
var speaker = cache(function(scope, state, QUD) {
  return Infer({model: function(){
    var utterance = utterancePrior()
    var qState = QUDFun(QUD, state)
    factor(alpha*(literalListener(utterance,scope,QUD).score(qState) 
                  - cost(utterance)))
    return utterance
  }})
})

// Pragmatic listener (L1)
var pragmaticListener = cache(function(utterance) {
  Infer({model: function(){
    var state = statePrior();
    var scope = scopePrior();
    var QUD = QUDPrior();
    observe(speaker(scope,state,QUD),utterance);
    return state
  }});
});

// Pragmatic speaker (S2)
var pragmaticSpeaker = cache(function(state) {
  Infer({model: function(){
    var utterance = utterancePrior();
    factor(pragmaticListener(utterance).score(state))
    return utterance
  }})
})



// A speaker decides whether to endorse the ambiguous utterance as a 
// description of the not-all world state
viz.table(pragmaticSpeaker(1))
viz(pragmaticSpeaker(1))
//literalListener("surface", 2, "all red?")
~~~~

~~~~
// Here is the code for English Expt 2 (inverse scope)
//different qud prior, different state prior, access to alternative utterances

// Here is the code for the quantifier scope model

// possible utterances
var utterances = ["null","not-two"];

var utterancePrior = function() {
  categorical({vs:["null","not-two"],ps:[1,10]})
}

var cost = function(utterance) {
  return utterance == "not-two" ? 1 : 
  1
}

// possible world states
var states = [0,1,2,3,4];
var statePrior = function() {
  categorical({vs:[0,1,2,3,4],ps:[1,1,1,1,1]})
}

// possible scopes
var scopePrior = function(){ 
  return categorical({vs:["surface", "inverse"],ps:[1,1]})
}

// meaning function
var meaning = function(utterance, state, scope) {
  return utterance == "not-two" ? 
    scope == "surface" ? (state < 2):
  (state < 3) :
  true;
};

// QUDs
var QUDs = ["how many?","all red?","none red?"];
var QUDPrior = function() {
  categorical({vs:["how many?","all red?","none red?"],ps:[1,1,1]})
  //uniformDraw(QUDs);
}

var QUDFun = function(QUD,state) {
  QUD == "all red?" ? state == 4 :
  QUD == "none red?" ? state == 0 :
  state;
};

// Literal listener (L0)
var literalListener = cache(function(utterance,scope,QUD) {
  Infer({model: function(){
    var state = uniformDraw(states);
    var qState = QUDFun(QUD,state)
    condition(meaning(utterance,state,scope));
    return qState;
  }});
});

var alpha = 1

// Speaker (S)
var speaker = cache(function(scope, state, QUD) {
  return Infer({model: function(){
    var utterance = utterancePrior()
    var qState = QUDFun(QUD, state)
    factor(alpha*(literalListener(utterance,scope,QUD).score(qState) 
                  - cost(utterance)))
    return utterance
  }})
})

// Pragmatic listener (L1)
var pragmaticListener = cache(function(utterance) {
  Infer({model: function(){
    var state = statePrior();
    var scope = scopePrior();
    var QUD = QUDPrior();
    observe(speaker(scope,state,QUD),utterance);
    return state
  }});
});

// Pragmatic speaker (S2)
var pragmaticSpeaker = cache(function(state) {
  Infer({model: function(){
    var utterance = utterancePrior();
    factor(pragmaticListener(utterance).score(state))
    return utterance
  }})
})

// A speaker decides whether to endorse the ambiguous utterance as a 
// description of the not-all world state
//viz.table(pragmaticSpeaker(0))
viz.table(pragmaticSpeaker(2))
//viz.table(pragmaticSpeaker(2))
//literalListener("surface", 2, "all red?")
~~~~

#### Chinese full model

Below are the codes of Chinese scope ambiguity models for the simulation of both Experiment 1 and 2 in Su (2008).

~~~~
// Here is the code of Chinese model for Expt 1.
//different qud prior, different state prior, access to alternative utterances

// Here is the code for the quantifier scope model

// possible utterances
var utterances = ["null","not-two","none"];

var utterancePrior = function() {
  categorical({vs:["null","not-two","none"],ps:[1,1,1]})
}

var cost = function(utterance) {
  return utterance == "not-two" ? 1 : 
  utterance == 'none'? 1 : 
  1
}

// possible world states
var states = [0,1,2];
var statePrior = function() {
  categorical({vs:[0,1,2],ps:[1,1,1]})
}

// possible scopes
var scopePrior = function(){ 
  return categorical({vs:["surface", "inverse"],ps:[100,1]})
}

// meaning function
var meaning = function(utterance, state, scope) {
  return utterance == "none"? state == 0:
    utterance == "not-two" ? 
    scope == "surface" ? (state < 2):
  (state == 0) :
  true;
};

// QUDs
var QUDs = ["how many?","all red?","none red?"];
var QUDPrior = function() {
  categorical({vs:["how many?","all red?","none red?"],ps:[1,1,1]})
  //uniformDraw(QUDs);
}
var QUDFun = function(QUD,state) {
  QUD == "all red?" ? state == 2 :
  QUD == "none red?" ? state == 0 :
  state;
};

// Literal listener (L0)
var literalListener = cache(function(utterance,scope,QUD) {
  Infer({model: function(){
    var state = uniformDraw(states);
    var qState = QUDFun(QUD,state)
    condition(meaning(utterance,state,scope));
    return qState;
  }});
});

var alpha = 1

// Speaker (S)
var speaker = cache(function(scope, state, QUD) {
  return Infer({model: function(){
    var utterance = utterancePrior()
    var qState = QUDFun(QUD, state)
    factor(alpha*(literalListener(utterance,scope,QUD).score(qState) 
                  - cost(utterance)))
    return utterance
  }})
})

// Pragmatic listener (L1)
var pragmaticListener = cache(function(utterance) {
  Infer({model: function(){
    var state = statePrior();
    var scope = scopePrior();
    var QUD = QUDPrior();
    observe(speaker(scope,state,QUD),utterance);
    return state
  }});
});

// Pragmatic speaker (S2)
var pragmaticSpeaker = cache(function(state) {
  Infer({model: function(){
    var utterance = utterancePrior();
    factor(pragmaticListener(utterance).score(state))
    return utterance
  }})
})

// A speaker decides whether to endorse the ambiguous utterance as a 
// description of the not-all world state
//viz.table(pragmaticSpeaker(0))
//viz.table(pragmaticSpeaker(1))
viz.table(pragmaticSpeaker(1))
//literalListener("surface", 2, "all red?")
~~~~

~~~~
// Here is the code of Chinese model for Expt 2.
//different qud prior, different state prior, access to alternative utterances

// Here is the code for the quantifier scope model

// possible utterances
var utterances = ["null","not-two","none"];

var utterancePrior = function() {
  categorical({vs:["null","not-two","none"],ps:[1,1,1]})
}

var cost = function(utterance) {
  return utterance == "not-two" ? 1 : 
  utterance == 'none'? 1 : 
  1
}

// possible world states
var states = [0,1,2,3,4];
var statePrior = function() {
  categorical({vs:[0,1,2,3,4],ps:[1,1,1,1,1]})
}

// possible scopes
var scopePrior = function(){ 
  return categorical({vs:["surface", "inverse"],ps:[100,1]})
}

// meaning function
var meaning = function(utterance, state, scope) {
  return utterance == "none"? state == 0:
    utterance == "not-two" ? 
    scope == "surface" ? (state < 2):
  (state < 3) :
  true;
};

// QUDs
var QUDs = ["how many?","all red?","none red?"];
var QUDPrior = function() {
  categorical({vs:["how many?","all red?","none red?"],ps:[1,1,1]})
  //uniformDraw(QUDs);
}
var QUDFun = function(QUD,state) {
  QUD == "all red?" ? state == 4 :
  QUD == "none red?" ? state == 0 :
  state;
};

// Literal listener (L0)
var literalListener = cache(function(utterance,scope,QUD) {
  Infer({model: function(){
    var state = uniformDraw(states);
    var qState = QUDFun(QUD,state)
    condition(meaning(utterance,state,scope));
    return qState;
  }});
});

var alpha = 1

// Speaker (S)
var speaker = cache(function(scope, state, QUD) {
  return Infer({model: function(){
    var utterance = utterancePrior()
    var qState = QUDFun(QUD, state)
    factor(alpha*(literalListener(utterance,scope,QUD).score(qState) 
                  - cost(utterance)))
    return utterance
  }})
})

// Pragmatic listener (L1)
var pragmaticListener = cache(function(utterance) {
  Infer({model: function(){
    var state = statePrior();
    var scope = scopePrior();
    var QUD = QUDPrior();
    observe(speaker(scope,state,QUD),utterance);
    return state
  }});
});

// Pragmatic speaker (S2)
var pragmaticSpeaker = cache(function(state) {
  Infer({model: function(){
    var utterance = utterancePrior();
    factor(pragmaticListener(utterance).score(state))
    return utterance
  }})
})

// A speaker decides whether to endorse the ambiguous utterance as a 
// description of the not-all world state
//viz.table(pragmaticSpeaker(0))
viz.table(pragmaticSpeaker(2))
//viz.table(pragmaticSpeaker(2))
//literalListener("surface", 2, "all red?")
~~~~

## Results

### English

####  Utility

We manipulated the parameters of utility as 1 or 10. However, the utility does not affect the endorsement very much.
~~~~
var alpha = 1

var alpha = 10
~~~~

#### Cost

We manipulated the cost of 'not-two' and 'null' in three different ways: (i) the two utterances are equally costly; (2) the 'not-two' utterance is slightly more costly than the 'null' utterance; and (iii) the 'not-two' utterance is much more costly than the 'null' utterance; 

~~~~
var cost = function(utterance){
  return utterance == "not-two" ? 1 : 1
}

var cost = function(utterance){
  return utterance == "not-two" ? 1 : 0
}

var cost = function(utterance){
  return utterance == "not-two" ? 10 : 0
}
~~~~

As expected, the endorsement rate of 'not-two' utterances drops in both experiment 1 and experiment 2, which makes the model prediction more similar to children's pattern. 

#### QUDs

We manipulated the QUDs in four different ways, where all three QUDs are equally likely, or one of the QUDs is more likely than the others.

~~~~
// QUDs
var QUDs = ["how many?","all feed?","none feed?"];
var QUDPrior = function() {
  uniformDraw(QUDs);
}

var QUDPrior = function() {
  categorical({vs:["how many?","all feed?","none feed?"],ps:[10,1,1]})
}

var QUDPrior = function() {
  categorical({vs:["how many?","all feed?","none feed?"],ps:[1,10,1]})
}

var QUDPrior = function() {
  categorical({vs:["how many?","all feed?","none feed?"],ps:[1,1,10]})
}
~~~~

When the QUDs are about whether or not the event has happend ('all feed' or 'none feed'), the endorsement rate of ambiguous sentence in surface scope condition increased to a greater extent than in inverse scope condition, which is similar to children's preference. 

#### State Priors

We manipulated the state priors for English models and to see their impacts on the endorsement rate of the ambiguous "not-two" utterance in model predictions.  The state priors were set up as below for each behavioral experiment, with all the other parameters set up as the default values:

~~~~
// For Expt 1 in Lidz & Musolino (2002)

var states = [0,1,2];

var statePrior = function() {
  categorical({vs:[0,1,2],ps:[10,1,1]})
}

var statePrior = function() {
  categorical({vs:[0,1,2],ps:[1,10,1]})
}

var statePrior = function() {
  categorical({vs:[0,1,2],ps:[1,1,10]})
}

// For Expt 2 in Lidz & Musolino (2002)

var states = [0,1,2,3,4];

var statePrior = function() {
  categorical({vs:[0,1,2,3,4],ps:[10,1,1,1,1]})
}

var statePrior = function() {
  categorical({vs:[0,1,2,3,4],ps:[1,10,1,1,1]})
}

var statePrior = function() {
  categorical({vs:[0,1,2,3,4],ps:[1,1,10,1,1]})
}
~~~~

When state 2 is preferred in the prior, there is a higher endorsement rate in model prediction for the ambiguous "not-two" utterance when the true state is 1 (which favors the surface-scope reading), compared to the scenario where the true state is 2 (which favors the inverse-scope reading) (surface 77.8% vs. inverse 50.5%). This is probably because "not-two" is informative when the true state 1 mismatches the preferred state 2 in the prior. 

If it is state 1 that is preferred in the prior, the model-predicted endorsement rate of "not-two" when the true state is 1 is higher than when the true state is 2 (surface 50.2% vs. inverse 42.9%). One possible explanation is that, although the utterance is supposed to be more informative in general when the true state is 2, which mismatches the preferred state in prior, the utterance "not-two" might not give the listener a better chance to rule out state 1 and to correctly infer that the true state is 2. This is due to the fact that state 2 can only be consistent with the semantics of "not-two" in the inverse scope, which has been shown to be difficult for English speakers to access. Meanwhile, even if the speakers are able to access the inverse scope, state 2 only stands a small semantic space in the meaning (state 0, 1, and 2 are all true conditions for the inverse scope of "not-two"). Therefore, only a very small probability mass will be assigned to state 2 for listeners when they hear the utterance "not-two," possibly leading to the lower endorsement rate that has been observed in our model prediction for pragmatic speakers.

If the state prior is biased towards state 0, there is a lower model-predicted endorsement rate when the true state is 1 in the experiment scenario (surface 30.1% vs. inverse 37.7%). This is counterintuitive as state 1 is consistent with both the surface and the inverse scope, whereas state 2 is only consistent with the inverse scope. Further detailed investigation is needed to explain why there is such a pattern in model predictions when the preferred state is 0 in the prior.

Generally speaking, when it is the state 1 or 2 is preferred in the prior, the model-predicted endorsement rate of the ambiguous "not-two" utterance matches the empirical data observed in Lidz and Musolino (2002), whereby there is a higher endorsement rate on "not-two" when the true state favors the surface-scope reading.

#### Scope Prior

The scope prior was set up as below, with all the other parameters set up as the default values:

~~~~
var scopePrior = function(){ 
  return categorical({vs:["surface", "inverse"],ps:[10,1]})
}

var scopePrior = function(){ 
  return categorical({vs:["surface", "inverse"],ps:[1,10]})
}
~~~~

The simulation shows that, when the prior is biased towards the surface scope, the model-predicted endorsement rate of "not-two" is generally higher for the scenario where the true state is 1, and vice versa. This indicates that, if it is the surface scope preferred in the prior, the model prediction would be able to capture the empirical pattern observed in Lidz and Musolino (2002) regarding the endorsement rate of "not-two". Such a prediction is consistent with our knowledge that the surface scope is more accessible than the inverse scope for English speakers, and implicates that the empirical pattern of English scope ambiguity can be accounted for from the processing perspective.

#### Model with "none" utterance

We also add to the English model a "none" utterance, which corresponds to the sentence such as "I didn't feed any of the rabbits," to see how this alternative utterance would competes with the "not-two" utterance modify its endorsement rate in model predictions. The codes used to simulate the endorsement rate of "not-two" for both Experiment 1 and 2 in Lidz and Musolino (2002) are shown below. We ran the simulation with all the default parameters. Compared to the model without "none" utterance, the endorsement rate of "not-two" decreases when there is an alternative "none" available in the model. On the other hand, the model prediction shows the opposite qualitative pattern: For the model without "none," the endorsement rate of "not-two" is higher when the scenario favors the inverse scope (surface 50.8% vs. inverse 51.5%); for the new model, the predicted endorsement rate is higher when the scenario favors the surface scope (surface 42.2% vs. inverse 41.1%).  


### Mandarin Chinese

#### Utility

We manipulated the utility paramters in a similar way as in the English model. When utility drops from 10 to 1, the endorsement rate in the surface scope condition decreases from 64% to 51%, and the endorsement rate of the inverse scope condition increases from 31% to 32%. It seems like that children are less optimal speakers because the children's pattern fit better to the prediction of low utility model. 

#### Cost

We manipulated the cost such that (i) all utterances are equally costly; (ii) 'not-two' and 'none' utterances are slightly more costly than 'null', (iii) 'not-two' utterance is much more costly, and (iv) 'none' utterance is much more costly.

~~~~
var cost = function(utterance) {
  return utterance == "not-two" ? 1 : 
  utterance == 'none'? 1 : 
  1
}

var cost = function(utterance) {
  return utterance == "not-two" ? 1 : 
  utterance == 'none'? 1 : 
  0
}

var cost = function(utterance) {
  return utterance == "not-two" ? 10 : 
  utterance == 'none'? 1 : 
  0
}

var cost = function(utterance) {
  return utterance == "not-two" ? 1 : 
  utterance == 'none'? 10 : 
  0
}
~~~~

As the cost for utterances increases, the endorsement rate of ambiguous sentences decreases in surface scope condition decreases while it remains steady in the inverse scope condition. Given that children also have a lower endorsement rate in the surface scope condition, it seems that generating a non-null utternance is more effortful for children.

#### QUDs

The QUDs in Chinese model is tuned in the same way as in the English model. Similar to the English model, the endorsement rates in experiment 1 and experiment 2 are most similar to the empirical results from children when QUDs are concerned with 'all' or 'none'. The results suggest that children's endorsement might be related to their under-developing ability of number and counting.

#### State Prior

The manipulation of the state prior proceeded in the same way as the English model, with all the other parameters set up as the default value.

Generally speaking, when the scenario favors the surface scope (true state = 1), there is a higher model-predicted endorsement rate for "not-two," which qualitatively matches the empirical pattern among Mandarin-speaking adults in Su (2008). However, this pattern can never be reversed no matter how the state prior is manipulated. That is to say, manipulating the state prior cannot yield a higher endorsement rate for the scenario that favors the inverse scope (true state = 0), which is the empirical pattern observed among Mandarin-speaking children in Su (2008). Therefore, the empirical pattern on Mandarin scope ambiguity cannot be fully accounted for, not even qualitatively, by the state prior alone.

#### Scope Prior

The scope prior for Mandarin experiments was set up as below, with all the other parameters set up as the default values:

~~~~
var scopePrior = function(){ 
  return categorical({vs:["surface", "inverse"],ps:[100,1]})
}

var scopePrior = function(){ 
  return categorical({vs:["surface", "inverse"],ps:[50,1]})
}

var scopePrior = function(){ 
  return categorical({vs:["surface", "inverse"],ps:[1,50]})
}

var scopePrior = function(){ 
  return categorical({vs:["surface", "inverse"],ps:[1,100]})
}
~~~~



Not surprisingly, if the prior is biased towards the surface scope, the model-predicted endorsement rate of "not-two" would be higher for the scenario that favors the surface scope (true state = 1), which captures the empirical pattern observed among Mandarin-speaking adults in Su (2008). On the contrary, if it is the inverse scope that is preferred in the prior, there is a higher endorsement rate for the scenario that favors the inverse scope (true state = 2), which corresponds to the pattern observed among Mandarin-speaking children. However, although the model prediction does capture  the empirical patterns for both adults and children by manipulating the scope prior, we speculate that the facilitated access to the inverse scope is the underlying reason for the empirical pattern observed among children: It is counterintuitive to assume that children have easier access to the inverse scope, given that the inverse scope is generally difficult to process and that Mandarin Chinese has been argued in the literature to be scope rigid without inverse-scope reading.

#### Utterance Prior

The utterance prior for Mandarin experiments was set up as below, with all the other parameters set up as the default values:

~~~~
var utterancePrior = function() {
  categorical({vs:["null","not-two","none"],ps:[10,1,1]})
}

var utterancePrior = function() {
  categorical({vs:["null","not-two","none"],ps:[1,10,1]})
}

var utterancePrior = function() {
  categorical({vs:["null","not-two","none"],ps:[1,1,10]})
}
~~~~

Generally speaking, if the prior favors the utterance "not-two," there is a higher model-predicted endorsement rate for this ambiguous utterance. However, similar to the effect of state prior, no matter how the utterance prior is manipulated, the endorsement rate is always higher when the scenario favors the surface scope (true state = 1), which matches the empirical pattern of Mandarin-speaking adults. That is to say, the manipulation of utterance prior alone can never qualitatively capture the empirical pattern of Mandarin-speaking children.

#### Alternative meaning of "two"

In previous sections, we assume the literal meaning of "two" as "at least two." In this section, we explore the possibility that the literal meaning of "two" is exactly two. Therefore, the utterance "not-two," instead of being understood as "less than two," now means "not exactly two." Since the scenario in Experiment 1 of Su (2008) has only two rabbits in total, we only revised the meaning function for the simulation of Experiment 2 as below, with all the parameters set up as the default values:

~~~~
// Here is the code of Chinese model for Expt 2 with alternative meaning function.
//different qud prior, different state prior, access to alternative utterances

// Here is the code for the quantifier scope model

// possible utterances
var utterances = ["null","not-two","none"];

var utterancePrior = function() {
  categorical({vs:["null","not-two","none"],ps:[1,1,1]})
}

var cost = function(utterance) {
  return utterance == "not-two" ? 1 : 
  utterance == 'none'? 1 : 
  1
}

// possible world states
var states = [0,1,2,3,4];
var statePrior = function() {
  categorical({vs:[0,1,2,3,4],ps:[1,1,1,1,1]})
}

// possible scopes
var scopePrior = function(){ 
  return categorical({vs:["surface", "inverse"],ps:[100,1]})
}

// meaning function
var meaning = function(utterance, state, scope) {
  return utterance == "none"? state == 0:
    utterance == "not-two" ? 
    scope == "surface" ? (state != 2):
  (state < 3) :
  true;
};

// QUDs
var QUDs = ["how many?","all red?","none red?"];
var QUDPrior = function() {
  categorical({vs:["how many?","all red?","none red?"],ps:[1,1,1]})
  //uniformDraw(QUDs);
}
var QUDFun = function(QUD,state) {
  QUD == "all red?" ? state == 4 :
  QUD == "none red?" ? state == 0 :
  state;
};

// Literal listener (L0)
var literalListener = cache(function(utterance,scope,QUD) {
  Infer({model: function(){
    var state = uniformDraw(states);
    var qState = QUDFun(QUD,state)
    condition(meaning(utterance,state,scope));
    return qState;
  }});
});

var alpha = 1

// Speaker (S)
var speaker = cache(function(scope, state, QUD) {
  return Infer({model: function(){
    var utterance = utterancePrior()
    var qState = QUDFun(QUD, state)
    factor(alpha*(literalListener(utterance,scope,QUD).score(qState) 
                  - cost(utterance)))
    return utterance
  }})
})

// Pragmatic listener (L1)
var pragmaticListener = cache(function(utterance) {
  Infer({model: function(){
    var state = statePrior();
    var scope = scopePrior();
    var QUD = QUDPrior();
    observe(speaker(scope,state,QUD),utterance);
    return state
  }});
});

// Pragmatic speaker (S2)
var pragmaticSpeaker = cache(function(state) {
  Infer({model: function(){
    var utterance = utterancePrior();
    factor(pragmaticListener(utterance).score(state))
    return utterance
  }})
})

// A speaker decides whether to endorse the ambiguous utterance as a 
// description of the not-all world state
//viz.table(pragmaticSpeaker(0))
viz.table(pragmaticSpeaker(2))
//viz.table(pragmaticSpeaker(2))
//literalListener("surface", 2, "all red?")
~~~~

The simulation with the revised meaning function shows that, for the scenario that favors the inverse scope (true state = 2), the model-predicted endorsement rate of "not-two" decreases compared to the model which assumes the literal meaning of "not-two" as "less than two" (less than two 32.1% vs. not exactly two 24.8%). 

### Quantitative simulation of formal semantic account for Mandarin-speaking children

Given that none of the paramters above could explain the Mandarin Chinese children's pattern, we tested how well the Lee (1996) and Huang (1988)'s formal semantic account. Accroding to both analysis, children's surface scope interpretaion of ambiguous sentence is going to be referential: there are two rabbits that the person did not feed. We modified the meaning function for surface scope condition so that the true state of the world is 0 under the surface scope interpretation. 

~~~
// meaning function
var meaning = function(utterance, state, scope) {
  return utterance == "none"? state == 0:
    utterance == "not-two" ? 
    scope == "surface" ? (state ==0):
  (state < 3) :
  true;
};
~~~

For the inverse scope condition, the surface interpretation corresponds to the true state of 2.
~~~
// meaning function
var meaning = function(utterance, state, scope) {
  return utterance == "none"? state == 0:
    utterance == "not-two" ? 
    scope == "surface" ? (state ==2):
  (state < 3) :
  true;
};
~~~

Given the modified meaning function, the endorsement rate of the critical utterance under the surface scope condition (exp1) is 27%, and for the inverse scope condition (exp2) the endorsement rate boosts to 62%. The results are very close to the empirical observation in Mandarin-speaking children. It seems that formal semantics account are powerful explanations for the developmental effects in Mandarin Chinese.

## Conclusions

By comparing the simulation results of RSA models and the empirical observation in both Lidz and Musolino (2002) and Su (2008), we found that the scope ambiguity resolution is beyond formal semantics, but involves complex pragmatic reasoning and developmental trajectory of other general cognitive abilities. 

Our model predict the developmental effects in English, and adults ambiguity resolution in Chinese without having to revise the fundemental conclusion from the previous literal -- that inverse scope is not unavailable in Chinese. Our model confirms that English children's ambiguity resolution pattern is related to their low prior probability over the inverse scope. More importantly, our model emphasizes the role of rational inference and cognitive development in the difference of linguistic preference between adults and children. Adults are more capable of rational inferences about the listener, whereas children are less optimal and considerate during conversation. In addition, utternances are generally more costly for children than for adults, which leads to the developmental effect in both Mandarin and English. Finally, the empirical results could be related to the fact that children have lower prior probability over QUDs concerned with number counting, which might be attributed to children's general cognitive ability. 

The most important highlight of our finding is that none of the parameters alone in our RSA models can capture the empirical pattern among Mandarin-speaking children. We modified the meaning function in a non-traditional way, and the model with a different meaning function successfully simulated the empirical observations from Mandarin-speaking children. Our prediction provides quantitative supports for the formal semantic analysis by Huang (1988) and Lee (1996). We suggest that children might have a different literal understanding of the sentence from adults. Further experimental investigation is thus needed to understand the underlying reason of a non-traditional representation of scope ambiguous sentence in Mandarin-speaking children.

## References

Bergen, L., Levy, R., & Goodman, N. (2016). Pragmatic reasoning through semantic inference. Semantics and Pragmatics, 9.

Frank, M. C., & Goodman, N. D. (2012). Predicting pragmatic reasoning in language games. Science, 336(6084), 998-998.

Goodman, N. D., & Stuhlmüller, A. (2013). Knowledge and implicature: Modeling language understanding as social cognition. Topics in cognitive science, 5(1), 173-184.

Huang, C.-T. James (1988) Wo pao de kuai and Chinese phrase structure. Language, 64, 274-311.

Kao, J. T., Wu, J. Y., Bergen, L., & Goodman, N. D. (2014). Nonliteral understanding of number words. Proceedings of the National Academy of Sciences, 111(33), 12002-12007.

Lassiter, D., & Goodman, N. D. (2013, August). Context, scale structure, and statistics in the interpretation of positive-form adjectives. In Semantics and linguistic theory (Vol. 23, pp. 587-610).

Lidz, J., & Musolino, J. (2002). Children's command of quantification. Cognition, 84(2), 113-154.

Lee, Thomas Hun-tak. (1996) Scope and distributivity in child Mandarin. In E. V. Clark (ed.) The Proceedings of the Twenty-eighth Annual Child Language Research Forum.

Scontras, G., & Pearl, L. S. (2021). When pragmatics matters more for truth-value judgments: An investigation of quantifier scope ambiguity. Glossa: a journal of general linguistics, 6(1).

Scontras, G., Tsai, C. Y. E., Mai, K., & Polinsky, M. (2014). Chinese scope: An experimental investigation. In Proceedings of Sinn und Bedeutung (Vol. 18, pp. 396-414).

Su, Y. C. (2008). Structure and context effects in scope ambiguity resolution. Language and Linguistics, 9(3), 585-627.

Yoon, E. J., Tessler, M. H., Goodman, N. D., & Frank, M. C. (2016). Talking with tact: Polite language as a balance between kindness and informativity. In Proceedings of the 38th annual conference of the cognitive science society (pp. 2771-2776). Cognitive Science Society.