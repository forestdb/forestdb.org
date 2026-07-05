---
layout: model
title: Irony with Shared Background Knowledge
model-language: webppl
model-category: Language and Pragmatics
model-status: code
---

*By Annie, Jon, Stella*

**Introduction to the RSA model**

The standard Rational Speech Acts (RSA) model is a Bayesian framework for modeling pragmatic language understanding. It assumes communication is a cooperative, rational process where speakers choose utterances to be informative given a listener’s beliefs, and listeners infer speaker intent by reasoning about this choice. In its basic form, the RSA model captures literal meaning and simple pragmatic implicature (e.g., scalar implicature like “some” implying “not all”) by recursively modeling how a literal listener (L0) interprets an utterance and how a pragmatic speaker (S1) chooses an utterance to maximize informativeness.
However, this basic RSA model treats agents as purely rational—aiming to optimize informativeness without accounting for affective or emotional nuance.
The irony RSA model (Kao & Goodman, 2015) extends the standard RSA by addressing this limitation: it recognizes that human communication often involves emotional expression, not just factual reporting. It introduces additional variables—valence (emotional positivity/negativity) and arousal (intensity)—into the speaker’s communicative goals. Instead of assuming speakers only care about conveying objective state (e.g., weather), the ironic model assumes they may aim to express frustration, sarcasm, or enthusiasm.


**Empirical phenomena**

*Application: Irony and Communicative Goals*

Verbal irony serves a range of social and communicative functions: it can heighten or soften criticism (Colston, 1997), elicit affective responses (Leggitt & Gibbs, 2000), signal shared social identity (Gibbs, 2000), and express speaker stance (Colston & Keller, 1998). In all cases, listeners rely heavily on contextual information to reason pragmatically when an utterance is intended non-literally and to infer the speaker’s intended meaning accordingly.
Consider the utterance “The weather is terrible” spoken in Newport Beach, California, during a day of clear skies and sunshine. Listeners do not assign high probability to the literal interpretation of this utterance. Instead, given the observable weather state and the assumption of shared contextual access between speaker and listener, comprehenders infer that the speaker is reacting positively and with high emotional arousal to the weather (i.e., excited). This inference is not a fallback on priors alone: under the same world state, the utterance “The weather is okay” is interpreted as conveying neutral valence and low arousal. Thus, the irony model captures the shift in polarity and affective tone that characterizes ironic speech.
By contrast, the hyperbolic utterance “The kettle cost $5000”—though exaggerated—remains compatible with the conventional semantic interpretation of “expensive.” The divergence lies in that ironic statements involve a mismatch between literal content and contextual plausibility, rather than mere scalar strengthening or emphasis. This prompts the central question: how do comprehenders derive coherent, contextually appropriate interpretations when the literal meaning of an utterance does not correspond to the perceived world state?
Kao and Goodman (2015) propose that irony understanding, like hyperbole, can be modeled as pragmatic inference under communicative uncertainty. Crucially, they extend the Question Under Discussion (QUD) framework to account for multiple possible communicative goals that speakers may have when selecting a given utterance. In the case of ironic language, three aspects of the world are taken to be critical: the objective world state (e.g., the actual weather), the speaker’s valence (positive or negative attitude toward the state), and the speaker’s arousal (the intensity with which they feel that attitude).


**Piece-by-piece Breakdown of Model**

states and statePriors
As mentioned in the empirical phenomena, there are three possible states to describe the weather: “terrible”, “ok”, and “amazing”. Let’s use the infer model to display the state priors to gather a better understanding of this model.


~~~~
var states = ['terrible', 'ok', 'amazing']

var statePrior = function() {
  categorical([1, 50, 50], states)
}

viz.hist(Infer({model:function(){
  return statePrior();
}}))
~~~~

As you can see, the states “amazing” and “ok” have a much higher probability compared to the state “terrible”. This is because the states are drawn from a categorical distribution. As this example is set in California, the priors are skewed more towards generally positive weather as California weather is more commonly described as “amazing” and/or “ok” compared to “terrible”.

valence and valencePriors
As we move through understanding the code, the next step is tackling valence. Valence is used in this model to describe how the speaker (or whoever can perceive the actual weather) thinks about the weather. A valence of -1 is associated with negative feelings towards the weather, while a valence of 1 is associated with positive feelings towards the weather. Let’s take a look at the valence code in our model:

~~~~
var valencePrior = function(state) {
  state === "terrible" ? flip(0.99) ? -1 : 1 :
  state === "ok" ? flip(0.5) ? -1 : 1 :
  state === "amazing" ? flip(0.01) ? -1 : 1 :
  true
}

display("valence distribution for 'terrible'")
viz.table(Infer({model:function(){
  return valencePrior("terrible");
}}))

display("valence distribution for 'ok'")
viz.table(Infer({model:function(){
  return valencePrior("ok");
}}))

display("valence distribution for 'amazing'")
viz.table(Infer({model:function(){
  return valencePrior("amazing");
}}))
~~~~

When we run this code, we can see how the speaker generally feels towards each state. They generally feel negative towards “terrible” weather, uncertain about their feelings towards “ok” weather, and generally positive towards “amazing” weather.

arousal and arousalPriors
The third element that is used to display irony is arousal, the level of feeling towards this subject. Essentially, this models if the speaker cares or doesn’t care about what they’re communicating about. 

~~~~
var arousals = ["low", "high"]
var arousalPrior = function(state) {
  state === "terrible" ? categorical([0.1, 0.9], arousals) :
  state === "ok" ? categorical([0.9, 0.1], arousals) :
  state === "amazing" ? categorical([0.1, 0.9], arousals) :
  true
}

display("arousal distribution for 'terrible'")
viz.hist(Infer({model:function(){
  return arousalPrior("terrible");
}}))

display("arousal distribution for 'ok'")
viz.hist(Infer({model:function(){
  return arousalPrior("ok");
}}))

display("arousal distribution for 'amazing'")
viz.hist(Infer({model:function(){
  return arousalPrior("amazing");
}}))
~~~~

We can see that arousal is high when communicating the states “terrible” and “amazing”, which can be intuitive as “ok” is often used in conversation when the speaker does not want to communicate interest. 

goals and goalPriors & utterances and utterancePriors
Now let’s take a look at the goals and utterances of our code. The goal is what the speaker is trying to convey to the listener, and for our model, there are three possible goals. “goalState” communicates the state of the weather, “goalValence” communicates whether they like or dislike the weather, and “goalArousal” communicates how strongly they feel about the weather. To view the goalPrior, we're using an infer histogram model, similar to the ones we’ve used previously in this code breakdown. This shows they have equal probability as their priors are all equal.

Utterances in our model are essentially the same as states; however, utterances are the actual phrases being spoken by the speaker and heard by the listener. To view this prior, we also use an infer histogram model, which shows that these utterances have equal probabilities.

~~~~
var states = ['terrible', 'ok', 'amazing']

var goals = ["goalState", "goalValence", "goalArousal"]
var goalPrior = function() {
  categorical([1, 1, 1], goals)
}

var utterances = states
var utterancePrior = function() {
  uniformDraw(utterances)
}

viz.hist(Infer(goalPrior))
viz.hist(Infer({model:function(){
  return utterancePrior();
}}))
~~~~

**literalListener**

Let’s skip ahead a bit to discuss the literal listener, which literally interprets what the state is based on the utterance they hear and the goal the speaker wants to communicate with them.


~~~~
var states = ['terrible', 'ok', 'amazing']

var statePrior = function() {
  categorical([1, 50, 50], states)
}

var valencePrior = function(state) {
  state === "terrible" ? flip(0.99) ? -1 : 1 :
  state === "ok" ? flip(0.5) ? -1 : 1 :
  state === "amazing" ? flip(0.01) ? -1 : 1 :
  true
}

var arousals = ["low", "high"]

var goals = ["goalState", "goalValence", "goalArousal"]

var goalPrior = function() {
  categorical([1, 1, 1], goals)
}

var utterances = states

var utterancePrior = function() {
  uniformDraw(utterances)
}

var arousalPrior = function(state) {
  state === "terrible" ? categorical([0.1, 0.9], arousals) :
  state === "ok" ? categorical([0.9, 0.1], arousals) :
  state === "amazing" ? categorical([0.1, 0.9], arousals) :
  true
}

var literalInterpretation = function(utterance, state) {
  utterance === state
}

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

viz.table(literalListener("terrible","goalState"))
viz.table(literalListener("terrible","goalValence"))
viz.table(literalListener("terrible","goalArousal"))
~~~~

Note how these results match the priors!

speaker

Now let’s see how the speaker behaves. 

~~~~
var states = ['terrible', 'ok', 'amazing']

var statePrior = function() {
  categorical([1, 50, 50], states)
}

var valencePrior = function(state) {
  state === "terrible" ? flip(0.99) ? -1 : 1 :
  state === "ok" ? flip(0.5) ? -1 : 1 :
  state === "amazing" ? flip(0.01) ? -1 : 1 :
  true
}

var arousals = ["low", "high"]

var goals = ["goalState", "goalValence", "goalArousal"]

var goalPrior = function() {
  categorical([1, 1, 1], goals)
}

var utterances = states

var utterancePrior = function() {
  uniformDraw(utterances)
}

var arousalPrior = function(state) {
  state === "terrible" ? categorical([0.1, 0.9], arousals) :
  state === "ok" ? categorical([0.9, 0.1], arousals) :
  state === "amazing" ? categorical([0.1, 0.9], arousals) :
  true
}

var literalInterpretation = function(utterance, state) {
  utterance === state
}

var goalState = function(goal, state, valence, arousal) {
  goal === "goalState" ? state :
  goal === "goalValence" ? valence :
  goal === "goalArousal" ? arousal :
  true
}

var literalListener = function(utterance, goal) {
  Infer({model: function(){
    var state = uniformDraw(states)
    var valence = valencePrior(state)
    var arousal = arousalPrior(state)
    condition(literalInterpretation(utterance,state))
    return goalState(goal, state, valence, arousal)
  }})
}

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

viz.table(speaker("terrible", 1, "high", "goalValence"))
//viz.table(speaker("ok", 1, "high", "goalArousal"))
~~~~

Take a look at the utterance the speaker has the highest probability of saying compared to the actual state. Why do these not match? – This is because of the goal. The goal the speaker is trying to communicate to the listener is “goalValence”, so although the state is terrible, they want to communicate their high valence, therefore placing more probability on uttering a high valence utterance, “amazing”. We can further see the speaker’s loyalty to the goal with the final commented input. Even if the state is ‘ok’, the speaker almost never says ok because that doesn’t communicate a high arousal. 

**pragmaticListener**

Lastly, let’s cover the last part of the code, the pragmaticListener.


~~~~
var states = ['terrible', 'ok', 'amazing']

var statePrior = function() {
  categorical([1, 50, 50], states)
}

var valencePrior = function(state) {
  state === "terrible" ? flip(0.99) ? -1 : 1 :
  state === "ok" ? flip(0.5) ? -1 : 1 :
  state === "amazing" ? flip(0.01) ? -1 : 1 :
  true
}

var arousals = ["low", "high"]

var goals = ["goalState", "goalValence", "goalArousal"]

var goalPrior = function() {
  categorical([1, 1, 1], goals)
}

var utterances = states

var utterancePrior = function() {
  uniformDraw(utterances)
}

var arousalPrior = function(state) {
  state === "terrible" ? categorical([0.1, 0.9], arousals) :
  state === "ok" ? categorical([0.9, 0.1], arousals) :
  state === "amazing" ? categorical([0.1, 0.9], arousals) :
  true
}

var literalInterpretation = function(utterance, state) {
  utterance === state
}

var goalState = function(goal, state, valence, arousal) {
  goal === "goalState" ? state :
  goal === "goalValence" ? valence :
  goal === "goalArousal" ? arousal :
  true
}

var literalListener = function(utterance, goal) {
  Infer({model: function(){
    var state = uniformDraw(states)
    var valence = valencePrior(state)
    var arousal = arousalPrior(state)
    condition(literalInterpretation(utterance,state))
    return goalState(goal, state, valence, arousal)
  }})
}

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
var terribleDist = pragmaticListener("terrible")
var amazingDist = pragmaticListener("amazing")
var okDist = pragmaticListener("ok")




viz.table(pragmaticListener("terrible"))
viz.marginals(terribleDist)
~~~~

The pragmaticListener thinks critically of the utterance they hear, the priors they have, and what they believe the speaker’s goal to be. The output of this code is a distribution table over states, valences, and arousals to display the pragmatic listener’s beliefs of each outcome. Now that we have a full model, we can see the element of irony in the output of the model. When we run the model given the utterance “terrible”, we can see that the most probable state is a speaker who is enthusiastically enjoying amazing weather. Thus the utterance ‘terrible’ is ironic. This occurs because the likelihood of a terrible weather state is so low that the speaker is more likely to be communicating a valence or arousal level associated with the utterance ‘terrible’. The last line outputs a marginal distribution for ‘terrible’ (swap it with okDist or amazingDist to see distributions of the other utterances). Now we can see the probability of particular features (arousal, state, and valence) for our inputted utterance. 

**Critiques of the Model**

*Changing statePrior*

Although the model does display irony when the pragmatic listener hears the utterance “terrible”, let’s take a closer look at the statePriors to help us understand why this irony only occurs with this utterance. Let’s say we change the priors to be the ones below:


~~~~
var states = ['terrible', 'ok', 'amazing']

var statePrior = function() {
  categorical([50, 50, 50], states)
}

viz.hist(Infer({model:function(){
  return statePrior();
}}))
~~~~

  *What if there is no Arousal Prior?*

Some would argue arousal has too much of an impact on the results. Experiment with the model below which has the arousal component removed. 



~~~~
/// 
//There are three possible states the weather could be in: 
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


var goals = ["goalState", "goalValence"]

var goalPrior = function() {
  categorical([1, 1], goals)
}

// Assume possible utterances are identical to possible states
var utterances = states

// Assume cost of utterances is uniform.
var utterancePrior = function() {
  uniformDraw(utterances)
}



// Literal interpretation is just whether utterance equals state
var literalInterpretation = function(utterance, state) {
  utterance === state
}

// A speaker's goal is satisfied if the listener infers the correct 
// and relevant information.
var goalState = function(goal, state, valence) {
  goal === "goalState" ? state :
  goal === "goalValence" ? valence :
  true
}

// Define a literal listener
var literalListener = function(utterance, goal) {
  Infer({model: function(){
    var state = uniformDraw(states)
    var valence = valencePrior(state)
    
    condition(literalInterpretation(utterance,state))
    return goalState(goal, state, valence)
  }})
}

// Define a speaker
var speaker = function(state, valence, goal) {
  Infer({model: function(){
    var utterance = utterancePrior()
    factor(1 * literalListener(utterance, 
                    goal).score(goalState(goal, 
                                          state, 
                                          valence 
                                        )))
    return utterance
  }})
}
///

// Define a pragmatic listener
var pragmaticListener = function(utterance) {
  Infer({model: function(){
    var state = statePrior()
    var valence = valencePrior(state)

    var goal = goalPrior()
    observe(speaker(state, valence, goal),utterance)
    return {state, valence}
  }})
}

viz.table(pragmaticListener("terrible"))

viz.table(pragmaticListener("ok"))

viz.table(pragmaticListener("amazing"))
~~~~

**Similarities Between Hyperbole and Irony Models**

Both the hyperbole and irony models explain nonliteral language interpretation through the lens of pragmatic inference, modeling how listeners infer a speaker’s underlying communicative goals. Despite their different surface strategies, both models assume:

*Emotion as a communicative goal*

- In both models, S1 chooses utterances to maximize the listener’s belief in a goal-relevant interpretation—typically one that conveys affective meaning.

*Gricean Maxim of Quality violations*

- Both forms of nonliteral speech violate the expectation of truthfulness, compelling the listener to infer a deeper communicative intent beyond the literal meaning.

*Dependence on contextual priors*

- Listeners use world knowledge and prior probabilities to determine when a statement is likely nonliteral, adjusting their interpretation accordingly.

Both the hyperbole and irony models fundamentally rely on shared context and contextual adjustment, enabling listeners to infer the speaker’s intended meaning beyond the literal utterance. In both models, the speaker and listener are assumed to share background knowledge—modeled as categorical priors—which guides interpretation.

In the hyperbole model, the listener draws on prior knowledge of real-world prices (e.g., knowing that an electric kettle rarely costs $10,000) and emotional cues to infer the speaker’s intended affect (e.g., frustration) and approximate the true price. This process reflects a downward adjustment from the exaggerated utterance to a plausible state.

Similarly, the irony model uses contextual priors over states like weather, where positive or neutral conditions (e.g., “OK” or “amazing”) are more probable than “terrible.” When the speaker says something that contradicts this likely context (e.g., calling sunny weather “terrible”), the listener interprets it as ironic—inferring a positive state with negative phrasing.

Although both models assume shared categorical priors between speaker and listener and require listeners to adjust from literal utterances to contextually plausible meanings, they differ in that the hyperbole model adjusts in a unidirectional, scalar manner (e.g., exaggerating quantity), while the irony model infers a bidirectional, oppositional shift—interpreting statements as meaning the opposite of their literal form.

In real-world communication, this flexibility in interpreting nonliteral language highlights how irony and hyperbole depend on a listener’s ability to use context, probability, and shared experience to uncover the speaker’s true intent (Question under discussion: QUD).
Specifically, in the hyperbole model, the utterance interpretation depends on three key variables:

- Price, valence, and QUD form the fullState.

QUDs focus the listener’s attention on approximate price, emotional valence, or both (e.g., approxPrice, priceValence).
In the irony model, interpretation considers:

- State, valence, arousal, and goal, with goal replacing QUD to reflect broader communicative intent.


**Contextual Flexibility**

Both models allow for contextual flexibility. Because QUDs may vary across conversational settings, the same utterance may receive different interpretations depending on the speaker’s identity and shared background assumptions. For instance, a Toronto native describing the weather as “amazing” may be interpreted more ironically than a Californian using the same word under the same conditions. These differences are captured by manipulating state priors.


~~~~
var states = ['terrible', 'ok', 'amazing'];
var statePrior = function() {
  categorical([1, 50, 50], states);
};
~~~~

**Irony Model**

*Changing the State Prior to Model Regional Differences*

The original irony model used an arbitrary prior favoring “ok” and “amazing” weather, assuming speaker and listener are from California. Here, the utterance “The weather is terrible!” strongly triggers irony—listener infers positive weather state despite the negative utterance.

If we instead assume speaker and listener are from London or Canada, where terrible weather is frequent, we reverse the prior:



~~~~
// There are three possible states the weather could be in: 
// terrible, ok, or amazing
var states = ['terrible', 'ok', 'amazing']

// In a context where terrible weather is common, the prior
// gives that state much more weight than amazing weather.
var statePrior = function() {
  categorical([50, 50, 1], states)
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
    return {state, valence, arousal}
  }})
}

// Visualize state posterior

print("State Posterior:");
var listenerPosterior = pragmaticListener("terrible");
var statePosteriorDist = marginalize(listenerPosterior, "state");
viz.hist(statePosteriorDist);

// Visualize state prior

print("State Prior:")
viz.hist(Infer(function() {
  return statePrior()
}))
~~~~

Now, the ironic interpretation fades. The listener takes the utterance at face value: it really is terrible out. The probability assigned to the literal weather state (“terrible”) increases significantly.

**Hyperbole Model**

*Changing the State Prior to Model Inflation*

In the original hyperbole model, participants were asked how likely someone would be to buy an item (e.g., electric kettle, laptop, or watch) at various prices without any linguistic input. This yielded a price prior, operationalized as the likelihood of purchase.

If we simulate inflation by increasing the likelihood of high prices in the prior (e.g., the electric kettle being $10,000), the utterance “This kettle cost $10,000!” is less likely to be interpreted hyperbolically. Now the literal meaning is more plausible.

~~~~
// Round x to nearest multiple of b (used for approximate interpretation):
var approx = function(x,b) {
  var b = 10
  return b * Math.round(x / b)
}

// Here is the code from the Kao et al. hyperbole model
// Prior probability of kettle prices (taken from human experiments)
var prices = [
  50, 51,
  500, 501,
  1000, 1001,
  5000, 5001,
  10000, 10001
]
var pricePrior = function() {
  return categorical({
    vs: prices,
    ps: [
      0.0205, 0.0065,
      0.1533, 0.1538,
      0.1223, 0.1211,
      0.1012, 0.1011,
      0.1183, 0.1020
    ]
  })
}

/// fold:
//Probability that given a price state, the speaker thinks it's too
// expensive (taken from human experiments)
var valencePrior = function(state) {
  var probs = {
    50 : 0.3173,
    51 : 0.3173,
    500 : 0.7920,
    501 : 0.7920,
    1000 : 0.8933,
    1001 : 0.8933,
    5000 : 0.9524,
    5001 : 0.9524,
    10000 : 0.9864,
    10001 : 0.9864
  }
  var tf = flip(probs[state])
  return tf
}

// Literal interpretation "meaning" function;
// checks if uttered number reflects price state
var meaning = function(utterance, price) {
  return utterance == price
}

var qudFns = {
  price : function(state) {return { price: state.price } },
  valence : function(state) {return { valence: state.valence } },
  priceValence : function(state) {
    return { price: state.price, valence: state.valence }
  },
  approxPrice : function(state) {return { price: approx(state.price) } },
  approxPriceValence: function(state) {
    return { price: approx(state.price), valence: state.valence  }
  }
}
///

// Prior over QUDs
var qudPrior = function() {
  categorical({
    vs: ["price", "valence", "priceValence", "approxPrice", "approxPriceValence"],
    ps: [1, 1, 1, 1, 1]
  })
}

// Define list of possible utterances (same as price states)
var utterances = [
  50, 51,
  500, 501,
  1000, 1001,
  5000, 5001,
  10000, 10001
]
var utterancePrior = function() {
  return  uniformDraw(utterances)
}

// precise numbers can be assumed to be costlier than round numbers
var preciseNumberCost = 1
var cost = function(utterance){
  return utterance == approx(utterance) ? // if it's a round number utterance
    0 : // no cost
  preciseNumberCost // cost of precise numbers (>= 0)
}

// Literal listener, infers the qud answer assuming the utterance is
// true of the state
var literalListener = cache(function(utterance, qud) {
  return Infer({model: function(){
    var price = uniformDraw(prices)
    var valence = valencePrior(price)
    var fullState = {price, valence}
    var qudFn = qudFns[qud]
    var qudAnswer = qudFn(fullState)
    condition( meaning(utterance, price) )
    return qudAnswer
  }
               })})

// set speaker optimality
var alpha = 1

// Speaker, chooses an utterance to convey a particular answer of the qud
var speaker = cache(function(fullState, qud) {
  return Infer({model: function(){
    var utterance = utterancePrior()
    var qudFn = qudFns[qud]
    var qudAnswer = qudFn(fullState)
    factor(alpha*(literalListener(utterance,qud).score(qudAnswer) 
                  - cost(utterance)))
    return utterance
  }})
})

// Pragmatic listener, jointly infers the price state, speaker valence, and QUD
var pragmaticListener = cache(function(utterance) {
  return Infer({model: function(){
    //////// priors ////////
    var price = pricePrior()
    var valence = valencePrior(price)
    var qud = qudPrior()
    ////////////////////////
    var fullState = {price, valence}
    observe(speaker(fullState, qud), utterance)
    return fullState
  }})
})

var listenerPosterior = pragmaticListener(10000)

print("pragmatic listener's joint interpretation of 'The kettle cost $10,000':")
viz(listenerPosterior)
///

// print("marginal distributions:")
// viz.table(marginalize(listenerPosterior, "price"))
// viz.hist(marginalize(listenerPosterior, "valence"))


Infer(function(){
var price = pricePrior();
var valence = valencePrior(price);
return {price: price, valence: valence}
})
~~~~

Much like the irony model, under this shift in prior distribution for states, both S1 (speaker) and L1 (listener) converge on a literal interpretation. The hyperbolic effect diminishes.