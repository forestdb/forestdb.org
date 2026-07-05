---
layout: model
title: Ambiguity Resolution in Non-Native Language Understanding
model-language: webppl
model-category: Language and Pragmatics
model-status: code
---



# Modeling Ambiguity Resolution in Non-Native Language Understanding

**Authors**: Seojin Hwang and Moldir Baidildinova  
**Course**: Probabilistic Language Understanding  
**Date**: June 12, 2025

## The Empirical Phenomenon of Interest

In everyday communication, native and non-native speakers often encounter ambiguity in language. One striking example is the utterance "Do you like Shirley Temple?", where the expression "Shirley Temple" can refer to either a person (the actress) or a drink (a non-alcoholic mixed drink). For native speakers, context and world knowledge help disambiguate; for non-native speakers, especially those with *limited* exposure or familiarity, interpretation is much more uncertain.

This project explores **how non-native speakers resolve ambiguity** in cases where they lack full access to intended meanings (or interpretations). We ask: **"How do speakers and listeners manage ambiguity when linguistic experience and interpretative priors differ?"**

## Description of the Model

To investigate ambiguity resolution, we construct a probabilistic model using *Rational Speech Act* (RSA) principles. The RSA framework models communication as a process of recursive social reasoning between speakers and listeners. A pragmatic listener (L₁) infers the intended meaning by reasoning about a cooperative speaker (S₁), who in turn selects utterances that would help a literal listener (L₀) recover the intended state. This layered inference structure is formalized using Bayesian reasoning, capturing the interplay between literal meaning and pragmatic interpretation. For more details, see [Chapter 1: Introduction](https://www.problang.org/chapters/01-introduction.html). The key components of the model are as follows.

### Participants

- **L₀**: Literal Listener (non-native speaker)

- **S₁**: Speaker (native speaker, reasons about L₀)

- **L₁**: Pragmatic Listener (native or highly proficient non-native listener)

### Key Components

- **Utterances**: `"shirleyTemple"` (ambiguous, cost = 1), `"shirleyTemple person"` (unambiguous, cost = 2), `"shirleyTemple drink"` (unambiguous, cost = 2)

- **States**: `"person"`, `"drink"`

- **Interpretations**: `"thePerson"`, `"theDrink"`

- **Noises**: the probability of misinterpreting the meaning; `personNoise`, `drinkNoise`

### Meaning Function

The **meaning function** models how an utterance maps to a state given an interpretation. 

### Listener and Speaker Functions

- **L₀:** Infers the state from a literal reading of the utterance.

- **S₁:** Chooses utterances to communicate a state effectively to L₀, balancing informativeness and cost.

- **L₁:** Infers both the intended state and interpretation using pragmatic reasoning.



## Similarities to Models Covered in Class

Our model closely resembles the RSA models described in [Chapter 4: Jointly inferring parameters and interpretations](https://www.problang.org/chapters/04-ambiguity.html) and in [Scontras & Pearl (2021)](https://www.glossa-journal.org/article/id/5724/).

### Component Comparisons Between Models

 **L₀**
  - *Original RSA Model*: Interprets using state + scope + QUD
  - *Our Model*: Interprets using state + interpretation

**S₁**
  - *Original RSA Model*: Communicates state (scope + QUD)
  - *Our Model*: Communicates state + interpretation

**L₁**
  - *Original RSA Model*: Infers state and scope
  - *Our Model*: Infers state and interpretation

**Utterance Prior**
  - *Both Models*: Uniform

**State Prior**
  - *Both Models*: Uniform

**Cost Function**
  - *Original RSA Model*: Fixed
  - *Our Model*: Per-utterance cost

**Alpha**
  - *Both Models*: Speaker optimality parameter (same)

The code of the simple model is given below: 

~~~~
// Model without Noise: Similar to Ch 4 Ambiguity Resolution Model

// possible utterances
var utterances = ["shirleyTemple", "shirleyTemple person", "shirleyTemple drink"]
var utterancePrior = function() {
  uniformDraw(utterances)
}

// utterance cost
var cost = {
  "shirleyTemple": 1,
  "shirleyTemple person": 2,
  "shirleyTemple drink": 2
}

// possible world states
var states = ["person", "drink"]
var statePrior = function() {
   uniformDraw(states)
}

// possible interpretation
var interpretations = ["thePerson", "theDrink"]
var interpretationPrior = function() {
   uniformDraw(interpretations)
}

// meaning function
var meaning = function(utterance, state, interpretation) {
  return utterance == "shirleyTemple"
    ? (interpretation == "thePerson" ? state == "person" : state == "drink") :
  utterance == "shirleyTemple person" ? state == "person" : 
  utterance == "shirleyTemple drink" ? state == "drink" :
  false;
}

// Literal listener (L0) = non-native listener
var literalListener = cache(function(utterance, interpretation) {
  return Infer({model: function(){
    var state = uniformDraw(states)
    condition(meaning(utterance, state, interpretation))
    return state
  }})
})

var alpha = 1

// Speaker 1 (S1) = native speaker
var speaker = cache(function(interpretation, state) {
  return Infer({model: function(){
    var utterance = uniformDraw(utterances)
    factor(alpha*(literalListener(utterance, interpretation).score(state)
                  - cost[utterance]))
    return utterance
  }})
})

// Pragmatic listener (L1) = proficient non-native listener or native listener
var pragmaticListener = cache(function(utterance) {
  return Infer({model: function(){
    var state = statePrior()
    var interpretation = interpretationPrior()
    observe(speaker(interpretation, state), utterance)
    return {state: state, int: interpretation}
  }})
})


print("State prior:")
viz(Infer(statePrior))

print("Literal listener's posterior over utterance 'shirleyTemple' and interpretation 'thePerson':")
viz(literalListener("shirleyTemple", "thePerson"))
print("Literal listener's posterior over utterance 'shirleyTemple' and interpretation 'theDrink':")
viz(literalListener("shirleyTemple", "theDrink"))

print("Speaker's posterior over interpretation 'thePerson' and state 'person':")
viz(speaker("thePerson", "person"))
print("Speaker's posterior over interpretation 'theDrink' and state 'drink':")
viz(speaker("theDrink", "drink"))

print("Pragmatic listener's posterior over utterance 'shirleyTemple':")
viz.marginals(pragmaticListener("shirleyTemple"))
~~~~

As we can see from the model predictions, **L₀** is highly biased toward only one interpretation (either `thePerson` or `theDrink`) and lacks access to both possible states. Despite this bias, **S₁** chooses the ambiguous utterance to communicate the state `person` or the state `drink`. In contrast, **L₁**, given their higher proficiency and background knowledge, can successfully access both interpretations and infer the intended state.

While we would expect L₀ to make noisier predictions over both possible states rather than exhibiting strong bias toward only one, the model instead predicts a highly skewed interpretation. Likewise, our intuition expects S₁ to prefer unambiguous utterances when communicating with a less proficient listener like L₀. However, the model predicts that S₁ favors ambiguous utterances in all situations, regardless of L₀’s inferred proficiency.

## Differences: Key Extensions and Predictions

### (1) Introduction of Noise
We model the non-native speaker’s uncertainty by adding **noise** to the meaning function. This allows for probabilistic misinterpretation of both ambiguous and unambiguous utterances and provides a better fit for real-world variability in comprehension. For example,

- Low `personNoise` → L₀ prefers interpreting “Shirley Temple” as a person.

- High `drinkNoise` → L₀ is less likely to interpret “Shirley Temple” as a drink.

#### Updated Predictions
- **L₀** tends to misinterpret ambiguous utterances depending on noise levels.

- **S₁** may choose unambiguous utterances when high noise could lead to misinterpretation.

- **L₁**, with low noise for both interpretations, correctly interprets ambiguous utterances.

We kept everything the same as the model without noise in utterances, costs, states and interpretations. Noises are passed into `meaning function`, L₀, S₁, and L₁.

~~~~
// Extended Model with Noise

///fold:
// possible utterances
var utterances = ["shirleyTemple", "shirleyTemple person", "shirleyTemple drink"]
var utterancePrior = function() {
  uniformDraw(utterances)
}

// utterance cost
var cost = {
  "shirleyTemple": 1,
  "shirleyTemple person": 2,
  "shirleyTemple drink": 2
}

// possible world states
var states = ["person", "drink"]
var statePrior = function() {
   uniformDraw(states)
}

// possible interpretation
var interpretations = ["thePerson", "theDrink"]
var interpretationPrior = function() {
   uniformDraw(interpretations)
}
///

// adding noise
// meaning function
var meaning = function(utterance, state, interpretation, personNoise, drinkNoise) {
  return utterance == "shirleyTemple" 
    ? (interpretation == "thePerson" ? 
       state == "person" ? flip(1-personNoise) : flip(0+personNoise) :
       state == "drink" ? flip(1-drinkNoise) : flip(0+drinkNoise)) :
    utterance == "shirleyTemple person" ? state == "person" :
    utterance == "shirleyTemple drink" ? state == "drink" :
   false;
}

// Literal listener (L0) = non-native listener
var literalListener = cache(function(utterance, interpretation, personNoise, drinkNoise) {
  return Infer({model: function(){
    var state = uniformDraw(states)
    condition(meaning(utterance, state, interpretation, personNoise, drinkNoise))
    return state
  }})
})

var alpha = 1

// Speaker 1 (S1) = native speaker
var speaker = cache(function(interpretation, state, personNoise, drinkNoise) {
  return Infer({model: function(){
    var utterance = uniformDraw(utterances)
    factor(alpha*(literalListener(utterance, interpretation, personNoise, drinkNoise).score(state) - cost[utterance]))
    return utterance
  }})
})

// Pragmatic listener (L1) = proficient non-native listener or native listener
var pragmaticListener = cache(function(utterance, personNoise, drinkNoise) {
  return Infer({model: function(){
    var state = statePrior()
    var interpretation = interpretationPrior()
    observe(speaker(interpretation, state, personNoise, drinkNoise), utterance)
    return {state: state, int: interpretation}
  }})
})


// Predictions
print("Literal listener's posterior over utterance 'shirleyTemple' and interpetation 'thePerson' with high noise on 'drink':")
viz(literalListener("shirleyTemple", "thePerson", 0.2, 0.7))
print("Literal listener's posterior over utterance 'shirleyTemple' and interpetation 'theDrink' with high noise on 'drink':")
viz(literalListener("shirleyTemple", "theDrink", 0.2, 0.7))

print("Literal listener's posterior over utterance 'shirleyTemple person' and interpetation 'thePerson' with high noise on 'drink':")
viz(literalListener("shirleyTemple person", "thePerson", 0.2, 0.7))
print("Literal listener's posterior over utterance 'shirleyTemple drink' and interpetation 'theDrink' with high noise on 'drink':")
viz(literalListener("shirleyTemple drink", "theDrink", 0.2, 0.7))

print("Speaker's posterior over interpretation 'thePerson' and state 'person' with high noise on 'drink':")
viz(speaker("thePerson", "person", 0.2, 0.7))
print("Speaker's posterior over interpretation 'theDrink' and state 'drink' with high noise on 'drink':")
viz(speaker("theDrink", "drink", 0.2, 0.7))

print("Pragmatic listener's posterior over utterance 'shirleyTemple' with low noise on both states:")
viz.marginals(pragmaticListener("shirleyTemple", 0.1, 0.1))
~~~~

When **L₀** hears an ambiguous utterance, they are more likely to interpret it as the state with lower noise, regardless of the intended interpretation. In contrast, when L₀ hears an unambiguous utterance, they correctly infer the intended state with 100% accuracy.

With this kind of L₀ in mind, **S₁** is more likely to choose an ambiguous utterance when trying to communicate an interpretation with low noise, and an unambiguous utterance when trying to communicate an interpretation with high noise. This aligns well with our intuition.

In the case of **L₁**, since the noise is equally low for both interpretations, the posterior over state and interpretation remains balanced when they hear an ambiguous utterance.



### (2) Addition of S₂ Layer

We extend the model by introducing **S₂**, a speaker who reasons about L₁. This layer allows us to capture more nuanced pragmatic reasoning, as S₂ balances informativeness with cost knowing L₁ can handle ambiguity.

~~~~
// Extended Model with Noise and S2

///fold:
// possible utterances
var utterances = ["shirleyTemple", "shirleyTemple person", "shirleyTemple drink"]
var utterancePrior = function() {
  uniformDraw(utterances)
}

// utterance cost
var cost = {
  "shirleyTemple": 1,
  "shirleyTemple person": 2,
  "shirleyTemple drink": 2
}

// possible world states
var states = ["person", "drink"]
var statePrior = function() {
   uniformDraw(states)
}

// possible interpretation
var interpretations = ["thePerson", "theDrink"]
var interpretationPrior = function() {
   uniformDraw(interpretations)
}

// adding noise
// meaning function
var meaning = function(utterance, state, interpretation, personNoise, drinkNoise) {
  return utterance == "shirleyTemple" 
    ? (interpretation == "thePerson" ? 
       state == "person" ? flip(1-personNoise) : flip(0+personNoise) :
       state == "drink" ? flip(1-drinkNoise) : flip(0+drinkNoise)) :
    utterance == "shirleyTemple person" ? state == "person" :
    utterance == "shirleyTemple drink" ? state == "drink" :
   false;
}

// Literal listener (L0) = non-native listener
var literalListener = cache(function(utterance, interpretation, personNoise, drinkNoise) {
  return Infer({model: function(){
    var state = uniformDraw(states)
    condition(meaning(utterance, state, interpretation, personNoise, drinkNoise))
    return state
  }})
})

var alpha = 1

// Speaker 1 (S1) = native speaker
var speaker = cache(function(interpretation, state, personNoise, drinkNoise) {
  return Infer({model: function(){
    var utterance = uniformDraw(utterances)
    factor(alpha*(literalListener(utterance, interpretation, personNoise, drinkNoise).score(state) - cost[utterance]))
    return utterance
  }})
})

// Pragmatic listener (L1) = proficient non-native listener or native listener
var pragmaticListener = cache(function(utterance, personNoise, drinkNoise) {
  return Infer({model: function(){
    var state = statePrior()
    var interpretation = interpretationPrior()
    observe(speaker(interpretation, state, personNoise, drinkNoise), utterance)
    return {state: state, int: interpretation}
  }})
})
///

// adding S2 layer
// Speaker 2 (S2) = native speaker
var pragmaticSpeaker = cache(function(state, personNoise, drinkNoise) {
    Infer({model: function(){
    var utterance = utterancePrior();
    factor(marginalize(pragmaticListener(utterance, personNoise, drinkNoise), "state").score(state) 
           - cost[utterance])
    return utterance
  }})
})


// Predictions
print("Pragmatic speaker's posterior over state 'drink' with low noise on both states:")
viz(pragmaticSpeaker("drink", 0.1, 0.1))
~~~~

We can see that **S₂** prefers ambiguous utterances if L₁ is fully competent and cost matters.

## Considering other possible outcomes

### 1. What if noise also applies to the unambiguous utterances?

To examine what happens when noise is applied to *unambiguous utterances* like `shirleyTemple person` or `shirleyTemple drink`, we modified **the meaning function** in the model with noise and observed the behavior of L₀ and S₁.

We kept the noise levels as before, `personNoise` = 0.2 and `drinkNoise` = 0.7, assuming that L₀ finds it relatively easy to access the person interpretation but has difficulty with the drink interpretation.

~~~~
// FEEDBACK: what if noise also applies to the unambiguous utterances?

/// fold:
// possible utterances
var utterances = ["shirleyTemple", "shirleyTemple person", "shirleyTemple drink"]
var utterancePrior = function() {
  uniformDraw(utterances)
}

// utterance cost
var cost = {
  "shirleyTemple": 1,
  "shirleyTemple person": 2,
  "shirleyTemple drink": 2
}

// possible world states
var states = ["person", "drink"]
var statePrior = function() {
   uniformDraw(states)
}

// possible interpretation
var interpretations = ["thePerson", "theDrink"]
var interpretationPrior = function() {
   uniformDraw(interpretations)
}
///

// adding noise to unambiguous utterances
// meaning function
var meaning = function(utterance, state, interpretation, personNoise, drinkNoise) {
  return utterance == "shirleyTemple" 
    ? (interpretation == "thePerson" ? 
       state == "person" ? flip(1-personNoise) : flip(0+personNoise) :
       state == "drink" ? flip(1-drinkNoise) : flip(0+drinkNoise)) :
    utterance == "shirleyTemple person" 
    ? (state == "person" ? flip(1-personNoise) : flip(0+personNoise)) :
    utterance == "shirleyTemple drink" 
    ? (state == "drink" ? flip(1-drinkNoise) : flip(0+drinkNoise)) :
   false;
}

// Literal listener (L0) = non-native listener
var literalListener = cache(function(utterance, interpretation, personNoise, drinkNoise) {
  return Infer({model: function(){
    var state = uniformDraw(states)
    condition(meaning(utterance, state, interpretation, personNoise, drinkNoise))
    return state
  }})
})

var alpha = 1

// Speaker 1 (S1) = native speaker
var speaker = cache(function(interpretation, state, personNoise, drinkNoise) {
  return Infer({model: function(){
    var utterance = uniformDraw(utterances)
    factor(alpha*(literalListener(utterance, interpretation, personNoise, drinkNoise).score(state) - cost[utterance]))
    return utterance
  }})
})


print("Literal listener's posterior:")
viz(literalListener("shirleyTemple person", "thePerson", 0.2, 0.7))
viz(literalListener("shirleyTemple drink", "theDrink", 0.2, 0.7))

print("Speaker's posterior:")
viz(speaker("thePerson", "person", 0.2, 0.7))
viz(speaker("theDrink", 'drink', 0.2, 0.7))
~~~~

**L₀**, when hearing an unambiguous utterance, is more likely to interpret it as person, the interpretation with lower noise. However, unlike the case where noise was not applied to unambiguous utterances, there is now a non-negligible chance that L₀ interprets the utterance as referring to a different state than what the utterance explicitly indicates.

For example, even when the utterance includes “person,” there is still about a 30% chance it will be interpreted as drink; conversely, even when “drink” is included, there is about an 80% chance it will be interpreted as person.
This result contradicts our intuition: if an utterance is unambiguous, we would expect it to be interpreted as referring to what it explicitly denotes.

In the case of **S₁**, previously only two utterances were viable: the ambiguous utterance and the unambiguous utterance matching the state. However, once noise was added to unambiguous utterances, all three utterances became possible options in the speaker’s posterior.

This also runs counter to our intuition. For instance, we would expect that, in the presence of high drink noise, a speaker aiming to communicate the drink state would prefer the unambiguous utterance `shirleyTemple drink`. Yet in this model, the speaker is likely to choose the ambiguous `shirleyTemple` (~60%).

### 2. What if L1 has asymmetric noise? How does that change the predictions of L1?

To answer this question, we introduced asymmetry in L₁’s noise. In this scenario, we aimed to configure L₁ to be more biased toward interpreting “Shirley Temple” as a drink rather than a person. Accordingly, we set L₁’s `personNoise` = 0.7 and `drinkNoise` = 0.1.

~~~~
// FEEDBACK: what if L1 has asymmetric noise? how does that change the predictions of L1?

/// fold:
// possible utterances
var utterances = ["shirleyTemple", "shirleyTemple person", "shirleyTemple drink"]
var utterancePrior = function() {
  uniformDraw(utterances)
}

// utterance cost
var cost = {
  "shirleyTemple": 1,
  "shirleyTemple person": 2,
  "shirleyTemple drink": 2
}

// possible world states
var states = ["person", "drink"]
var statePrior = function() {
   uniformDraw(states)
}

// possible interpretation
var interpretations = ["thePerson", "theDrink"]
var interpretationPrior = function() {
   uniformDraw(interpretations)
}

// meaning function
var meaning = function(utterance, state, interpretation, personNoise, drinkNoise) {
  return utterance == "shirleyTemple" 
    ? (interpretation == "thePerson" ? 
       state == "person" ? flip(1-personNoise) : flip(0+personNoise) :
       state == "drink" ? flip(1-drinkNoise) : flip(0+drinkNoise)) :
    utterance == "shirleyTemple person" ? state == "person" :
    utterance == "shirleyTemple drink" ? state == "drink" :
   false;
}

// Literal listener (L0) = non-native listener
var literalListener = cache(function(utterance, interpretation, personNoise, drinkNoise) {
  return Infer({model: function(){
    var state = uniformDraw(states)
    condition(meaning(utterance, state, interpretation, personNoise, drinkNoise))
    return state
  }})
})

var alpha = 1

// Speaker 1 (S1) = native speaker
var speaker = cache(function(interpretation, state, personNoise, drinkNoise) {
  return Infer({model: function(){
    var utterance = uniformDraw(utterances)
    factor(alpha*(literalListener(utterance, interpretation, personNoise, drinkNoise).score(state) - cost[utterance]))
    return utterance
  }})
})
///

// Pragmatic listener (L1) = proficient non-native listener or native listener
var pragmaticListener = cache(function(utterance, personNoise, drinkNoise) {
  return Infer({model: function(){
    var state = statePrior()
    var interpretation = interpretationPrior()
    observe(speaker(interpretation, state, personNoise, drinkNoise), utterance)
    return {state: state, int: interpretation}
  }})
})

// Speaker 2 (S2) = native speaker
var pragmaticSpeaker = cache(function(state, personNoise, drinkNoise) {
    Infer({model: function(){
    var utterance = utterancePrior();
    factor(marginalize(pragmaticListener(utterance, personNoise, drinkNoise), "state").score(state) 
           - cost[utterance])
    return utterance
  }})
})


print("Pragmatic listener's posterior:")
viz.marginals(pragmaticListener("shirleyTemple", 0.7, 0.1))

print("Pragmatic Speaker's posterior")
print("for state 'drink':")
viz(pragmaticSpeaker("drink", 0.7, 0.1))
print("for state 'person':")
viz(pragmaticSpeaker("person", 0.7, 0.1))
~~~~

**L₁**, upon hearing “Shirley Temple,” tends to interpret the state as `drink` while interpreting the meaning as `thePerson`. Although it may seem *counterintuitive* that the state and interpretation differ, the tendency for an ambiguous utterance to be taken as drink when `personNoise` is high aligns with our intuitions—at least when considering only state in isolation.

With such an L₁ in mind, **S₂** is likely to choose the ambiguous utterance “Shirley Temple” to communicate the `drink` state, and the unambiguous “Shirley Temple person” to communicate the `person` state.

### 3. What if you change the interpretation prior — so that some people are biased to one interpretation?

This question is asking about how the behavior in the model changes when the interpretation prior is adjusted, without introducing noise. Therefore, we first sought to change the interpretation prior from a uniform draw to a categorical distribution.

In our model, the interpretation prior influences both **L₁** and **S₂**. Accordingly, we added an S₂ layer to the most basic model without noise.

In this scenario, we assumed that L₁ is biased toward **the drink interpretation**. Thus, while keeping everything else constant, we modeled the interpretation prior by assigning a value of 1 to `thePerson` and 10 to `theDrink`.

~~~~
// FEEDBACK: what if you change the interpretation prior — so that some people are biased to one interpretation?

///fold:
// possible utterances
var utterances = ["shirleyTemple", "shirleyTemple person", "shirleyTemple drink"]
var utterancePrior = function() {
  uniformDraw(utterances)
}

// utterance cost
var cost = {
  "shirleyTemple": 1,
  "shirleyTemple person": 2,
  "shirleyTemple drink": 2
}

// possible world states
var states = ["person", "drink"]
var statePrior = function() {
   uniformDraw(states)
}
///

// possible interpretation
var interpretations = ["thePerson", "theDrink"]
var interpretationPrior = function() {
//    uniformDraw(interpretations)
// }
   categorical({
    vs: ["thePerson", "theDrink"],
    ps: [1, 10]
  })
}

///fold:
// meaning function
var meaning = function(utterance, state, interpretation) {
  return utterance == "shirleyTemple"
    ? (interpretation == "thePerson" ? state == "person" : state == "drink") :
  utterance == "shirleyTemple person" ? state == "person" : 
  utterance == "shirleyTemple drink" ? state == "drink" :
  false;
}

// Literal listener (L0) = non-native listener
var literalListener = cache(function(utterance, interpretation) {
  return Infer({model: function(){
    var state = uniformDraw(states)
    condition(meaning(utterance, state, interpretation))
    return state
  }})
})

var alpha = 1

// Speaker 1 (S1) = native speaker
var speaker = cache(function(interpretation, state) {
  return Infer({model: function(){
    var utterance = uniformDraw(utterances)
    factor(alpha*(literalListener(utterance, interpretation).score(state)
                  - cost[utterance]))
    return utterance
  }})
})
///

// Pragmatic listener (L1) = proficient non-native listener or native listener
var pragmaticListener = cache(function(utterance) {
  return Infer({model: function(){
    var state = statePrior()
    var interpretation = interpretationPrior()
    observe(speaker(interpretation, state), utterance)
    return {state: state, int: interpretation}
  }})
})

// Speaker 2 (S2) = native speaker
var pragmaticSpeaker = cache(function(state) {
  Infer({model: function(){
    var utterance = utterancePrior();
    factor(marginalize(pragmaticListener(utterance), "state").score(state)
           - cost[utterance])
    return utterance
  }})
})

viz.marginals(pragmaticListener("shirleyTemple"))
viz(pragmaticSpeaker("drink"))  
viz(pragmaticSpeaker("person"))  
~~~~

When **L₁** hears “Shirley Temple,” they are most likely to interpret both the state and the interpretation as drink. (There is a small chance—less than 10%—that they interpret it as person.)

With this L₁ in mind, **S₂** is more likely to choose the ambiguous “Shirley Temple” when they want to communicate the `drink` state, and the unambiguous “Shirley Temple person” when they want to communicate the `person` state.

These results, especially with respect to L₁, seem to better align with our intuition than the previous ones, where we put asymmetric noises in L₁. It suggests that instead of modeling difficulty in interpreting ambiguous utterances through noise, it may be more effective to reflect such difficulty by **adjusting the prior**.

### 4. What if S2 noise goes up in parallel? Will unambiguous increase?

To answer this question, we looked at how S1 and S2 behave in the model with noise and the S2 layer.

~~~~
// FEEDBACK: What if S2 noise goes up in parallel? will unambiguous increase?

/// fold:
// possible utterances
var utterances = ["shirleyTemple", "shirleyTemple person", "shirleyTemple drink"]
var utterancePrior = function() {
  uniformDraw(utterances)
}

// utterance cost
var cost = {
  "shirleyTemple": 1,
  "shirleyTemple person": 2,
  "shirleyTemple drink": 2
}

// possible world states
var states = ["person", "drink"]
var statePrior = function() {
   uniformDraw(states)
}

// possible interpretation
var interpretations = ["thePerson", "theDrink"]
var interpretationPrior = function() {
   uniformDraw(interpretations)
}

// meaning function
var meaning = function(utterance, state, interpretation, personNoise, drinkNoise) {
  return utterance == "shirleyTemple" 
    ? (interpretation == "thePerson" ? 
       state == "person" ? flip(1-personNoise) : flip(0+personNoise) :
       state == "drink" ? flip(1-drinkNoise) : flip(0+drinkNoise)) :
    utterance == "shirleyTemple person" ? state == "person" :
    utterance == "shirleyTemple drink" ? state == "drink" :
   false;
}

// Literal listener (L0) = non-native listener
var literalListener = cache(function(utterance, interpretation, personNoise, drinkNoise) {
  return Infer({model: function(){
    var state = uniformDraw(states)
    condition(meaning(utterance, state, interpretation, personNoise, drinkNoise))
    return state
  }})
})
///

var alpha = 1

// Speaker 1 (S1) = native speaker
var speaker = cache(function(interpretation, state, personNoise, drinkNoise) {
  return Infer({model: function(){
    var utterance = uniformDraw(utterances)
    factor(alpha*(literalListener(utterance, interpretation, personNoise, drinkNoise).score(state) - cost[utterance]))
    return utterance
  }})
})

// Pragmatic listener (L1) = proficient non-native listener or native listener
var pragmaticListener = cache(function(utterance, personNoise, drinkNoise) {
  return Infer({model: function(){
    var state = statePrior()
    var interpretation = interpretationPrior()
    observe(speaker(interpretation, state, personNoise, drinkNoise), utterance)
    return {state: state, int: interpretation}
  }})
})

// Speaker 2 (S2) = native speaker
var pragmaticSpeaker = cache(function(state, personNoise, drinkNoise) {
    Infer({model: function(){
    var utterance = utterancePrior();
    factor(marginalize(pragmaticListener(utterance, personNoise, drinkNoise), "state").score(state) 
           - cost[utterance])
    return utterance
  }})
})

viz(speaker("theDrink", "drink", 0.1, 0.1))
viz(speaker("theDrink", "drink", 0.3, 0.3))
viz(speaker("theDrink", "drink", 0.9, 0.9))

viz.table(pragmaticSpeaker("drink", 0.1, 0.1))
viz.table(pragmaticSpeaker("drink", 0.3, 0.3))
viz.table(pragmaticSpeaker("drink", 0.9, 0.9))

viz(pragmaticSpeaker("drink", 0.1, 0.2))
viz(pragmaticSpeaker("drink", 0.1, 0.8))
~~~~

Interestingly, **S₁** and **S₂** respond differently to changes in noise levels. When noise increases in parallel for both person and drink interpretations, **S₁** shows a stronger preference for unambiguous utterances. This is because S₁ relies on L₀, who is likely to misinterpret ambiguous utterances with high noise.

In contrast, **S₂**’s utterance choice remains the same under parallel noise increases. S₂ relies on L₁, who jointly infers both state and interpretation. As relative uncertainty stays balanced, there is no strong reason to shift utterance preferences. However, when noise increases asymmetrically (e.g., only drink noise rises), S₂ favors unambiguous utterances to reduce the risk of misunderstanding.

## Conclusion 

In this project, we explored how ambiguity in conversations between native and non-native speakers can be resolved within the RSA framework. By modeling ambiguity as a form of noise, we captured the variability in interpretation that arises from differences in linguistic proficiency. Our findings highlight that ambiguity resolution is not purely linguistic, but also relies on the speaker’s ability to anticipate the listener’s perspective. Specifically, the speaker **S₁** plays a crucial role by selecting utterances that guide the listener **L₀** toward the intended interpretation, underscoring the inherently collaborative and context-sensitive nature of language use.

## References

Bender, E. M., & Koller, A. (2020, July). Climbing towards NLU: On meaning, form, and understanding in the age of data. In *Proceedings of the 58th annual meeting of the association for computational linguistics* (pp. 5185-5198).

Savinelli, K. J., Scontras, G., & Pearl, L. (2017). Modeling scope ambiguity resolution as pragmatic inference: Formalizing differences in child and adult behavior. In *Proceedings of the Annual Meeting of the Cognitive Science Society* (Vol. 39).

Scontras, G. & Pearl, L. S. (2021). “When pragmatics matters more for truth-value judgments: An investigation of quantifier scope ambiguity”, *Glossa: a journal of general linguistics* 6(1): 110. doi: https://doi.org/10.16995/glossa.5724


## Acknowledgements
Special thanks to Professor Scontras for his invaluable guidance throughout the development of this model. This wouldn’t have been possible without his help.