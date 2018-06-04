---
layout: model
title: Singh & Uyeda Pronoun Uncertainty
model-language: webppl
---

### Resolving Pronoun Ambiguity Using Subject and Parallel Strategies

*Authors: Sunny Singh and Ellen Uyeda*

When using language, the speaker follows principles that are identified as Gricean Maxims. The Maxims that are the most interesting in regards to pronouns are the Maxim of quality (making true statements) and the Maxim of manner (being perspicuous). The main use of pronouns is to make it easier for the speaker and listener to refer to a person (or a group of people) without needing to name him or her at each instance. However, with some sentences, different listeners can arrive at different interpretation.

In the sentence, "John hit Fred and Ellen hit him" the pronoun 'him' can refer to John or Fred (without any extra context). How do we determine who 'him' is referring to?

  - John~subject hit Fred~object and Ellen hit **him**

[Semick and Amsili (2017)](http://www.aclweb.org/anthology/E17-4006) looked at two different strategies:

1. Subject Assignment Strategy: resolves the pronoun to the referent in the subject position of the previous clause
  - John~subject hit Fred~object and Ellen hit him~subject

2. Parallel Function Strategy: resolves the pronoun to the referent with the same syntactic function
  - John~subject hit Fred~object and Ellen hit him~object



From this we can conclude that there are two states of the world. One is that John is getting hit, and the other is that Fred is getting hit. If the utterance is  'Ellen hit John' or 'Ellen hit Fred' then we are refering to John or Fred respectively. However, when the uttereance is 'Ellen hit him', 'him' may be refering to either, John or Fred, based on which strategy is being used. We can capture the possible meanings of this sentence with the two strategies. <!--A base model using our sentence to explain the meaning function.-->

~~~~
// This is for the sentence "John hit Fred and Ellen hit him"

// possible utterances: Ambiguous, Unambiguous Fred, Unambiguous John
var utterances = ["him", "Fred", "John"]

// samples the utterances that fills in the following templete:
// "John hit Fred and Ellen hit (the utterance)"

var utterancePrior = function() {
  categorical([2,1,1], utterances)
}

//possible world states of who Ellen hit
var states = ["John", "Fred"]
var statePrior = function() {
  return uniformDraw(states)
}

//possible strategies
var strategyPrior = function(){
  return uniformDraw(["Subject", "Parallel"])
}

//meaning function
var meaning = function(utterance, state, strategy){
  return utterance == "him" ?
    strategy == "Subject" ? state == "John" :
  state == "Fred" : 
  utterance == state
}

var utt = utterancePrior()
var state = statePrior()
var strat = strategyPrior()

print("Reality: Ellen hit " + state + ". Strategy used: " + strat)
print("Utterance: \"John hit Fred and Ellen hit " + utt + ".\"")
print("This statement is " + meaning(utt, state, strat) + ".")
~~~~

> Try to run this code multiple times to see how the meaning function works.

A pronoun is resolved based on the possible world states and strategy. Therefore, an RSA model can demonstrate the resolution of ambiguous utterances.

The literal listener $$L_{0}$$ will hear an utterance, and given a strategy will return a distribution of the states of the world. <!--The base model for the sentence with L0.-->

~~~~
///fold:
// possible utterances: Ambiguous, Unambiguous Fred, Unambiguous John
var utterances = ["him", "Fred", "John"]

// samples the utterances that fills in the following templete:
// "John hit Fred and Ellen hit (the utterance)"

var utterancePrior = function() {
    categorical([2,1,1], utterances)
}

//possible world states of who Ellen hit
var states = ["John", "Fred"]
var statePrior = function() {
    return uniformDraw(states)
}

//possible strategies
var strategyPrior = function(){
    return uniformDraw(["Subject", "Parallel"])
}

//meaning function
var meaning = function(utterance, state, strategy){
    return utterance == "him" ?
        strategy == "Subject" ? state == "John" :
    state == "Fred" : 
    utterance == state
}
///
// Literal Listener (L0)
var literalListener = cache(function(utterence, strategy){
    return Infer({model: function(){
        var state = statePrior()
        condition(meaning(utterence, state, strategy))
        return state
    }})
})

literalListener("him", "Subject")
~~~~

> Try changing what the `LiteralListener` arguments and seeing the results.

The speaker, $$S$$ is aware of the state and will return a distribution of the utterances that will lead $$L_{0}$$ to arrive at the correct state of the world. <!--The base model for the sentence with S1.-->

~~~~
///fold:
// possible utterances: Ambiguous, Unambiguous Fred, Unambiguous John
var utterances = ["him", "Fred", "John"]

// samples the utterances that fills in the following templete:
// "John hit Fred and Ellen hit (the utterance)"

var utterancePrior = function() {
    categorical([2,1,1], utterances)
}

//possible world states of who Ellen hit
var states = ["John", "Fred"]
var statePrior = function() {
    return uniformDraw(states)
}

//possible strategies
var strategyPrior = function(){
    return uniformDraw(["Subject", "Parallel"])
}

//meaning function
var meaning = function(utterance, state, strategy){
    return utterance == "him" ?
        strategy == "Subject" ? state == "John" :
    state == "Fred" : 
    utterance == state
}
///
// Literal Listener (L0)
var literalListener = cache(function(utterence, strategy){
    return Infer({model: function(){
        var state = statePrior()
        condition(meaning(utterence, state, strategy))
        return state
    }})
})

// Speaker (S)
var speaker = cache(function(strategy, state){
    return Infer({model: function() {
        var utterance = utterancePrior()
        observe(literalListener(utterance, strategy), state)
        return utterance
    }})
})

speaker("Subject", "Fred")
~~~~

The pragmatic listener, $$L_{1}$$ will hear an utterance and return a probability distribution over the possible states and the strategies it uses. <!--The base model for the sentence with L1.-->

~~~~
///fold:
// possible utterances: Ambiguous, Unambiguous Fred, Unambiguous John
var utterances = ["him", "Fred", "John"]

// samples the utterances that fills in the following templete:
// "John hit Fred and Ellen hit (the utterance)"

var utterancePrior = function() {
    categorical([2,1,1], utterances)
}

//possible world states of who Ellen hit
var states = ["John", "Fred"]
var statePrior = function() {
    return uniformDraw(states)
}

//possible strategies
var strategyPrior = function(){
    return uniformDraw(["Subject", "Parallel"])
}

//meaning function
var meaning = function(utterance, state, strategy){
    return utterance == "him" ?
        strategy == "Subject" ? state == "John" :
    state == "Fred" : 
    utterance == state
}

// Literal Listener (L0)
var literalListener = cache(function(utterence, strategy){
    return Infer({model: function(){
        var state = statePrior()
        condition(meaning(utterence, state, strategy))
        return state
    }})
})

// Speaker (S)
var speaker = cache(function(strategy, state){
    return Infer({model: function() {
        var utterance = utterancePrior()
        observe(literalListener(utterance, strategy), state)
        return utterance
    }})
})
///
// Pragmatic listener (L1)
var pragmaticListener = cache(function(utterance) {
    return Infer({model: function(){
        var state = statePrior()
        var strategy = strategyPrior()
        observe(speaker(strategy,state),utterance)
        return state
    }})
})
pragmaticListener("him")
~~~~

> Run `pragmaticListener` with the utterance 'him' to see the probability distribution.

Now we can conclude if we hear the ambiguous uttereance, 'him' we will get a 50/50 distribution between Fred and John. This is not interesting, how can we push the distribution around to model our intuitions? 

Lets add in noise to push the distribution in favor of the Subject Assignment Strategy. This is similar to how noise was added in the [Plural predication model](https://gscontras.github.io/probLang/chapters/06-plurals.html). Following our human intuition, the Subject Assignment Strategy is used more often than the Parallel Function Strategy. <!--Adding in noise to shift the probabilities.-->

~~~~
// error function
var erf = function(x) {
  var a1 =  0.254829592;
  var a2 = -0.284496736;
  var a3 =  1.421413741;
  var a4 = -1.453152027;
  var a5 =  1.061405429;
  var p  =  0.3275911;
  var sign = x < 0 ? -1 : 1
  var z = Math.abs(x);
  var t = 1.0/(1.0 + p*z);
  var y = 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*Math.exp(-z*z);
  var answer = sign*y
  return answer
}

var strategyPrior = function(noise){
    var weight = noise == undefined ? .5 : erf(noise)
    return flip(weight) ? "Subject" : "Parallel"
}


///fold:
// possible utterances: Ambiguous, Unambiguous Fred, Unambiguous John
var utterances = ["him", "Fred", "John"]

// samples the utterances that fills in the following templete:
// "John hit Fred and Ellen hit (the utterance)"

var utterancePrior = function() {
    categorical([2,1,1], utterances)
}

//possible world states of who Ellen hit
var states = ["John", "Fred"]
var statePrior = function() {
    return uniformDraw(states)
}

//meaning function
var meaning = function(utterance, state, strategy){
    return utterance == "him" ?
        strategy == "Subject" ? state == "John" :
    state == "Fred" : 
    utterance == state
}
///
var noise = function(){
    return uniformDraw([0.01, 0.1, 0.2, 0.3, 0.5, 0.6, 0.7, 0.8, 0.9, 
                        1.0, 1.1, 1.2, 1.3, 1.4, 1.5])
}
// Literal Listener (L0)
var literalListener = cache(function(utterence, strategy, noise){
    return Infer({model: function(){
        var state = statePrior()
        condition(meaning(utterence, state, strategy))
        return state
    }})
})

// Speaker (S)
var speaker = cache(function(strategy, state){
    return Infer({model: function() {
        var utterance = utterancePrior()
        observe(literalListener(utterance, strategy), state)
        return utterance
    }})
})
///
// Pragmatic listener (L1)
var pragmaticListener = cache(function(utterance) {
    return Infer({model: function(){
        var state = statePrior()
        var noise = noise()
        var strategy = strategyPrior(noise)
        observe(speaker(strategy, state),utterance)
        return state
    }})
})
pragmaticListener("him")
~~~~

> Try running `pragmaticListener`  and compare to see how the probabilities have now shifted.

Now that the model more accurately represents human intuitions, let’s introduce other possible states. The possible states can include John, Fred, Ellen, and both John and Fred. In order to account for gender and plurality, we need to change our states and utterance to objects. This is similar to the [first model](https://gscontras.github.io/probLang/chapters/01-introduction.html) where the objects are structured objects, the difference here being that utterance are also these structured objects. This is used for in order to reduce the size of the strategy function as they now check to see if these features are compatible with the utterance.

~~~~
// error function
var erf = function(x) {
    var a1 =  0.254829592;
    var a2 = -0.284496736;
    var a3 =  1.421413741;
    var a4 = -1.453152027;
    var a5 =  1.061405429;
    var p  =  0.3275911;
    var sign = x < 0 ? -1 : 1
    var z = Math.abs(x);
    var t = 1.0/(1.0 + p*z);
    var y = 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*Math.exp(-z*z);
    var answer = sign*y
    return answer
}

// This is for the sentence "John hit Fred and Ellen hit him"
var state = [{string: "John", gender: "Male", plurality: "Single"},
             {string: "Fred", gender: "Male", plurality: "Single"},
             {string: "Ellen", gender: "Female", plurality: "Single"},
             {string: "JohnFred", gender: "Male", plurality: "Plural"}]

var statePrior = function(){
    var state = uniformDraw(state)
    return state
}

var utterances = [{string: "John", gender: "Male", plurality: "Single"},
                  {string: "Fred", gender: "Male", plurality: "Single"},
                  {string: "Ellen", gender: "Female", plurality: "Single"},
                  {string: "him", gender: "Male", plurality: "Single"},
                  {string: "them", gender: "Male" || "Female", plurality: "Plural"}]

var utterancePrior = function() {
    categorical([1,1,1,2,2], utterances)
}

var noise = function(){
    return uniformDraw([0.01, 0.1, 0.2, 0.3, 0.5, 0.6, 0.7, 0.8, 0.9, 
                        1.0, 1.1, 1.2, 1.3, 1.4, 1.5])
}

var strategyPrior = function(noise){ 
    var weight = noise == undefined ? .5 : erf(noise)
    return flip(weight) ? "Subject" : "Parallel"
}

//meaning function
var meaning = function(strategy, utterance, state, subject, object){
    return strategy == "Subject" ? meaningSubjectStrategy(utterance, state, subject, object):
    meaningParellelStrategy(utterance, state, subject, object)
}


var meaningSubjectStrategy = function(utterance, state, subject, object){
    return state.string == utterance.string? true :
    utterance.string == "John" || utterance.string == "Fred"|| utterance.string == "Ellen" || utterance.string == "JohnFred"? false :
    utterance.gender == state.gender & utterance.plurality == state.plurality ? 
        state.string == subject || state.string == subject + object ? true :
    state.string != subject & state.string != object ? true :
    false :
    false
}

var meaningParellelStrategy = function(utterance, state, subject, object){
    return state.string == utterance.string? true :
    utterance.string == "John" || utterance.string == "Fred"|| utterance.string == "Ellen" || utterance.string == "JohnFred"? false :
    utterance.gender == state.gender & utterance.plurality == state.plurality ? 
        state.string == object || state.string == subject + object ? true :
    state.string != subject & state.string != object ? true :
    false :
    false
}

// Literal Listener (L0)
var literalListener = cache(function(utterence, noise){
    return Infer({model: function(){
        var strategy = strategyPrior(noise)
        var state = statePrior()
        condition(meaning(strategy, utterence, state, "John", "Fred"))
        return state
    }})
})

// Speaker (S1)
var speaker = cache(function(state, noise){
    return Infer({model: function() {
        var utterance = utterancePrior()
        observe(literalListener(utterance, noise), state)
        return utterance
    }})
})

// Pragmatic Listener (L1)
var pragmaticListener = cache(function(utterance){
    Infer({model: function(){
        var noise = noise()
        var state = statePrior()
        var strategy = strategyPrior(noise)
        observe(speaker(state, noise), utterance)
        return state
    }})
})
marginalize(pragmaticListener(utterances[3]), "string")
~~~~

> Try running `pragmaticListener` with different utterances.

Let's say the person who is being hit could be someone who is not mentioned in the discourse, but exist in the world. For example, Bob.

~~~~
// error function
var erf = function(x) {
    var a1 =  0.254829592;
    var a2 = -0.284496736;
    var a3 =  1.421413741;
    var a4 = -1.453152027;
    var a5 =  1.061405429;
    var p  =  0.3275911;
    var sign = x < 0 ? -1 : 1
    var z = Math.abs(x);
    var t = 1.0/(1.0 + p*z);
    var y = 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*Math.exp(-z*z);
    var answer = sign*y
    return answer
}

// This is for the sentence "John hit Fred and Ellen hit him"
var state = [{string: "John", gender: "Male", plurality: "Single"},
             {string: "Fred", gender: "Male", plurality: "Single"},
             {string: "Ellen", gender: "Female", plurality: "Single"},
             {string: "Bob", gender: "Male", plurality: "Single"},
             {string: "JohnFred", gender: "Male", plurality: "Plural"}]

var statePrior = function(){
    var state = uniformDraw(state)
    return state
}

var utterances = [{string: "John", gender: "Male", plurality: "Single"},
                  {string: "Fred", gender: "Male", plurality: "Single"},
                  {string: "Ellen", gender: "Female", plurality: "Single"},
                  {string: "Bob", gender: "Male", plurality: "Single"},
                  {string: "him", gender: "Male", plurality: "Single"},
                  {string: "them", gender: "Male" || "Female", plurality: "Plural"}]
var utterancePrior = function() {
    categorical([1,1,1,1,2,2], utterances)
}

var noise = function(){
    return uniformDraw([0.01, 0.1, 0.2, 0.3, 0.5, 0.6, 0.7, 0.8, 0.9, 
                        1.0, 1.1, 1.2, 1.3, 1.4, 1.5])
}

var strategyPrior = function(noise){ 
    var weight = noise == undefined ? .5 : erf(noise)
    return flip(weight) ? "Subject" : "Parallel"
}

//meaning function
var meaning = function(strategy, utterance, state, subject, object){
    return strategy == "Subject" ? meaningSubjectStrategy(utterance, state, subject, object):
    meaningParellelStrategy(utterance, state, subject, object)
}


var meaningSubjectStrategy = function(utterance, state, subject, object){
    return state.string == utterance.string? true :
    utterance.string == "John" || utterance.string == "Fred"|| utterance.string == "Ellen" || utterance.string == "Bob" || utterance.string == "JohnFred"? false :
    utterance.gender == state.gender & utterance.plurality == state.plurality ? 
        state.string == subject || state.string == subject + object ? true :
    state.string != subject & state.string != object ? true :
    false :
    false
}

var meaningParellelStrategy = function(utterance, state, subject, object){
    return state.string == utterance.string? true :
    utterance.string == "John" || utterance.string == "Fred"|| utterance.string == "Ellen" || utterance.string == "Bob" || utterance.string == "JohnFred"? false :
    utterance.gender == state.gender & utterance.plurality == state.plurality ? 
        state.string == object || state.string == subject + object ? true :
    state.string != subject & state.string != object ? true :
    false :
    false
}

// Literal Listener (L0)
var literalListener = cache(function(utterence, noise){
    return Infer({model: function(){
        var strategy = strategyPrior(noise)
        var state = statePrior()
        condition(meaning(strategy, utterence, state, "John", "Fred"))
        return state
    }})
})

// Speaker (S1)
var speaker = cache(function(state, noise){
    return Infer({model: function() {
        var utterance = utterancePrior()
        observe(literalListener(utterance, noise), state)
        return utterance
    }})
})

// Pragmatic Listener (L1)
var pragmaticListener = cache(function(utterance){
    Infer({model: function(){
        var noise = noise()
        var state = statePrior()
        var strategy = strategyPrior(noise)
        observe(speaker(state, noise), utterance)
        return state
    }})
})
marginalize(pragmaticListener(utterances[4]), "string")
~~~~

> When the `prgamaticListener` hears the utterance, 'him', how does the probability shift? Explain the probability of the state, 'Bob'.
