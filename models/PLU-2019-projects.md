---
layout: model
title: ProbLang 2019 class projects
model-language: webppl
---

##Xirong Cao

###Section One

Setup Scene: A couple is shopping in the mall, and the girlfriend is trying a new dress. Asking her boyfriend, "what do you think?"

~~~~
// possible world state: the actual quality of the dress and person
var states = [{dress: "cute", person: "cute"},
              {dress: "cute", person: "not-cute"},
              {dress: "not-cute", person: "cute"},
              {dress: "not-cute", person: "not-cute"}]

// randomly choose a state describe the real world situation
var statePrior = function() {
  uniformDraw(states)
}
~~~~

The possible utterances that  could affect girlfriend's mood

~~~~
// possible utternaces, "null" means slient
// "emmm" means you want to say somehting, but you don't know how to begin
var utterances = ["null", "emmm", "good", "cute"]
var utterancePrior = function(){
  uniformDraw(utterances)
}
~~~~

There is a situation that the girlfriend could be asking comments on the dress or on the person herself. Therefore, the meaning function should include interpretations between the dress and the person.

~~~~
// meaning function: interpretation = scope, different interpretation for listener to choose
var meaning = function(utterance, state, guess) {
  return utterance == "null" ? true:
  utterance == "emmm" ? state.dress == "not-cute" | state.person == "not-cute":
  utterance == "cute" ? state.dress == "cute" | state.person == "cute":
  guess == "dress" ? state.dress == "cute":
  state.person == "cute"
}


// interpretation about dress or person should be a guess: 
// randomly choose from "dress" and "person"
var guessPrior = function() {
  uniformDraw(["dress", "person"])
}
~~~~

cost function is meant to measure the strength needed for utter a word or a sentence physically

~~~~
// the cost function:
// it's meant to measure the physical strength needed to pronounce the word or sentences
var cost = function(utterance) {
  utterance == "null" ? 0:
  utterance == "good" ? 1:
  utterance == "emmm" ? 2:
  1 
}

// the optimal function 
var alpha = 1
~~~~

The RSA listener and speaker model

~~~~
// Literal Listener (L0)
var literalListener = cache(function(utterance, guess) {
  Infer({model: function() {
    var state = uniformDraw(states)
    condition(meaning(utterance, state, guess))
    return state
  }})
})


// Speaker (S)
var speaker = cache(function(guess, state) {
  return Infer({model: function(){
    var utterance = utterancePrior()
    factor(alpha*(literalListener(utterance, guess).score(state) 
                  - cost[utterance]))
    return utterance
  }})
})


// Pragmatic Listener (L1)
var pragmaticListener = cache(function(utterance) {
  return Infer({model: function(){
    var state = statePrior()
    var guess = guessPrior()
    observe(speaker(guess, state), utterance)
    return {state: state,
            guess: guess}
  }})
})
~~~~

Complete RSA model

~~~~
var states = [{dress: "cute", person: "cute"},
              {dress: "cute", person: "not-cute"},
              {dress: "not-cute", person: "cute"},
              {dress: "not-cute", person: "not-cute"}]


var statePrior = function() {
  uniformDraw(states)
}


var utterances = ["null", "emmm", "good", "cute"]
var utterancePrior = function(){
  uniformDraw(utterances)
}


var meaning = function(utterance, state, guess) {
  return utterance == "null" ? true:
  utterance == "emmm" ? state.dress == "not-cute" | state.person == "not-cute":
  utterance == "cute" ? state.dress == "cute" | state.person == "cute":
  guess == "dress" ? state.dress == "cute":
  state.person == "cute"
}


var guessPrior = function() {
  uniformDraw(["dress", "person"])
}


var cost = function(utterance) {
  utterance == "null" ? 0:
  utterance == "good" ? 1:
  utterance == "emmm" ? 2:
  1 
}

var alpha = 1

// Literal Listener (L0)
var literalListener = cache(function(utterance, guess) {
  Infer({model: function() {
    var state = uniformDraw(states)
    condition(meaning(utterance, state, guess))
    return state
  }})
})


// Speaker (S)
var speaker = cache(function(guess, state) {
  return Infer({model: function(){
    var utterance = utterancePrior()
    factor(alpha*(literalListener(utterance, guess).score(state) 
                  - cost(utterance)))
    return utterance
  }})
})


// Pragmatic Listener (L1)
var pragmaticListener = cache(function(utterance) {
  return Infer({model: function(){
    var state = statePrior()
    var guess = guessPrior()
    observe(speaker(guess, state), utterance)
    return {state: state,
            guess: guess}
  }})
})

print("cute")
viz(pragmaticListener("cute"))
print("good")
viz(pragmaticListener("good"))
print("emmm")
viz(pragmaticListener("emmm"))
print("null")
viz(pragmaticListener("null"))
~~~~

~~~~
// politeness model
var states = [{dress: "cute", person: "cute"},
              {dress: "cute", person: "not-cute"},
              {dress: "not-cute", person: "cute"},
              {dress: "not-cute", person: "not-cute"}]


var statePrior = function() {
  uniformDraw(states)
}


var utterances = ["null", "emmm", "good", "cute"]
var utterancePrior = function(){
  uniformDraw(utterances)
}


var meaning = function(utterance, state, guess) {
  return utterance == "null" ? true:
  utterance == "emmm" ? state.dress == "not-cute" | state.person == "not-cute":
  utterance == "cute" ? state.dress == "cute" | state.person == "cute":
  guess == "dress" ? state.dress == "cute":
  state.person == "cute"
}


var guessPrior = function() {
  uniformDraw(["dress", "person"])
}


var cost = function(utterance) {
  utterance == "null" ? 0:
  utterance == "good" ? 1:
  utterance == "emmm" ? 1:
  1 
}

var alpha = 1

// Literal Listener (L0)
var literalListener = cache(function(utterance, guess) {
  Infer({model: function() {
    var state = uniformDraw(states)
    condition(meaning(utterance, state, guess))
    return state
  }})
})


// Speaker (S)
var speaker = cache(function(guess, state, phi) {
  return Infer({model: function(){
    var utterance = utterancePrior()
    var L0belief = literalListener(utterance, guess)
    var utility = {
      epistemic: L0belief.score(state),
      social: marginalize(L0belief, "person").score("cute")
    }
    var speakerUtility = phi * utility.epistemic +
        (1 - phi) * utility.social
    factor(alpha*( speakerUtility - cost(utterance)))
    return utterance
  }})
})


// Pragmatic Listener (L1)
var pragmaticListener = cache(function(utterance) {
  return Infer({model: function(){
    var state = statePrior()
    var guess = guessPrior()
    var phi = 0.1
//     var phi = 0.9
    observe(speaker(guess, state, phi), utterance)
    return {state: state,
            guess: guess}
  }})
})

print("cute")
viz(pragmaticListener("cute"))
print("good")
viz(pragmaticListener("good"))
print("emmm")
viz(pragmaticListener("emmm"))
print("null")
viz(pragmaticListener("null"))
~~~~

###Section Two

What should we say in the first model in order to have the effect of praising a lady? 

The second model will analyze the effect of those the adjective in the first model

~~~~
// Given a grading scale from 0 - 10, we are able to assign a score to either person or dress
// In order to be accurate, I divide 10 into 20 pieces. Plus 0, there are 21 possible states
// according to a general belief, 7.5 is the average score
// possible states: 
var grading = {
  "beauty": [0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5,
             5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10],
  "probabilities": 1, 1, 2, 2, 4, 4, 6, 6, 8, 8, 9, 9,
                    10, 10, 10, 9, 8, 6, 4, 2, 1]
}


// assign each score with a probability
var statePrior = function() {
  return categorical(grading.probabilities, grading.beauty)
}


// randomly choose a score as the threhold from grading scale
var thetaPrior = function() {
  return uniformDraw(grading.beauty)
}
~~~~

Possible Utterances & Cost Function

~~~~
// generally, we can describe beauty in terms of three level: "bad", "good(average)", "cute"
// utterances
var utterances = ["bad", "good", "cute"]
var utterancePrior = function() {
  return uniformDraw(utterances)
}

// cost: same cost for three utterances
var cost = {
  "bad": 1,
  "good": 1,
  "cute": 1
}

// optimality
var alpha = 1
~~~~

Meaning Function

~~~~
// Set the threshold that has praise effect as "cute", theta
// the threshold for "good" will always be at the middle from 0 - theta
// below "good" is "bad", above "good" below "cute" is "good"
// meaning function:

var meaning = function(utterance, _score, theta) {
  utterance == "cute" ? _score >= theta:
  utterance == "good" ? (theta/2) <= _score & _score <= theta:
  utterance == "bad"  ? _score <= (theta/2):
  true
}
~~~~

RSA from vagueness model

~~~~
// literalListener (L0)
var literalListener = cache(function(utterance, theta) {
  return Infer({method: "enumerate"}, function() {
    var _score = uniformDraw(grading.beauty)
    condition(meaning(utterance, _score, theta))
    return _score
  })
})

// Speaker (S)
var speaker = cache(function(_score, theta) {
  return Infer({method: "enumerate"}, function() {
    var utterance = utterancePrior()
    factor(alpha*(literalListener(utterance,theta).score(_score)
                 - cost[utterance]))
    return utterance
  })
})

// PragmaticListener (L1)
var pragmaticListener = function(utterance) {
  return Infer({method: "enumerate"}, function() {
    var _score = statePrior()
    var theta = thetaPrior()
    factor(speaker(_score, theta).score(utterance))
    return {_score: _score, theta: theta}
  })
}
~~~~

Full Model

~~~~
var grading = {
  "beauty": [0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5,
             5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10],
  "probabilities": [1, 1, 2, 2, 4, 4, 6, 6, 8, 8, 9, 9,
                    10, 10, 10, 9, 8, 6, 4, 2, 1]
}

var statePrior = function() {
  return categorical(grading.probabilities, grading.beauty)
}

var thetaPrior = function() {
  return uniformDraw(grading.beauty)
}

var utterances = ["bad", "good", "cute"]
var utterancePrior = function() {
  return uniformDraw(utterances)
}

var cost = {
  "bad": 1,
  "good": 1,
  "cute": 1
}

var alpha = 1

var meaning = function(utterance, _score, theta) {
  utterance == "cute" ? _score >= theta:
  utterance == "good" ? (theta/2) <= _score & _score <= theta:
  utterance == "bad"  ? _score <= (theta/2):
  true
}

// literalListener (L0)
var literalListener = cache(function(utterance, theta) {
  return Infer({method: "enumerate"}, function() {
    var _score = uniformDraw(grading.beauty)
    condition(meaning(utterance, _score, theta))
    return _score
  })
})

// Speaker (S)
var speaker = cache(function(_score, theta) {
  return Infer({method: "enumerate"}, function() {
    var utterance = utterancePrior()
    factor(alpha*(literalListener(utterance,theta).score(_score)
                 - cost[utterance]))
    return utterance
  })
})

// PragmaticListener (L1)
var pragmaticListener = function(utterance) {
  return Infer({method: "enumerate"}, function() {
    var _score = statePrior()
    var theta = thetaPrior()
    factor(speaker(_score, theta).score(utterance))
    return {_score: _score, theta: theta}
  })
}

// viz(pragmaticListener("cute"))

var BAD = pragmaticListener("bad")
print("------------------------------------BAD------------------------------------")
print("the listener's posterior over adjective bad:")
viz.density(marginalize(BAD, "_score"))
print("the listener's posterior over adjective bad thresholds:")
viz.density(marginalize(BAD, "theta"))

var GOOD = pragmaticListener("good")
print("------------------------------------GOOD------------------------------------")
print("the listener's posterior over adjective good:")
viz.density(marginalize(GOOD, "_score"))
print("the listener's posterior over adjective good thresholds:")
viz.density(marginalize(GOOD, "theta"))

var CUTE = pragmaticListener("cute")
print("------------------------------------CUTE------------------------------------")
print("the listener's posterior over adjective cute:")
viz.density(marginalize(CUTE, "_score"))
print("the listener's posterior over adjective cute thresholds:")
viz.density(marginalize(CUTE, "theta"))
~~~~


##Grant Chodos & Adeh Hakobian

The “Kiki/Bouba” effect is a phenomenon in language reasoning wherein the vast majority of humans hold the preference for phrases like “kiki” to visibly ‘pointy’ shapes, and “bobo” to visibly ‘rounded’ shapes. This was first recorded by Wolfgang Kohler in 1929 and has been extensively researched ever since. Here, we have modeled this behavior using an RSA model to portray how a certain combination of sounds can elicit a preference for certain shapes. 
   
This effect pertains to two regions of linguistics primarily:  Iconicity; which is role shapes/imagery play in semantics; and Arbitrariness: which is the role of ambiguity. The Kiki/Bobo effect supports the notion that language is not so arbitrary; that it is formed and understood on conditional bases that are barely context dependent. In other words; some meanings are formed or restricted due to universal forces such as dimensionality, geometry, human anatomy-- what have you. 


Conditions that contribute to the effect include the phonetic capabilities of the human vocal tract (all possible sounds humans can physically make) and the muscle movements required to produce such sounds. These movements potentially delineable associations to shapes. Moreover, consonants are distinct sounds that guide when words begin/end, and consonants vary in average loudness, pitch, length, etc,  ergo some consonants such as “k” or “z” are perceivably more “loud” or harsh than “b” or “l”. 


Understanding contributing mechanisms allowed us to infer better ways to tweak our model so that it could reflect “real” human intuitions. It is also helped us in building the model, to begin with. 


Before diving into the model; it is important to discuss what is known as RSA; or “The Rational Speech Act framework”. RSA is a hypothetical simulatory design that effectively models communicative inference. It is a simulation because it is constructed in the form of code which exists solely in a probabilistic program (WebPPL). It is also hypothetical in that it simplifies reality and makes mathematical assumptions about human language reasoning. Despite this; RSA has a great deal of utility in language research because it provides scholars a closed (albeit “faux”) sample space that can closely capture “real” experimental results. Moreover, it provides an adjustable tool that can capably represent virtually any scenario in communication, and it can do so with large sample sizes (unattainable in reality). Thus the “imaginary data” that comes from skilled/careful RSA modelers can ultimately guide real experimental designs in future language studies. 


The code was designed as a “simple referential communication game” wherein synthetic speakers or listeners engage in recursive reasoning. In order to “build reason”; the framework employs a “reasoning hierarchy” that builds . In the simplest model, there are 3 levels to the hierarchy required for it to function: first exists the literal listener who hears a defined utterance and reasons about the state it conveys using the meaning function directly. Second is the pragmatic speaker that hears that state; then relies on Bayesian inference to choose an utterance corresponding to that state based on its utility (utility varies by cost, meaning, and other functions, as well as the data itself). The third is a pragmatic listener that calculates the probability of a state of the world given an utterance the pragmatic speaker would choose. 


This framework requires some input of probabilities (as numerical data)  in order to approximately imitate as well as predict human behavior in various instances of pragmatic reasoning. These probabilities are defined by priors that interact with the set of states, utterances, or other variables. 


When brainstorming for this project; we quickly turned to the Kiki/Bouba effect as a phenomenon to model inside of RSA because we knew it offered a very specific experimental design and dataset that we thought would be easy to work with. 


Although the model has a lot of information to work with; much of which fit smoothly into the RSA framework; some aspects of the model were difficult to capture and required thinking outside of the box! 


So now for the code:


We set up the world state prior so that there are two possible states defined as strings: “round” or “pointy”, with equal likelihood on either state due to uniformDraw in our statePrior. 


~~~~
//possible states of the world
var shapes = ["pointy","round"]


var shapeprior = function() {
  return uniformDraw(shapes)
~~~~

Next, we define two variables: Vowels and Consonants. These are not yet utterances that directly correspond to possible states of the world. These variables are introduced as the componential strings that will later feed into our possibleUtterance function, previously named our “wordMaker” function. These values were found based on our own interpretation of each sound and the human data presented by Fort, Martin, and Peperkamp (2015). 


We chose “i” and “o” as our vowels to keep the model simple, as opposed to including “e” and “u”, since the research data we based our inference on grouped “i/e” and “o/u” anyways.


We chose "b","z","n","k","l" as our consonants to keep things simple; our set contains only 5 instead of a possible 20+ consonant sounds.  Moreover, this set contains a super pointy letter “z”, a rather pointy letter “k”, a fringe letter “n”, a rather round letter “l”, and a super round letter “b”. This made it easy to estimate corresponding probabilities for each consonant from high roundness (b =0.9) and low roundness (z=0.05) that we thought would closely reflect human intuition. This also allowed us to create a polar relationship between states in the model; so that: cR closer to 0; utterance is perceivably more “pointy”
cR closer to 1; utterance is perceivable more “round”


~~~~
///fold:


var shapes = ["pointy","round"]


var shapeprior = function() {
  return uniformDraw(shapes)


var vowels = ["i","o"]




var consonants = ["b","z","n","k","l"]




var consonantsLookup = {
  b: 0.9,
  l: 0.849,
  n: 0.5,
  z: 0.05,
  k: 0.1
}
var vowelsLookup = {
  o: 0.8,
  i: 0.5
}




var roundinessLookup = function(word) {
  var consonant = word.split("")[0]
  var vowel = word.split("")[1]
  var consonantRoundiness = consonantsLookup[consonant]
  var vowelRoundiness = vowelsLookup[vowel]
  var compositeRoundiness = ((0.75*consonantRoundiness) + (0.25*vowelRoundiness)/2)
  return compositeRoundiness
}






var possibleUtterance = function(){
  var vowel = uniformDraw(vowels)
  var consonant = uniformDraw(consonants)
  var utt = consonant.concat(vowel)
  var utterance = utt.concat(utt)
  return utterance
}


//gathers the roundiness of the utterance based on the shape 
var meaning = function(utterance,shape) {
  shape == "round" ? flip(roundinessLookup(utterance)) : 
  flip(1-roundinessLookup(utterance)) 
}


possibleUtterance()


Infer({model: function(){meaning("bibi","round")}})


~~~~

Our model is unique in that it generates all possible utterances by using a concatenating function. To achieve this we introduced variable “utt” within the function and defined it as the concatenated product of a consonant and vowel sampled one time from the set. We then define “utterance” as a single “utt” that then gets concatenated with itself once more to create a full utterance. For example, if you run possibleUtterance() it will return a single utterance, which is any potential utterance that the word making function can spit out. 


Because consonants and vowels were individually considered in the research we based our model off of, we estimated prior probabilities for individual vowels and consonants that corresponded to a “roundiness” score in our model. To get our model to do some inference about whole utterance roundiness, we made a roundinessLookup function. This function acts as a lookup table that finds the probability associated with a vowel and a consonant; and calculates a joint probability by adding the values and dividing by two. NOTE: We also made the following revision to the compositeRoundiness function:  var compositeRoundiness = ((0.75*consonantRoundiness) + (0.25*vowelRoundiness)/2). By weighting consonant roundiness with three times the weight of vowel roundiness; we aligned our model better with real intuitions evidenced by Fort, Martin, and Peperkamp (2015); the intuition here being that consonants are much more significant in driving the Kiki/Bobo effect than vowels are. 


The round bias is exhibited in our meaning function as well, which checks to see if a possibleUtterance is “round”; if it is round, a coin is flipped with the roundiness probability that we defined. If it is NOT “round” (ergo “pointy”), then a coin is flipped with 1 minus the probability.  If you run Infer({model: function(){meaning("bibi","round")}}) it will demonstrate how bibi is only slightly round with 65% on round interpretation and 35% pointy. This is strictly on the literal level in the model, which becomes informative for our literal listener and the other layers in the model that become more pragmatic. 

Here, we've created a literal listener that takes in an utterance that it hears, and returns a shape based on the utterance itself, and only when the utterance and shapes return true. The cost function is for the speaker in the next code box.


~~~~
///fold:
var shapes = ["pointy","round"]


var shapeprior = function() {
  return uniformDraw(shapes)
}


var vowels = ["i","o"]


var consonants = ["b","z","n","k","l"]


var consonantsLookup = {
  b: 0.9,
  l: 0.849,
  n: 0.5,
  z: 0.05,
  k: 0.1
}
var vowelsLookup = {
  o: 0.8,
  i: 0.5
}


var roundinessLookup = function(word) {
  var consonant = word.split("")[0]
  var vowel = word.split("")[1]
  var consonantRoundiness = consonantsLookup[consonant]
  var vowelRoundiness = vowelsLookup[vowel]
  var compositeRoundiness = ((0.75*consonantRoundiness) + (0.25*vowelRoundiness)/2)
  return compositeRoundiness
}


var possibleUtterance = function(){
  var vowel = uniformDraw(vowels)
  var consonant = uniformDraw(consonants)
  var utt = consonant.concat(vowel)
  var utterance = utt.concat(utt)
  return utterance
}


var meaning = function(utterance,shape) {
  shape == "round" ? flip(roundinessLookup(utterance)) : 
  flip(1-roundinessLookup(utterance)) 
}




var cost = function(utterance) {
  return 0
}


//the Literal Listener
var literalListener = function(utterance) {
  Infer({model: function(){
    var shape = shapeprior()
    condition(meaning(utterance, shape))
    return shape
  }})
}


display("Literal Listener's posterior for 'bobo'")
viz(literalListener("bobo"))


display("Literal Listener's posterior for 'nini'")
viz(literalListener("nini"))


display("Literal Listener's posterior for 'kiki'")
viz(literalListener("kiki"))


display("Literal Listener's posterior for 'zizi'")
viz(literalListener("zizi"))
~~~~

Now we introduce the speaker; who takes in information from the literal listener. The speaker considers what the literal listener would infer the shape to be from the sound, and thereby chooses a sound based on the shape. The speaker must choose from all possibleUtterances simultaneously, and then distributes probability across each utterance depending on which are more “round” or “pointy”. Also, the cost function is set to 0, because we assume no difference in the cost of uttering the words themselves as they are all the same number of letters.


The probability can be shifted either by the optimality value by making the speaker more tuned towards giving a more accurate utterance to convey the best shape. In addition, composite roundiness can be changed to make the model more accurate. Also, the values for the vowels can also be toggled to create a more accurate model, such that the closer they are in value, the better they represent the sharper and rounder consonant sounds, but the farther they are from each other, the better they represent the consonant phonemes in the middle such as /n/. Lastly, the speaker's optimality can also be changed to affect the roundiness/pointiness probability of each utterance, such that the higher the alpha value is, the probability of each utterance's shape quality shifts towards the extremes. We have found that an alpha value of 3 seems to best represent the data collected from the Fort, Martin, and Peperkamp (2015) paper. 


~~~~
///fold:
var shapes = ["pointy","round"]


var shapeprior = function() {
  return uniformDraw(shapes)
}




var vowels = ["i","o"]




var consonants = ["b","z","n","k","l"]




var consonantsLookup = {
  b: 0.9,
  l: 0.849,
  n: 0.5,
  z: 0.05,
  k: 0.1
}
var vowelsLookup = {
  o: 0.8,
  i: 0.5
}


var roundinessLookup = function(word) {
  var consonant = word.split("")[0]
  var vowel = word.split("")[1]
  var consonantRoundiness = consonantsLookup[consonant]
  var vowelRoundiness = vowelsLookup[vowel]
  var compositeRoundiness = ((0.75*consonantRoundiness) + (0.25*vowelRoundiness)/2)
  return compositeRoundiness
}




var possibleUtterance = function(){
  var vowel = uniformDraw(vowels)
  var consonant = uniformDraw(consonants)
  var utt = consonant.concat(vowel)
  var utterance = utt.concat(utt)
  return utterance
}
///




var meaning = function(utterance,shape) {
  shape == "round" ? flip(roundinessLookup(utterance)) : 
  flip(1-roundinessLookup(utterance)) 
}






var cost = function(utterance) {
  return 0
}




var literalListener = function(utterance) {
  Infer({model: function(){
    var shape = shapeprior()
    condition(meaning(utterance, shape))
    return shape
  }})
}




var alpha = 3


var speaker = function(shape){
  Infer({model: function(){
    var utterance = possibleUtterance()
    factor(alpha * (literalListener(utterance).score(shape) - cost(utterance)))
    return utterance
  }})
}


display("the speaker's posterior for pointy")
viz.hist(speaker("pointy"))


display("the speaker's posterior for round")
viz.hist(speaker("round"))
~~~~

Voila! The speaker is choosing from all possible utterances and doing a superb job reflecting proper reasoning about which utterances ought to have “round” interpretations (“bobo” and “lolo”) and which ought to have “pointy” interpretations (“zizi” and “kiki”). 

Next, we introduce the pragmatic listener. This listener hears a single utterance and interprets it based on the choices the speaker would make, then pragmatically reasons the shape the word communicates. This completes our RSA model of the Kiki/Boba experimental design; our model now demonstrates pragmatic reasoning where iconicity decreases arbitrariness in language. 


~~~~
///fold:
//possible states of the world
var shapes = ["pointy","round"]


var shapeprior = function() {
  return uniformDraw(shapes)
}


//possible vowels
var vowels = ["i","o"]


//possible consonants
var consonants = ["b","z","n","k","l"]


//roundiness value for consonants and vowels from 0 to 1
var consonantsLookup = {
  b: 0.9,
  l: 0.849,
  n: 0.5,
  z: 0.05,
  k: 0.1
}
var vowelsLookup = {
  o: 0.8,
  i: 0.5
}


//This function looks up the roundiness value of the nonsense word as a whole
var roundinessLookup = function(word) {
  var consonant = word.split("")[0]
  var vowel = word.split("")[1]
  var consonantRoundiness = consonantsLookup[consonant]
  var vowelRoundiness = vowelsLookup[vowel]
  var compositeRoundiness = ((0.75*consonantRoundiness) + (0.25*vowelRoundiness)/2)
  return compositeRoundiness
}


//this function creates a possible utterance based on possible consonants and vowels
var possibleUtterance = function(){
  var vowel = uniformDraw(vowels)
  var consonant = uniformDraw(consonants)
  var utt = consonant.concat(vowel)
  var utterance = utt.concat(utt)
  return utterance
}
///


//gathers the roundiness of the utterance based on the shape 
var meaning = function(utterance,shape) {
  shape == "round" ? flip(roundinessLookup(utterance)) : 
  flip(1-roundinessLookup(utterance)) 
}




//cost of the utterance, its set to 0 because no difference between each of the utterances
var cost = function(utterance) {
  return 0
}


//the Literal Listener
var literalListener = function(utterance) {
  Infer({model: function(){
    var shape = shapeprior()
    condition(meaning(utterance, shape))
    return shape
  }})
}


//the speaker's optimality value
var alpha = 3


var speaker = function(shape){
  Infer({model: function(){
    var utterance = possibleUtterance()
    factor(alpha * (literalListener(utterance).score(shape) - cost(utterance)))
    return utterance
  }})
}


//the Pragmatic Listener
var pragmaticListener = function(utterance){
  Infer({model: function(){
    var shape = shapeprior()
    factor((speaker(shape).score(utterance)))
    return shape
  }})
}


display("Pragmatic Listener's posterior for 'bobo'")
viz(pragmaticListener("bobo"))


display("Pragmatic Listener's posterior for 'nini'")
viz(pragmaticListener("nini"))


display("Pragmatic Listener's posterior for 'nono'")
viz(pragmaticListener("nono"))


display("Pragmatic Listener's posterior for 'kiki'")
viz(pragmaticListener("kiki"))


display("Pragmatic Listener's posterior for 'zizi'")
viz(pragmaticListener("zizi"))
~~~~

This model is similar to those discussed in class only in that it utilizes the RSA framework. It tackles a unique phenomenon with niche qualities that requires for us to think outside the box.  


One difference from other models is how we do the meaning function: rather than a clean mapping from utterances to states as seen in the vanilla model; our mapping is more indirect . 
In our model; possible utterances are not explicitly defined; but rather “generated” simultaneously.

##Zeinab Kackakeche

###"Heavy or not?": a comparison class model

Suppose that you hear an utterance "This box is heavy" from an adult, you're likely to infer that this box is actually heavy, and  weighs between let's say 20 lbs and 100 lbs depending on your experience with heavy weights.

If you hear a child saying this box is heavy, however, you might infer that this box is heavy relative to the boxes that this child has lifted. It might be true that this box weighs 100 lbs, but there is a chance that this box is not heavy and weighs as little as 2 lbs. 

This might also be true if you hear a bodybuilder saying this box is "light". Because "light" for a bodybuilder might be heavy for the listener. 

Children, adults, and body builders have different experiences with weights, so the listener's interpretation of the box weight after hearing the utterance "heavy" changes depending on who said the utterance.

I model this empirical phenomenon using the Rational Speech Act (RSA) framework. RSA views communication as recursive reasoning between a speaker and a listener. The listener interprets the utterance they heard by reasoning about the speaker. In this case, the listener reasons about who said the "box is heavy". The listener assumes that the speaker's intention is to inform a naive listener about the actual state of the world. Then the listener reasons about what the state of the world is likely to be given that a speaker produced some utterance, knowing that the speaker is reasoning about how a listener is most likely to interpret that utterance. So in this model, the listener reasons about the actual weight of the box, given that the speaker who said "the box is heavy" is cooperative and intends to deliver her utterance in a way that the listener interprets it correctly. 
I chose to use a comparison-class model from chapter 5. The model discusses gradable adjectives, for example: "tall". The listener has uncertainty about the relevant comparison class: superordinate (e.g., tall compared to all people) or subordinate (e.g., compared to gymnasts or soccer players or basketball players). And this reasoning depends on prior knowledge about the relevant categories.


In my model, the listener's prior knowledge about the comparison class is the experience of each speaker with weights. We have three comparison classes: a child, an adult, and a bodybuilder. Each of these three comparison-classes have different experience with weights, i.e. different priors.


~~~~
// helper function
var exp = function(x){return Math.exp(x)}

// helper function
var marginalize = function(dist, key){
  return Infer({model: function(){sample(dist)[key]}})
}
// for discretization
// turns a continuous distribution into a discrete one by deviding it into bins
var binParam = 5; 

//original comparison model:
var superordinate = {mu: 0, sigma: 1}; 

//my model:
var superordinate = {mu: 3, sigma: 1}; 

//a list of possible box weights (state values)
//this makes the range of the distribution between 0 and 6
// so we're dealing with weights between 0(lightest weight) and 6(heaviest weight)
var stateVals = _.range(superordinate.mu - 3 * superordinate.sigma,
                superordinate.mu + 3 * superordinate.sigma,
                superordinate.sigma/binParam)


// for each possible weight, calculate its probability of occurrence
// Using a Gaussian function is simply using a normal distribution 
//that's more concentrated in the around the mu(mean)
var stateProbs = function(stateParams){
  return map(function(s){
    exp(Gaussian(stateParams).score(s))
  }, stateVals)
}; 


// generate a statePrior using the possible weights and their probabilities
var generateStatePrior = function(stateParams) {
  return Infer({
    model: function(){ 
      return categorical({vs: stateVals, ps: stateProbs(stateParams)}) 
    }
  })
};


// SubParams in the original model:
var subParams = {
  low: {mu: -1, sigma: 0.5}, // gymnast heights
  middle: {mu: 0, sigma: 0.5}, // soccer player heights
  high: {mu: 1, sigma: 0.5} // basketball player heights
}

// information about the category priors
var speakerParams = {
  child: {mu: 0.5, sigma: 1}, // child experience with weights
  adult: {mu: 2, sigma: 3}, // adult experiance with weights
  bodybuilder: {mu:5, sigma:3}, // body builder experience with weights
}


display("hypothetical child experience with heavy weights")
viz.density(generateStatePrior(speakerParams["child"]))
display("hypothetical adult experience with heavy weights")
viz.density(generateStatePrior(speakerParams["adult"]))
display("hypothetical bodybuilders experience with heavy weights")
viz.density(generateStatePrior(speakerParams["bodybuilder"]))
~~~~

These results match intuitions: children experience with weights is mostly with very light to light weights, while adults have more experience with heavier weights, and bodybuilders have experience with very heavy weights.
After we've seen the experience (or the listener's prior) for each of the speakers, let's see how would the listener interpret the adjective "heavy" after hearing it from each one of the speakers.

~~~~
// helper function
var exp = function(x){return Math.exp(x)}

// helper function
var marginalize = function(dist, key){
  return Infer({model: function(){sample(dist)[key]}})
}
// for discretization
var binParam = 5; 

//my model:
var superordinate = {mu: 3, sigma: 1}; 

//a list of possible box weights (state values)
var stateVals = _.range(superordinate.mu - 3 * superordinate.sigma,
                superordinate.mu + 3 * superordinate.sigma,
                superordinate.sigma/binParam)


// for each possible weight, calculate its probability of occurrence
var stateProbs = cache(function(stateParams){
  return map(function(s){
    exp(Gaussian(stateParams).score(s))
  }, stateVals)
}); 


// generate a statePrior using the possible weights and their probabilities
var generateStatePrior = cache(function(stateParams) {
  return Infer({
    model: function(){ 
      return categorical({vs: stateVals, ps: stateProbs(stateParams)}) 
    }
  })
});

// information about the category priors
var speakerParams = {
  child: {mu: 0.5, sigma: 1}, // child experience with weights
  adult: {mu: 2, sigma: 3}, // adult experiance with weights
  bodybuilder: {mu:5, sigma:3}, // body builder experience with weights
}


// generate the uniform threshold prior

//original model:
var thresholdBins = cache(function(form, stateSupport){
  return map(function(x){
    return form == "positive" ? x - (1/(binParam*2)) : x + (1/(binParam*2));
  }, sort(stateSupport))
})

//my model:
var thresholdBins = cache(function(utterance, stateSupport){ 
  return map(function(x){
    return utterance == "heavy" ? x - (1/(binParam*2)) : x + (1/(binParam*2));
  }, sort(stateSupport))
})


var thresholdPrior = cache(function(utterance, stateSupport){
  return Infer({
    model: function() { return uniformDraw(thresholdBins(utterance, stateSupport)) }
  });
});

// original model:
var utterances = {
  positive: ["positive_null", "positive_sub", "positive_super"],
  negative: ["negative_null", "negative_sub", "negative_super"]
}

//my model:
var utterances = ["heavy","light"]


// meaning function for utterances
var meaning = function(utterance, state, threshold) {
  utterance == "heavy" ? state > threshold ? flip(0.9999) : flip(0.0001) :
  utterance == "light" ? state < threshold ? flip(0.9999) : flip(0.0001) :
  true
}




// set sepeaker optimality
var alpha = 5;

//original model:
var literalListener = cache(
  function(u, threshold, comparisonClass, subordinate) {
    Infer({model: function(){
      var utterance =  u.split("_")[0], explicitCC =  u.split("_")[1]
      // if the comparison class is explicit in the utterance, use that
      // otherwise, use whatever the pragmaticListener model passes in
      var cc = explicitCC == "null" ?  comparisonClass :
               explicitCC == "silence" ? comparisonClass : explicitCC
      var state = sample(generateStatePrior(cc === "super" ? 
         superordinate : subordinate));
      var m = meaning(utterance, state, threshold);
      condition(m);
      return state;
    }})
  }, 10000 // limit cache size
)

//my model:
var literalListener = cache(
  function(utterance, threshold,comparisonClass){
    Infer({model: function(){
      var state = sample(generateStatePrior(speakerParams[comparisonClass]))
      var m = meaning(utterance, state, threshold);
      condition(m);
      return state;
    }})
  })

// literalListener("heavy", 0.5, "child")

//original model:
var speaker1 = cache(
  function(state, threshold, comparisonClass, form, subordinate) {
    Infer({model: function(){
      var utterance = uniformDraw(utterances[form]);
      var L0 = literalListener(utterance, threshold, 
                               comparisonClass, subordinate);
      factor( alpha * L0.score(state) );
      return utterance;
    }})
  }, 10000 // limit cache size
)

//my model:
var speaker1 = cache(
  function(state, threshold,comparisonClass){
    Infer({model: function(){
      var utterance = uniformDraw(utterances);
      var L0 = literalListener(utterance, threshold, comparisonClass);
      factor( alpha * L0.score(state) );
      return utterance;
    }})
  })


// generateStatePrior(speakerParams["child"]) .support()


//original model:
var pragmaticListener = cache(function(utterance, subordinate) {
  Infer({model: function(){
    var form = utterance.split("_")[0];
    var explicitCC = utterance.split("_")[1];

    var statePrior = generateStatePrior(
      subordinate
    );
    var state = sample(statePrior);
    var threshold = sample(thresholdPrior(form, statePrior.support()))
    // uncertainty about the comparison class (super vs. sub)
    var c = sample(classPrior)

    var S1 = speaker1(state, threshold, c, form, subordinate);
    observe(S1, utterance);
    return { comparisonClass: c, state: state }
  }})
}, 10000 // limit cache size
                             )



//My model:
var pragmaticListener = cache(function(utterance,comparisonClass){
  Infer({model: function(){
    var statePrior = generateStatePrior(speakerParams[comparisonClass]);
    var state = sample(statePrior);
    var threshold = sample(thresholdPrior(utterance, statePrior.support()))
    var S1 = speaker1(state, threshold,comparisonClass);    
    observe(S1, utterance);
    return state
}})
})
   

display("Listener's interpretation after hearing a child saying 'this box is heavy'")
viz.density(pragmaticListener("heavy","child"))
display("Listener's interpretation after hearing an adult saying 'this box is heavy'")
viz.density(pragmaticListener("heavy","adult"))
display("Listener's interpretation after hearing a bodybuilder saying 'this box is heavy'")
viz.density(pragmaticListener("heavy","bodybuilder"))
display("Listener's interpretation after hearing a child saying 'this box is light'")
viz.density(pragmaticListener("light","child"))
display("Listener's interpretation after hearing an adult saying 'this box is light'")
viz.density(pragmaticListener("light","adult"))
display("Listener's interpretation after hearing a body builder  saying 'this box is light'")
viz.density(pragmaticListener("light","bodybuilder"))
~~~~

Similar to what one would expect, the listener's interpretation of the adjectives "heavy" and "light" are different depending on who said the utterance and on the listener's prior knowledge about each speaker . But with this previous code, we only considered one threshold for both adjective. If the actual weight of the box was less than threshold then the box is interpreted as "light", and if the actual weight of the box was more than threshold then the box is interpreted as "heavy". However, in the real world the area in which we consider boxes to be heavy or light is grey and not binary. So, for the following code, we consider two different thresholds for "heavy" and "light".

~~~~
// helper function
var exp = function(x){return Math.exp(x)}

// helper function
var marginalize = function(dist, key){
  return Infer({model: function(){sample(dist)[key]}})
}
// for discretization
var binParam = 3; 

//my model:
var superordinate = {mu: 3, sigma: 1}; 

//a list of possible box weights (state values)
var stateVals = _.range(superordinate.mu - 3 * superordinate.sigma,
                superordinate.mu + 3 * superordinate.sigma,
                superordinate.sigma/binParam)


// for each possible weight, calculate its probability of occurrence
var stateProbs = function(stateParams){
  return map(function(s){
    exp(Gaussian(stateParams).score(s))
  }, stateVals)
}; 


// generate a statePrior using the possible weights and their probabilities
var generateStatePrior = cache(function(stateParams) {
  return Infer({
    model: function(){ 
      return categorical({vs: stateVals, ps: stateProbs(stateParams)}) 
    }
  })
});

// information about the category priors
var speakerParams = {
  child: {mu: 0.5, sigma: 1}, // child experience with weights
  adult: {mu: 2, sigma: 3}, // adult experiance with weights
  bodybuilder: {mu:5, sigma:3}, // body builder experience with weights
}


// generate the uniform threshold prior

var thresholdBins = cache(function(utterance, stateSupport){ 
  return map(function(x){
    return utterance == "heavy" ? x - (1/(binParam*2)) : x + (1/(binParam*2));
  }, sort(stateSupport))
})


var thresholdPrior = cache(function(utterance, stateSupport){
  return Infer({
    model: function() { return uniformDraw(thresholdBins(utterance, stateSupport)) }
  });
});



var utterances = ["heavy","light"]


// meaning function for utterances
var meaning = function(utterance, state, thresholdHeavy, thresholdLight) {
  utterance == "heavy" ? state > thresholdHeavy ? flip(0.9999) : flip(0.0001) :
  utterance == "light" ? state < thresholdLight ? flip(0.9999) : flip(0.0001) :
  true
}




// set sepeaker optimality
var alpha = 2;


var literalListener =cache(
  function(utterance, thresholdHeavy, thresholdLight,comparisonClass){
    Infer({model: function(){
      var state = sample(generateStatePrior(speakerParams[comparisonClass]))
      var m = meaning(utterance, state, thresholdHeavy, thresholdLight);
      condition(m);
      return state;
    }})
  })


//literalListener("light", 4, 2, "adult")


//my model:
var speaker1 = 
  function(state, thresholdHeavy, thresholdLight,comparisonClass){
    Infer({model: function(){
      var utterance = uniformDraw(utterances);
      var L0 = literalListener(utterance, thresholdHeavy, thresholdLight, comparisonClass);
      factor( alpha * L0.score(state) );
      return utterance;
    }})
  }


// generateStatePrior(speakerParams["child"]) .support()

//My model:
var pragmaticListener = cache(function(utterance,comparisonClass){
  Infer({method: "enumerate", model: function(){
    var statePrior = generateStatePrior(speakerParams[comparisonClass]);
    var state = sample(statePrior);
    var thresholdHeavy = sample(thresholdPrior("heavy", statePrior.support()))
    var thresholdLight = sample(thresholdPrior("light", statePrior.support()))
    var S1 = speaker1(state, thresholdHeavy, thresholdLight,comparisonClass);    
    condition(thresholdLight< thresholdHeavy)
    observe(S1, utterance);
    return state
//     return {state: state, thresholdHeavy: thresholdHeavy, thresholdLight: thresholdLight}
}})
})
   
// var L1childHeavy = pragmaticListener("heavy","child")
// var L1childLight = pragmaticListener("light","child")
// var thetaH = expectation(marginalize(L1childHeavy,"thresholdHeavy"))
// display(thetaH)
// var thetaL = expectation(marginalize(L1childLight,"thresholdLight"))
// display(thetaL)

display("Listener's interpretation after hearing a child saying 'this box is heavy'")
viz.density(pragmaticListener("heavy","child"))
display("Listener's interpretation after hearing an adult saying 'this box is heavy'")
viz.density(pragmaticListener("heavy","adult"))
display("Listener's interpretation after hearing a bodybuilder saying 'this box is heavy'")
viz.density(pragmaticListener("heavy","bodybuilder"))
display("Listener's interpretation after hearing a child saying 'this box is light'")
viz.density(pragmaticListener("light","child"))
display("Listener's interpretation after hearing an adult saying 'this box is light'")
viz.density(pragmaticListener("light","adult"))
display("Listener's interpretation after hearing a body builder  saying 'this box is light'")
viz.density(pragmaticListener("light","bodybuilder"))
~~~~

There are instances when the child for example says "heavy" and she means it's heavy for an adult as well, not only compared to what she has lifted before. So let's say a child sees a box that weighs 100 lbs and says it's heavy. The listener in this case takes into account the prior distribution of both child and adult, not only the child distribution.

My complete model below:

~~~~
// helper function
var exp = function(x){return Math.exp(x)}

// helper function
var marginalize = function(dist, key){
  return Infer({model: function(){sample(dist)[key]}})
}
// for discretization
var binParam = 5; 

//my model:
var superordinate = {mu: 3, sigma: 1}; 

//a list of possible box weights (state values)
var stateVals = _.range(superordinate.mu - 3 * superordinate.sigma,
                superordinate.mu + 3 * superordinate.sigma,
                superordinate.sigma/binParam)


// for each possible weight, calculate its probability of occurrence
var stateProbs = function(stateParams){
  return map(function(s){
    exp(Gaussian(stateParams).score(s))
  }, stateVals)
}; 


// generate a statePrior using the possible weights and their probabilities
var generateStatePrior = cache(function(stateParams) {
  return Infer({
    model: function(){ 
      return categorical({vs: stateVals, ps: stateProbs(stateParams)}) 
    }
  })
});

// information about the category priors
var speakerParams = {
  child: {mu: 0.5, sigma: 1}, // child experience with weights
  adult: {mu: 2, sigma: 3}, // adult experiance with weights
  bodybuilder: {mu:5, sigma:3}, // body builder experience with weights
}


// generate the uniform threshold prior

var thresholdBins = cache(function(utterance, stateSupport){ 
  return map(function(x){
    return utterance == "heavy" ? x - (1/(binParam*2)) : x + (1/(binParam*2));
  }, sort(stateSupport))
},10000
)

var thresholdPrior = cache(function(utterance, stateSupport){
  return Infer({
    model: function() { return uniformDraw(thresholdBins(utterance, stateSupport)) }
  });
},10000
);



var utterances = ["heavy","light"]


// meaning function for utterances
var meaning = function(utterance, state, thresholdHeavy, thresholdLight) {
  utterance == "heavy" ? state > thresholdHeavy ? flip(0.9999) : flip(0.0001) :
  utterance == "light" ? state < thresholdLight ? flip(0.9999) : flip(0.0001) :
  true
}




// set sepeaker optimality
var alpha = 5;


var literalListener =cache(
  function(utterance, thresholdHeavy, thresholdLight,comparisonClass){
    Infer({model: function(){
      var state = sample(generateStatePrior(speakerParams[comparisonClass]))
      var m = meaning(utterance, state, thresholdHeavy, thresholdLight);
      condition(m);
      return state;
    }})
 },10000
)


//literalListener("light", 4, 2, "adult")


//my model:
var speaker1 = cache(
  function(state, thresholdHeavy, thresholdLight,comparisonClass){
    Infer({model: function(){
      var utterance = uniformDraw(utterances);
      var L0 = literalListener(utterance, thresholdHeavy, thresholdLight, comparisonClass);
      factor( alpha * L0.score(state) );
      return utterance;
    }})
  },10000
)


// generateStatePrior(speakerParams["child"]) .support()

var comparisonClasses = ["child","adult","bodybuilder"]
var comparisonClassPrior = function(whoSaidIt) {
    whoSaidIt == "child" ? categorical([0.75, 0.25,0.15],comparisonClasses):
    whoSaidIt == "adult" ? categorical([0.01,0.7,0.5],comparisonClasses):
                           categorical([0.0001,0.2, 0.99],comparisonClasses)
                
                                                 
}

var pragmaticListener = cache(function(utterance,whoSaidIt){
  Infer({model: function(){
    var CC = comparisonClassPrior(whoSaidIt);
    var statePrior = generateStatePrior(speakerParams[CC]);
    var state = sample(statePrior);
    var thresholdHeavy = sample(thresholdPrior("heavy", statePrior.support()))
    var thresholdLight = sample(thresholdPrior("light", statePrior.support()))
    var S1 = speaker1(state, thresholdHeavy,thresholdLight,CC);    
    observe(S1, utterance);
    return (state)
}})
},10000
)
   


// pragmaticListener("heavy","adult")
display("Listener's interpretation after hearing a child saying 'this box is heavy'")
viz.density(pragmaticListener("heavy","child"))
display("Listener's interpretation after hearing an adult saying 'this box is heavy'")
viz.density(pragmaticListener("heavy","adult"))
display("Listener's interpretation after hearing a bodybuilder saying 'this box is heavy'")
viz.density(pragmaticListener("heavy","bodybuilder"))
display("Listener's interpretation after hearing a child saying 'this box is light'")
viz.density(pragmaticListener("light","child"))
display("Listener's interpretation after hearing an adult saying 'this box is light'")
viz.density(pragmaticListener("light","adult"))
display("Listener's interpretation after hearing a bodybuilder  saying 'this box is light'")
viz.density(pragmaticListener("light","bodybuilder"))
~~~~

As we can see from these results, the listener's interpretation is slightly different if he doesn't know what the speaker's intention is. So, if a child says the box is "heavy" and the listener doesn't actually know if the box is heavy for a child or heavy in general, the listener has to take into account the other comparison classes' priors. However, the listener would give more probability to child prior and less probability to the other two classes.


##Abimael Hernandez Jimenez, Paula Aruby Marquez, and Cesar Manuel Rosales Jr.

The Rational Speech Act framework views language as recursive reasoning between a literal listener, speaker, and a pragmatic listener. Using Bayesian inference, the pragmatic listener updates their prior beliefs about the state of the world by reasoning over an utterance given by a cooperative speaker. The speaker selects an utterance based on the probability of a literal listener arriving at the correct state of the world given the literal meaning of an utterance.

Models have been created to depict non-literal language. Non-literal language refers to when the utterance is false. There are several forms of non-literal language, including hyperbole, metaphor, and irony, the latter which is the focus of this chapter. Irony is when a person states an utterance but means the opposite of what is said. In our example, we describe individuals conversing about the weather in a sunny context i.e. California. An ironic expression in this model is conveyed when the individual complains about the weather when in fact the weather is nice or pleasant. Irony is depicted because the speaker says something but means the opposite. 

The original irony model shares characteristics with the hyperbole model. The hyperbole model introduced and implemented QUD manipulation into its code. The pricePrior in the hyperbole model depicts how much is spent, i.e. state of the world. It parallels the purpose of the statePrior in the irony model, which identifies the status of the weather. Further, the valencePrior points out how the speaker feels toward the state in both models. In the hyperbole model, it demonstrates how the speaker feels toward the amount spent. And in the irony model, it showcases how the speaker feels about the weather. 

In our irony model, the arousalPrior is introduced. Arousal is used to describe the speaker's feelings toward a state. For example, “terrible” correlates with a strong level of arousal, or "high." On the other hand, “ok” would be at the lower spectrum of arousal, or "low." The original model introduced arousal as a binary option where only high and low arousal could be assumed. Assuming binary arousal is not accurate enough even though the listener does interpret non-literal language. We changed our model to a continuous interpretation. Continuous arousal would make the model more accurate for understanding irony. We then added the prior which is a function that takes in a state and for that state runs a categorical over all the arousals. The categorical helps to define which state would carry higher arousal. 

The following section discusses key differences between the original irony model and our revised one.

~~~~
/old
var states = ['terrible', 'ok', 'amazing']

//new
var states = ['terrible','bad','ok','good','amazing']
~~~~

In order to account for our "continuous" arousal, we need to add additional states. In the original model, there are 3 states, which we expanded to 5.

~~~~
//old
var statePrior = function() {
  categorical([1, 50, 50], states)
}

//new
var statePrior = function() {
  categorical([1,20,50,50,50], states)
}
~~~~

We also increased the number of priors corresponding to the states.

~~~~
//old
var valencePrior = function(state) {
  state === "terrible" ? flip(0.99) ? -1 : 1 :
  state === "ok" ? flip(0.5) ? -1 : 1 :
  state === "amazing" ? flip(0.01) ? -1 : 1 :
  true
}

//new
var valencePrior = function(state) {
  state === "terrible" ? flip(0.99) ? -1 : 1 :
  state === "bad" ? flip(0.90) ? -1 : 1 :
  state === "ok" ? flip(0.5) ? -1 : 1 :
  state === "good" ? flip(0.09) ? -1 : 1 :
  state === "amazing" ? flip(0.01) ? -1 : 1 :
  true
}
~~~~

Due to adding new states, we added more valencePriors. Each state was assigned a probability of having a negative valence.

~~~~
//old
var arousals = ["low", "high"]

//new
var arousals = [.1,.3,.5,.7,.9]
~~~~

The original model computed arousal in a binary manner. We modified it to be “continuous.” We assigned probabilities onto each of the arousal levels. As a result, this increases the likelihood of assigning a higher level (.9) of arousal for terrible as it is 50 times more likely to occur than at a lower level of arousal such as .1.

~~~~
//old
var arousalPrior = function(state) {
  state === "terrible" ? categorical([0.1, 0.9], arousals) :
  state === "ok" ? categorical([0.9, 0.1], arousals) :
  state === "amazing" ? categorical([0.1, 0.9], arousals) :
  true
}

//new
var arousalPrior = function(state) {
  state === "terrible" ? categorical([1,10,30,45,50], arousals) :
  state === "bad" ? categorical([1,5,25,40,45], arousals) :
  state === "ok" ? categorical([50,45,30,10,1], arousals) :
  state === "good" ? categorical([1,5,25,40,45], arousals) :
  state === "amazing" ? categorical([1,10,30,45,50], arousals) :
  true
}
~~~~

In the arousalPrior function, we expanded the number of probabilities that map onto the state. For "terrible", there is a greater chance of being assigned a higher level of arousal. In contrast, "ok" has a lower chance of being assigned a higher level of arousal.

~~~~
// Define goals and goal priors. Could want to communicate state of the world,    
// valence about it, or arousal (intensity of feeling) about it.
var goals = ["goalState", "goalValence", "goalArousal"]

var goalPrior = function() {
  categorical([1, 1, 1], goals)
}
~~~~

The purpose of the goalState is for the speaker to be able to communicate to the literalListener what the question under discussion is (the topic of conversation). The value of this function is that it aids in the pragmatic listener’s ability to interpret non-literal language by running through every possible question under discussion that the speaker could be referring to. The addition of the two affect dimensions (valence and arousal) provide the speaker with additional pieces of information that can be conveyed in order to produce ironic utterances. For example, a speaker may wish to convey how they feel about the weather over simply describing the state of the weather. The goalState allows the speaker to determine exactly what topic of conversation is.

Below is the full model.

~~~~
// There are three possible states the weather could be in: 
// terrible, ok, or amazing
var states = ['terrible','bad','ok','good','amazing']

// Since we are in California, the prior over these states
// are the following. Once could also imagine this being 
// the prior in a certain context, e.g. when it's clearly
// sunny and nice out.
var statePrior = function() {
  categorical([1,5,40,40,40], states)
}

// Valence prior defined in terms of negative valence. 
// If the current state is terrible, it's extremely likely
// that the valence associated is negative. If it's ok, then 
// the valence could be negative or positive with equal 
// probability.
var valencePrior = function(state) {
  state === "terrible" ? flip(0.99) ? -1 : 1 :
  state === "bad" ? flip(0.90) ? -1 : 1 :
  state === "ok" ? flip(0.5) ? -1 : 1 :
  state === "good" ? flip(0.09) ? -1 : 1 :
  state === "amazing" ? flip(0.01) ? -1 : 1 :
  true
}

// Define binary arousals (could model as continuous).
// var arousals = ["low", "high"]
var arousals = [.1,.3,.5,.7,.9]

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
  state === "terrible" ? categorical([1,10,30,45,50], arousals) :
  state === "bad" ? categorical([1,5,25,40,45], arousals) :
  state === "ok" ? categorical([50,45,30,10,1], arousals) :
  state === "good" ? categorical([1,5,25,40,45], arousals) :
  state === "amazing" ? categorical([1,10,30,45,50], arousals) :
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

//The speaker takes in a state, valence, arousal, and a goal and returns an utterance 
//based on the probability of the literalListener arriving at the correct
//state given a goalState
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
    return {state,valence, arousal}
  }})
}

viz.table(pragmaticListener('terrible'))
~~~~

Our model was successful in capturing irony. When the speaker utters "terrible," the most likely state that the pragmatic listener predicts that the speaker actually means to say is "amazing." This is supported by the fact that it associates "amazing" with positive valence and 0.9 arousal. Positive valence indicates that the pragmatic listener infers that the speaker feels positively about the state. Additionally, an arousal level of 0.9 demonstrates that it thinks the speaker feels very strongly about the state. In this case, the pragmatic listener understands that the speaker does not actually mean that the weather is terrible. Their valence and arousal indicate that they understand the speaker is talking about good weather. The rest of the results also illustrate an understanding of irony. The second state predicted was "ok," with negative valence and 0.3 arousal. In this instance, the pragmatic listener is saying that the speaker does not feel that the state is unpleasant, but rather that the weather is neither good or bad. 

This section provides a summary and discussion. Our revised model added two states: "bad" and "good," leading to a total of five states. The original model also saw arousal as a binary option: "low" or "high." Our model revised this to include a continuous view of arousal. The effect this had on our results was that there was now much more probability to distribute as there were more states and arousals that were possible. 

The priors in our model play a major role in determining what the model concludes from an utterance. In this scenario, our five states correspond to the states of the world in California. Most of the time the weather will be “ok”, “good”, or “amazing.” Therefore, we assign more probability to these states apriori. Meaning, these are the more likely utterances given the current state of the world. These states are then used by the speaker to reason about the probability of a literalListener arriving at the correct state of the world. The pragmaticListener takes into account the speaker’s utterance and may infer that the speaker is selecting a certain state to convey arousal or valence. By considering valence, the model is able to infer a positive state from an utterance such as "terrible" or "bad" (Kao et. al., 2015).


##Yongjia Song

###Politeness Model of Acceptance and Refusal

Sometimes, people choose utterances to express their thoughts clearly. However, 
sometimes, people like to choose utterances that may not directly express their 
real thoughts. Politeness is one example of this kind of indirect expression. In the 
process of learning politeness model, we know that people choose utterances 
that express the opposite meaning of what the speaker is truly thinking. For 
example, when people are rating a terrible bread baked by their friend, people 
would like to say "it's amazing" instead of " it's terrible" to show their politeness. 
People also need to be polite when they accept or refuse a gift. We assume that 
one person gives a gift to another person, who is a speaker. In general, the 
speaker need to choose either "Thanks" as acceptance or "No thanks" as refusal. 
However, if a speaker doesn't like this gift but, at the same time, he or she also 
need to be polite, what utterance will the speaker choose? To show politeness, 
the speaker is more likely to choose to accept the gift. And based on his or her 
statement on the gift, he or she would choose an appropriate utterance to let the 
pragmatic listener understand. After hearing the words, the pragmatic listener, 
who gave the gift to the speaker, will estimate whether the speaker accept the 
gift or refuse it based on the utterance and the potential politeness that the 
speaker has.  

The Basic RSA Model
Firstly, I built up a basic RSA model without politeness on the speaker. The RSA 
model has the same structure as we learned about the RSA model of choosing a 
word (either the color or the shape) to describe an object with blue/green color 
and square/circle shape. 

In my model, there are two statement ("accept" and "refuse") to state the attitude 
of the speaker. The speaker have two choice of utterance: "thanks" and "no 
thanks" to express their statement. 
I considered some complex language situations that people use "thanks" or "no 
thanks" to express the meaning opposite to its common meaning. For example, 
people may use very ambiguous words to express their appreciation, but with 
indirect refusal. In this case, the word "thanks" for the speaker would be refusal 
meaning. Therefore, in the meaning function, "thanks" strongly suggests 
acceptance, but there is possibility that it also means refusal. The similar 
condition also happens on "no thanks" utterance. Therefore, "no thanks" strongly 
suggests refusal, but the possibility to express acceptance will also be 
considered. 

The Basic RSA model shows below

~~~~
//state
var states = ["accept","refuse"]

//utterance
var utterances = ["thanks","no thanks"]

// stateprior
var statePrior = function(){
  var state = uniformDraw(states)
  return state 
}


// meaning of utterance
// check if the utterance is "thanks", if yes, flip with 0.75 if the state is "accept"
// otherwise, flip with 0.25.
// if the utterance is "no thanks", flip with 0.25 if the state is "accept", and
// flip with 0.75 if the state is "refuse".
var meaning = function(utterance,state){
  return utterance == "thanks" ? 
    state == "accept" ? flip(0.75) : flip(0.25) :
  state == "accept" ? flip(0.25) : flip(0.75)
}

//literal listener
var literalListener = function(utterance){
  Infer({model: function(){
    var state = statePrior();
    condition(meaning(utterance,state))
    return state
  }})
}

//alpha
var alpha = 1

// pragmatic speaker without politeness
var speaker = function(state){
  Infer({model: function(){
    var utterance = uniformDraw(utterances)
    factor(alpha * (literalListener(utterance).score(state)))
    return utterance
  }})
}

//pragmatic listener
var pragmaticListener = function(utterance){
  Infer({model:function(){
    var state = statePrior()
    observe(speaker(state),utterance)
    return state
  }})
}

print("The probability of accept/refuse when people hear thanks")
viz(pragmaticListener("thanks"))
print("The probability of accept/refuse when people hear no thanks")
viz(pragmaticListener("no thanks"))
~~~~

From the result, we can see that when people say "thanks", it will be 0.75 
probability that the speaker wants to accept the gift, and 0.25 probability that the 
speaker want to refuse it. When people say "no thanks", the probability of two 
statement shifts.

Then I added the speaker's utility to adjust the politeness of the speaker. 

In the original politeness model, the speaker has two utilities: epistemic and 
social. The speaker will be more likely to express truth when he or she is 
epistemic. And the speaker will be more likely to choose the statement to make 
the listener happy if he or she is social. 

In my model, I also set up two similar utilities. The true utility is exactly same as 
the epistemic utility in the original model. The polite utility, instead of scoring all 
possible states, will only score "accept" statement. The speaker will like to choose 
"accept" more to show their politeness. 

The utility model shows below. 

~~~~
var utility = {
  True: L0_posterior.score(state),
  Polite: L0_posterior.score("accept")  
}
var speakerUtility = phi * utility.True +
                        (1 - phi) * utility.Polite
~~~~

The full model of acceptance and refusal with politeness shows below

~~~~
////////////////////////////western condiiton////////////////////////////
//state
var states = ["accept","refuse"]

//utterance
var utterances = ["thanks","no thanks"]

// stateprior
var statePrior = function(){
  var state = uniformDraw(states)
  return state 
}

// meaning of utterance
var meaning = function(utterance,state){
  return utterance == "thanks" ? 
    state == "accept" ? flip(0.75) : flip(0.25) :
  state == "accept" ? flip(0.25) : flip(0.75)
}

//literal listener
var literalListener = function(utterance){
  Infer({model: function(){
    var state = statePrior();
    condition(meaning(utterance,state))
    return state
  }})
}

//alpha
var alpha = 1

//cost
var cost = function(utterance){
  utterance == "thanks" ? 1 :
  utterance == "no thanks" ? 1 :
  0
}

//speaker with politeness
var speaker = function(state, phi) {
  Infer({model: function(){
    var utterance = uniformDraw(utterances)
    var L0_posterior = literalListener(utterance)
    var utility = {
      True: L0_posterior.score(state),
      Polite: L0_posterior.score("accept")
    }
    var speakerUtility = phi * utility.True +
                        (1 - phi) * utility.Polite
    factor(alpha * speakerUtility - cost(utterance))
    return utterance
  }})
};

//pragmatic listener
// pragmatic listener will uniformly draw a phi from five possible numbers
var pragmaticListener = function(utterance){
  Infer({model:function(){
    var state = statePrior()
    var phi = uniformDraw([0.1,0.3,0.5,0.7,0.9])
    observe(speaker(state,phi),utterance)
    return state
  }})
}

print("The probability of accept/refuse when people hear thanks")
viz(pragmaticListener("thanks"))
print("The probability of accept/refuse when people hear no thanks")
viz(pragmaticListener("no thanks"))
~~~~

In this model, I added politeness in the speaker part. People who are polite 
would be more likely to accept the gift. Therefore, we can see from the figures 
that the probability of accepting shifted a little bit to refusing, which means that 
when people hear "thanks", the probability of refusal increases. The similar 
situation happened for the utterance"no thanks". When people hear "no thanks", 
the probability of acceptance increases a little compared to the basic RSA model. 

In previous model, we assume that people more like to choose to accept the gift 
to show their politeness. However, it is not always true for all people. People in 
different culture have different sense of politeness. In western culture, people 
think that acceptance shows politeness. However, people in eastern culture 
consider refusal as a kind of politeness. For people in eastern culture, it's polite to 
refuse others' gifts at first. In eastern culture, it's also more costly for people to 
say "thanks" than "no thanks", because saying "no thanks" gives the listener a 
chance to continue the conversation with the speaker and build up a more close 
relationship. 

Therefore, in the model for eastern culture, the cost of two utterances are not 
equal to each other. It will be more costly to choose "thanks" than "no thanks". In 
the utility function, the state "refuse" will be scored instead of "accept" in the 
previous model. 

The model of eastern culture shows below

~~~~
////////////////////////////eastern condiiton////////////////////////////
//state
var states = ["accept","refuse"]

//utterance
var utterances = ["thanks","no thanks"]

// stateprior
var statePrior = function(){
  var state = uniformDraw(states)
  return state 
}

// meaning of utterance
var meaning = function(utterance,state){
  return utterance == "thanks" ? 
    state == "accept" ? flip(0.75) : flip(0.25) :
  state == "accept" ? flip(0.25) : flip(0.75)
}

//literal listener
var literalListener = function(utterance){
  Infer({model: function(){
    var state = statePrior();
    condition(meaning(utterance,state))
    return state
  }})
}

//alpha
var alpha = 1

//cost
var cost = function(utterance){
  utterance == "thanks" ? 3 :
  utterance == "no thanks" ? 1 :
  0
}

//speaker with politeness
var speaker = function(state, phi) {
  Infer({model: function(){
    var utterance = uniformDraw(utterances)
    var L0_posterior = literalListener(utterance)
    var utility = {
      True: L0_posterior.score(state),
      Polite: L0_posterior.score("refuse")
    }
    var speakerUtility = phi * utility.True +
                        (1 - phi) * utility.Polite
    factor(alpha * speakerUtility - cost(utterance))
    return utterance
  }})
};

//pragmatic listener
var pragmaticListener = function(utterance){
  Infer({model:function(){
    var state = statePrior()
    var phi = uniformDraw([0.1,0.3,0.5,0.7,0.9])
    observe(speaker(state,phi),utterance)
    return state
  }})
}

print("The probability of accept/refuse when people hear thanks")
viz(pragmaticListener("thanks"))
print("The probability of accept/refuse when people hear no thanks")
viz(pragmaticListener("no thanks"))
~~~~

In the result of hearing "thanks", the probability shifted back to acceptance again. 
However, when people hear "no thanks" in eastern condition, the probability of 
acceptance and refusal are close to each other. It means that for eastern culture, 
the meaning of saying "no thanks" is more vague than saying "thanks". 
Compared with the result of the model for western culture, the estimation of 
refusal goes some to the acceptance when the listener hear "no thanks". 

Changing the cost of utterances is one way to adjust the speaker's choice on 
utterances. If the utterance is less costly than other, the speaker will be more 
likely to choose this utterance. Therefore, the probability of "accept" increases for 
hearing "no thanks" in the model for eastern culture. Another way to adjust the 
speaker's choice is to change the alpha. Alpha means how optimal the speaker 
will be. If the speaker is ideal, he or she will choose an utterance that can 
accurately express their thoughts. Therefore, in the result of changing to greater 
alpha, the probability of "accept" and "refuse" will have bigger difference for 
both utterances.


##Jun Zhu

###Understanding Negated Antonyms
####Ⅰ The empirical phenomenon of interest
<br>
If John says that 'I'm not unhappy about my project', does that mean that he is happy about his project? Such utterances, known as 'negated antonyms', convey a subtle meaning that is not as simple as their logical forms look like. In this example, we might perceive that the 'not unhappy' John is in a state which is below the positive adjective 'happy' but above the antonym 'unhappy'. This empirical phenomenon demonstrate that the two negations of this case might not be simply '¬¬happy' in the logical form, otherwise they would cancel each other and produce the same result as 'happy'. (Tessler, 2018)
<br> 
In my opinion, the best way to capture the mechanism under 'negated antonyms' is to show the quantitative difference between utterances. Thus, I adopt a scenario that is similar to the one we studied in class (discussed further in section 3), and I will try to determine the difference between 'expensive' and 'not-inexpensive' utterance which describe the price of items based on the prediction of my model, as a way to dig into the mechanism of 'negated antonyms'

####Ⅱ Introduction to the RSA framework

Rational Speech Act Framework is dedicated to formally explain these nuanced aspects of meaning and better understand the compositional mechanism that delivers them. (Scontras et al.) RSA utilizes a recursive-reasoning computational model that relies on simulation-based probabilistic programs. In this model, a pragmatic listener L1  (fully rational) will try to resolve the intended meaning (or QUD) of an utterance with their observation on a speaker model. The speaker S will have certain degree of rationality, and produce an utterance based on their knowledge of a naive literal listener L0. The literal listener will only consult the literal meaning and the world states to understand an utterance.  Together, they make up the basic RSA framework, which is widely used in the fields of language understandings, and generates reliable predictions that could be tested by behavioral data. 

####Ⅲ Similarities 
<br>
My model is based on the model of gradable adjectives 'expensive' that we studies in class. The listener will hear an utterance from a speaker such as ' The watch I bought is expensive', then the listener will predict the actual price of the speaker paid for the item and the threshold for this gradable adjective utterance.  Additionally, my model also draws on data of prior knowledge of the price of an item in the real world and will be able to generate predictions for various items.
####Ⅳ Differences
<br>
My model also takes ideas from the Tessler's model (2018) on negated antonyms. Tessler's model highlights the idea that negated antonyms add extra uncertainty to listener's comprehension and might introduce new threshold. Such contrary opposition, 'not inexpensive' could mean either ¬¬A or ¬Ā in the logical form. This is represented by the pragmatic listener's behavior in the model. 
 

####V Model Description
This model aims to predict listeners' estimation on the price that the speaker paid when they hear the speaker's utterance about an item she bought, and the relevant threshold to the utterance (of a gradable adjective or negated antonyms). Prior measurement about an item will be available, and the utterance could be 'the item is expensive/inexpensive/not expensive/not inexpensive'.  
Utterances with negated antonyms will be slightly more costly than the ordinary adjectival utterance, possibly because of the mental efforts or the extra utterance time of negations.
The priors are defined below, which is similar to the model we covered in class:

~~~~
var sweater = {
  "prices": [1.5, 4.5, 7.5, 10.5, 13.5, 16.5],
  "probabilities": [0.00482838499944466, 0.00832934578733181, 0.0112952500492109, 0.0173774790108894, 0.0232006658974883, 0.0258422772579257]
};
var data = {
  "sweater": sweater
};
var prior = function(item) {
  var prices = data[item].prices;
  var probabilities = data[item].probabilities;
  return function() {
    return categorical(probabilities, prices);
  };
};

var theta_prior = function(item) {
  var thetas = data[item].prices;
  return function() {
    return uniformDraw(thetas) ;
  };
};
var alpha = 2; // optimality parameter

var utterances = ["expensive", "not-inexpensive","inexpensive","not-expensive"];

var cost = {
  "not-inexpensive":2,
  "expensive": 1,
  "not-expensive":2,
  "inexpensive":1,
};
var utterancePrior = function() {
  return uniformDraw(utterances);
};
~~~~

The meaning function takes a soft semantics to judge the price and threshold, so that we could strictly use ">" instead of ">=" in our model.  

~~~~
var meaning = function(utterance, price, theta) {
  utterance == "expensive" ? price > theta.expensive ? flip(0.9999) : flip(0.0001) : 
  utterance == "not-expensive" ? !(price > theta.expensive) ? flip(0.9999) : flip(0.0001) : 
  utterance == "inexpensive" ? price < theta.inexpensive ? flip(0.9999) : flip(0.0001) : 
  utterance == "not-inexpensive" ? !(price < theta.inexpensive)? flip(0.9999) : flip(0.0001):
  true;
};
~~~~

The pragmatic listener exhibits a certain amount of uncertainty (e.g. flip(0.2) ) when hearing negated antonyms. In some cases, there will be an individual threshold for the antonyms (known as inexpensive_threshold), and in other cases, the threshold will be the same as the expensive one. The 'theta' variable in my model will be a lookup table to handle both situations which will be interpreted properly by the meaning function.

~~~~
var literalListener = cache(function(utterance, theta, item) {
  return Infer({method: "enumerate"}, function() {
    var price = uniformDraw(data[item].prices)
    condition(meaning(utterance, price, theta))
    return price;
  });
});

var speaker = cache(function(price, theta, item) {
  return Infer({method: "enumerate"}, function() {
    var utterance = utterancePrior();
    factor( alpha * (literalListener(utterance, theta, item).score(price) 
                    - cost[utterance]));
    return utterance;
  });
});

var pragmaticListener = function(utterance, item) {
  // first identify the relevant priors
  var pricePrior = prior(item);
  var thetaPrior = theta_prior(item);
  // then run inference
  return Infer({method: "enumerate"}, 
  function() {
    var uncertainty = 0.2
    var an_neg_thre = flip(uncertainty)
    var expensive_theta= thetaPrior() 
    var inexpensive_threshold = an_neg_thre ?
       thetaPrior() :
      expensive_theta;
    var price = pricePrior();
    var theta =  {
      expensive: expensive_theta,
      inexpensive: inexpensive_threshold
    }
    var Posexp = theta.expensive
    var Posneg = theta.inexpensive
    factor( speaker(price, theta, item).score(utterance) );
    return { price: price, Posexp: Posexp , Posneg: Posneg };
  });
};
~~~~

The complete model is below. Relevant visualizations will be available when you run this code block.

~~~~
var watch = {
  "prices": [25, 75, 125, 175, 225, 275, 325, 375, 425, 475, 525, 575, 625, 675, 725, 775, 825, 875, 925, 975, 1025, 1075, 1125, 1175, 1225, 1275, 1325, 1375, 1425, 1475, 1525, 1575, 1625, 1675, 1725, 1775, 1825, 1875, 1925, 1975, 2025, 2075, 2125, 2175, 2225, 2275, 2325, 2375, 2425, 2475, 2525, 2575, 2625, 2675, 2725, 2775, 2825, 2875, 2925, 2975],
  "probabilities": [0.040844560268751, 0.0587099798246933, 0.0656194599591356, 0.0667642412698035, 0.0615953803048016, 0.0510809063784378, 0.0467203673419258, 0.0446735950187136, 0.040047421916613, 0.0350583957334483, 0.0297508215717606, 0.0256829651118227, 0.024135920250668, 0.0228891907259206, 0.021706684520276, 0.0186449440066946, 0.0187249266247728, 0.0179250744798993, 0.0173698811746238, 0.0165581725818319, 0.0160745066032247, 0.0127927305129066, 0.0113730680265067, 0.0109485307623827, 0.00923468422650943, 0.00899007751887508, 0.00880520147998275, 0.00838023585866885, 0.00841052411004918, 0.00828830635037619, 0.00834008093757411, 0.00750681534099784, 0.00724072133740109, 0.00717291664158004, 0.00682823777708754, 0.00646995193940331, 0.00697139732982518, 0.00711846547272734, 0.00698781312802354, 0.00732316558583701, 0.00594973158122097, 0.00557461443747403, 0.00541637601910211, 0.00518850469148531, 0.00572025848989677, 0.0051443557601358, 0.00510282169734075, 0.00493720252580643, 0.00560198932991028, 0.00519158715054485, 0.00473398797752786, 0.00540907722833213, 0.00494653421540979, 0.00495500420164643, 0.00494083025189895, 0.00481566268206312, 0.00442965937328148, 0.00441189688100535, 0.00415116538135834, 0.00361842012002631]
};
var sweater = {
  "prices": [1.5, 4.5, 7.5, 10.5, 13.5, 16.5],
  "probabilities": [0.00482838499944466, 0.00832934578733181, 0.0112952500492109, 0.0173774790108894, 0.0232006658974883, 0.0258422772579257]
};
var data = {
  "watch": watch,
  "sweater": sweater
};

var prior = function(item) {
  var prices = data[item].prices;
  var probabilities = data[item].probabilities;
  return function() {
    return categorical(probabilities, prices);
  };
};

var theta_prior = function(item) {
  var thetas = data[item].prices;
  return function() {
    return uniformDraw(thetas) ;
  };
};

var alpha = 2; // optimality parameter


var utterances = ["expensive","not-inexpensive","inexpensive","not-expensive"];

var cost = {
  "not-inexpensive":2,
  "expensive": 1,
  "not-expensive":2,
  "inexpensive":1,
};
var utterancePrior = function() {
  return uniformDraw(utterances);
};

var meaning = function(utterance, price, theta) {
  utterance == "expensive" ? price > theta.expensive ? flip(0.9999) : flip(0.0001) :
  utterance == "not-expensive" ? !(price > theta.expensive) ? flip(0.9999) : flip(0.0001) : 
  utterance == "inexpensive" ? price < theta.inexpensive ? flip(0.9999) : flip(0.0001) : 
  utterance == "not-inexpensive" ? !(price < theta.inexpensive)? flip(0.9999) : flip(0.0001):
  true;
};

var literalListener = cache(function(utterance, theta, item) {
  return Infer({method: "enumerate"}, function() {
    var price = uniformDraw(data[item].prices)
    condition(meaning(utterance, price, theta))
    return price;
  });
});

var speaker = cache(function(price, theta, item) {
  return Infer({method: "enumerate"}, function() {
    var utterance = utterancePrior();
    factor( alpha * (literalListener(utterance, theta, item).score(price) 
                    - cost[utterance]));
    return utterance;
  });
});

var pragmaticListener = function(utterance, item) {
  // first identify the relevant priors
  var pricePrior = prior(item);
  var thetaPrior = theta_prior(item);
  // then run inference
  return Infer({method: "enumerate"}, 
  function() {
    var uncertainty = 0.2
    var an_neg_thre = flip(uncertainty)
    var expensive_theta= thetaPrior()
    
    var inexpensive_threshold = an_neg_thre ?
       thetaPrior() :
      expensive_theta;
    var price = pricePrior();
    var theta =  {
      expensive: expensive_theta,
      inexpensive: inexpensive_threshold
    }
    var Posexp = theta.expensive
    var Posneg = theta.inexpensive
    factor( speaker(price, theta, item).score(utterance) );
    return { price: price, Posexp: Posexp , Posneg: Posneg };
  });
};


var expensiveSweater= pragmaticListener("expensive", "sweater");
print("Expensive:Prices")
viz.density(marginalize(expensiveSweater, "price"));
display(expectation(marginalize(expensiveSweater, "price")))
print("Expensive:Thresholds:")
viz.density(marginalize(expensiveSweater, "Posexp"));
display(expectation(marginalize(expensiveSweater, "Posexp")))
var notinexpensiveSweater= pragmaticListener("not-inexpensive", "sweater");
print("NOT-Inexpensive : Prices")
viz.density(marginalize(notinexpensiveSweater, "price"));
display(expectation(marginalize(notinexpensiveSweater, "price")))
print("NOT-Inexpensive:Thresholds:")
viz.density(marginalize(notinexpensiveSweater, "Posneg"));
display(expectation(marginalize(notinexpensiveSweater, "Posneg")))
~~~~

####Ⅵ Discussions
The result of my model matches my intuition about the utterances.  From the density graph, it is obvious that negated antonyms are different from the original utterance, because uncertainty is involved when using such utterances. The expectation of prices and threshold (the number below the graph) demonstrates that the expectation price that listener predicts is slightly lower for 'not-inexpensive' than its for 'expensive', while the threshold is higher for 'not-inexpensive' than for its 'expensive'. Accordingly, my intuition is that when the speaker pays extra costs to use negated antonyms, she will try to be more informative, thus making the threshold should be higher. As for the price, people will usually expect the price of the item to be in a certain state where it is below the 'expensive' one but above the 'inexpensive' one, which is anticipated by Jespersen(1924) and following Tessler(2018).  It is quite noticeable that when there are only two utterances possible (expensive and not-inexpensive), the difference is weaker; when there are four utterances possible in the final model, the difference is more significant.
<br>
There are some remaining problems, However. First, we need further data from behavior experiments will be needed to better justify the model. Second, what exactly is the degree of uncertainty when uttering negated utterance? In my model, I use a fixed probability of 0.2 which was taken from the Tessler's model, but does it actually reflect the reality? When trying to adjust the uncertainty to higher numbers, the difference between original utterances and negated antonym utterances become slightly weaker.  What the exact mechanism of the uncertainty degrees is remains unclear. Such questions would be interesting for future research.


