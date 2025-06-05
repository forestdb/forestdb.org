---
layout: model
title: Adjectives + QUD - Frankie, Shane
model-language: webppl
---

Adjectives + QUD Model
------



- When a speaker chooses to communicate about expensiveness, they may simply wish to inform their interlocutor about the price, or they may be communicating that they are unhappy about paying too much. Perhaps it's some mixture of both. 

- To model this phenomenon, we drew from both the hyperbole model (ch. 3) and the vagueness model (ch. 5).

- Our resulting model captures a pragmatic listener's interpretation of vague adjectives while incorporating uncertainty about communicative goals.

_____________

### We used the price priors in the hyperbole model as our baseline: 
- Prior probability of kettle prices (taken from human experiments)

 


~~~~
// original ch. 3 hyperbole model 

var approx = function(x,b) {
  var b = 10
  return b * Math.round(x / b)
}

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
      0.4205, 0.3865,
      0.0533, 0.0538,
      0.0223, 0.0211,
      0.0112, 0.0111,
      0.0083, 0.0120
    ]
  })
}
~~~~

### ...and tweaked them to fit our model:
- The round and precise number distinction is trivial here because we're modeling adjective interpretation rather than number interpretation. 

- We adjusted by adding the probabilities of the precise numbers to the corresponding round numbers. 


~~~~
var prices = [
  50, 
  500,
  1000,
  5000,
  10000
]

var pricePrior = function() {
  return categorical({
    vs: prices,
    ps: [
      0.8070,
      0.1070,
      0.0434,
      0.0223,
      0.0203
    ]
  })
}
~~~~

### We then turned to the valence priors in the hyperbole model:  
- Probability that given a price state, the speaker thinks it's too expensive (taken from human experiments)

~~~~
// original ch. 3 hyperbole model 

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
~~~~

### ...and fit them to our model:
- Since the corresponding precise and round numbers had the same valence value, we omitted the precise number from the valence prior without altering the original probabilities. 

~~~~
///fold: 
var prices = [
  50, 
  500,
  1000,
  5000,
  10000
]

var pricePrior = function() {
  return categorical({
    vs: prices,
    ps: [
      0.8070,
      0.1070,
      0.0434,
      0.0223,
      0.0203
    ]
  })
}
///

var valencePrior = function(state) {
  var probs = {
    50 : 0.3173,
    500 : 0.7920,
    1000 : 0.8933,
    5000 : 0.9524,
    10000 : 0.9864
  }
  var tf = flip(probs[state])
  return tf
}
~~~~

### We added the theta prior from the ch. 5 adjectives model:
- Theta is a free threshold variable which moderates the interpretation of "expensive."

~~~~
var thetaPrior = function() {
  return uniformDraw(prices)
}
~~~~

### Here's where we deviate significantly from the hyperbole model: 
- The possible utterances are same as the price states.



~~~~
// original ch. 3 hyperbole model 

var meaning = function(utterance, price) {
  return utterance == price
}

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
~~~~

### Here's what we did instead: 
- We changed possible utterances from integers corresponding to kettle prices to either adjectives or silence (inspired by the vagueness model in ch. 5).

- We incorporated the null utterance because, crucially, the rational speaker in our model prefers silence over uninformative speech
- We also took the meaning function from the vagueness model, which uses the free-threshold value "theta" to moderate interpretation of ambiguous adjectives. 

~~~~
// in the default hyperbole model, the utterances = prices
// in this model, utterances = adjectives or silence

var utterances = ["expensive", "notExpensive", "null"]

var utterancePrior = function() {
  return  uniformDraw(utterances)
}

var cost = function(utterance) {
  return utterance== 'expensive'? 1 : 
  utterance== 'notExpensive' ? 2 :
  0
};

var meaning = function(utterance, price, theta) {
return utterance == "expensive" ? price >= theta : 
  utterance == "notExpensive" ? price <= theta :
  true
}
~~~~

### Our model's QUDs:
- We have the same QUDs as original hyperbole model but without the "approxPrice" and the "approxPriceValence" QUDs

~~~~
var qudFns = {
  price : function(state) {return { price: state.price } },
  valence : function(state) {return { valence: state.valence } },
  priceValence : function(state) {
    return { price: state.price, valence: state.valence }
  }
  }

var qudPrior = function() {
  categorical({
    vs: ["price", "valence", "priceValence"],
    ps: [1, 1, 1]
  })
}
~~~~

### Literal listener (L0)
- We took our literal listener from the hyperbole model and incorporated theta. 

~~~~
///fold: 
var prices = [
  50, 
  500,
  1000,
  5000,
  10000
]

var pricePrior = function() {
  return categorical({
    vs: prices,
    ps: [
      0.8070,
      0.1070,
      0.0434,
      0.0223,
      0.0203
    ]
  })
}

var thetaPrior = function() {
  return uniformDraw(prices)
}

var valencePrior = function(state) {
  var probs = {
    50 : 0.3173,
    500 : 0.7920,
    1000 : 0.8933,
    5000 : 0.9524,
    10000 : 0.9864
  }
  var tf = flip(probs[state])
  return tf
}

var utterances = ["expensive", "null", "notExpensive"]


var utterancePrior = function() {
  return  uniformDraw(utterances)
}

var cost = function(utterance) {
  return utterance== 'expensive'? 1 : 
  utterance== 'notExpensive' ? 2 :
  0
};

var meaning = function(utterance, price, theta) {
return utterance == "expensive" ? price >= theta : 
  utterance == "notExpensive" ? price <= theta :
  true
  
}

var qudFns = {
  price : function(state) {return { price: state.price } },
  valence : function(state) {return { valence: state.valence } },
  priceValence : function(state) {
    return { price: state.price, valence: state.valence }
  }
  }

var qudPrior = function() {
  categorical({
    vs: ["price", "valence", "priceValence"],
    ps: [1, 1, 1]
  })
}

///

var literalListener = cache(function(utterance, qud, theta) {
  return Infer({model: function(){
    var price = uniformDraw(prices)
    var valence = valencePrior(price)
    var fullState = {price, valence}
    var qudFn = qudFns[qud]
    var qudAnswer = qudFn(fullState)
    condition( meaning(utterance, price, theta) )
    return qudAnswer
  }
               })})

print('L0 predictions:')
viz(literalListener("expensive", "priceValence", 500))
~~~~

### Literal listener predictions from the original hyperbole model 


~~~~
///fold:
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
      0.4205, 0.3865,
      0.0533, 0.0538,
      0.0223, 0.0211,
      0.0112, 0.0111,
      0.0083, 0.0120
    ]
  })
}

// Probability that given a price state, the speaker thinks it's too
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
///

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

print('The original hyperbole model L0 predictions:"')
viz(literalListener(500, "priceValence"))
~~~~

### Pragmatic speaker (S1)
- We took our pragmatic speaker from the hyperbole model and incorporated theta. 

~~~~
///fold: 

var prices = [
  50, 
  500,
  1000,
  5000,
  10000
]

var pricePrior = function() {
  return categorical({
    vs: prices,
    ps: [
      0.8070,
      0.1070,
      0.0434,
      0.0223,
      0.0203
    ]
  })
}

var thetaPrior = function() {
  return uniformDraw(prices)
}

var valencePrior = function(state) {
  var probs = {
    50 : 0.3173,
    500 : 0.7920,
    1000 : 0.8933,
    5000 : 0.9524,
    10000 : 0.9864
  }
  var tf = flip(probs[state])
  return tf
}

var utterances = ["expensive", "null", "notExpensive"]


var utterancePrior = function() {
  return  uniformDraw(utterances)
}

var cost = function(utterance) {
  return utterance== 'expensive'? 1 : 
  utterance== 'notExpensive' ? 2 :
  0
};

var meaning = function(utterance, price, theta) {
return utterance == "expensive" ? price >= theta : 
  utterance == "notExpensive" ? price <= theta :
  true
  
}

var qudFns = {
  price : function(state) {return { price: state.price } },
  valence : function(state) {return { valence: state.valence } },
  priceValence : function(state) {
    return { price: state.price, valence: state.valence }
  }
  }

var qudPrior = function() {
  categorical({
    vs: ["price", "valence", "priceValence"],
    ps: [1, 1, 1]
  })
}


var literalListener = cache(function(utterance, qud, theta) {
  return Infer({model: function(){
    var price = uniformDraw(prices)
    var valence = valencePrior(price)
    var fullState = {price, valence}
    var qudFn = qudFns[qud]
    var qudAnswer = qudFn(fullState)
    condition( meaning(utterance, price, theta) )
    return qudAnswer
  }
               })})

///

// speaker optimality
var alpha = 1

var speaker = cache(function(fullState, qud, theta) {
  return Infer({model: function(){
    var utterance = utterancePrior()
    var qudFn = qudFns[qud]
    var qudAnswer = qudFn(fullState)
    factor(alpha*(literalListener(utterance,qud,theta).score(qudAnswer) 
                  - cost(utterance)))
    return utterance
  }})
})

print('Our speaker predictions:')
viz(speaker({price:500, valence:true}, "valence", 50))
~~~~

### New pragmatic listener (L1)
- We took our pragmatic listener from the hyperbole model and incorporated theta. 

~~~~
///fold: 

var prices = [
  50, 
  500,
  1000,
  5000,
  10000
]

var pricePrior = function() {
  return categorical({
    vs: prices,
    ps: [
      0.8070,
      0.1070,
      0.0434,
      0.0223,
      0.0203
    ]
  })
}

var thetaPrior = function() {
  return uniformDraw(prices)
}

var valencePrior = function(state) {
  var probs = {
    50 : 0.3173,
    500 : 0.7920,
    1000 : 0.8933,
    5000 : 0.9524,
    10000 : 0.9864
  }
  var tf = flip(probs[state])
  return tf
}

var utterances = ["expensive", "null", "notExpensive"]


var utterancePrior = function() {
  return  uniformDraw(utterances)
}

var cost = function(utterance) {
  return utterance== 'expensive'? 1 : 
  utterance== 'notExpensive' ? 2 :
  0
};

var meaning = function(utterance, price, theta) {
return utterance == "expensive" ? price >= theta : 
  utterance == "notExpensive" ? price <= theta :
  true
  
}

var qudFns = {
  price : function(state) {return { price: state.price } },
  valence : function(state) {return { valence: state.valence } },
  priceValence : function(state) {
    return { price: state.price, valence: state.valence }
  }
  }

var qudPrior = function() {
  categorical({
    vs: ["price", "valence", "priceValence"],
    ps: [1, 1, 1]
  })
}


var literalListener = cache(function(utterance, qud, theta) {
  return Infer({model: function(){
    var price = uniformDraw(prices)
    var valence = valencePrior(price)
    var fullState = {price, valence}
    var qudFn = qudFns[qud]
    var qudAnswer = qudFn(fullState)
    condition( meaning(utterance, price, theta) )
    return qudAnswer
  }
               })})

///
var alpha = 1

var speaker = cache(function(fullState, qud, theta) {
  return Infer({model: function(){
    var utterance = utterancePrior()
    var qudFn = qudFns[qud]
    var qudAnswer = qudFn(fullState)
    factor(alpha*(literalListener(utterance,qud,theta).score(qudAnswer) 
                  - cost(utterance)))
    return utterance
  }})
})


var pragmaticListener = cache(function(utterance) {
  return Infer({model: function(){
    //////// priors ////////
    var price = pricePrior()
    var valence = valencePrior(price)
    var qud = qudPrior()
    var theta = thetaPrior()
    ////////////////////////
    var fullState = {price, valence, theta, qud}
    observe(speaker(fullState, qud, theta), utterance)
    return {price, valence}
//         return {price, qud}
  }})
})

var listenerPosterior1 = pragmaticListener("expensive")
var listenerPosterior2 = pragmaticListener('notExpensive')
var listenerPosterior3 = pragmaticListener('null')

print('Pragmatic listener hears "expensive":')
viz(listenerPosterior1)
print('Pragmatic listener hears "not expensive":')
viz(listenerPosterior2)
print('Pragmatic listener hears nothing:')
viz(listenerPosterior3)
~~~~

original hyperbole model predictions:
-

~~~~
///fold:

// CHAPTER 3 ORIGINAL MODEL 
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
      0.4205, 0.3865,
      0.0533, 0.0538,
      0.0223, 0.0211,
      0.0112, 0.0111,
      0.0083, 0.0120
    ]
  })
}

// Probability that given a price state, the speaker thinks it's too
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
//     return {price, qud}
  }})
})
var listenerPosterior = pragmaticListener(10000)

print("Original hyperbole model predictions:")
print("Pragmatic listener's joint interpretation of 'The kettle cost $10,000':")
viz(listenerPosterior)
~~~~

## Full adjectives + QUD model:


~~~~
// ADJECTIVES + QUD MODEL
// frankie + shane RSA project 

// code adapted from the Kao et al. hyperbole model + gradable 
// adjectives & vagueness resolution model 

// prior probability of kettle prices (taken from human experiments)
var prices = [
  50, 
  500,
  1000,
  5000,
  10000
]

var pricePrior = function() {
  return categorical({
    vs: prices,
    ps: [
      0.8070,
      0.1070,
      0.0434,
      0.0223,
      0.0203
    ]
  })
}


var thetaPrior = function() {
  return uniformDraw(prices)
}


// probability that given a price state, the speaker thinks it's too
// expensive (taken from human experiments)
var valencePrior = function(state) {
  var probs = {
    50 : 0.3173,
    500 : 0.7920,
    1000 : 0.8933,
    5000 : 0.9524,
    10000 : 0.9864
  }
  var tf = flip(probs[state])
  return tf
}

var utterances = ["expensive", "null", "notExpensive"]


var utterancePrior = function() {
  return  uniformDraw(utterances)
}

var cost = function(utterance) {
  return utterance== 'expensive'? 1 : 
  utterance== 'notExpensive' ? 2 :
  0
};

var meaning = function(utterance, price, theta) {
return utterance == "expensive" ? price >= theta : 
  utterance == "notExpensive" ? price <= theta :
  true
  
}

var qudFns = {
  price : function(state) {return { price: state.price } },
  valence : function(state) {return { valence: state.valence } },
  priceValence : function(state) {
    return { price: state.price, valence: state.valence }
  }
  }

var qudPrior = function() {
  categorical({
    vs: ["price", "valence", "priceValence"],
    ps: [1, 1, 1]
  })
}

var literalListener = cache(function(utterance, qud, theta) {
  return Infer({model: function(){
    var price = uniformDraw(prices)
    var valence = valencePrior(price)
    var fullState = {price, valence}
    var qudFn = qudFns[qud]
    var qudAnswer = qudFn(fullState)
    condition( meaning(utterance, price, theta) )
    return qudAnswer
  }
               })})

var alpha = 1

var speaker = cache(function(fullState, qud, theta) {
  return Infer({model: function(){
    var utterance = utterancePrior()
    var qudFn = qudFns[qud]
    var qudAnswer = qudFn(fullState)
    factor(alpha*(literalListener(utterance,qud,theta).score(qudAnswer) 
                  - cost(utterance)))
    return utterance
  }})
})

var pragmaticListener = cache(function(utterance) {
  return Infer({model: function(){
    //////// priors ////////
    var price = pricePrior()
    var valence = valencePrior(price)
    var qud = qudPrior()
    var theta = thetaPrior()
    ////////////////////////
    var fullState = {price, valence, theta, qud}
    observe(speaker(fullState, qud, theta), utterance)
    return {price, qud} 
  }})
})

var listenerPosterior1 = pragmaticListener("expensive")
var listenerPosterior2 = pragmaticListener('notExpensive')
var listenerPosterior3 = pragmaticListener('null')

print('Pragmatic listener hears "expensive":')
viz(listenerPosterior1)
print('Pragmatic listener hears "not expensive":')
viz(listenerPosterior2)
print('Pragmatic listener hears nothing:')
viz(listenerPosterior3)
~~~~