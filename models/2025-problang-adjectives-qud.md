---
layout: model
title: Vague Adjectives with QUD and Valence
model-language: webppl
model-category: Language and Pragmatics
model-status: code
---

## Adjectives + QUD Model - Frankie & Shane
__________
You’re texting your friend, who's known to drink copious amounts of coffee. They tell you that they just bought a new kettle to make pour overs. Then they tell you it was expensive. What do you infer about their communicative goal? 

Because you’re texting, you don’t have access to body language or prosody cues when deciding how to interpret their message. Are they making a neutral comment about the price? Or are they implying that they’re feeling the sting of paying too much? Perhaps they’re experiencing buyer's remorse or feeling jipped. This is something you might reason about when interpreting their use of the word “expensive.” 

In the vagueness model in chapter 5, we saw how the interpretation of "expensive" is influenced by the item being referred to and prior beliefs about the price threshold for that item.

But a pragmatic speaker might intend to communicate more than simply information about the price they paid. They might be implying that they're unhappy about paying too much. Perhaps it's some mixture of both.

With this in mind, we were curious what would happen when we combined the QUD and valence factor in the hyperbole model (from ch. 3) with the adjectives and threshold variable in the vagueness model (from ch. 5). 

The resulting model captures a pragmatic listener’s interpretations of vague adjectives by reasoning about the likely price of an item while incorporating uncertainty about a speaker's communicative goals.






_______

## 1. The priors


#### To begin, we turned to the hyperbole model:
- Defined here are the possible utterances, utterance priors, and the "meaning" function.
 
- The possible utterances are the same as the price states.


~~~~
// original ch. 3 hyperbole model 

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

var meaning = function(utterance, price) {
  return utterance == price
}
~~~~

#### ...and changed the possible utterances.

- We changed possible utterances from integers corresponding to kettle prices to the adjective "expensive" and its negated version, "not expensive." This list of utterances echoes the utterances in the vagueness model, except that we substituted "null" with "not expensive" in this iteration.


- We did away with the hyperbole model's meaning function altogether.



~~~~
// in the default hyperbole model, the utterances = prices
// in this model, utterances = adjectives or silence

var utterances = ["expensive", "notExpensive"]

var utterancePrior = function() {
  return  uniformDraw(utterances)
}
~~~~

#### Turning to the vagueness model, we took the theta prior, the meaning function, and the cost function, slightly tweaking the latter two. 

- Because we are incorporating the threshold variable theta, we took the theta prior from the vagueness model. 

- This meaning function uses theta to moderate interpretation of ambiguous adjectives. We made ours identical to that of the vagueness model in ch. 5, except that we added a "notExpensive" utterance. This function returns "expensive" if the price is greater than or equal to theta and "notExpensive" if the price is less than or equal to theta.

- The cost function assigns a higher cost to the utterance with more words.


~~~~
var thetaPrior = function() {
  return uniformDraw(prices)
}

var meaning = function(utterance, price, theta) {
return utterance == "expensive" ? price >= theta : 
  utterance == "notExpensive" ? price <= theta :
  true
}
var cost = function(utterance) {
  return utterance== 'notExpensive'? 1 : 
  0
};
~~~~

#### We used the price priors in the hyperbole model as our baseline:

- In this model, there are round and precise numbers, with a probability assigned to each one. 

- These probabilities correspond to the prior knowledge about what people can expect to pay for an electric kettle.



~~~~
// original ch. 3 hyperbole model

// taken from human experiments  
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

#### …but tweaked them to fit our model.

- The round and precise number distinction is trivial here because we’re modeling adjective interpretation rather than number interpretation.

- Therefore, we omitted the precise numbers in our model's price prior.

- We adjusted by adding the probabilities of the precise numbers to the corresponding round numbers, so the total still sums to 1.

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

#### We then turned to the valence priors in the hyperbole model:
- These correspond to the probability that given a price state, the speaker thinks it’s too expensive.


~~~~
// original ch. 3 hyperbole model 

// taken from human experiments  
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

#### …and fit them to our model.
- Since the corresponding precise and round numbers had the same valence value, we omitted the precise number from the valence prior without altering the original probabilities.

- The likelihood of valence being true is determined by valence probability at the specified price state passed in as an argument.


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

// visualize the priors

print('The joint distribution on price and valence:')
 Infer(function(){
  var aPrice = pricePrior()
  var aValence = valencePrior(aPrice)
  return {price: aPrice, valence: aValence}
})
~~~~

#### Our model’s QUDs:
- QUD = question under discussion.

- We have the same QUDs as the original hyperbole model but without the “approxPrice” and the “approxPriceValence” QUDs.


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

## 2. Levels of inference

### Literal listener (L0)
- We took our literal listener from the hyperbole model and added theta as an input value to account for our new meaning function. 

- Now, the utterances “expensive” and “not expensive” are semantically able to pass through this listener.





#### *Interpreting L0*:
- Below is a visualization of the literal listener run on the "expensive" utterance with the priceValence QUD and a theta value of 500. 

- Our new literal listener only reasons about prices at or above 500, because the literal meaning of expensive doesn’t allow for values below the given theta. 


- Initially, we had a version of the model that included a null utterance, but that version did not have hyperbole at higher alpha levels.

- We found that by removing the null utterance, hyperbole occurred even at high alpha levels (which can be seen in the pragmatic listener’s visualizations).



~~~~
///fold: 

var utterances = ["expensive", "notExpensive"]

var utterancePrior = function() {
  return  uniformDraw(utterances)
}

var thetaPrior = function() {
  return uniformDraw(prices)
}

// theta moderates interpretation of utterances
var meaning = function(utterance, price, theta) {
return utterance == "expensive" ? price >= theta : 
  utterance == "notExpensive" ? price <= theta :
  true
}

// more words = higher cost 
var cost = function(utterance) {
  return utterance== 'notExpensive'? 1 :
  0
};

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


### *Comparing L0 between models* 
- We can compare our literal listener's predictions (above) to the hyperbole model predictions (below) under similar parameters to observe what has changed. 

- The original L0 is being run on an utterance of 500, under the priceValence QUD. 
 - In the original hyperbole model, the literal listener is only capable of displaying a value equal to the specified price. Because it is literal, it can only interpret the utterance as meaning literally what is stated. Therefore, all probability lies on the price of 500.
- In both models, the valence associated with the price can be seen in the priors, which are similar between both models. 


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

print('The original hyperbole model L0 predictions:')
viz(literalListener(500, "priceValence"))
~~~~

___________
### Pragmatic speaker (S1)
- Our pragmatic speaker is identical to the hyperbole model's speaker except that it takes theta as an argument, such that the speaker can properly observe the literal listener.




#### *Interpreting S1*

- The speaker, visualized below, is run on a price of 500, a true valence, the valence QUD, and a theta value of 50. This returns more probability on “expensive” than “notExpensive”, because of the costs related to both of these utterances. 

- It is notable that, as an utterance, “expensive” becomes useful as theta becomes higher. Essentially, when the bar for expensiveness is higher, the utterance for "expensive" becomes more useful. 
- However, cost has the strongest effect on the speaker despite this, with both utterances being equally likely under lower theta values. This can be seen when the speaker is run on a price of 10000, a true valence, the priceValence QUD, and a theta of 10000. In that case, the "expensive" utterance does become more useful than the “notExpensive” utterance.

- When including "null" as a possible utterance in our prior iteration, we found that, under the same initial parameters, most of the probability was on the "null" utterance because it was the least costly utterance in that version (i.e., it had the most utility to this speaker). Because our adjective utterances were not as useful to communicate price as the actual prices (as seen in the original hyperbole model), it was more difficult for the "expensive" utterance to be useful—especially because there was another utterance with no cost.


~~~~
///fold: 

var utterances = ["expensive", "notExpensive"]

var utterancePrior = function() {
  return  uniformDraw(utterances)
}

var thetaPrior = function() {
  return uniformDraw(prices)
}

// theta moderates interpretation of utterances
var meaning = function(utterance, price, theta) {
return utterance == "expensive" ? price >= theta : 
  utterance == "notExpensive" ? price <= theta :
  true
}

// more words = higher cost 
var cost = function(utterance) {
  return utterance== 'notExpensive'? 1 :
  0
};

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

___________
### Pragmatic listener (L1)

- As with L0 and S1, the only difference between our pragmatic listener and that of the hyperbole model is the inclusion of a theta value. 

- We incorporate theta via our theta prior, as well as in the speaker that exists within our pragmatic listener. 

#### *Interpreting L1*

- Below are visualizations for the pragmatic listener after hearing both "expensive" and "notExpensive". 
- The "notExpensive" utterance has the majority of probability on 50, with a high likelihood that the speaker has a false valence. There is some probability that the speaker has true valence, and is communicating about higher prices, but those values are relatively low. 
- The "expensive" utterance pulls probability away from 50 (particularly the false valence probability on 50) and allocates it to the larger prices in a relatively even distribution, such that no price above 50 seems to get a significantly larger amount of probability than the other prices. Here, 5000 marginally gets the least probability. This utterance also has less probability for a positive valence, with most of the graph being made of probability that indicates that the speaker is upset. 

- When we raise alpha on this model, we can find that the results remain similar to how they were at the lower alpha levels. This is different from the outcome we had before, in which raising alpha caused the "expensive" utterance to exclusively result in our L1 returning only 10000 as an utterance. While this may be useful in a different way, the loss of hyperbole was not satisfactory. By removing the null utterance, hyperbole remains in the model, albeit in a different, less extreme way than in the chapter 3 model. However, in our current model, while we do see preservation of the shape of the posteriors, we still have a lot of probability on a false valence for 50 when a very optimal L1 hears “expensive.” This seems strange. 

- Additionally, we can also visualize this model by returning price and QUD instead of price and valence to see which QUD is responsible for the shape of the posterior probability distribution. Under the “expensive” utterance, we can see a similar shape for the valence QUD as there is high probability on low prices, which approaches the high prices with lower and lower probability, while the other two QUDs of price and priceValence have probability that is less specific, weighted towards 50 but having far more probability on all prices other than 50 when compared to the “notExpensive” utterance. The “notExpensive” utterance doesn’t have very much probability on values other than 50, and considers the price and priceValence QUDs more strongly than the valence QUD. When we increase alpha, we can see that the probability of the valence QUD moves down very slightly, but there are hardly any changes otherwise. 



~~~~
///fold: 

var utterances = ["expensive", "notExpensive"]

var utterancePrior = function() {
  return  uniformDraw(utterances)
}

var thetaPrior = function() {
  return uniformDraw(prices)
}

// theta moderates interpretation of utterances
var meaning = function(utterance, price, theta) {
return utterance == "expensive" ? price >= theta : 
  utterance == "notExpensive" ? price <= theta :
  true
}

// more words = higher cost 
var cost = function(utterance) {
  return utterance== 'notExpensive'? 1 :
  0
};

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

///
var pragmaticListener = cache(function(utterance) {
  return Infer({model: function(){
    //////// priors ////////
    var price = pricePrior()
    var valence = valencePrior(price)
    var qud = qudPrior()
    var theta = thetaPrior()
    ////////////////////////
    var fullState = {price, valence}
    observe(speaker(fullState, qud, theta), utterance)
    return {price, valence}
  }})
})

var listenerPosterior1 = pragmaticListener("expensive")
var listenerPosterior2 = pragmaticListener('notExpensive')

print('Pragmatic listener hears "expensive":')
viz(listenerPosterior1)
print('Pragmatic listener hears "not expensive":')
viz(listenerPosterior2)
~~~~

____

### Original hyperbole model predictions (for comparison):
- This is the pragmatic listener from the hyperbole model.  
- We can compare the return values we found in our current model to the old QUD model to see where the changes in predictions occur. 

- By running the old model on the 10000 utterance, the output indicates a lot of probability on 50 and 51, as well as on 10000 and 10001. This highlights the hyperbole model’s ability to show hyperbole, as there are drastically different probable prices despite the utterance literally communicating a price of 10000. 
- When the alpha level is raised on this model, probability moves away from the 10000 and 10001 utterances, moving towards 50 and 51, though the very high prices are still relatively probable. 
- When visualized using {price, QUD} instead, we can see that the valence QUD takes up the probability for all prices that are not 10000 or 10001, with the other four QUDs taking up the probability of those two prices.


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
///
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

print("Original hyperbole model predictions")
print("Pragmatic listener's joint interpretation of 'The kettle cost $10,000':")
viz(listenerPosterior)
~~~~

## 3. Putting it all together

### Full adjectives + QUD model: 



~~~~
// ADJECTIVES + QUD MODEL
// frankie + shane RSA project 

// code adapted from the Kao et al. hyperbole model + 
// gradable adjectives & vagueness resolution model 


var utterances = ["expensive", "notExpensive"]

var utterancePrior = function() {
  return  uniformDraw(utterances)
}

var thetaPrior = function() {
  return uniformDraw(prices)
}

// theta moderates interpretation of utterances
var meaning = function(utterance, price, theta) {
return utterance == "expensive" ? price >= theta : 
  utterance == "notExpensive" ? price <= theta :
  true
}

// more words = higher cost 
var cost = function(utterance) {
  return utterance== 'notExpensive'? 1 :
  0
};

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

var pragmaticListener = cache(function(utterance) {
  return Infer({model: function(){
    //////// priors ////////
    var price = pricePrior()
    var valence = valencePrior(price)
    var qud = qudPrior()
    var theta = thetaPrior()
    ////////////////////////
    var fullState = {price, valence}
    observe(speaker(fullState, qud, theta), utterance)
    return {price, valence}

  }})
})

var listenerPosterior1 = pragmaticListener("expensive")
var listenerPosterior2 = pragmaticListener('notExpensive')

print('Pragmatic listener hears "expensive":')
viz(listenerPosterior1)
print('Pragmatic listener hears "not expensive":')
viz(listenerPosterior2)
~~~~