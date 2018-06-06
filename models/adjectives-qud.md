---
layout: model
title: Daniel, Hondel & Ramirez Adjectives QUD
model-language: webppl
---

Extending the Vagueness resolution with a QUD

Authors: Aaliyah Daniel, Sylvia Ramirez, and Nic Hondel

Vagueness~
Lassiter & Goodman (2013)

Sometimes words without context can be vague, take the word "expensive" into different context it can mean many different things. When we use the word "expensive" for buying an ice cream it is much different than what someone would mean when they say "expensive" for a purchasing a bike or a car. Then how do we conclude what the meaning of "expensive" really is?

"Semanticists settle on the least common denominator: a threshold semantics by which the adjective asserts that holders of the relevant property surpass some point on the relevant scale (i.e., expensive means more expensive than d for some contextually-determined degree of price d). Whereas semanticists punt on the mechanism by which context fixes these aspects of meaning, the RSA framework is well-suited to meet the challenge". 


Lassiter and Goodman suggest gradable adjectives...

(1) [Almost all ice cream in the OC cost $2-38.]

Someone says, "That ice cream is expensive"

a. A $38 ice cream is expensive

b. A $1 ice cream is not expensive

c. A $6 ice cream is neither expensive nor not expensive- rather its roughly average. 

Dependent on what question you ask .. can have an affect on what the listener believes to be expensive. 


Savinelli et. al 2017

What someone interprets as the QUD may impact their interpretation.



~~~~
var marginalize = function(dist, key){
  return Infer( {model: function(){
    return sample(dist)[key];
  }})
};
var icecream = {
   "prices":  [1, 4, 6, 10, 14, 18, 22, 30, 34, 38],
"probabilities": [0.01, 0.50, 0.85, 0.63, 0.35, 0.10, 0.04, 0.02, 0.02, 0.01]
};
~~~~

This model depends on our prior knowledge of the world state. 

Next, we create a prior for the degree threshold θ. Since we’re talking about expensive ice cream, θ will be the price cutoff to count as expensive. But we want to be able to use expensive to describe anything with a price, so we’ll set the thetaPrior to be uniform over the possible prices in our world.

We then introduce three new utterances, "expensive", "cheap", or "null". The semantics of the expensive utterance checks the relevant item’s price against the price cutoff. The "cheap utterance" is true only when it checks the relevant item's price against expensive and if it is not true that it is expensive, than it must be cheap. The “null utterance” is true everywhere and it is assumed to be less likely than uttering expensive.  

~~~~
var icecream = {
  "prices":  [1, 4, 6, 10, 14, 18, 22, 30, 34, 38],
"probabilities": [0.01, 0.50, 0.85, 0.63, 0.35, 0.10, 0.04, 0.02, 0.02, 0.01]
};

var statePrior = function() {
  return categorical(icecream.probabilities, icecream.prices);
};

var thetaPrior = function() {
    return uniformDraw(icecream.prices);
};

var alpha = 1; // optimality parameter

var utterances = ["expensive", "null", "cheap"];
var cost = {
  "expensive": 1,
  "cheap": 2,
  "null": 0
};

var utterancePrior = function() {
  var uttProbs = map(function(u) {return Math.exp(-cost[u]) }, utterances);
  return categorical(uttProbs, utterances);
};

var meaning = function(utterance, price, theta) {
  utterance == "expensive" ? price >= theta : true;
 };
~~~~

~~~~
// QUDs
var QUDs = ["how expensive?","less than 10?"]
var QUDPrior = function() {
 // uniformDraw(QUDs)
categorical([1,1,1],QUDs)// this is equivalent to uniformDraw
categorical([0,0,1],QUDs)// this is your baseline version

}
var QUDFun = function(QUD,state) {
  QUD == "how expensive?" ? state :
  QUD == "less than 10?" ? state == 0 :
  state;
};
~~~~

By adding an S1 and L1 layer we create a full RSA model. The L1 hears the ambiguous utterance “expensive” or  “cheap”  and proceeds to determine what the speaker meant and the price cutoff to count as “expensive” and “cheap” while inferring the state of the world. 


~~~~
var literalListener = cache(function(utterance, theta, QUD) {
  return Infer({model:function() {
    var price = statePrior()
    var qPrice = QUDFun(QUD,price)
    condition(meaning(utterance, price, theta))
    return qPrice;
}})
});

var speaker = cache(function(price, theta,QUD) {
  return Infer( {model: function() {
    var utterance = utterancePrior()
    var qPrice= QUDFun(QUD, price)
    factor( alpha * literalListener(utterance, theta, QUD).score(qPrice) );
    return utterance;
  }});
});

var pragmaticListener = function(utterance) {
  return Infer({model: function() {
    var price = statePrior()
    var theta = thetaPrior()
    var QUD =QUDPrior()
    factor(speaker(price, theta, QUD).score(utterance));
    return { price: price, theta: theta, qud: QUD};
  }})
}
~~~~

Here is the full model. 

~~~~
var marginalize = function(dist, key){
  return Infer( {model: function(){
    return sample(dist)[key];
  }})
};

var icecream = {
  "prices":  [1, 4, 6, 10, 14, 18, 22, 30, 34, 38],
"probabilities": [0.01, 0.50, 0.85, 0.63, 0.35, 0.10, 0.04, 0.02, 0.02, 0.01]
};

var statePrior = function() {
  return categorical(icecream.probabilities, icecream.prices);
};

var thetaPrior = function() {
    return uniformDraw(icecream.prices);
};

var alpha = 1; // optimality parameter

var utterances = ["expensive", "null", "cheap"];
var cost = {
  "expensive": 1,
  "cheap": 2,
  "null": 0
};
var utterancePrior = function() {
  var uttProbs = map(function(u) {return Math.exp(-cost[u]) }, utterances);
  return categorical(uttProbs, utterances);
};

var meaning = function(utterance, price, theta) {
utterance == "expensive" ? price >= theta : true;
  
};
// QUDs
var QUDs = ["expensive?","less than 100?","what is the price?"]
var QUDPrior = function() {
  //uniformDraw(QUDs)
categorical([1,1,1],QUDs)// this is equivalent to uniformDraw
categorical([0,0,1],QUDs)// this is your baseline version
}
var QUDFun = function(QUD,state) {
  QUD == "expensive?" ? state :
  QUD == "less than 100?" ? state == 0 :
  state;
};

var literalListener = cache(function(utterance, theta, QUD) {
  return Infer({model:function() {
    var price = statePrior()
    var qPrice = QUDFun(QUD,price)
    condition(meaning(utterance, price, theta))
    return qPrice;
}})
});

var speaker = cache(function(price, theta,QUD) {
  return Infer( {model: function() {
    var utterance = utterancePrior()
    var qPrice= QUDFun(QUD, price)
    factor( alpha * literalListener(utterance, theta, QUD).score(qPrice) );
    return utterance;
  }});
});

var pragmaticListener = function(utterance) {
  return Infer({model: function() {
    var price = statePrior()
    var theta = thetaPrior()
    var QUD =QUDPrior()
    factor(speaker(price, theta, QUD).score(utterance));
    return { price: price, theta: theta, qud: QUD};
  }})
}

print('Expensive ice cream')
var expensiveIcecream = pragmaticListener('expensive');
viz.auto(marginalize(expensiveIcecream, "price"));
viz.auto(marginalize(expensiveIcecream, "theta"));


print('Cheap ice cream')
var cheapIcecream = pragmaticListener('cheap');
viz.auto(marginalize(expensiveIcecream, "price"));
viz.auto(marginalize(expensiveIcecream, "theta"));

// print('Null ice cream')
// var expensiveIcecream = pragmaticListener('null');
// viz.auto(marginalize(expensiveIcecream, "price"));
// viz.auto(marginalize(expensiveIcecream, "theta"));
~~~~