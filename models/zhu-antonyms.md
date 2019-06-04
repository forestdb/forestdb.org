---
layout: model
title: Zhu Negated Antonyms Extension
model-language: webppl
---

###Understanding Negated Antonyms

*Author: Jun Zhu*

###Model Description
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

var utterances = ["expensive", "not-inexpensive"];

var cost = {
  "not-inexpensive":2,
  "expensive": 1,
};
var utterancePrior = function() {
  return uniformDraw(utterances);
};
~~~~

The meaning function takes a soft semantics to judge the price and threshold, so that we could strictly use ">" instead of ">=" in our model.  

~~~~
var meaning = function(utterance, price, theta) {
  utterance == "expensive" ? price > theta.expensive ? flip(0.9999) : flip(0.0001) : 
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
    var an_neg_thre = flip(0.2)
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


var utterances = ["expensive","not-inexpensive"];

var cost = {
  "not-inexpensive":2,
  "expensive": 1,
};
var utterancePrior = function() {
  return uniformDraw(utterances);
};

var meaning = function(utterance, price, theta) {
  utterance == "expensive" ? price > theta.expensive ? flip(0.9999) : flip(0.0001) : 
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
    var an_neg_thre = flip(0.2)
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

