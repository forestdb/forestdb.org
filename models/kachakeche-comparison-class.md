---
layout: model
title: "Heavy or not?": A comparison class model
model-language: webppl
---

### Zeinab Kachakeche

Suppose that you hear an utterance "This box is heavy" from an adult, you're likely to infer that this box is actually heavy, and weighs between let's say 20 lbs and 100 lbs depending on your experience with heavy weights.

If you hear a child saying this box is heavy, however, you might infer that this box is heavy relative to the boxes that this child has lifted. It might be true that this box weighs 100 lbs, but there is a chance that this box is not heavy for you and weighs as little as 2 lbs. 

This might also be true if you hear a bodybuilder saying this box is "light". Because "light" for a bodybuilder might be heavy for the listener. 

Children, adults, and body builders have different experiences with weights, so the listener's interpretation of the box weight after hearing the utterance "heavy" changes depending on who said the utterance.

Tessler et al. (2017) present an adjective model that highly depends on our prior knowledge about the relevant comparison classes, with the presence of uncertainty about the comparison class. For example, the listener's interpretation of tall differs if he knows that the person is a basketball player, a gymnast, or a soccer player. Their example suggested 4 classes: a superordinate category (e.g. tall for all people), and 3 subordinate categories (e.g. tall for gymnasts, basketball player, or soccer player).
 
I use the model by Tessler et al. (2017) to describe how listeners interpret "heavy" if they heard it from three different classes of people: a child, an adult, or a bodybuilder.  Each of these three comparison-classes have different experience with weights, i.e. different priors. For that reason, the listener's interpretation of the utterance "heavy" is going to be different depending on the who said the utterance. 

To model this empirical phenomenon, I use the Rational Speech Act (RSA) framework. RSA views communication as recursive reasoning between a speaker and a listener. The listener interprets the utterance they heard by reasoning about the speaker. In this case, the listener reasons about who said the "box is heavy". The listener assumes that the speaker's intention is to inform a naive listener about the actual state of the world. Then the listener reasons about what the state of the world is likely to be given that a speaker produced some utterance, knowing that the speaker is reasoning about how a listener is most likely to interpret that utterance. So in this model, the listener reasons about the actual weight of the box, given that the speaker who said "the box is heavy" is cooperative and intends to deliver her utterance in a way that the listener interprets it correctly. 

In the first code box, I highlight the differences between my model and the original model by Tessler et al. (2017). 

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

// my model:
// I made mean 3 just to make all the numbers positive. 
var superordinate = {mu: 3, sigma: 1}; 

//a list of possible box weights (state values)
//this makes the range of the distribution between 0 and 6
// so we're dealing with weights between 0(lightest weight) and 6(heaviest weight)
var stateVals = _.range(superordinate.mu - 3 * superordinate.sigma,
                superordinate.mu + 3 * superordinate.sigma,
                superordinate.sigma/binParam)


// for each possible weight, calculate its probability of occurrence
// using a Gaussian function is simply using a normal distribution 
// that's more concentrated around the mu(mean)
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

// my model: information about the category priors
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
}, method:"enumerate"})
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


**References:**

Tessler, M. H., Lopez-Brau, M., & Goodman, N. D. (2017). Warm (for winter): Comparison class understanding in vague language. In 15th International Conference on Cognitive Modeling (p. 193).