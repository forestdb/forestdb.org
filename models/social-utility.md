---
layout: model
title: Social Construction of Value
model-language: webppl
---

First, we set up our scenario: there are three restaurants, and each
has an equal chance of being good or bad. An agent develops their own
beliefs about the value of each restaurant by picking one from its
prior, observing whether it's good or bad, and then updating their
beliefs using a utility function solely related to their own
observation.

~~~~
var restaurants = ["Taco Town", "Burger Barn", "Stirfry Shack"];
var restaurantPrior = Enumerate(function(){
  return uniformDraw(restaurants);
});

var observe = function(restaurant) {
  return flip() ? "good" : "bad";
};

var calcUtility = function(possibility, event) {
  return event.outcome === "good" ? .5 : -.5;
};

var updateBeliefs = function(prior, event) {
  return Enumerate(function() {
    var possibility = sample(prior);
    factor(possibility === event.choice ?
	   calcUtility(possibility, event) :
	   0);
    return possibility;
  });
};

var timeStep = function(prior, remainingIterations){
  if (remainingIterations == 0) {
    return prior;
  } else {
    var choice = sample(prior);
    var outcome = observe(choice);
    var event = {choice : choice, outcome : outcome};
    var posterior = updateBeliefs(prior, event);
    return timeStep(posterior, remainingIterations - 1);
  }
};

print(timeStep(restaurantPrior, 50));
~~~~