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

We see that after many time steps, an agent converges on a single "favorite", even though no restaurant is actually better than another. This is simply because agents are more likely to go to a place where they've had a positive experience, and by chance they'll have more initial positive experiences at one place than another.

Next, we add social influence into our utility function. An agent has relationships with others, represented by a vector of weights $w$. If the weight $w_i$ corresponding to a particular other agent is positive, they will slightly increase the value they assign to a restaurant when that agent has a positive experience there, and slightly decrease it when that agent has a negative experience. For now, we do not allow agents to change their relationships over time. We simulate three agents simultaneously, all of whom have positive relationships with one another.

~~~~

var restaurants = ["Taco Town", "Burger Barn", "Stirfry Shack"];
var restaurantPrior = Enumerate(function(){
  return uniformDraw(restaurants);
});

var agentList = ["Alice", "Bob", "Carol"];
var getOtherAgents = function(name) {
  return _.without(agentList, name);
}; 

var indWeight = 1;
var initSocWeight = .5;

// initialize with uniform prior and neutral relationships ([1,1])
var createAgent = function(name) {
  var otherAgents = getOtherAgents(name);
  var relationships = _.object(otherAgents, [initSocWeight,initSocWeight]); 
  return {
    values: restaurantPrior,
    relationships: relationships
  };
};

var initializeAgents = function() {
  var agentProperties = map(function(agent) {
    return createAgent(agent);
  }, agentList);
  return _.object(agentList, agentProperties);
};

var observe = function(restaurant) {
  return flip() ? "good" : "bad";
};

var makeChoices = function(agents) {
  return mapObject(function(agentName, agentProps) {
    var restaurantChoice = sample(agentProps.values);
    var outcome = observe(restaurantChoice);
    return {choice: restaurantChoice, outcome: outcome};
  }, agents);
};

// Utility can come from own observation, as well as some 
// intrinsic benefit to doing what one's friends are doing
var calcUtility = function(possibility, info) {
  // only get input on individual utility about the place you chose
  var indUtil = (possibility === info.ownObs.choice ?
		 (info.ownObs.outcome === "good" ? indWeight : -indWeight) :
		 0);
  var pairwiseSocialUtil = sum(_.values(mapObject(function(name, obs) {
    var relWeight = info.relationships[name];
    var socialUtility = (obs.choice === possibility ?
			 (obs.outcome === "good" ? relWeight : -relWeight) :
			 0);
    return socialUtility;
  }, info.socialObs)));
  return indUtil + pairwiseSocialUtil;
};

var updateBeliefs = function(agents, allAgentsChoices) {
  return mapObject(function(agentName, agentProps) {
    // collect relevant info for agent
    var info = {ownObs : allAgentsChoices[agentName],
		relationships: agentProps.relationships,
		socialObs: _.omit(allAgentsChoices, agentName)};
    // update values by computing utility
    var newVals = Enumerate(function() {
      var possibility = sample(agentProps.values);
      factor(calcUtility(possibility, info))
      return possibility;});
    // update relationships
    var newRelationships = info.relationships;
    return {
      values: newVals,
      relationships: newRelationships
    };
  }, agents);
};

var timeStep = function(agents, remainingIterations){
  if (remainingIterations == 0) {
    return agents;
  } else {
    // Each agent chooses a restaurant and observes how good it is
    var allAgentsChoices = makeChoices(agents);
    // Agents update their beliefs, using own observation and social cues
    var updatedAgents = updateBeliefs(agents, allAgentsChoices);
    return timeStep(updatedAgents, remainingIterations - 1);
  }
};

var agents = initializeAgents();
var simResults = timeStep(agents, 50);

print("Alice's values:");
print(simResults['Alice'].values);

print("Bob's values:");
print(simResults['Bob'].values);

print("Carol's values:");
print(simResults['Carol'].values);
~~~~

We see that after enough steps, the three friends coordinate their values on the same location. If we turn the social relationship weights down to 0, we recover the phenomena we observed earlier: each agent evolves independently, and we do not see any coordination.
