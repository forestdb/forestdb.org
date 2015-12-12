---
layout: model
title: Social Construction of Value
model-language: webppl
---

First, we set up our scenario: there are three restaurants, and each has an equal chance of being good or bad. An agent develops their own beliefs about the value of each restaurant by picking one, proportionally to their expected value in the prior, observing whether it's good or bad, and then conditioning on this observation.

~~~~
///fold:

var condition = function(x){
  factor(x ? 0 : -Infinity);
};

var mean = function(thunk){
  return expectation(Enumerate(thunk), function(v){return v;});
};

var uniformDraw = function (xs) {
  return xs[randomInteger(xs.length)];
};

var normalize = function(arr) {
  var s = reduce(function(memo, num){ return memo + num; }, 0, arr);
  return map(function(val) {return val/s; }, arr);
};

var expectedVals = function(agentPrior){
  var outs = map(function(key) { 
    return mean(function(){
      return sample(agentPrior[key]);
    });
  }, _.keys(agentPrior));
  return normalize(outs);
};
///

var restaurants = ["Taco Town", "Burger Barn", "Stirfry Shack"];
var goodnessWeights = [.1,.2,.3,.4,.5,.6,.7,.8,.9];

var restaurantPrior = map(function(restaurant) {
  return Enumerate(function(v){
    return uniformDraw(goodnessWeights);
  });
}, restaurants);

var agentPrior = _.object(restaurants, restaurantPrior);

// In fact, all restaurants are equally likely to be good
var observe = function(restaurant) {
  return flip() ? "good" : "bad";
};

// Condition on observation
var updateBeliefs = function(prior, event) {
  return mapObject(function(restaurant, value) {
    return Enumerate(function() {
      var possibleWeight = sample(value);
      var possibleOutcome = flip(possibleWeight) ? "good" : "bad";
      condition(restaurant === event.choice ? 
                possibleOutcome === event.outcome :
                true);
      return possibleWeight;
    });
  }, prior);
};

// Pick restaurants proportionally to their mean expected payoff
var makeChoices = function(prior) {
  var restaurants = _.keys(agentPrior);
  var choice = categorical(expectedVals(agentPrior), restaurants);
  var outcome = observe(choice);
  return {choice: choice, outcome : outcome};
};

var timeStep = function(prior, remainingIterations){
  if (remainingIterations == 0) {
    return prior;
  } else {
    var event = makeChoices(prior);
    var posterior = updateBeliefs(prior, event);
    return timeStep(posterior, remainingIterations - 1);
  }
};

var res = timeStep(agentPrior, 500);
print(res["Taco Town"]);
print(res["Burger Barn"]);
print(res["Stirfry Shack"]);
~~~~

We see that after many time steps, our rational agent learns the true value of each location.

Next, we add social influence into our utility function. An agent has relationships with others, represented by a vector of weights $w$. If the weight $w_i$ corresponding to a particular other agent is positive, they will slightly increase the value they assign to a restaurant when that agent has a positive experience there, and slightly decrease it when that agent has a negative experience. For now, we do not allow agents to change their relationships over time. We simulate three agents simultaneously, all of whom have positive relationships with one another.

~~~~
///fold:
var condition = function(x){
  factor(x ? 0 : -Infinity);
};

var mean = function(thunk){
  return expectation(Enumerate(thunk), function(v){return v;});
};

var uniformDraw = function (xs) {
  return xs[randomInteger(xs.length)];
};

var normalize = function(arr) {
  var s = reduce(function(memo, num){ return memo + num; }, 0, arr);
  return map(function(val) {return val/s; }, arr);
};

var expectedVals = function(agentPrior){
  var outs = map(function(key) { 
    return mean(function(){
      return sample(agentPrior[key]);
    });
  }, _.keys(agentPrior));
  return normalize(outs);
};
///

var restaurants = ["Taco Town", "Burger Barn", "Stirfry Shack"];
var goodnessWeights = [.1,.2,.3,.4,.5,.6,.7,.8,.9];

var restaurantPrior = map(function(restaurant) {
  return Enumerate(function(v){
    return uniformDraw(goodnessWeights);
  });
}, restaurants);

var agentList = ["Alice", "Bob", "Carol"];
var getOtherAgents = function(name) {
  return _.without(agentList, name);
}; 

var indWeight = 1;
var initSocWeight = .5;

// initialize with uniform prior and neutral relationships ([.5,.5])
var createAgent = function(name) {
  var otherAgents = getOtherAgents(name);
  var relationships = _.object(otherAgents, [initSocWeight,initSocWeight]); 
  return {values: _.object(restaurants, restaurantPrior), 
          relationships: relationships};
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
    var restaurants = _.keys(agentProps.values);
    var choice = categorical(expectedVals(agentProps.values), restaurants);
    var outcome = observe(choice);
    return {choice: choice, outcome: outcome};
  }, agents);
};

// // Adjust relationships based on shared experience
var updateRelationships = function(agentProps, info){
  return info.relationships;
};

// // Adjust values proportionally to utility of each restaurant
var updateValues = function(agentProps, info){
  return mapObject(function(restaurant, value) {
    return Enumerate(function() {
      var possibleWeight = sample(value);
      
      var relevantOthers = filter(function(key) {
        return (info.socialObs[key].choice === restaurant ?
                flip(info.relationships[key]) :
                false);
      }, _.keys(info.socialObs));
      
      var otherOutcomes = map(function(person){
        return info.socialObs[person].outcome;
      }, relevantOthers);
      
      var outcomes = (restaurant === info.ownObs.choice ? 
                      append(otherOutcomes, info.ownObs.outcome) : 
                      otherOutcomes);

      var possibleOutcomes = repeat(outcomes.length, function(){
        return flip(possibleWeight) ? "good" : "bad";
      });
     
      condition(_.isEqual(outcomes, possibleOutcomes));
      
      return possibleWeight;
    });
  }, agentProps.values);
};

// Loop through each agent, update their values and their relationships
var updateBeliefs = function(agents, allAgentsChoices) {
  return mapObject(function(agentName, agentProps) {
    var info = {ownObs : allAgentsChoices[agentName],
                relationships: agentProps.relationships,
                socialObs: _.omit(allAgentsChoices, agentName)};
    var newValues = updateValues(agentProps, info);
    var newRelationships = updateRelationships(agentProps, info);
    return {values: newValues, relationships: newRelationships};
  }, agents);
};

// Advance simulation by one timeStep:
// 1) Each agent chooses a restaurant and observes how good it is
// 2) Agents update their beliefs, using own observation and social cues
var timeStep = function(agents, remainingIterations){
  if (remainingIterations == 0) {
    return agents;
  } else {
    var allAgentsChoices = makeChoices(agents);
    var updatedAgents = updateBeliefs(agents, allAgentsChoices);
    return timeStep(updatedAgents, remainingIterations - 1);
  }
};

var agents = initializeAgents();
var simResults = timeStep(agents, 500);
print("Alice's values:");
print(simResults['Alice'].values['Taco Town']);
print(simResults['Alice'].values['Burger Barn']);
print(simResults['Alice'].values['Stirfry Shack']);

print("Bob's values:");
print(simResults['Bob'].values['Taco Town']);
print(simResults['Bob'].values['Burger Barn']);
print(simResults['Bob'].values['Stirfry Shack']);

print("Carol's values:");
print(simResults['Carol'].values['Taco Town']);
print(simResults['Carol'].values['Burger Barn']);
print(simResults['Carol'].values['Stirfry Shack']);
~~~~

We see that after enough steps, the three friends coordinate their values on the same location. If we turn the social relationship weights down to 0, we recover the phenomena we observed earlier: each agent evolves independently, and we do not see any coordination.

  Next, we allow agents to update their relationships based on common experiences. If one agent highly values the restaurant that another agent chooses, they will increase their opinion of that agent, allowing that agent's experiences to influence their utility more strongly in the future. On the other hand, if one agent assigns low value to the restaurant that another agent chooses, they will decrease their opinion of that agent, so that they'll be less influenced by that agent in the future. This relationship update rule is meant to reflect a basic "like attracts like" social dynamic. We tend to associate with people who have similar values (as evidenced through their choices), and therefore allow their experiences to influence our values more strongly. For example, if I have a friend who shares my taste in music, I'm more likely to take their recommendation of a new album than the recommendation of a stranger.

~~~~

var restaurants = ["Taco Town", "Burger Barn", "Stirfry Shack"];
var restaurantPrior = Enumerate(function(){
  return uniformDraw(restaurants);
});

var agentList = ["Alice", "Bob", "Carol"];
var getOtherAgents = function(name) {
  return _.without(agentList, name);
};

var indWeight = 2;
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

var calcIndividualUtil = function(possibility, info) {
  var weight = info.ownObs.outcome === "good" ? indWeight : -indWeight;
  return info.ownObs.choice === possibility ? weight : 0;
};

// possibility has more utility if another agent liked it, weighted by
// strength of relationship to that person
var calcSocialUtil = function(possibility, info) {
  return sum(_.values(mapObject(function(name, obs) {
    var relWeight = info.relationships[name];
    var weight = obs.outcome === "good" ? relWeight : -relWeight;
    return obs.choice === possibility ? weight : 0;
  }, info.socialObs)));
};

// Utility can come from own observation, as well as some
// intrinsic benefit to doing what one's friends are doing.
// only get input on individual utility about the place you chose
var calcUtility = function(possibility, info) {
  var indUtil = calcIndividualUtil(possibility, info);
  var pairwiseSocialUtil = calcSocialUtil(possibility, info);
  return indUtil + pairwiseSocialUtil;
};

// Adjust relationships based on shared experience:
//   if agent 1 highly values the restaurant agent 2 chooses,
//   relationship gets stronger
var updateRelationships = function(agentProps, info){
  return mapObject(function(otherName, otherWeight) {
    var otherObs = info.socialObs[otherName];
    var choiceVal = Math.exp(agentProps.values.score([], otherObs.choice));
    var newWeight = otherWeight * (choiceVal + 2/3);
    return newWeight > 1 ? 1 : newWeight;
  }, info.relationships);
};

// Adjust values proportionally to utility of each restaurant
var updateValues = function(agentProps, info){
  return Enumerate(function() {
    var possibility = sample(agentProps.values);
    factor(calcUtility(possibility, info))
    return possibility;});
};

// Loop through each agent, update their values and their relationships
var updateBeliefs = function(agents, allAgentsChoices) {
  return mapObject(function(agentName, agentProps) {
    var info = {ownObs : allAgentsChoices[agentName],
                relationships: agentProps.relationships,
                socialObs: _.omit(allAgentsChoices, agentName)};
    var newValues = updateValues(agentProps, info);
    var newRelationships = updateRelationships(agentProps, info);
    return {values: newValues, relationships: newRelationships};
  }, agents);
};

// Advance simulation by one timeStep:
// 1) Each agent chooses a restaurant and observes how good it is
// 2) Agents update their beliefs, using own observation and social cues
var timeStep = function(agents, remainingIterations){
  //  print(agents);
  if (remainingIterations == 0) {
    return agents;
  } else {
    var allAgentsChoices = makeChoices(agents);
    var updatedAgents = updateBeliefs(agents, allAgentsChoices);
    return timeStep(updatedAgents, remainingIterations - 1);
  }
};

var agents = initializeAgents();
var simResults = timeStep(agents, 100);

print("Alice's values:");
print(simResults['Alice'].values);
print(simResults['Alice'].relationships);

print("Bob's values:");
print(simResults['Bob'].values);
print(simResults['Bob'].relationships);

print("Carol's values:");
print(simResults['Carol'].values);
print(simResults['Carol'].relationships);

~~~~

We see that several different equilibria can form, depending on the exact sequence of early observations. The most obvious is what happened above: all three agents converge on a single restaurant, and all have strong pairwise relationships. Another possibility is two agents with mutually strong relationships who have converged on the same restaurant, while the third agent has weak social bonds and prefers a different restaurant. A third possibility is all three agents prefering different restaurants and mutually disregarding one another. 
