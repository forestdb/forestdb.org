---
layout: model
title: Social Construction of Value
model-language: webppl
model-language-version: v0.9.7
---

Suppose there are $M$ restaurants, which generate noisy reward signals
$r_j \in \{0, 1\}$. Each agent $a_i$ in the population assigns some
subjective utility $u_j$ to each restaurant $j$, such that $u_j =
P(r_j = 1)$. These subjective utilities are drawn from a shared normal
distribution, so all agents have relatively similar utilities
functions. We will model a particular agent, Alice, as she infers her
own utility function. She uses two sources of information. First,
Alice assumes that all other agents know their own utility and decide
which restaurants to visit according to a soft-max rule. Second, when
Alice chooses according to her beliefs about her own utility, she
observes a noisy reward signal from her true utility function. For
each time step, then, Alice makes a choice according to her best guess
at her utility function, then updates her beliefs based on the reward
signal of this choice and her observations of the choices that others
made on that time step.

~~~~
///fold:
var truncate = function(obj) {
  return mapObject(function(key, val) {
    return (val <= 0 ? 0.001 :
            val >= 1 ? 0.999 :
            val);
  }, obj);
};

// Each agent chooses proportional to own utility (Luce choice...)
var choiceDist = function(utility) {
  var ps = normalize(_.values(utility));
  var vs = _.keys(utility);
  return Categorical({ps, vs});
};

// Given a sampled utility for each agent, how likely are their choices?
var otherLikelihoods = function(otherUtilities, otherChoices) {
  var likelihoods = map2(function(otherUtility, otherChoice) {
    return choiceDist(otherUtility).score(otherChoice);
  }, otherUtilities, otherChoices);
  return sum(likelihoods);
};

var sampleAgentUtility = function(groupParams) {
  var mean = groupParams.groupMean;
  var sd = groupParams.groupSD;
  return truncate({
    "Burger Barn" : gaussian(mean["Burger Barn"], sd),
    "Stirfry Shack" : gaussian(mean["Stirfry Shack"], sd)
  });
};

var sampleOtherUtilities = function(numAgents, groupParams) {
  return repeat(numAgents, function() {
    return sampleAgentUtility(groupParams);
  })
};
///

var numAgents = 3;

var sampleGroupParams = function() {
  return {
    groupMean : {
      "Burger Barn" : beta(1,1),
      "Stirfry Shack" : beta(1,1)
    },
    groupSD : uniform(0,.15)
  };
};

var prior = function() {
  var groupParams = sampleGroupParams(); 
  return {
    groupParams : groupParams,
    ownUtility: sampleAgentUtility(groupParams),
    otherUtilities : sampleOtherUtilities(numAgents, groupParams)
  };  
};

// case 1
var data = [{self: {choice : "Burger Barn", rewardSignal : true},
             others : ["Stirfry Shack", "Stirfry Shack", "Stirfry Shack"]}];
// case 2
// var data = [{self: {choice : "Burger Barn", rewardSignal : true},
//                  others : ["Stirfry Shack", "Stirfry Shack", "Stirfry Shack"]},
//                 {self: {choice : "Burger Barn", rewardSignal : true},
//                  others : ["Stirfry Shack", "Stirfry Shack", "Stirfry Shack"]},
//                 {self: {choice : "Burger Barn", rewardSignal : true},
//                  others : ["Stirfry Shack", "Stirfry Shack", "Stirfry Shack"]},
//                 {self: {choice : "Burger Barn", rewardSignal : true},
//                  others : ["Stirfry Shack", "Stirfry Shack", "Stirfry Shack"]}];
// case 3
// var data = [{self: {choice : "Burger Barn", rewardSignal : false},
//                  others : ["Stirfry Shack", "Stirfry Shack", "Stirfry Shack"]},
//                 {self: {choice : "Burger Barn", rewardSignal : false},
//                  others : ["Stirfry Shack", "Stirfry Shack", "Stirfry Shack"]},
//                 {self: {choice : "Burger Barn", rewardSignal : false},
//                  others : ["Stirfry Shack", "Stirfry Shack", "Stirfry Shack"]},
//                 {self: {choice : "Burger Barn", rewardSignal : false},
//                  others : ["Burger Barn", "Burger Barn", "Stirfry Shack"]}];

var model = function() {
  var beliefs = prior();
  mapData({data: data}, function(datum) {
    // What beliefs would make this reward signal most likely?
    var rewardExpectation = beliefs.ownUtility[datum.self.choice];
    observe(Bernoulli({p: rewardExpectation}), datum.self.rewardSignal)
    
    // What beliefs would make my friend's choices most likely?
    factor(otherLikelihoods(beliefs.otherUtilities, datum.others));
  });
  return beliefs;
};

var results = Infer({method: 'SMC', particles: 10000}, model);

console.log('estimation of own utility')
viz.marginals(marginalize(results, 'ownUtility'))

console.log('estimation of group params')
console.log(expectation(results, function(x) {return x['groupParams']['groupMean']['Burger Barn']}))
console.log(expectation(results, function(x) {return x['groupParams']['groupMean']['Stirfry Shack']}))
console.log(expectation(results, function(x) {return x['groupParams']['groupSD']}))
~~~~

Despite the fact Alice doesn't explicitly observe any information about the Stirfry Shack, she nonetheless forms strong beliefs about it by observing the actions of agents that she believes belong to her group. By comparing evidence1 to evidence2, we see that additional evidence strengthens Alice's belief and also leads to an inference that the SD of her group must be quite large (otherwise it's hard to explain why no one else is choosing the Burger Barn). By comparing evidence2 to evidence3, we see that observing just a few mixed signals (i.e. a bad experience at Burger Barn herself, and social evidence of some other choosing Burger Barn), her beliefs about SD shift much lower.

In this first simulation, we assumed that all agents belong to the same group. In real social situations, however, there exist many groups with many different values and preferences. To formalize an intuitive theory about such situations, we extend our model with a simple hierarchical prior such that Alice can infer group membership in addition to utilities. First, we sample a number of groups between 1 and K. Next, we sample means and SDs for each of these groups independently. Finally, we sample an assignment of all agents in the population to one of K groups, such that that agent's utilities are drawn from that group's statistics. 

~~~~

///fold:
var butLast = function(xs) {
  return xs.slice(0, xs.length - 1);
};

var truncate = function(obj) {
  return mapObject(function(key, val) {
    return (val <= 0 ? 0.001 :
            val >= 1 ? 0.999 :
            val);
  }, obj);
};

var normalize = function(xs) {
  var Z = sum(xs);
  return map(function(x) {
    return x / Z;
  }, xs);
};

var normalizeVals = function(agentVals){
  var arr = _.values(agentVals);
  return normalize(arr);
};

// Each agent soft-maxes utility
var makeChoiceERP = function(utility) {
  var ps = normalizeVals(utility);
  var vs = _.keys(utility);
  return categoricalERP(ps, vs);
};

var choiceLikelihood = function(ownUtility, choice) {
  var choiceERP = makeChoiceERP(ownUtility);
  return choiceERP.score([], choice);
};

var otherLikelihoods = function(otherUtilities, otherChoices) {
  var likelihoods = map2(function(otherUtility, otherChoice) {
    var otherChoiceERP = makeChoiceERP(otherUtility);
    return otherChoiceERP.score([], otherChoice);
  }, otherUtilities, otherChoices);
  return sum(likelihoods);
};

var sampleAgentUtility = function(groupParams) {
  var mean = groupParams.groupMean;
  var sd = groupParams.groupSD;
  return truncate({
    "Burger Barn" : gaussian(mean["Burger Barn"], sd),
    "Stirfry Shack" : gaussian(mean["Stirfry Shack"], sd)
  });
};

var sampleOtherUtilities = function(groupParams, groupMembership, numAgents) {
  return map(function(agentIndex) {
    var otherGroupParams = groupParams[groupMembership[agentIndex + 1]];
    return sampleAgentUtility(otherGroupParams);
  }, _.range(numAgents))
};

var sampleGroupParams = function(numGroups) {
  return repeat(numGroups, function() {
    return {groupMean : {"Burger Barn" : uniform(0,1),
                         "Stirfry Shack" : uniform(0,1)},
            groupSD : uniform(0, 0.1)};
  });
};

var sampleGroupMembership = function(numAgents, numGroups) {
  return repeat(numAgents + 1, function() {
    return randomInteger(numGroups);
  });
};

///

var numAgents = 3;
var maxNumGroups = 3;

var beliefPrior = function() {
  var numGroups = randomInteger(maxNumGroups - 1) + 1;
  var groupParams = sampleGroupParams(numGroups);
  var groupMembership = sampleGroupMembership(numAgents, numGroups);
  
  return {
    groupParams: groupParams,
    numGroups : numGroups,
    groupMembership: groupMembership,
    ownUtility: sampleAgentUtility(groupParams[groupMembership[0]]),
    otherUtilities: sampleOtherUtilities(groupParams, groupMembership, numAgents)
  };
};

var infer = function(evidence) {
  if(evidence.length === 0) {
    return beliefPrior(); // Take a sample from prior
  } else {
    var newEvidence = last(evidence);

    // Recursively reason about what I would have believed last time step
    var beliefs = infer(butLast(evidence));

    // What beliefs would make this reward signal most likely?
    factor(bernoulliERP.score([beliefs.ownUtility[newEvidence.self.choice]],
                              newEvidence.self.rewardSignal));

    // What beliefs would make my friend's choices most likely?
    factor(otherLikelihoods(beliefs.otherUtilities, newEvidence.others));
    return beliefs;
  }
};

var results = SMC(function() {
  // evidence 1
  var evidence = [{self: {choice : "Burger Barn", rewardSignal : true},
                   others : ["Stirfry Shack", "Stirfry Shack", "Stirfry Shack"]}];
  // evidence 2
  // var evidence = [{self: {choice : "Burger Barn", rewardSignal : true},
  //                  others : ["Stirfry Shack", "Stirfry Shack", "Stirfry Shack"]},
  //                 {self: {choice : "Burger Barn", rewardSignal : true},
  //                  others : ["Stirfry Shack", "Stirfry Shack", "Stirfry Shack"]},
  //                 {self: {choice : "Burger Barn", rewardSignal : true},
  //                  others : ["Stirfry Shack", "Stirfry Shack", "Stirfry Shack"]},
  //                 {self: {choice : "Burger Barn", rewardSignal : true},
  //                  others : ["Stirfry Shack", "Stirfry Shack", "Stirfry Shack"]}];
  // evidence 3
  // var evidence = [{self: {choice : "Burger Barn", rewardSignal : true},
  //                  others : ["Stirfry Shack", "Stirfry Shack", "Stirfry Shack"]},
  //                 {self: {choice : "Burger Barn", rewardSignal : true},
  //                  others : ["Stirfry Shack", "Stirfry Shack", "Stirfry Shack"]},
  //                 {self: {choice : "Burger Barn", rewardSignal : true},
  //                  others : ["Stirfry Shack", "Stirfry Shack", "Stirfry Shack"]},
  //                 {self: {choice : "Burger Barn", rewardSignal : false},
  //                  others : ["Burger Barn", "Burger Barn", "Stirfry Shack"]}];
  return infer(evidence);
}, {particles : 10000});

vizPrint(Enumerate(function() { return sample(results).ownUtility;}));
print(Enumerate(function() { return sample(results).groupMembership;}));
print(Enumerate(function() { return sample(results).numGroups;}));
~~~~

Note that evidence2 now constitutes fairly strong evidence that there are two groups, one solely containing the agent and the other containing everyone else.

Next, we add incidental features to the agents. In the real world, we don't always get to observe the choices of other agents, but we do observe perceptual features like skin color, hair color, and the team name on a sports jersey. Work on stereotyping and ingroup-outgroup perception suggests that we expect many groups to share such features. To incorporate this aspect of an intuitive theory of groups, we sample a "feature probability" for each group. If it is 1, then we expect all agents in the group to have that feature. If it is .5, we expect roughly half of the agents to have that features. Our agent then takes these stable perceptual traits into account when inferring group membership and utility.

~~~~

///fold:
var butLast = function(xs) {
  return xs.slice(0, xs.length - 1);
};

var truncate = function(obj) {
  return mapObject(function(key, val) {
    return (val <= 0 ? 0.001 :
            val >= 1 ? 0.999 :
            val);
  }, obj);
};

var normalize = function(xs) {
  var Z = sum(xs);
  return map(function(x) {
    return x / Z;
  }, xs);
};

var normalizeVals = function(agentVals){
  var arr = _.values(agentVals);
  return normalize(arr);
};

// Each agent soft-maxes utility
var makeChoiceERP = function(utility) {
  var ps = normalizeVals(utility);
  var vs = _.keys(utility);
  return categoricalERP(ps, vs);
};

var choiceLikelihood = function(ownUtility, choice) {
  var choiceERP = makeChoiceERP(ownUtility);
  return choiceERP.score([], choice);
};

var otherChoiceLikelihoods = function(beliefs, otherChoices) {
  var likelihoods = map2(function(otherUtility, otherChoice) {
    var otherChoiceERP = makeChoiceERP(otherUtility);
    return otherChoiceERP.score([], otherChoice);
  }, beliefs.otherUtilities, otherChoices);
  return sum(likelihoods);
};

var featureLikelihoods = function(groupParams, groupMembership, features) {
  var likelihoods = map2(function(agentID, feature) {
    var group = groupMembership[agentID];
    var featureProb = groupParams[group].groupFeatureProb;
    return bernoulliERP.score([featureProb], feature);
  }, _.range(groupMembership.length), features);
  return sum(likelihoods);
};

var sampleAgentUtility = function(groupParams) {
  var mean = groupParams.groupMean;
  var sd = groupParams.groupSD;
  return truncate({
    "Burger Barn" : gaussian(mean["Burger Barn"], sd),
    "Stirfry Shack" : gaussian(mean["Stirfry Shack"], sd)
  });
};

var sampleOtherUtilities = function(groupParams, groupMembership, numAgents) {
  return map(function(agentIndex) {
    var otherGroupParams = groupParams[groupMembership[agentIndex + 1]];
    return sampleAgentUtility(otherGroupParams);
  }, _.range(numAgents));
};

var sampleGroupParams = function(numGroups) {
  return repeat(numGroups, function() {
    return {groupMean : {"Burger Barn" : uniform(0,1),
                         "Stirfry Shack" : uniform(0,1)},
            groupSD : uniform(0, 0.1),
            groupFeatureProb : uniform(0,1)};
  });
};

var sampleGroupMembership = function(numAgents, numGroups) {
  return repeat(numAgents + 1, function() {
    return randomInteger(numGroups);
  });
};

///

var numAgents = 3;
var maxNumGroups = 3;

var beliefPrior = function(features) {
  var numGroups = randomInteger(maxNumGroups - 1) + 1;
  var groupParams = sampleGroupParams(numGroups);
  var groupMembership = sampleGroupMembership(numAgents, numGroups);

  // Incorporate perceptual features into initial belief prior
  factor(featureLikelihoods(groupParams, groupMembership, features));
  
  return {
    numGroups : numGroups,
    groupParams: groupParams,
    groupMembership: groupMembership,
    ownUtility: sampleAgentUtility(groupParams[groupMembership[0]]),
    otherUtilities: sampleOtherUtilities(groupParams, groupMembership, numAgents)
  };
};

var infer = function(input) {
  if(input.evidence.length === 0) {
    return beliefPrior(input.features); // Take a sample from prior
  } else {
    var newEvidence = last(input.evidence);

    // Recursively reason about what I would have believed last time step
    var beliefs = infer(_.extend(input, {evidence : butLast(input.evidence)}));

    // What beliefs would make this reward signal most likely?
    factor(bernoulliERP.score([beliefs.ownUtility[newEvidence.self.choice]],
                              newEvidence.self.rewardSignal));

    // What beliefs would make my friend's choices most likely?
    factor(otherChoiceLikelihoods(beliefs, newEvidence.others));

    return beliefs;
  }
};

var results = SMC(function() {
  var features = [false, true, true, true];

  // evidence 1
  var input = {
    features: features,
    evidence: []};
    
  // evidence 2
  // var input = {
  //   features: features,
  //   evidence: [{self: {choice : "Stirfry Shack", rewardSignal : true},
  //               others : ["Stirfry Shack", "Stirfry Shack", "Stirfry Shack"]},
  //              {self: {choice : "Stirfry Shack", rewardSignal : true},
  //               others : ["Stirfry Shack", "Stirfry Shack", "Stirfry Shack"]},
  //              {self: {choice : "Stirfry Shack", rewardSignal : true},
  //               others : ["Stirfry Shack", "Stirfry Shack", "Stirfry Shack"]},
  //              {self: {choice : "Stirfry Shack", rewardSignal : true},
  //              others : ["Stirfry Shack", "Stirfry Shack", "Stirfry Shack"]}]};

  // evidence 3
  // var input = {
  //   features: features,
  //   evidence: [{self: {choice : "Stirfry Shack", rewardSignal : true},
  //               others : ["Stirfry Shack", "Stirfry Shack", "Stirfry Shack"]},
  //              {self: {choice : "Stirfry Shack", rewardSignal : true},
  //               others : ["Stirfry Shack", "Stirfry Shack", "Stirfry Shack"]},
  //              {self: {choice : "Stirfry Shack", rewardSignal : true},
  //               others : ["Stirfry Shack", "Stirfry Shack", "Stirfry Shack"]},
  //              {self: {choice : "Stirfry Shack", rewardSignal : true},
  //               others : ["Stirfry Shack", "Stirfry Shack", "Stirfry Shack"]},
  //              {self: {choice : "Burger Barn", rewardSignal : true},
  //               others : ["Stirfry Shack", "Stirfry Shack", "Stirfry Shack"]}]};

  return infer(input);
}, {particles : 20000});

vizPrint(Enumerate(function() { return sample(results).ownUtility;}));
print(Enumerate(function() { return sample(results).groupMembership;}));
print(Enumerate(function() { return sample(results).numGroups;}));
vizPrint(Enumerate(function() {
  var probList = map(function(groupParam) {
    return groupParam.groupFeatureProb;
  }, sample(results).groupParams);
  return (probList.length == 1 ? append(probList, [0,0]) :
          probList.length == 2 ? append(probList, [0]) : probList);
}));
~~~~

<!---
~~~~
///fold:
var normalize = function(xs) {
  var Z = sum(xs);
  return map(function(x) {
    return x / Z;
  }, xs);
};

var utilityMean = function(knowledge) {
  var options = _.keys(sample(knowledge).utility);
  var means = map(function(key) {
    return expectation(knowledge, function(v){
      return [v.utility[key]];
    });
  }, options);
  return _.object(options, means);
};


var normalizeVals = function(agentVals){
  var arr = _.values(agentVals);
  return normalize(arr);
};

// Sample noisy reward signal for r_j based on agent's utility
var observe = function(utility, restaurant) {
  return flip(utility[restaurant]) ? "good" : "bad";
};

// Each agent soft-maxes utility
var makeChoiceERP = function(utility) {
  var ps = normalizeVals(utility);
  var vs = _.keys(utility);
  return categoricalERP(ps, vs);
};

// Sample a choice for all agents in the population
var getOtherChoices = function(agents) {
  return map(function(a){
    var choiceERP = makeChoiceERP(a.utility);
    return sample(choiceERP);
  }, agents);
};
///

// All agents in the population have utilities drawn from a shared prior
var truePrior = function(groupA) {
  return {
    "Taco Town" : gaussian(.5, .05),
    "Burger Barn" : groupA ? gaussian(.75, .05) : gaussian(.25, .05),
    "Stirfry Shack" : groupA ? gaussian(.25, .05) : gaussian(.75, .05)
  };
};

// Create population of agents
var initializeAgents = function(numAgents) {
  return repeat(numAgents, function() {
    var groupAssignment = flip();
    return {utility : truePrior(groupAssignment)};
  });
};

// Alice updates her beliefs by conditioning on her reward signal and others
var infer = function(agent, ownChoice, otherChoices) {
  var updatedKnowledge = Enumerate(function() {

    // Sample a possible utility function & group assignment
    var knowledge = sample(agent.knowledge);
    var utility = knowledge.utility;
    var groupAssignments = knowledge.groupAssignments;
    var expectedChoiceERP = makeChoiceERP(utility);

    // Take true reward signal into account
    factor(bernoulliERP.score([utility[ownChoice.choice]],
                              ownChoice.rewardSignal));

    // Try to maximize log-likelihood of others' choices,
    // if you think they're from the same group
    // TODO: fix bias toward ignoring people
    var relevantOthers = map(function(v){return v[0]},
                             filter(function(a) {return a[1];},
                                    zip(otherChoices, groupAssignments)));
    var otherLikelihoods = map(function(otherChoice) {
      return expectedChoiceERP.score([], otherChoice);
    }, relevantOthers);
    factor(otherLikelihoods.length === 0 ?
           -Infinity :
           sum(otherLikelihoods)/otherLikelihoods.length);

    return {utility: utility, groupAssignments: groupAssignments};
  });
  return {
    knowledge : updatedKnowledge,
    trueUtility : agent.trueUtility
  };
};

// How many agents do we want in our population?
var numAgents = 5;

// Fixed population of agents, with their choices as data
var agents = initializeAgents(numAgents);
print(agents);

// Alice has some prior on her own utility, as well as a true utility
var alice = {
  knowledge : Enumerate(function() {
    return {
      utility : {
        "Taco Town"     : uniformDraw([.1, .25, .5, .75, .9]),
        "Burger Barn"   : uniformDraw([.1, .25, .5, .75, .9]),
        "Stirfry Shack" : uniformDraw([.1, .25, .5, .75, .9])
      },
      groupAssignments  : repeat(numAgents, function() {return flip();})
    };
  }),
  trueUtility : truePrior(true),
};
print(alice.trueUtility);

// Advance simulation by one timeStep:
// 1) Alice makes a choice based on current beliefs, observes reward signal
// 2) Alice observes other agents' choices
var timeStep = function(agent, remainingIterations){
  if (remainingIterations == 0) {
    return agent;
  } else {
    // Use current beliefs about utility to make a choice
    var choiceERP = makeChoiceERP(utilityMean(agent.knowledge));
    var choice = sample(choiceERP);
    var outcome = (observe(agent.trueUtility, choice) === "good" ? true : false);
    var ownChoice = {choice : choice, rewardSignal : outcome};

    // Each time step get new data from others
    var otherChoices = getOtherChoices(agents);
    var updatedAgent = infer(agent, ownChoice, otherChoices);
    return timeStep(updatedAgent, remainingIterations - 1);
  }
};

var results = timeStep(alice, 100);
vizPrint(Enumerate(function() { return sample(results.knowledge).utility}));
print(Enumerate(function() { return sample(results.knowledge).groupAssignments}));
~~~~

// Discrete version

///fold:
var normalize = function(xs) {
  var Z = sum(xs);
  return map(function(x) {
    return x / Z;
  }, xs);
};

var utilityMean = function(utilityERP) {
  var options = _.keys(sample(utilityERP));
  var means = map(function(key) {
    return expectation(utilityERP, function(v){
      return [v[key]];
    });
  }, options);
  return _.object(options, means);
};

var normalizeVals = function(agentVals){
  var arr = _.values(agentVals);
  return normalize(arr);
};
///

// All agents in the population have utilities drawn from a shared prior
var truePrior = function() {
  return {
    "Taco Town" : gaussian(.5, .05),
    "Burger Barn" : gaussian(.25, .05),
    "Stirfry Shack" : gaussian(.75, .05)
  };
};

// Create population of agents
var initializeAgents = function(numAgents) {
  return repeat(numAgents, function() {return {utility : truePrior()};});
};

// Sample noisy reward signal for r_j based on agent's utility
var observe = function(utility, restaurant) {
  return flip(utility[restaurant]) ? "good" : "bad";
};

// Each agent soft-maxes utility
var makeChoiceERP = function(utility) {
  var ps = normalizeVals(utility);
  var vs = _.keys(utility);
  return categoricalERP(ps, vs);
};

// Sample a choice for all agents in the population
var getOtherChoices = function(agents) {
  return map(function(a){
    var choiceERP = makeChoiceERP(a.utility);
    return sample(choiceERP);
  }, agents);
};

// Alice updates her beliefs by conditioning on her reward signal and others
var inferUtility = function(agent, ownChoice, otherChoices) {
  return {
    trueUtility : agent.trueUtility,
    utilityERP : Enumerate(function() {

      // Sample a possible utility function
      var utility = sample(agent.utilityERP);
      var expectedChoiceERP = makeChoiceERP(utility);

      // Take true reward signal into account
      factor(bernoulliERP.score([utility[ownChoice.choice]],
                                ownChoice.rewardSignal));

      // Try to maximize log-likelihood of others' choices
      var otherLikelihoods = map(function(otherChoice) {
        return expectedChoiceERP.score([], otherChoice);
      }, otherChoices);
      factor(sum(otherLikelihoods));

      return utility;
    })
  };
};

// How many agents do we want in our population?
var numAgents = 10;

// Fixed population of agents, with their choices as data
var agents = initializeAgents(numAgents);

// Alice has some prior on her own utility, as well as a true utility
var alice = {
  utilityERP : Enumerate(function() {
    return {
      "Taco Town"     : uniformDraw([.1, .25, .5, .75, .9]),
      "Burger Barn"   : uniformDraw([.1, .25, .5, .75, .9]),
      "Stirfry Shack" : uniformDraw([.1, .25, .5, .75, .9])
    };
  }),
  trueUtility : truePrior()
};

// Advance simulation by one timeStep:
// 1) Alice makes a choice based on current beliefs, observes reward signal
// 2) Alice observes other agents' choices
var timeStep = function(agent, remainingIterations){
  if (remainingIterations == 0) {
    return agent;
  } else {
    // Use current beliefs about utility to make a choice
    var choiceERP = makeChoiceERP(utilityMean(agent.utilityERP));
    var choice = sample(choiceERP);
    var outcome = (observe(agent.trueUtility, choice) === "good" ? true : false);
    var ownChoice = {choice : choice, rewardSignal : outcome};

    // Each time step get new data from others
    var otherChoices = getOtherChoices(agents);
    var updatedAgent = inferUtility(agent, ownChoice, otherChoices);
    return timeStep(updatedAgent, remainingIterations - 1);
  }
};

var results = timeStep(alice, 100);
vizPrint(results.utilityERP);


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

var getOtherAgents = function(agentNames, name) {
  return _.without(agentNames, name);
}; 

///

var restaurants = ["Taco Town", "Burger Barn", "Stirfry Shack"];
var goodnessWeights = [.1,.2,.3,.4,.5,.6,.7,.8,.9];

var restaurantPrior = map(function(restaurant) {
  return Enumerate(function(v){
    return uniformDraw(goodnessWeights);
  });
}, restaurants);

// initialize with uniform prior and neutral relationships ([.5,.5])
var createAgent = function(name, agentNames) {
  var otherAgents = getOtherAgents(name, agentNames);
  var otherWeights = repeat(otherAgents.length, function(){return .5;});
  var relationships = _.object(otherAgents, otherWeights);
  return {values: _.object(restaurants, restaurantPrior), 
          relationships: relationships};
};

var initializeAgents = function(agentNames) {
  var agentProperties = map(function(agent) {
    return createAgent(agent, agentNames);
  }, agentNames);
  return _.object(agentNames, agentProperties);
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

// Adjust relationships based on shared experience
var updateRelationships = function(agentProps, info){
  return info.relationships;
};

// Sample outcomes of other agents propotionally to strength of 
// relationship
var sampleOtherOutcomes = function(restaurant, info) {
  var relevantOthers = filter(function(key) {
    return (info.socialObs[key].choice === restaurant ?
            flip(info.relationships[key]) :
            false);
  }, _.keys(info.socialObs));

  return map(function(person){
    return info.socialObs[person].outcome;
  }, relevantOthers);
  
};

// Adjust values proportionally to utility of each restaurant
var updateValues = function(agentProps, info){
  return mapObject(function(restaurant, value) {
    return Enumerate(function() {
      var possibleWeight = sample(value);    
      var otherOutcomes = sampleOtherOutcomes(restaurant, info);      
      var trueOutcomeSet = (restaurant === info.ownObs.choice ? 
                          append(otherOutcomes, info.ownObs.outcome) : 
                          otherOutcomes);
      var simulatedOutcomeSet = repeat(trueOutcomeSet.length, function(){
        return flip(possibleWeight) ? "good" : "bad";
      });
      condition(_.isEqual(trueOutcomeSet, simulatedOutcomeSet));
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

var agents = initializeAgents(["Alice", "Bob", "Carol"]);
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
var getOtherAgents = function(agentList, name) {
  return _.without(agentList, name);
};

var indWeight = 2;
var initSocWeight = .5;

// initialize with uniform prior and neutral relationships ([1,1])
var createAgent = function(name) {
  var otherAgents = getOtherAgents(agentList, name);
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

--->
