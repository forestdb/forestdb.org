---
layout: model
title: Counterfactual Oversight
model-language: webppl
model-language-version: pre-v0.7
---

Follow-up to:

- [Scoring Rules](/models/scoring-rules.html)
- [The Principal-Agent Problem for Delegation-as-Prediction](/models/principal-agent.html)

Warm-up: Suppose there are two agents, and we (as one of the agents) expect the other agent to tell (what we think is) the truth (or use any other distribution that isn't the one this agent would prefer), and suppose the user supervises whenever we don't report the same distribution as the other guy. Then we'll be honest, and the user won't supervise. The counterfactual threat of supervision moved us to tell the truth.

~~~~
///fold:
var Infer = function(thunk){
  return Enumerate(thunk);
}

var Expectation = function(thunk){
  return expectation(Infer(thunk));
}

var makeCategorical = function(vs, ps){
  return window.makeCategoricalERP(ps, vs);
};

var prob = function(erp, val) {
  return Math.exp(erp.score([], val))
}

var squared = function(x) {
  return Math.pow(x, 2)
}

var distPrior = Enumerate(function(){
  var potentialPs = [
    [.5, .3, .2],
    [.3, .5, .2],
    [.2, .3, .5],
    [0.005, .99, 0.005]
  ];
  var ps = uniformDraw(potentialPs);
  return makeCategorical(['A', 'B', 'C'], ps);
});

var alpha = 100;

var logScore = function(dist, event) {
  return Math.log(prob(dist, event));
}

var distributionsAreEqual = function(dist1, dist2) {
  return JSON.stringify(dist1) == JSON.stringify(dist2);
}
///

var getReportedDistForParams = function(params) {

  var predictionScore = logScore;  
  
  var eventScore = function(event) {
    return event == 'A' ? params.eventRewardA : 0;
  }

  var agent = function(){
    return Infer(function(){      
      var beliefDist = makeCategorical(['A', 'B', 'C'], [.3, .5, .2]);
      var reportDist = sample(distPrior);
      var expectedScore = Expectation(function(){        
        var othersReportDist = beliefDist;
        var isSupervised = !distributionsAreEqual(reportDist, othersReportDist);
        var event = sample(isSupervised ? beliefDist : reportDist);
        return predictionScore(reportDist, event) + eventScore(event);
      });
      factor(alpha * expectedScore);
      return reportDist;
    });
  }

  return agent().MAP().val;

};

var exploreRewardParams = function(){  
  var rewards = [0, 0.0001, 0.001, 0.01, 0.1, 1, 10]
  map(
    function(reward){
      var reportDist = getReportedDistForParams({
        eventRewardA: reward
      });
      var cheated = reportDist.score([], 'A') != Math.log(.3);
      print([reward, cheated ? 'cheated' : 'honest']);
    },
    rewards);
};


print('Reported distribution with supervision conditional on disagreement:');
print(getReportedDistForParams({
  eventRewardA: 5
}))

print('\nAs reward for action "a" increases, probability of cheating does not increase:');
exploreRewardParams()
~~~~

Instead of equality, we can investigate in proportion to disagreement.  How sensitive we are to disagreement needs to be in balance with how tempting it is for the agent to cheat.

~~~~
///fold:
var Infer = function(thunk){
  return Enumerate(thunk);
}

var Expectation = function(thunk){
  return expectation(Infer(thunk));
}

var makeCategorical = function(vs, ps){
  return window.makeCategoricalERP(ps, vs);
};

var prob = function(erp, val) {
  return Math.exp(erp.score([], val))
}

var squared = function(x) {
  return Math.pow(x, 2)
}

var distPrior = Enumerate(function(){
  var potentialPs = [
    [.5, .3, .2],
    [.3, .5, .2],
    [.2, .3, .5],
    [0.005, .99, 0.005]
  ];
  var ps = uniformDraw(potentialPs);
  return makeCategorical(['A', 'B', 'C'], ps);
});

var alpha = 100;

var logScore = function(dist, event) {
  return Math.log(prob(dist, event));
}

var distributionsAreEqual = function(dist1, dist2) {
  return JSON.stringify(dist1) == JSON.stringify(dist2);
}

var distance = function(dist1, dist2, options) {
  // assuming that support is the same for now
  var maxPair = maxWith(
    function(value){
      return Math.abs(Math.exp(dist1.score([], value)) - 
                      Math.exp(dist2.score([], value)));
    },
    dist1.support()
  );
  var maxDistance = maxPair[1];
  return Math.min(maxDistance * options.multiplier, 1);
};
///

var getReportedDistForParams = function(params) {

  var predictionScore = logScore;  
  
  var eventScore = function(event) {
    return event == 'A' ? params.eventRewardA : 0;
  }

  var agent = function(){
    return Infer(function(){      
      var beliefDist = makeCategorical(['A', 'B', 'C'], [.3, .5, .2]);
      var reportDist = sample(distPrior);
      var expectedScore = Expectation(function(){        
        var othersReportDist = beliefDist;
        var disagreement = distance(reportDist, othersReportDist, {multiplier: 5});
        var isSupervised = flip(disagreement);
        var event = sample(isSupervised ? beliefDist : reportDist);
        return predictionScore(reportDist, event) + eventScore(event);
      });
      factor(alpha * expectedScore);
      return reportDist;
    });
  }

  return agent().MAP().val;

};

var exploreRewardParams = function(){  
  var rewards = [0, 0.0001, 0.001, 0.01, 0.1, 1, 10]
  map(
    function(reward){
      var reportDist = getReportedDistForParams({
        eventRewardA: reward
      });
      var cheated = reportDist.score([], 'A') != Math.log(.3);
      print([reward, cheated ? 'cheated' : 'honest']);
    },
    rewards);
};


print('Reported distribution with supervision conditional on disagreement:');
print(getReportedDistForParams({
  eventRewardA: 5
}))

print('\nAs reward for action "a" increases:');
exploreRewardParams()
~~~~

What if all agents are noisy and biased -- that is, they would prefer some option that is different from what they predict the principal would prefer? In that case (from the perspective of one of the agents) sometimes the other agent(s) will return the distribution that we would prefer. If that happens frequently enough, and if utility is high enough, then we'll cheat (and report our preferred distribution) because those occasions where the others also report it are worth it.

~~~~
///fold:
var Infer = function(thunk){
  return Enumerate(thunk);
}

var Expectation = function(thunk){
  return expectation(Infer(thunk));
}

var makeCategorical = function(vs, ps){
  return window.makeCategoricalERP(ps, vs);
};

var prob = function(erp, val) {
  return Math.exp(erp.score([], val))
}

var squared = function(x) {
  return Math.pow(x, 2)
}

var distPrior = Enumerate(function(){
  var potentialPs = [
    [.5, .3, .2],
    [.3, .5, .2],
    [.2, .3, .5],
    [0.005, .99, 0.005]
  ];
  var ps = uniformDraw(potentialPs);
  return makeCategorical(['A', 'B', 'C'], ps);
});

var alpha = 100;

var logScore = function(dist, event) {
  return Math.log(prob(dist, event));
}

var distributionsAreEqual = function(dist1, dist2) {
  return JSON.stringify(dist1) == JSON.stringify(dist2);
}

var distance = function(dist1, dist2, options) {
  // assuming that support is the same for now
  var maxPair = maxWith(
    function(value){
      return Math.abs(Math.exp(dist1.score([], value)) - 
                      Math.exp(dist2.score([], value)));
    },
    dist1.support()
  );
  var maxDistance = maxPair[1];
  return Math.min(maxDistance * options.multiplier, 1);
};
///

var getReportedDistForParams = function(params) {

  var predictionScore = logScore;  
  
  var agent = function(otherAgent, eventScore){
    return Infer(function(){      
      var beliefDist = makeCategorical(['A', 'B', 'C'], [.3, .5, .2]);
      var reportDist = sample(distPrior);
      var expectedScore = Expectation(function(){        
        var othersReportDist = sample(otherAgent());
        var disagreement = distance(reportDist, othersReportDist, {multiplier: 5});
        var isSupervised = flip(disagreement);
        var event = sample(isSupervised ? beliefDist : reportDist);
        return predictionScore(reportDist, event) + eventScore(event);
      });
      factor(alpha * expectedScore);
      return reportDist;
    });
  }

  var basicAgent = function(){
    return Infer(function(){
      // to make our life harder, let's assume that this agent
      // colludes with agentPrefersA
      return makeCategorical(['A', 'B', 'C'], [.5, .3, .2]);
    });
  }

  var eventScorePreferA = function(event) {
    return event == 'A' ? params.eventReward : 0;
  }

  var eventScorePreferC = function(event) {
    return event == 'C' ? params.eventReward : 0;
  }  

  var agentPrefersA = function(options){
    if (options.depth == 0) {
      return basicAgent();
    } else {
      var otherAgent = function(){return agentPrefersC({depth: options.depth - 1})};
      return agent(otherAgent, eventScorePreferA);
    }
  };

  var agentPrefersC = function(options){
    if (options.depth == 0) {
      return basicAgent();
    } else {
      var otherAgent = function(){return agentPrefersA({depth: options.depth - 1})}
      return agent(otherAgent, eventScorePreferC);
    }
  };

  return agentPrefersA({ depth: 3 }).MAP().val;

};

var exploreRewardParams = function(){  
  var rewards = [0, 0.0001, 0.001, 0.01, 0.1, 1, 10]
  map(
    function(reward){
      var reportDist = getReportedDistForParams({
        eventReward: reward
      });
      var cheated = reportDist.score([], 'A') != Math.log(.3);
      print([reward, cheated ? 'cheated' : 'honest']);
    },
    rewards);
};


print('Reported distribution with supervision conditional on disagreement:');
print(getReportedDistForParams({
  eventReward: 2
}))

print('\nAs reward for action "a" increases, probability of cheating increases:');
exploreRewardParams()
~~~~
