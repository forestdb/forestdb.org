---
layout: model
title: The Principal-Agent Problem for Delegation-as-Prediction
model-language: webppl
---

Follow-up to:

- [Scoring Rules](/models/scoring-rules.html)

The agent gets reward based on which event happens. As long as the agent's prediction doesn't influence what event happens (full supervision), it will continue to make truthful predictions:

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
  return makeCategorical(['a', 'b', 'c'], ps);
});

var alpha = 100;

var logScore = function(dist, event) {
  return Math.log(prob(dist, event));
}
///

var getReportedDistForParams = function(params) {

  var predictionScore = logScore;  
  
  var eventScore = function(event) {
    return event == 'a' ? params.eventRewardA : 0;
  }

  var agent = function(){
    return Infer(function(){
      var beliefDist = makeCategorical(['a', 'b', 'c'], [.3, .5, .2]);
      var reportDist = sample(distPrior);
      var expectedScore = Expectation(function(){
        var event = sample(beliefDist);
        return predictionScore(reportDist, event) + eventScore(event);
      });
      factor(alpha * expectedScore);
      return reportDist;
    });
  }

  return agent().MAP().val;

};

print('Reported distribution with full supervision, side-utility:');
print(getReportedDistForParams({
  eventRewardA: 5
}))
~~~~

We move to partial (stochastic) supervision, and in the unsupervised case, it is the agent's prediction determines what happens. Now utilities skew predictions:

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
  return makeCategorical(['a', 'b', 'c'], ps);
});

var alpha = 100;

var logScore = function(dist, event) {
  return Math.log(prob(dist, event));
}
///

var getReportedDistForParams = function(params) {

  var predictionScore = logScore;  
  
  var eventScore = function(event) {
    return event == 'a' ? params.eventRewardA : 0;
  }

  var agent = function(){
    return Infer(function(){      
      var beliefDist = makeCategorical(['a', 'b', 'c'], [.3, .5, .2]);
      var reportDist = sample(distPrior);
      var expectedScore = Expectation(function(){
        var isSupervised = flip(params.probOfSupervision);
        if (isSupervised) {
          var event = sample(beliefDist);
          return predictionScore(reportDist, event) + eventScore(event);
        } else {
          var event = sample(reportDist); // enact what agent predicts will happen
          return eventScore(event);
        }
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
        eventRewardA: reward,
        probOfSupervision: .1
      });
      var cheated = reportDist.score([], 'a') != Math.log(.3);
      print([reward, cheated ? 'cheated' : 'honest']);
    },
    rewards);
};

var exploreSupervisionParams = function(){  
  var supervisionProbs = [0.01, 0.1, 1, 0.5, 1]
  map(
    function(probOfSupervision){
      var reportDist = getReportedDistForParams({
        eventRewardA: .1,
        probOfSupervision: probOfSupervision
      });
      var cheated = reportDist.score([], 'a') != Math.log(.3);
      print([probOfSupervision, cheated ? 'cheated' : 'honest']);
    },
    supervisionProbs);
};


print('Reported distribution with partial supervision, side-utility:');
print(getReportedDistForParams({
  eventRewardA: 5,
  probOfSupervision: .1
}))

print('\nAs reward for action "a" increases, probability of cheating increases:');
exploreRewardParams()

print('\nAs probability of supervision increases, probability of cheating decreases:');
exploreSupervisionParams()
~~~~
