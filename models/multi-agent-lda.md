---
layout: model
title: Multi-Agent LDA
model-language: webppl
---

~~~~
// Parameters

var actions = ['a', 'b', 'c', 'd'];

var groups = {
  'group1': null,
  'group2': null
};

var agents = {
  'agent1': 'a b a b a d a b a b a c'.split(' '),
  'agent2': 'c a c d c d c d c b c d'.split(' '),
  'agent3': 'a b a b a b a b a b a b'.split(' '),
  'agent4': 'c d c d c d c d c d c d'.split(' '),
  'agent5': 'a b a b a b a b a b a c'.split(' ')
};


// Constants and helper functions

var ones = function(n) {
  return repeat(n, function() {return 1.0;});
}

// Model

var makeActionDist = function() {
  return dirichlet(ones(actions.length));
};

var makeGroupDist = function() {
  return dirichlet(ones(_.size(groups)));
};

var discreteFactor = function(vs, ps, v) {
  var i = indexOf(v, vs);
  factor(Math.log(ps[i]));
}

var model = function() {

  var actionDistForGroup = mapObject(makeActionDist, groups);
  var groupDistForAgent = mapObject(makeGroupDist, agents);
  var makeGroupForAction = function(agentName, word) {
    var i = discrete(groupDistForAgent[agentName]);
    return _.keys(groups)[i];
  };
  var makeActionGroups = function(agentName, actions) {
    return map(function(action) {return makeGroupForAction(agentName, action);},
	       actions);
  };
  var groupsForAgent = mapObject(makeActionGroups, agents);

  mapObject(
    function(agentName, agentActions) {
      map2(
	function(group, action) {
	  discreteFactor(actions, actionDistForGroup[group], action);
	},
	groupsForAgent[agentName],
	agentActions);
    },
    agents);
  // console.log(actionDistForGroup);

  return groupDistForAgent.agent1
};

vizPrint(MH(model, 10000))
~~~~
