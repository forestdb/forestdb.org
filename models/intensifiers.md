---
layout: model
title: Intensifiers
model-status: code
model-category: Reasoning about Reasoning
model-tags: linguistics, pragmatics
model-language: webppl
model-language-version: pre-v0.7
---

<script src="http://web.stanford.edu/~erindb/webppl-viz/webppl.min.js"></script>  
<link rel="stylesheet" href="http://web.stanford.edu/~erindb/webppl-viz/viz.css">

A model for intensifying degree adverbs based on Ref:Lassiter2013adj, as presented in Ref:Bennett2015int.

~~~
///fold:
var discrete_gaussian = function() {
  var values = [
    -2.0, -1.9, -1.8, -1.7, -1.6, -1.5, -1.4, -1.3, -1.2, -1.1,
    -1.0, -0.9, -0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1,
    0.0,
    0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0,
    1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0
  ];
  var probabilities = map(
    function(x) {return Math.exp(gaussianERP.score([0, 1], x));},
    values
  );
  return function() { return values[discrete(probabilities)]; };
}
var discrete_uniform = function() {
  var values = [
    -2.0, -1.9, -1.8, -1.7, -1.6, -1.5, -1.4, -1.3, -1.2, -1.1,
    -1.0, -0.9, -0.8, -0.7, -0.6, -0.5, -0.4, -0.3, -0.2, -0.1,
    0.0,
    0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0,
    1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0
  ];
  return function() { return uniformDraw(values); };
}
///
var x_prior = discrete_gaussian();
var theta_prior = discrete_uniform();

var alpha = 1; // rationality parameter
var n_particles = 500;

var utterances = [
  "",
  "adjective",
  "intensifier1+adj",
  "intensifier2+adj",
  "intensifier3+adj"
];
var cost = {
  "": 0,
  "adjective": 1,
  "intensifier1+adj": 2,
  "intensifier2+adj": 3,
  "intensifier3+adj": 4
};
var utterance_prior = function() {
  return utterances[discrete(map(function(u) {return Math.exp(-cost[u]);}, utterances))];
};

var meaning = function(utterance, x, thetas) {
  if (utterance == "adjective") {
    return x >= thetas[0];
  } else if (utterance == "intensifier1+adj") {
    return x >= thetas[1];
  } else if (utterance == "intensifier2+adj") {
    return x >= thetas[2];
  } else if (utterance == "intensifier3+adj") {
    return x >= thetas[3];
  } else {
    return true;
  }
};

var literalERP = cache(function(utterance, thetas) {
  return Enumerate(function() {
    var x = x_prior();
    condition(meaning(utterance, x, thetas));
    return x;
  });
});

var speakerERP = cache(function(x, thetas) {
  return Enumerate(function() {
    var utterance = utterance_prior();
    factor( alpha * literalERP(utterance, thetas).score([], x) );
    return utterance;
  });
});

var listenerERP = cache(function(utterance) {
  return ParticleFilter(function() {
    var x = x_prior();
    var thetas = [
      theta_prior(),
      theta_prior(),
      theta_prior(),
      theta_prior()
    ];
    factor( alpha * speakerERP(x, thetas).score([], utterance) );
    return x;
  }, n_particles);
});

vizPrint({
  "prior": ParticleFilter(x_prior, n_particles),
  "adjective": listenerERP("adjective"),
  "intensifier1+adj": listenerERP("intensifier1+adj"),
  "intensifier2+adj": listenerERP("intensifier2+adj"),
  "intensifier3+adj": listenerERP("intensifier3+adj")
});

var expectations = {
  "prior": 0,
  "adjective": expectation(listenerERP("adjective")),
  "intensifier1+adj": expectation(listenerERP("intensifier1+adj")),
  "intensifier2+adj": expectation(listenerERP("intensifier2+adj")),
  "intensifier3+adj": expectation(listenerERP("intensifier3+adj"))
};

expectations;
~~~

References:

- Cite:Lassiter2013adj
- Cite:Bennett2015int
