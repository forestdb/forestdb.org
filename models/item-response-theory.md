---
layout: model
title: Item Response Theory (Rasch Model)
model-status: code
model-category: Probability and Bayesian Data Analysis
model-tags: psychometrics, item response theory, identifiability
model-language: webppl
model-language-version: v0.9.15
---

The Rasch model is the simplest item response theory model: each person has a latent ability, each item has a latent difficulty, and the probability that a person answers an item correctly is the logistic function of the gap between the two, `sigmoid(ability - difficulty)` (Rasch, 1960). Fitting it to a matrix of right/wrong responses means jointly inferring every person's ability and every item's difficulty from nothing but that matrix.

The model as stated has one more degree of freedom than the data can pin down: shifting every ability and every difficulty by the same constant leaves every `ability - difficulty` gap, and so every predicted probability, unchanged. We fix this by centering the sampled abilities to mean zero before using them, which removes exactly the one dimension the data cannot identify. Anchoring a single item's difficulty at zero instead is an equivalent convention; centering spreads the constraint over all persons rather than singling out one item as the reference (Bürkner, 2021).

~~~~
var nPersons = 8;
var nItems = 8;

var sigmoid = function(x) { return 1 / (1 + Math.exp(-x)); };
var listMean = function(xs) { return sum(xs) / xs.length; };

// Fixed observed data: an 8-person by 8-item matrix of correct (true) and
// incorrect (false) responses, simulated once offline from abilities
// -2.8..2.8 and difficulties -2.4..2.4 (both evenly spaced). Hardcoded here
// so inference always targets the same matrix instead of a fresh noisy
// draw on every run.
var data = [
  [false, true,  false, false, false, false, false, false],
  [true,  false, true,  false, false, false, false, false],
  [true,  true,  false, false, false, false, false, false],
  [true,  true,  false, false, false, false, false, true],
  [true,  true,  true,  true,  false, false, false, false],
  [true,  true,  true,  true,  true,  true,  false, false],
  [true,  true,  false, true,  true,  true,  true,  false],
  [true,  true,  true,  true,  true,  true,  true,  false]
];

var model = function() {
  var rawTheta = repeat(nPersons, function() { return gaussian(0, 1); });
  var theta = map(function(t) { return t - listMean(rawTheta); }, rawTheta); // mean-zero abilities
  var difficulty = repeat(nItems, function() { return gaussian(0, 1); });

  mapIndexed(function(i, row) {
    mapIndexed(function(j, correct) {
      var p = sigmoid(theta[i] - difficulty[j]);
      factor(Bernoulli({ p: p }).score(correct));
    }, row);
  }, data);

  return { theta: theta, difficulty: difficulty };
};

var posterior = Infer({ method: 'MCMC', samples: 8000, burn: 2000 }, model);

var posteriorMean = function(field, n) {
  return map(function(k) {
    return expectation(posterior, function(v) { return v[field][k]; });
  }, _.range(n));
};

var fmt = function(xs) { return map(function(x) { return x.toFixed(2); }, xs).join(', '); };

display('ability posterior means:    ' + fmt(posteriorMean('theta', nPersons)));
display('difficulty posterior means: ' + fmt(posteriorMean('difficulty', nItems)));
viz.marginals(posterior);
~~~~

The posterior separates broad low-, middle-, and high-ability groups and likewise distinguishes easier from harder items, with abilities centered at zero by construction. It cannot recover a complete ordering from this tiny matrix: persons with the same total score, such as persons 1 and 2 or 5 and 6, are likelihood-symmetric in the Rasch model, as are items with equal totals. Their posterior means may tie or swap across runs, while the larger low-to-high pattern remains stable.

References:

- Rasch, G. (1960). *Probabilistic Models for Some Intelligence and Attainment Tests*. Danish Institute for Educational Research.
- Bürkner, P.-C. (2021). Bayesian item response modeling in R with brms and Stan. *Journal of Statistical Software*, 100(5), 1-54.
