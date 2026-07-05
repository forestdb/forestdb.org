---
layout: model
title: Bayes Net Structure Learning
model-status: code
model-category: Graphical Models and Causality
model-tags: concepts, bayesnet, structure learning
model-language: webppl
model-language-version: v0.9.15
---

Bayesian structure learning treats the graph of a Bayes net as a latent variable: we place a prior on directed acyclic graphs, score each graph by the marginal likelihood of observed data, and compute a posterior over graphs by Bayes' rule (Cooper & Herskovits 1992). This model implements the smallest interesting case, three binary variables.

The prior is uniform over the 25 DAGs on three nodes. We sample it by including each of the six possible directed edges with probability one half and conditioning on acyclicity.

Given a structure, each node has one Bernoulli parameter per assignment to its parents (one CPT row). We give each parameter a uniform prior and integrate it out analytically: a Bernoulli with a uniform prior on its weight that produced `n1` successes and `n0` failures has marginal likelihood `n1! n0! / (n1 + n0 + 1)!`. The marginal likelihood of the data given a structure is the product of these terms over nodes and parent assignments. Because the parameters are integrated out, the only remaining latent variable is the structure itself, and `Infer` can enumerate the posterior exactly.

The observed data are 200 joint observations of (A, B, C) with counts proportional to the joint distribution of the chain A→B→C, where P(A=true) = 0.5 and each child copies its parent with probability 0.9.

~~~~
var variables = ['A', 'B', 'C'];

// The six possible directed edges between distinct variables:
var possibleEdges = [['A','B'], ['B','A'], ['A','C'], ['C','A'], ['B','C'], ['C','B']];

// Observed data: counts for each joint assignment of (A, B, C). The counts
// are proportional to the joint distribution of the chain A -> B -> C with
// P(A=t) = 0.5, P(B=A) = 0.9, P(C=B) = 0.9, for 200 observations total.
var data = [
  {A: true,  B: true,  C: true,  n: 81},
  {A: true,  B: true,  C: false, n: 9},
  {A: true,  B: false, C: true,  n: 1},
  {A: true,  B: false, C: false, n: 9},
  {A: false, B: true,  C: true,  n: 9},
  {A: false, B: true,  C: false, n: 1},
  {A: false, B: false, C: true,  n: 9},
  {A: false, B: false, C: false, n: 81}
];

// --- Structure prior: uniform over directed acyclic graphs ---

var hasIncomingEdge = function(node, edges) {
  return any(function(e) { return e[1] === node; }, edges);
};

// A graph is acyclic iff we can repeatedly peel off a node with no incoming edges.
var isAcyclic = function(edges, nodes) {
  if (nodes.length === 0) { return true; }
  var roots = filter(function(node) { return !hasIncomingEdge(node, edges); }, nodes);
  if (roots.length === 0) { return false; }
  var root = roots[0];
  return isAcyclic(filter(function(e) { return e[0] !== root; }, edges),
                   filter(function(node) { return node !== root; }, nodes));
};

// Include each possible edge with probability 0.5, then condition on
// acyclicity; this puts a uniform prior on the 25 DAGs over three nodes.
var structurePrior = function() {
  var edges = filter(function(e) { return flip(0.5); }, possibleEdges);
  condition(isAcyclic(edges, variables));
  return edges;
};

// --- Marginal likelihood of the data given a structure ---

var parentsOf = function(node, edges) {
  return map(first, filter(function(e) { return e[1] === node; }, edges));
};

// All assignments of truth values to a list of variables.
var assignments = function(vars) {
  if (vars.length === 0) { return [{}]; }
  var rest = assignments(vars.slice(1));
  return map(function(a) { return extend(a, _.fromPairs([[vars[0], true]])); }, rest)
    .concat(map(function(a) { return extend(a, _.fromPairs([[vars[0], false]])); }, rest));
};

var matches = function(row, assignment) {
  return all(function(v) { return row[v] === assignment[v]; }, _.keys(assignment));
};

var countWhere = function(pred) {
  return sum(map(function(row) { return pred(row) ? row.n : 0; }, data));
};

var logFactorial = function(n) {
  return n === 0 ? 0 : Math.log(n) + logFactorial(n - 1);
};

// A Bernoulli with a uniform prior on its weight that produced n1 successes
// and n0 failures has marginal likelihood n1! n0! / (n1 + n0 + 1)!.
var logMarginalBernoulli = function(n1, n0) {
  return logFactorial(n1) + logFactorial(n0) - logFactorial(n1 + n0 + 1);
};

// Marginal likelihood of a node's data given its parents: a product of
// Beta-Bernoulli terms, one per assignment to the parents.
var familyScore = function(node, parents) {
  return sum(map(function(pa) {
    var n1 = countWhere(function(row) { return matches(row, pa) && row[node]; });
    var n0 = countWhere(function(row) { return matches(row, pa) && !row[node]; });
    return logMarginalBernoulli(n1, n0);
  }, assignments(parents)));
};

var structureScore = function(edges) {
  return sum(map(function(node) { return familyScore(node, parentsOf(node, edges)); },
                 variables));
};

// --- Posterior over structures ---

var model = function() {
  var edges = structurePrior();
  factor(structureScore(edges));
  return edges;
};

var showStructure = function(edges) {
  return edges.length === 0 ? '(no edges)' :
    sort(map(function(e) { return e[0] + '→' + e[1]; }, edges)).join('  ');
};

// Posterior probability that each directed edge is present:
map(function(e) {
  var marginal = Infer({method: 'enumerate'}, function() {
    return any(function(d) { return d[0] === e[0] && d[1] === e[1]; }, model());
  });
  display(e[0] + '→' + e[1] + ': ' + Math.exp(marginal.score(true)).toFixed(3));
}, possibleEdges);

var posterior = Infer({method: 'enumerate'}, function() {
  return showStructure(model());
});

viz.table(posterior);
~~~~

The posterior concentrates on three structures with about 0.30 probability each: the data-generating chain A→B→C, the reversed chain C→B→A, and the common cause A←B→C. These are exactly the Markov equivalence class of the chain—they share its skeleton and have no collider, so they imply the same conditional independencies and observational data cannot distinguish them. The collider A→B←C has the same skeleton but implies that A and C are marginally independent, which the data contradict, so it receives almost no probability. Denser graphs also fit the data, but each additional edge multiplies in more integrated-out parameters and so pays a Bayesian Occam penalty.

One caveat: the simple uniform parameter prior used here does not guarantee that Markov-equivalent structures receive identical scores. The three top structures tie exactly only because this data set is symmetric; the six complete DAGs, which are all Markov-equivalent to one another, receive visibly different posteriors (about 0.019 vs. 0.007). Score equivalence requires BDeu-style parameter priors (Heckerman, Geiger & Chickering 1995).

The two-variable special case of this model—deciding whether a single cause-effect edge is present—is the [causal support](/models/causal-support.html) model of Ref:Griffiths2005.

References:

- Cite:Griffiths2005
- Cooper, G. F., & Herskovits, E. (1992). A Bayesian method for the induction of probabilistic networks from data. *Machine Learning*, 9, 309-347.
- Heckerman, D., Geiger, D., & Chickering, D. M. (1995). Learning Bayesian networks: The combination of knowledge and statistical data. *Machine Learning*, 20, 197-243.
