---
layout: model
title: Interventional Bayes Net Structure Learning
model-status: code
model-category: Graphical Models and Causality
model-tags: concepts, bayesnet, causality, intervention, structure learning
model-language: webppl
model-language-version: v0.9.15
---

Observational data alone cannot always tell apart directed graphs that imply the same conditional independencies (their Markov equivalence class). This model first reproduces the classic three-way tie for a three-variable chain from observational data alone, then adds a small interventional data set, do(B), and breaks that tie by recovering the correct orientation.

The trick on the interventional side is what likelihood term to leave out, and what to pool. When B's value is forced by intervention rather than drawn from its usual parents, B stopped following its Bayes net mechanism for those units, so its family score should not be counted, and it is scored from the observational rows only. A and C, by contrast, keep running their normal (uncut) mechanisms throughout, so their sufficient statistics pool across both data sets into a single Beta-Bernoulli update per family, exactly as if the two data sets were one longer run of the same generative process. Comparing which of the three tied structures fits that pooled pattern best is what breaks the tie.

~~~~
var variables = ['A', 'B', 'C'];

// The six possible directed edges between distinct variables:
var possibleEdges = [['A','B'], ['B','A'], ['A','C'], ['C','A'], ['B','C'], ['C','B']];

// Observational data: 200 joint (A, B, C) observations from the chain
// A -> B -> C, with P(A=t) = 0.5, P(B=A) = 0.9, P(C=B) = 0.9 (same data as
// the companion bn-structure model).
var obsData = [
  {A: true,  B: true,  C: true,  n: 81},
  {A: true,  B: true,  C: false, n: 9},
  {A: true,  B: false, C: true,  n: 1},
  {A: true,  B: false, C: false, n: 9},
  {A: false, B: true,  C: true,  n: 9},
  {A: false, B: true,  C: false, n: 1},
  {A: false, B: false, C: true,  n: 9},
  {A: false, B: false, C: false, n: 81}
];

// Interventional data: 200 units under do(B = b), split evenly between
// do(B = true) and do(B = false). Intervening on B cuts its incoming edge,
// so A keeps its own marginal (P(A=t) = 0.5) regardless of the forced value
// of B, while C keeps tracking B through its untouched outgoing mechanism
// (P(C=B) = 0.9).
var intvData = [
  {B: true,  A: true,  C: true,  n: 45},
  {B: true,  A: true,  C: false, n: 5},
  {B: true,  A: false, C: true,  n: 45},
  {B: true,  A: false, C: false, n: 5},
  {B: false, A: true,  C: true,  n: 5},
  {B: false, A: true,  C: false, n: 45},
  {B: false, A: false, C: true,  n: 5},
  {B: false, A: false, C: false, n: 45}
];

// --- Structure prior: uniform over directed acyclic graphs ---

var hasIncomingEdge = function(node, edges) {
  return any(function(e) { return e[1] === node; }, edges);
};

var isAcyclic = function(edges, nodes) {
  if (nodes.length === 0) { return true; }
  var roots = filter(function(node) { return !hasIncomingEdge(node, edges); }, nodes);
  if (roots.length === 0) { return false; }
  var root = roots[0];
  return isAcyclic(filter(function(e) { return e[0] !== root; }, edges),
                   filter(function(node) { return node !== root; }, nodes));
};

var structurePrior = function() {
  var edges = filter(function(e) { return flip(0.5); }, possibleEdges);
  condition(isAcyclic(edges, variables));
  return edges;
};

// --- Marginal likelihood machinery (shared by both data sets) ---

var parentsOf = function(node, edges) {
  return map(first, filter(function(e) { return e[1] === node; }, edges));
};

var assignments = function(vars) {
  if (vars.length === 0) { return [{}]; }
  var rest = assignments(vars.slice(1));
  return map(function(a) { return extend(a, _.fromPairs([[vars[0], true]])); }, rest)
    .concat(map(function(a) { return extend(a, _.fromPairs([[vars[0], false]])); }, rest));
};

var matches = function(row, assignment) {
  return all(function(v) { return row[v] === assignment[v]; }, _.keys(assignment));
};

var countWhere = function(data, pred) {
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

var familyScore = function(data, node, parents) {
  return sum(map(function(pa) {
    var n1 = countWhere(data, function(row) { return matches(row, pa) && row[node]; });
    var n0 = countWhere(data, function(row) { return matches(row, pa) && !row[node]; });
    return logMarginalBernoulli(n1, n0);
  }, assignments(parents)));
};

var observationalScore = function(edges) {
  return sum(map(function(node) { return familyScore(obsData, node, parentsOf(node, edges)); },
                 variables));
};

// A and C keep the same mechanism in both data sets, so their sufficient
// statistics pool across observational and interventional rows into one
// Beta-Bernoulli update per family. B's mechanism only ran in the
// observational rows, so its family score is computed from obsData alone;
// pooling it in would treat the forced interventional values of B as if
// they had come from B's own parents.
var pooledData = obsData.concat(intvData);

var combinedScore = function(edges) {
  return familyScore(obsData, 'B', parentsOf('B', edges)) +
         familyScore(pooledData, 'A', parentsOf('A', edges)) +
         familyScore(pooledData, 'C', parentsOf('C', edges));
};

var showStructure = function(edges) {
  return edges.length === 0 ? '(no edges)' :
    sort(map(function(e) { return e[0] + '→' + e[1]; }, edges)).join('  ');
};

var topStructures = function(dist, k) {
  var withProbs = map(function(s) { return {structure: s, prob: Math.exp(dist.score(s))}; },
                       dist.support());
  return sort(withProbs, function(a, b) { return a.prob > b.prob; }).slice(0, k);
};

// Posterior from observational data alone:
var obsPosterior = Infer({method: 'enumerate'}, function() {
  var edges = structurePrior();
  factor(observationalScore(edges));
  return showStructure(edges);
});

// Posterior from observational data plus the interventional data set:
var combinedPosterior = Infer({method: 'enumerate'}, function() {
  var edges = structurePrior();
  factor(combinedScore(edges));
  return showStructure(edges);
});

display('observation only:');
map(function(r) { display('  ' + r.structure + ': ' + r.prob.toFixed(3)); },
    topStructures(obsPosterior, 3));

display('observation + intervention on B:');
map(function(r) { display('  ' + r.structure + ': ' + r.prob.toFixed(3)); },
    topStructures(combinedPosterior, 3));
~~~~

Observational data alone puts about 0.30 probability on each of three structures: the data-generating chain A→B→C, the reversed chain C→B→A (via B→A, C→B), and the common cause B→A, B→C. These share a skeleton and no collider, so they are Markov-equivalent and no amount of observational data can separate them, exactly as in the [bn-structure](/models/bn-structure.html) model.

Adding the interventional data set changes the picture completely. The correct chain A→B→C rises to about 0.99, and the other two members of the former tie all but vanish: the common cause drops to essentially zero, and the reversed chain does not even make the top three. The reversed chain loses because it needs C to have no parents, but the pooled data show C tracking the forced value of B almost perfectly, which a parentless model cannot explain. The common cause loses because it correctly gives C a parent in B, but it also gives A an unnecessary parent in B that the pooled data do not support (A stays at its own marginal no matter what B is set to), and with twice as much data now bearing on that one shared parameter, the penalty for that extra, unhelpful edge is sharper than it was with observational data alone. Only the true chain gets every term right at once: A parentless, C a child of B, and no wasted parameters.

This is a minimal illustration of a general point about causal discovery (Pearl 2000): observational independence tests can only recover a graph up to Markov equivalence, but a well-chosen intervention, scored by omitting the intervened node's own mechanism, can recover orientation that observation alone leaves ambiguous.

References:

- Cooper, G. F., & Herskovits, E. (1992). A Bayesian method for the induction of probabilistic networks from data. *Machine Learning*, 9, 309-347.
- Pearl, J. (2000). *Causality: Models, Reasoning, and Inference*. Cambridge University Press.
