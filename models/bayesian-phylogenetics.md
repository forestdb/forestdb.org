---
layout: model
title: Bayesian Phylogenetics on Four Taxa
model-status: code
model-category: Scientific and Physical Models
model-tags: phylogenetics, evolution, substitution model, model comparison
model-language: webppl
model-language-version: v0.9.15
---

Phylogenetics infers evolutionary trees from the traits (or DNA, or other characters) of present-day taxa: taxa that are more closely related should share more character states, because they diverged from a common ancestor more recently. With four taxa there are exactly three distinct unrooted binary tree topologies, one for each way of splitting the four leaves into two pairs of "cherries" joined by a single internal edge: `(A,B)|(C,D)`, `(A,C)|(B,D)`, and `(A,D)|(B,C)`. This model computes the exact posterior over those three topologies for a tiny four-taxon, eight-site binary alignment.

Each character evolves along the tree under the binary symmetric substitution model (also called the Cavender-Farris-Neyman model): two states, 0 and 1, that swap into each other at equal rate along every branch. Its transition probability has a closed form, `P(same state after branch length t) = 0.5 + 0.5 * exp(-2t)`, so no matrix exponential is needed. The likelihood of an alignment given a tree is computed with Felsenstein's pruning algorithm: recursively, each internal node's conditional likelihood (a 2-vector over its own unobserved state) is built from its children's conditional likelihoods and the branch lengths connecting them, and the two states are integrated out at the root using the model's uniform (0.5, 0.5) stationary distribution. Because this substitution model is reversible with that uniform stationary distribution, the likelihood does not depend on where the root is placed, so rooting arbitrarily at the internal edge (splitting it into two halves) computes the same likelihood as the true unrooted tree.

Branch lengths take one of three small discrete values, shared across all external branches and, separately, across the internal branch, which keeps the whole model, three topologies times nine branch-length combinations, small enough for `Infer`'s `enumerate` method to compute the exact posterior. The eight-site alignment was built by hand so that taxa A and B agree at all but one site, and taxa C and D agree at all but one site, the character pattern produced by short branches within each pair, i.e. by the generating topology `(A,B)|(C,D)`.

~~~~
// Four taxa, eight binary (0/1) character sites. A and B agree at every
// site but one; C and D agree at every site but one; A/B and C/D differ
// at most sites. The data were built by hand to look like they evolved
// on the tree ((A,B),(C,D)) with short branches within each pair.
var seqs = {
  A: [0, 0, 1, 0, 1, 1, 0, 1],
  B: [0, 0, 1, 0, 1, 1, 1, 1],
  C: [1, 1, 0, 1, 0, 0, 1, 0],
  D: [1, 1, 0, 1, 0, 0, 1, 1]
};
var nSites = seqs.A.length;

// The three unrooted topologies on four taxa, each written as the two
// "cherry" pairs joined by one internal edge.
var topologies = [
  {name: 'AB|CD', cherry1: ['A', 'B'], cherry2: ['C', 'D']},
  {name: 'AC|BD', cherry1: ['A', 'C'], cherry2: ['B', 'D']},
  {name: 'AD|BC', cherry1: ['A', 'D'], cherry2: ['B', 'C']}
];

// Small discrete branch-length support: a short and a long option for the
// external branches (within a cherry) and for the internal branch.
var branchGrid = [0.1, 0.3, 0.6];

// Binary symmetric (Cavender-Farris-Neyman) substitution model: states
// 0/1 swap at rate 1 in each direction, so P(same state after time t) is
// this simple closed form (no matrix exponential needed).
var transProb = function(from, to, t) {
  var same = 0.5 + 0.5 * Math.exp(-2 * t);
  var diff = 0.5 - 0.5 * Math.exp(-2 * t);
  return from === to ? same : diff;
};

// Felsenstein's pruning algorithm: a node's conditional likelihood is a
// vector over its own state, computed from its children's vectors.
var leafLikelihood = function(obs) {
  return [obs === 0 ? 1 : 0, obs === 1 ? 1 : 0];
};

var combineChild = function(childLikelihood, branchLength) {
  return function(parentState) {
    return transProb(parentState, 0, branchLength) * childLikelihood[0] +
           transProb(parentState, 1, branchLength) * childLikelihood[1];
  };
};

var internalLikelihood = function(L1, t1, L2, t2) {
  var f1 = combineChild(L1, t1);
  var f2 = combineChild(L2, t2);
  return [f1(0) * f2(0), f1(1) * f2(1)];
};

// Because the model is reversible with a uniform (0.5, 0.5) stationary
// distribution, the root can be placed anywhere without changing the
// likelihood, so we root at the internal edge and split it into two
// halves of length intLen / 2 on either side.
var siteLogLikelihood = function(topo, extLen, intLen, site) {
  var La = leafLikelihood(seqs[topo.cherry1[0]][site]);
  var Lb = leafLikelihood(seqs[topo.cherry1[1]][site]);
  var Lc = leafLikelihood(seqs[topo.cherry2[0]][site]);
  var Ld = leafLikelihood(seqs[topo.cherry2[1]][site]);
  var Lcherry1 = internalLikelihood(La, extLen, Lb, extLen);
  var Lcherry2 = internalLikelihood(Lc, extLen, Ld, extLen);
  var Lroot = internalLikelihood(Lcherry1, intLen / 2, Lcherry2, intLen / 2);
  return Math.log(0.5 * Lroot[0] + 0.5 * Lroot[1]);
};

var logLikelihood = function(topo, extLen, intLen) {
  return sum(map(function(site) { return siteLogLikelihood(topo, extLen, intLen, site); },
                  _.range(nSites)));
};

var model = function() {
  var topo = uniformDraw(topologies);
  var extLen = uniformDraw(branchGrid);
  var intLen = uniformDraw(branchGrid);
  factor(logLikelihood(topo, extLen, intLen));
  return topo.name;
};

var posterior = Infer({method: 'enumerate'}, model);

viz.table(posterior);

var ranked = sort(posterior.support(), function(a, b) {
  return Math.exp(posterior.score(a)) > Math.exp(posterior.score(b));
});

display('topologies ranked by posterior probability: ' + ranked.join(', '));
map(function(name) {
  display(name + ': ' + Math.exp(posterior.score(name)).toFixed(4));
}, ranked);
~~~~

The posterior ranks the generating topology `AB|CD` first, with about 0.936 probability, against about 0.032 each for the other two topologies. This matches the character data by construction, A and B (and separately C and D) disagree at only one of eight sites, so any topology that does not group them together has to explain those near-identical pairs as either coincidence or a very long chain of substitutions along the internal edge, both of which cost log-likelihood under this model. The exact enumeration also makes the result fully reproducible: with only 27 (topology, branch-length) combinations to score, there is no sampling noise to average away, and the same posterior comes out on every run.

References:

- Felsenstein, J. (1981). Evolutionary trees from DNA sequences: a maximum likelihood approach. *Journal of Molecular Evolution*, 17(6), 368-376.
- Neyman, J. (1971). Molecular studies of evolution: a source of novel statistical problems. In *Statistical Decision Theory and Related Topics* (pp. 1-27). Academic Press.
- Yang, Z., & Rannala, B. (2012). Molecular phylogenetics: principles and practice. *Nature Reviews Genetics*, 13(5), 303-314.
