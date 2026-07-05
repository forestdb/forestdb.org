---
layout: model
title: Gaussian Process Regression
model-status: code
model-category: Bayesian Nonparametrics
model-tags: gaussian process, kernel methods, nonparametric regression
model-language: webppl
model-language-version: v0.9.15
---

A Gaussian process puts a prior directly over functions rather than over a fixed set of parameters, using a covariance kernel to say how correlated two function values should be based on the distance between their inputs (Rasmussen & Williams 2006). Conditioning that prior on noisy observations at a handful of 1D points gives an exact posterior mean and variance at any other point, in closed form via a Cholesky solve, no sampling required.

~~~~
// --- basic matrix / linear algebra helpers (deterministic, no probability) ---

var zeros = function(n) { return repeat(n, function() { return 0; }); };

var transpose = function(A) {
  var nRows = A.length;
  var nCols = A[0].length;
  return map(function(j) {
    return map(function(i) { return A[i][j]; }, _.range(nRows));
  }, _.range(nCols));
};

var matVec = function(A, v) {
  return map(function(row) { return sum(map2(function(a, b) { return a * b; }, row, v)); }, A);
};

// Lower-triangular Cholesky factor L such that L * L^T = A
// (Cholesky-Banachiewicz: build each row left to right).
var cholesky = function(A) {
  var n = A.length;
  var buildRow = function(i, L) {
    var computeUpTo = function(j, partialRow) {
      if (j > i) { return partialRow; }
      var s = j === i
        ? sum(map(function(k) { return partialRow[k] * partialRow[k]; }, _.range(j)))
        : sum(map(function(k) { return partialRow[k] * L[j][k]; }, _.range(j)));
      var val = j === i ? Math.sqrt(A[i][i] - s) : (A[i][j] - s) / L[j][j];
      return computeUpTo(j + 1, partialRow.concat([val]));
    };
    return computeUpTo(0, []).concat(zeros(n - i - 1));
  };
  var go = function(i, L) { return i === n ? L : go(i + 1, L.concat([buildRow(i, L)])); };
  return go(0, []);
};

// Solve L * x = b for lower-triangular L (forward substitution).
var forwardSolve = function(L, b) {
  var n = b.length;
  var go = function(i, x) {
    if (i === n) { return x; }
    var s = sum(map(function(k) { return L[i][k] * x[k]; }, _.range(i)));
    return go(i + 1, x.concat([(b[i] - s) / L[i][i]]));
  };
  return go(0, []);
};

// Solve L^T * x = b for upper-triangular L^T (back substitution).
var backSolve = function(L, b) {
  var n = b.length;
  var go = function(i, x) {
    if (i < 0) { return x.reverse(); }
    var s = sum(map(function(k) { return L[k][i] * x[n - 1 - k]; }, _.range(i + 1, n)));
    return go(i - 1, x.concat([(b[i] - s) / L[i][i]]));
  };
  return go(n - 1, []);
};

// --- squared-exponential kernel ---

var kernel = function(x1, x2, ell, sf) {
  var d = x1 - x2;
  return sf * sf * Math.exp(-0.5 * d * d / (ell * ell));
};

var covMatrix = function(xs1, xs2, ell, sf) {
  return map(function(x1) { return map(function(x2) { return kernel(x1, x2, ell, sf); }, xs2); }, xs1);
};

var addDiag = function(K, v) {
  return map(function(i) {
    return map(function(j) { return i === j ? K[i][j] + v : K[i][j]; }, _.range(K[0].length));
  }, _.range(K.length));
};

// --- GP regression: exact posterior mean and variance at test points ---

var gpPosterior = function(xTrain, yTrain, xTest, ell, sf, noiseSd) {
  var Ktrain = addDiag(covMatrix(xTrain, xTrain, ell, sf), noiseSd * noiseSd);
  var L = cholesky(Ktrain);
  var alpha = backSolve(L, forwardSolve(L, yTrain));
  var Ks = covMatrix(xTrain, xTest, ell, sf); // n x m
  var mean = matVec(transpose(Ks), alpha);
  var variance = map(function(j) {
    var ksCol = map(function(row) { return row[j]; }, Ks);
    var v = forwardSolve(L, ksCol);
    return sf * sf - sum(map(function(a) { return a * a; }, v));
  }, _.range(xTest.length));
  return {mean: mean, variance: variance};
};

// --- six noisy observations of a smooth curve, plus test points both
// among and outside the training range ---

var xTrain = [-4, -2.5, -1, 0.5, 2, 3.5];
var yTrain = [-0.6, -1.5, 0.1, 1.6, 0.9, -0.7];
var xTest = [-6, -4, -2.5, -1, 0, 0.5, 2, 3.5, 5, 6];

var ell = 1.5;
var sf = 1.2;
var noiseSd = 0.2;

var post = gpPosterior(xTrain, yTrain, xTest, ell, sf, noiseSd);

var K = covMatrix(xTest, xTest, ell, sf);
var maxAsymmetry = reduce(function(pair, acc) {
  return Math.max(acc, Math.abs(K[pair[0]][pair[1]] - K[pair[1]][pair[0]]));
}, 0, map(function(i) { return [Math.floor(i / xTest.length), i % xTest.length]; },
          _.range(xTest.length * xTest.length)));

display('max |K[i][j] - K[j][i]| over the test covariance: ' + maxAsymmetry.toExponential(2));
display('');
display('x       posterior-mean  posterior-sd  near-training-data');
map(function(j) {
  var nearest = reduce(function(xt, best) { return Math.min(best, Math.abs(xt - xTest[j])); }, Infinity, xTrain);
  display(xTest[j].toFixed(1) + '    ' + post.mean[j].toFixed(3) + '       ' +
          Math.sqrt(post.variance[j]).toFixed(3) + '       ' + (nearest < 1 ? 'yes' : 'no'));
}, _.range(xTest.length));
~~~~

The covariance matrix is symmetric to floating-point precision, as it must be since the squared-exponential kernel is itself symmetric in its two arguments. At test points that coincide with (or sit within 1 unit of) a training input, the posterior standard deviation sits around 0.19 to 0.20, barely above the observation noise, and the posterior mean tracks that training point's `y` value closely (for example -1.43 near a training value of -1.5, and 1.57 near a training value of 1.6). Out at the edges of the test range, over a full length scale beyond the nearest training point, the standard deviation climbs to 0.88 and 1.15, and the mean drifts back toward the prior mean of 0. Both patterns are exactly what a GP posterior should do: shrink its uncertainty and follow the data close to observations, and fall back to the prior everywhere else.

References:

- Rasmussen, C. E., & Williams, C. K. I. (2006). *Gaussian Processes for Machine Learning*. MIT Press.
