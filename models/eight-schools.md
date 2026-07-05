---
layout: model
title: Eight Schools
model-status: code
model-category: Probability and Bayesian Data Analysis
model-tags: hierarchical models, partial pooling, meta-analysis, mcmc
model-language: webppl
model-language-version: v0.9.15
---

Eight Schools (Rubin 1981) is the textbook hierarchical Bayesian model: eight schools each ran a coaching program and reported an effect on SAT scores along with its standard error. How much should each school trust its own number versus treating all eight as noisy readings of one shared effect?

Three answers bracket the honest one. No pooling treats each school's reported effect as the truth, ignoring that a school with a large standard error is barely more informative than the prior. Full pooling assumes all eight schools share exactly one true effect and averages them, ignoring that some of the reported spread could be real between-school variation. Partial pooling puts a hierarchical prior over the school effects, mu and tau, and lets the data decide how much to shrink each school toward the group mean, more for noisy schools, less for precise ones.

The model uses a non-centered parameterization (`theta = mu + tau * thetaTilde`) rather than sampling each `theta_j` directly from `Gaussian(mu, tau)`. The centered version has a funnel-shaped posterior for `theta` and `tau` that is hard for any sampler to move through when `tau` is small; the non-centered version decouples the school-level noise from the group-level scale and mixes far more reliably.

~~~~
var y =     [28,  8, -3,  7, -1,  1, 18, 12];
var sigma = [15, 10, 16, 11,  9, 11, 10, 18];
var J = y.length;

var model = function() {
  var mu = gaussian(0, 20);
  var logTau = gaussian(1, 1);
  var tau = Math.exp(logTau);
  // Non-centered parameterization: thetaTilde is a standardized deviation,
  // and theta = mu + tau * thetaTilde. This avoids the tau-theta funnel
  // that plagues the centered form when tau is small.
  var thetaTilde = repeat(J, function() { return gaussian(0, 1); });
  var theta = map(function(tt) { return mu + tau * tt; }, thetaTilde);
  map2(function(t, j) { observe(Gaussian({mu: t, sigma: sigma[j]}), y[j]); }, theta, _.range(J));
  return {mu: mu, tau: tau, theta: theta};
};

var post = Infer({method: 'MCMC', samples: 30000, burn: 10000}, model);

var postMean = function(f) { return expectation(post, f); };

var muHat = postMean(function(s) { return s.mu; });
var tauHat = postMean(function(s) { return s.tau; });
var partialPool = map(function(j) { return postMean(function(s) { return s.theta[j]; }); }, _.range(J));

// No pooling: each school's own reported effect.
var noPool = y;

// Full pooling: a single inverse-variance-weighted mean applied to every school.
var weights = map(function(j) { return 1 / (sigma[j] * sigma[j]); }, _.range(J));
var fullPoolValue = sum(map2(function(w, j) { return w * y[j]; }, weights, _.range(J))) / sum(weights);

// Classical normal-normal shrinkage weight on the grand mean for each
// school: sigma_j^2 / (tau^2 + sigma_j^2), evaluated at the posterior mean
// of tau. Larger sigma_j means noisier own data, so more of the weight
// falls on the group mean rather than the school's own report.
var shrinkageWeight = map(function(j) {
  return (sigma[j] * sigma[j]) / (tauHat * tauHat + sigma[j] * sigma[j]);
}, _.range(J));

display('posterior mean of mu (grand mean): ' + muHat.toFixed(2));
display('posterior mean of tau (between-school sd): ' + tauHat.toFixed(2));
display('full-pooling estimate (all schools): ' + fullPoolValue.toFixed(2));
display('');
display('school  sigma  no-pool  partial-pool  shrinkage-weight-on-mean');
map(function(j) {
  display(j + '       ' + sigma[j] + '     ' + noPool[j].toFixed(1) + '     ' +
          partialPool[j].toFixed(2) + '         ' + shrinkageWeight[j].toFixed(3));
}, _.range(J));
~~~~

The posterior mean of `tau` comes out around 2.7, small relative to the schools' standard errors (9 to 18), so the model finds little evidence of real between-school variation and pools heavily: every school's partial-pooling estimate lands close to the full-pooling value of about 7.7, pulling school 0's reported 28 down to around 8 and pulling school 2's reported -3 up to around 7. The shrinkage weight on the group mean, `sigma_j^2 / (tau^2 + sigma_j^2)`, is above 0.9 for every school and increases monotonically with `sigma_j`, from about 0.92 for the most precise school (sigma = 9) to about 0.98 for the least precise one (sigma = 18), exactly the qualitative pattern partial pooling is supposed to produce: noisier schools defer more to the group.

References:

- Rubin, D. B. (1981). Estimation in parallel randomized experiments. *Journal of Educational Statistics*, 6(4), 377-401.
- Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., & Rubin, D. B. (2013). *Bayesian Data Analysis* (3rd ed.), section 5.5. CRC Press.
