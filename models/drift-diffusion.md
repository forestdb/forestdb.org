---
layout: model
title: Drift Diffusion Model
model-status: code
model-category: Time Series and Stochastic Processes
model-tags: cognitive modeling, decision making, reaction time, ABC
model-language: webppl
model-language-version: v0.9.15
---

The drift diffusion model (DDM) is the standard account of two-alternative forced-choice decisions in cognitive psychology (Ratcliff, 1978). Evidence accumulates as a noisy random walk that starts midway between two boundaries; whichever boundary it hits first determines the choice, and the time it takes is the reaction time (RT). A constant drift rate biases the walk toward one boundary, linking the decision process to both the choice and its timing.

This page simulates the DDM as a bounded random walk with a hard step cap, then infers the drift rate that produced a batch of choices and RTs. The pure Wiener diffusion process has a closed-form first-passage-time density (Navarro and Fuss, 2009), but this simulator's discrete time step and hard cap depart from that ideal case, and no closed-form likelihood for it is implemented here. The inference box instead uses approximate Bayesian computation (ABC): it compares summary statistics of simulated data to fixed observed data via a smoothing kernel, standing in for the likelihood.

~~~~
var a = 1;          // boundary separation
var z = 0.5;        // starting point, midway between the boundaries
var dt = 0.02;      // time step, in seconds
var noiseSd = Math.sqrt(dt);
var maxSteps = 150; // hard cap: no trial simulates past maxSteps * dt = 3s

// One trial: evidence x starts at z and moves by `drift` per second plus
// Gaussian noise, until it hits 0, hits a, or the step cap fires. The cap
// guarantees simulateTrial always returns, at the cost of mislabeling the
// rare trial that is still undecided when it fires.
var simulateTrial = function(drift) {
  var step = function(x, t) {
    if (x >= a) {
      return { choice: 'upper', rt: t * dt };
    } else if (x <= 0) {
      return { choice: 'lower', rt: t * dt };
    } else if (t >= maxSteps) {
      return { choice: x >= a / 2 ? 'upper' : 'lower', rt: t * dt };
    } else {
      var dx = drift * dt + noiseSd * gaussian(0, 1);
      return step(x + dx, t + 1);
    }
  };
  return step(z, 0);
};

var example = repeat(8, function() { return simulateTrial(0.8); });
display(example);
~~~~

Positive drift pulls the walk toward the upper boundary, so most trials above choose "upper", and trials land there faster the larger the drift is relative to the noise. The next box treats the drift rate as unknown and infers it from data.

~~~~
var a = 1;
var z = 0.5;
var dt = 0.02;
var noiseSd = Math.sqrt(dt);
var maxSteps = 150;

var simulateTrial = function(drift) {
  var step = function(x, t) {
    if (x >= a) {
      return { choice: 'upper', rt: t * dt };
    } else if (x <= 0) {
      return { choice: 'lower', rt: t * dt };
    } else if (t >= maxSteps) {
      return { choice: x >= a / 2 ? 'upper' : 'lower', rt: t * dt };
    } else {
      var dx = drift * dt + noiseSd * gaussian(0, 1);
      return step(x + dx, t + 1);
    }
  };
  return step(z, 0);
};

// Summary statistics: the fraction of trials that hit the upper boundary,
// and the mean reaction time. Reducing every simulated trial to these two
// numbers is what makes the inference below approximate.
var summarize = function(trials) {
  var upperFrac = sum(map(function(t) { return t.choice === 'upper' ? 1 : 0; }, trials)) / trials.length;
  var meanRT = listMean(map(function(t) { return t.rt; }, trials));
  return { upperFrac: upperFrac, meanRT: meanRT };
};

// Fixed observed summary: computed once, offline, from 300 trials
// simulated with an unknown-to-the-model drift of 0.7. Hardcoding it here
// keeps the inference target the same on every run of this box, rather
// than redrawing noisy "observed" data each time.
var trueDrift = 0.7;
var observed = { upperFrac: 0.69, meanRT: 0.33 };

// Approximate Bayesian computation: no closed-form likelihood is
// implemented for this discrete-step, hard-capped simulator (see intro).
// Instead, for each candidate drift, simulate fresh trials, reduce them to
// the same two summary statistics, and weight the candidate by a Gaussian
// kernel on the distance between simulated and observed summaries,
// standing in for the likelihood. This is a genuine approximation: it
// recovers the drift only as well as the two summary statistics constrain
// it, and the kernel bandwidth trades bias against variance.
var nSim = 60;
var bandwidth = 0.08;

var posterior = Infer({ method: 'SMC', particles: 3000 }, function() {
  var drift = uniform(-2, 2);
  var sim = summarize(repeat(nSim, function() { return simulateTrial(drift); }));
  var d2 = Math.pow(sim.upperFrac - observed.upperFrac, 2) / Math.pow(0.2, 2) +
           Math.pow(sim.meanRT - observed.meanRT, 2) / Math.pow(0.2, 2);
  factor(-d2 / (2 * bandwidth * bandwidth));
  return drift;
});

var draws = sort(repeat(2000, function() { return sample(posterior); }));
var pct = function(p) { return draws[Math.floor(p * draws.length)]; };
display('posterior mean: ' + expectation(posterior).toFixed(2));
display('90% interval: [' + pct(0.05).toFixed(2) + ', ' + pct(0.95).toFixed(2) + ']');
display('true drift: ' + trueDrift);
viz.density(posterior);
~~~~

The posterior concentrates around the true drift of 0.7, with a 90% interval that comfortably contains it. Both summary statistics carry information: the choice proportion mainly pins down the sign and rough size of the drift, while the mean RT helps separate a moderate drift from a much larger one that would also send most trials to the same boundary, only faster. Running the inference again with a smaller `nSim` or a tighter `bandwidth` sharpens or degrades this recovery, which is the usual ABC bias-variance trade-off (Turner and Van Zandt, 2012).

References:

- Ratcliff, R. (1978). A theory of memory retrieval. *Psychological Review*, 85(2), 59-108.
- Navarro, D. J., & Fuss, I. G. (2009). Fast and accurate calculations for first-passage times in Wiener diffusion models. *Journal of Mathematical Psychology*, 53(4), 222-230.
- Turner, B. M., & Van Zandt, T. (2012). A tutorial on approximate Bayesian computation. *Journal of Mathematical Psychology*, 56(2), 69-85.
