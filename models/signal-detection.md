---
layout: model
title: Signal Detection
model-status: code
model-category: Probability and Bayesian Data Analysis
model-tags: signal detection theory, psychophysics, mcmc
model-language: webppl
model-language-version: v0.9.15
---

Signal detection theory (Green and Swets 1966) separates sensitivity from response bias. Signal and noise trials generate internal responses, the observer says yes above an absolute threshold `k`, sensitivity `d'` measures the separation between distributions, and the conventional bias criterion is `c = k - d'/2`.

The hit and false-alarm rates satisfy `hitRate = Phi(d' - k)` and `falseAlarmRate = Phi(-k)`, so together they identify both `d'` and `k`. The model infers those values with probit-linked binomial likelihoods and then derives `c`, which is positive for a conservative observer and negative for a liberal one.

~~~~
// Abramowitz-Stegun approximation to the error function.
var erf = function(x) {
  var a1 =  0.254829592;
  var a2 = -0.284496736;
  var a3 =  1.421413741;
  var a4 = -1.453152027;
  var a5 =  1.061405429;
  var p  =  0.3275911;
  var sign = x < 0 ? -1 : 1;
  var z = Math.abs(x);
  var t = 1.0 / (1.0 + p * z);
  var y = 1.0 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * Math.exp(-z * z);
  return sign * y;
};

var stdNormalCdf = function(x) {
  return 0.5 * (1 + erf(x / Math.sqrt(2)));
};

// An observer does 100 signal-present trials and 100 noise-only trials,
// says "yes" (detected) on 60 of the signal trials (hits) and on 10 of the
// noise trials (false alarms). A moderate hit rate paired with a low false
// alarm rate suggests decent sensitivity and a response threshold high
// enough to produce a conservative bias relative to the signal/noise midpoint.
var nSignalTrials = 100;
var hits = 60;
var nNoiseTrials = 100;
var falseAlarms = 10;

var model = function() {
  var dprime = gaussian(0, 3);
  var threshold = gaussian(0, 2);

  var hitRate = stdNormalCdf(dprime - threshold);
  var faRate = stdNormalCdf(0 - threshold);

  observe(Binomial({n: nSignalTrials, p: hitRate}), hits);
  observe(Binomial({n: nNoiseTrials, p: faRate}), falseAlarms);

  return {dprime: dprime, threshold: threshold};
};

var post = Infer({method: 'MCMC', samples: 20000, burn: 5000}, model);
var postMean = function(f) { return expectation(post, f); };

var dprimeHat = postMean(function(s) { return s.dprime; });
var thresholdHat = postMean(function(s) { return s.threshold; });
var biasHat = postMean(function(s) { return s.threshold - s.dprime / 2; });

display('empirical hit rate: ' + (hits / nSignalTrials).toFixed(2));
display('empirical false alarm rate: ' + (falseAlarms / nNoiseTrials).toFixed(2));
display('posterior mean d-prime (sensitivity): ' + dprimeHat.toFixed(2));
display('posterior mean threshold k: ' + thresholdHat.toFixed(2));
display('posterior mean bias criterion c: ' + biasHat.toFixed(2));
display('d-prime positive (observer discriminates signal from noise): ' + (dprimeHat > 0));
display('criterion c positive (observer is conservative): ' + (biasHat > 0));
~~~~

The posterior mean of `d'` is about 1.5, indicating that the observer discriminates signal from noise. The absolute response threshold `k` is about 1.3, while the conventional criterion `c = k - d'/2` is about 0.5. Its positive sign captures the conservative response bias implied by a false-alarm rate of only 0.10.

References:

- Green, D. M., & Swets, J. A. (1966). *Signal Detection Theory and Psychophysics*. Wiley.
- Macmillan, N. A., & Creelman, C. D. (2005). *Detection Theory: A User's Guide* (2nd ed.). Lawrence Erlbaum Associates.
