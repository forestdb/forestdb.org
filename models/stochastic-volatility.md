---
layout: model
title: Stochastic Volatility
model-status: code
model-category: Time Series and Stochastic Processes
model-tags: time series, finance, latent state, mcmc, hamiltonian monte carlo
model-language: webppl
model-language-version: v0.9.15
---

Financial returns often alternate between calm periods and bursts of large swings, which constant-variance Gaussian noise cannot capture. Stochastic volatility models instead treat volatility as a latent process: each return is Gaussian, but its scale depends on an unobserved log-volatility that evolves over time (Taylor 1986). This page infers a short AR(1) log-volatility chain from synthetic returns containing a brief volatility shock.

The latent log-volatility follows a Gaussian AR(1) process:

    h[0] ~ Gaussian(0, sigma0)
    h[t] ~ Gaussian(phi * h[t-1], sigmaEta)   for t = 1 .. T-1

and each observed return is conditioned on the corresponding volatility level:

    y[t] ~ Gaussian(0, exp(h[t] / 2))

so a higher `h[t]` means a wider, higher-variance return distribution at that time step. The autoregressive coefficient `phi` controls how persistent volatility is; `phi` close to 1 makes volatility mean-revert slowly, matching the empirical clustering of calm and turbulent periods.

Inference uses Hamiltonian Monte Carlo because every latent variable is continuous and the joint log-density is differentiable. The short 15-step series keeps the example within a browser-friendly runtime; production analysis would still require multiple chains and convergence diagnostics.

~~~~
var T = 15;
var phi = 0.9;
var sigmaEta = 0.3;
var sigma0 = 0.5;

// Synthetic returns: mostly small day-to-day moves, with a burst of two
// large-magnitude returns at t = 7, 8 simulating a volatility shock.
var returns = [0.05, -0.03, 0.02, -0.04, 0.01, 0.03, -0.02,
                2.10, -1.80,
                0.05, -0.02, 0.03, -0.01, 0.02, -0.03];

var model = function() {
  var h0 = gaussian(0, sigma0);
  var loop = function(t, hPrev, acc) {
    if (t === T) { return acc; }
    var h = t === 0 ? hPrev : gaussian(phi * hPrev, sigmaEta);
    observe(Gaussian({mu: 0, sigma: Math.exp(h / 2)}), returns[t]);
    return loop(t + 1, h, acc.concat([h]));
  };
  return loop(0, h0, []);
};

var post = Infer({method: 'MCMC', kernel: {HMC: {steps: 5, stepSize: 0.15}}, samples: 1500, burn: 500}, model);

var meanVol = map(function(t) {
  return expectation(post, function(sample) { return Math.exp(sample[t] / 2); });
}, _.range(T));

display('inferred volatility (posterior mean of exp(h/2)) per time step:');
display(map(function(x) { return x.toFixed(2); }, meanVol));

viz.line(_.range(T), returns);
viz.line(_.range(T), meanVol);

var burstVol = listMean(meanVol.slice(7, 9));
var otherVol = listMean(meanVol.slice(0, 7).concat(meanVol.slice(9)));
display('mean inferred volatility during the return burst (t = 7, 8): ' + burstVol.toFixed(3));
display('mean inferred volatility elsewhere: ' + otherVol.toFixed(3));
display('volatility rises around the large returns: ' + (burstVol > otherVol));
~~~~

The inferred volatility trace stays low while returns are small, rises around the burst at t = 7, 8, and then declines as `phi` pulls the latent process toward its long-run mean. The final comparison checks that this run assigns higher posterior mean volatility to the burst than to the surrounding period. This is a qualitative demonstration, not a substitute for convergence diagnostics on a real time series.

Richer versions of this model add fat-tailed observation noise, leverage effects, or multivariate volatility, and are estimated from real asset returns with particle MCMC or variational methods (Kim, Shephard & Chib 1998); this page keeps the state space small enough to run comfortably in the browser.

References:

- Taylor, S. J. (1986). *Modelling Financial Time Series*. Wiley.
- Kim, S., Shephard, N., & Chib, S. (1998). Stochastic volatility: likelihood inference and comparison with ARCH models. *Review of Economic Studies*, 65(3), 361-393.
