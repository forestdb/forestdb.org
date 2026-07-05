---
layout: model
title: Hawkes Process
model-status: code
model-category: Time Series and Stochastic Processes
model-tags: point process, self-exciting process, hawkes process, time series, simulation
model-language: webppl
model-language-version: v0.9.15
---

A Hawkes process is a self-exciting point process: each event temporarily raises the rate at which future events occur, so events cluster in time instead of arriving independently as in a homogeneous Poisson process (Hawkes 1971). This model simulates both processes on a finite horizon and compares them.

The conditional intensity of a Hawkes process with an exponential kernel is

    lambda(t) = mu + sum over past events t_i < t of alpha * exp(-beta * (t - t_i))

where `mu` is the baseline rate, `alpha` is the jump in intensity caused by each event, and `beta` controls its decay. The branching ratio `alpha / beta` is the expected number of direct child events per event; values below one define the subcritical regime with a stationary finite long-run rate.

Simulation uses Ogata's thinning algorithm: propose the next candidate time from an exponential distribution with a rate that upper-bounds the true intensity, then accept it with probability equal to the ratio of the true intensity to that bound. Between events the intensity only decays, so the intensity right after the most recent accepted event (or the baseline `mu` at the start) is always a valid upper bound until the next acceptance. The simulator is bounded by both a finite horizon and a cap on the number of events and thinning steps, so it always terminates even for parameter settings that would otherwise generate unboundedly many events.

~~~~
var mu = 0.5;      // baseline intensity
var alpha = 0.6;   // excitation strength per event
var beta = 1.0;    // decay rate; alpha / beta < 1 keeps the process stable
var T = 20;        // finite time horizon
var maxEvents = 300;  // hard cap on event count
var maxSteps = 5000;  // hard cap on thinning steps (guards against many rejections)

var intensity = function(t, history) {
  return mu + sum(map(function(ti) { return alpha * Math.exp(-beta * (t - ti)); }, history));
};

// Ogata's thinning algorithm. Between events the intensity only decays, so
// the intensity value right after the last accepted point (or the start)
// is always a valid upper bound until the next acceptance.
var thin = function(t, history, bound, steps) {
  if (history.length >= maxEvents || steps >= maxSteps) { return history; }
  var w = exponential(bound);
  var tCand = t + w;
  if (tCand >= T) { return history; }
  var lambdaCand = intensity(tCand, history);
  if (flip(lambdaCand / bound)) {
    var newHistory = history.concat([tCand]);
    return thin(tCand, newHistory, intensity(tCand, newHistory), steps + 1);
  } else {
    return thin(tCand, history, lambdaCand, steps + 1);
  }
};

var simulateHawkes = function() {
  return thin(0, [], intensity(0, []), 0);
};

// The alpha = 0 special case is a homogeneous Poisson process at rate mu.
var simulatePoisson = function(rate) {
  var step = function(t, history) {
    if (history.length >= maxEvents) { return history; }
    var w = exponential(rate);
    var tCand = t + w;
    return tCand >= T ? history : step(tCand, history.concat([tCand]));
  };
  return step(0, []);
};

var hawkesEvents = simulateHawkes();
var poissonEvents = simulatePoisson(mu);

display('Poisson baseline event count over the horizon: ' + poissonEvents.length);
display('Hawkes event count over the horizon: ' + hawkesEvents.length);

// Trace the conditional intensity over time to visualize clustering.
var grid = _.range(0, T, 0.05);
var intensityPath = map(function(t) {
  var pastEvents = filter(function(ti) { return ti < t; }, hawkesEvents);
  return intensity(t, pastEvents);
}, grid);

viz.line(grid, intensityPath);

// If an event occurred, its excitation makes the intensity jump above baseline.
var hasEvent = hawkesEvents.length > 0;
var t1 = hasEvent ? hawkesEvents[0] : 0;
var afterFirst = hasEvent ? intensity(t1 + 1e-6, [t1]) : mu;
display('baseline intensity: ' + mu.toFixed(3));
display(hasEvent
  ? 'intensity just after the first self-excited event: ' + afterFirst.toFixed(3)
  : 'no Hawkes event occurred in this simulation');
display('post-event intensity exceeds baseline: ' + (hasEvent && afterFirst > mu));
~~~~

The intensity trace shows jumps at Hawkes events followed by exponential decay toward the baseline `mu`. Averaged over repeated simulations, excitation produces more events than the Poisson baseline and makes them cluster in bursts. The final check confirms that the conditional intensity immediately after an event exceeds the baseline when `alpha > 0`; in the rare simulation with no events, it reports that instead.

Hawkes processes are used to model earthquake aftershock sequences (the original motivation in Hawkes 1971 and the ETAS model of Ogata 1988), neural spike trains, and order-book events in finance, anywhere that one event raises the short-term probability of similar events.

References:

- Hawkes, A. G. (1971). Spectra of some self-exciting and mutually exciting point processes. *Biometrika*, 58(1), 83-90.
- Ogata, Y. (1988). Statistical models for earthquake occurrences and residual analysis for point processes. *Journal of the American Statistical Association*, 83(401), 9-27.
- Laub, P. J., Taimre, T., & Pollett, P. K. (2015). Hawkes processes. arXiv:1507.02822.
