---
layout: model
title: Survival Analysis with Right Censoring
model-status: code
model-category: Probability and Bayesian Data Analysis
model-tags: survival analysis, censoring, exponential, hazard, bayesian data analysis
model-language: webppl
model-language-version: v0.9.15
---

Survival models describe how long units last before an event such as death, failure, or churn. Right censoring means the event was not observed, but the unit is known to have survived at least until its final observation time. This page shows how event times contribute a density while censored times contribute a survival probability.

This model works with exponentially distributed survival times, so both terms have closed forms. If `T` has an exponential distribution with hazard rate `rate`, its density is `f(t) = rate * exp(-rate * t)` and, by memorylessness, its survival function is `S(t) = exp(-rate * t)`. In log space, an event at `t` contributes `log(rate) - rate * t` and a censored observation at `t` contributes `-rate * t`: the same tail term as the event, just without the leading `log(rate)`, since censoring tells us nothing about exactly when (or whether, within the study) the event would occur.

The data are a small two-group study: four control patients whose event times were observed, plus two control patients still event-free at the five-time-unit mark (right-censored); two treatment patients observed to the event, plus four treatment patients still event-free at seven time units. The model puts a discretized grid prior on the control hazard rate and on a multiplicative treatment effect (the treatment's hazard is `effect` times the control hazard, a simple proportional-hazards setup), then conditions on the censored-aware log-likelihood above. Because both parameters range over small finite grids, `Infer` can enumerate the exact posterior rather than approximate it by sampling.

~~~~
// Two-group survival study, four control patients observed to failure and
// two still alive at last follow-up (right-censored); two treatment
// patients observed to failure and four still alive (right-censored).
var controlEvents = [1.2, 2.5, 3.1, 4.0];
var controlCensored = [5.0, 5.0];
var treatmentEvents = [3.8, 6.2];
var treatmentCensored = [7.0, 7.0, 7.0, 7.0];

// Small discrete grids make the posterior exactly enumerable.
var rateGrid = [0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.6, 0.7, 0.8];
var effectGrid = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.2, 1.5, 2.0];

// An exponential event at time t: log-density log(rate) - rate * t.
var eventLogProb = function(rate, t) { return Math.log(rate) - rate * t; };

// A right-censored observation at time t only tells us T > t. By the
// memorylessness of the exponential, log P(T > t) = -rate * t: no density
// term, and no closing of the integral tail is needed.
var survivalLogProb = function(rate, t) { return -rate * t; };

var groupLogProb = function(rate, events, censored) {
  return sum(map(function(t) { return eventLogProb(rate, t); }, events)) +
         sum(map(function(t) { return survivalLogProb(rate, t); }, censored));
};

var model = function() {
  var rateControl = uniformDraw(rateGrid);
  var effect = uniformDraw(effectGrid); // treatment hazard = effect * control hazard
  var rateTreatment = effect * rateControl;

  factor(groupLogProb(rateControl, controlEvents, controlCensored) +
         groupLogProb(rateTreatment, treatmentEvents, treatmentCensored));

  return {rateControl: rateControl, effect: effect};
};

var posterior = Infer({method: 'enumerate'}, model);

viz.marginals(posterior);

var rateControlMarginal = marginalize(posterior, function(v) { return v.rateControl; });
var effectMarginal = marginalize(posterior, function(v) { return v.effect; });

display('E[control hazard] = ' + expectation(rateControlMarginal));
display('E[treatment effect] = ' + expectation(effectMarginal));
display('P(treatment effect < 1, i.e. treatment lowers hazard) = ' +
        expectation(effectMarginal, function(e) { return e < 1 ? 1 : 0; }));
~~~~

The posterior mean control hazard is about 0.20 (an expected event roughly every five time units), and the posterior mean treatment effect is about 0.44: the treatment group's hazard is inferred to be under half the control group's. The posterior assigns about 94% probability to `effect < 1`, i.e. to the treatment lowering the hazard rather than raising or leaving it unchanged. This conclusion is not just "treatment patients had fewer events" (2 of 6 versus 4 of 6): it also uses the information in the censoring. The four treatment patients who were censored at time 7 outlasted every control patient's observed event time, and the model correctly counts that as evidence for a lower treatment hazard even though those four patients contribute no event at all, only a long run of "still going" survival probability.

References:

- Kalbfleisch, J. D., & Prentice, R. L. (2002). *The Statistical Analysis of Failure Time Data* (2nd ed.). Wiley.
- Ibrahim, J. G., Chen, M.-H., & Sinha, D. (2001). *Bayesian Survival Analysis*. Springer.
