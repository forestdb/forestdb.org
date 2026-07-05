---
layout: model
title: A Program in WebPPL v0.9.6
model-status: hidden
model-category: Regression and Statistical Learning
model-language: webppl
model-language-version: v0.9.6
---

This compact Bayesian linear regression example demonstrates the WebPPL v0.9.6 page format. It infers a slope, intercept, and observation noise before predicting the response at a new input.

~~~~
var xs = [0, 1, 2, 3];
var ys = [0, 1, 4, 6];

var model = function() {
  var m = gaussian(0, 2);
  var b = gaussian(0, 2);
  var sigma = gamma(1, 1);

  var f = function(x) {
    return m * x + b;
  };

  map2(
    function(x, y) {
      observe(Gaussian({mu: f(x), sigma: sigma}, y));
    },
    xs,
    ys);

  return f(4);
}

viz(Infer({model, method: 'MCMC', samples: 10000}));
~~~~
