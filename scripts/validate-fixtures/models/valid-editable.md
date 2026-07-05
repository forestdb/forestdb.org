---
layout: model
title: Valid Editable Fixture
model-language: webppl
model-status: code
model-category: Regression and Statistical Learning
---

This model infers the slope and intercept of a line from noisy observed points, using a Gaussian likelihood over the residuals.

~~~~
var model = function() {
  var slope = gaussian(0, 1)
  return slope
}
~~~~
