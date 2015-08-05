---
layout: model
title: Inferences with Gaussians
model-status: code
model-category: Bayesian Data Analysis
model-language: webppl
---

## Lee & Wagenmakers 4.1: Inferring a mean and standard deviation

First, we want to infer the mean and standard deviation of a gaussian given some data.
We set the prior over the mean to a Gaussian centered at 0, but with very large variance.
We set the prior over the standard deviation to a uniform distribution over the interval [0,10]. 

    var data =  [1.1, 1.9, 2.3, 1.8];

    var model = function() {
      var mu = gaussian(0,1/Math.sqrt(0.001)) 
      var sigma = uniform(0, 10)

      var score = sum(map(function(dataPoint) {
        return gaussianERP.score([mu, sigma], dataPoint);
      }, data))

      factor(score)
      return [mu, sigma]
    }

    var results = MH(model, 10000)
    print("expected value of mu is")
    print(expectation(results, function(v){return v[0]}))
    print("expected value of sigma is")
    print(expectation(results, function(v){return v[1]}))

To address Exercise 1 in L&M, try some different data sets -- e.g., what if there's only a large value and a small value? What if they're symmetric around 0? For Exercise 2, we don't currently have the visualization tools needed in WebPPL. For Exercise 3 and 4, try setting sigma = 1 or mu = 0.

## Lee & Wagenmaker 4.2: Seven scientists

Next, we consider a situation where seven scientsts take a measurement of the same quantity. We expect some scientists to have better experimental skills than others, so we model their reported measurements as a set of seven Gaussians centered at the same mean value, but with different standard deviations. For the mean, we again use a wide Gaussian prior, but for the seven standard deviations, we we use a gamma distribution with shape and rate parameters close to zero. This is an approximation to the (improper) scale invariant distribution.

    var data = [-27.020, 3.570, 8.191, 9.898, 9.603, 9.945, 10.056]

    var model = function() {
  
      var mu = gaussian(0,1/Math.sqrt(0.001))
  
      // Pick either uniform prior or (improper-ish) gamma
      //var sigmas = repeat(data.length, function() {return gamma(0.1, 1/0.1)})
      var sigmas = repeat(data.length, function() {return uniform(0, 25)})
  
      var score = sum(map(function(scientistPair) {
        var dataPoint = scientistPair[0]
        var sigma = scientistPair[1]
        return gaussianERP.score([mu, sigma], dataPoint);
      }, _.zip(data, sigmas)))

  
      factor(score)
      return [mu, sigmas]
    }

    var results = MH(model, 20000)
    print("expected value of mu is")
    print(expectation(results, function(v){return v[0]}))
    print("expected value of first scientist's std is")
    print(expectation(results, function(v){return v[1][0]}))
    print("expected value of second scientist's std is")
    print(expectation(results, function(v){return v[1][1]}))
    print("expected value of third scientist's std is")
    print(expectation(results, function(v){return v[1][2]}))
    print("expected value of fifth scientist's std is")
    print(expectation(results, function(v){return v[1][4]}))    

For exercise 1, the mean of mu gives us an estimate for the true value of the measured quantity, and the different values of sigma indicate different levels of accuracy across the scientists. For exercise 2, switch sigma to a uniform distribution (currently commented out), and play with the upper bound.

## Lee & Wagenmaker 4.3: Repeated measurement of IQ

In the seven scientists example, we have one measurement corresponding to each individual, assuming that they have a shared mean but different variances.
In this example, we have three measurements corresponding to each individual, and we assume instead that they have *different* means (i.e. latent IQs) but a shared variance (i.e. measurement error).
Thus, we want to jointly estimate the shared sigma and the three means.

    // Three people with three measurements each; try different values...
    var data = [[90,95,100], [105,110,115], [150,155,160]]
    // var data = [[94, 95, 96], [109, 110,111], [154,155,156]]

    var model = function() {
      // single SD for all people (corresponding to measurement error)
      var sigma = uniform(0,100)

      // each person has a latent IQ; there are different priors we could set here...
      // var mus = repeat(data.length, function() {return gaussian(100, 15);})
      var mus = repeat(data.length, function() {return uniform(0, 300)})

      var score = sum(map(function(IQPair) {
        var measurements = IQPair[0]
        var mu = IQPair[1]
        return sum(map(function(measurement) {
          return gaussianERP.score([mu, sigma], measurement);
        }, measurements));
      }, _.zip(data, mus)))

      factor(score)
      return [mus, sigma]
    }

    var results = MH(model, 10000)
    print("expected value of sigma is")
    print(expectation(results, function(v){return v[1]}))
    print("expected value of first persons's IQ is")
    print(expectation(results, function(v){return v[0][0]}))
    print("expected value of second persons's IQ is")
    print(expectation(results, function(v){return v[0][1]}))
    print("expected value of third persons's IQ is")
    print(expectation(results, function(v){return v[0][2]}))

For exercise 1, look at the values we print out. For exercise 2, switch the prior over mu to a Gaussian (currently commented out). For exercise 3, switch the data set to the one with seemingly lower error (currently commented out).
