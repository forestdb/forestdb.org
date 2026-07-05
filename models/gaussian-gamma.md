---
layout: model
title: 1D Gaussian with Gaussian/Gamma Hyperparameters
model-status: code
model-category: Probability and Bayesian Data Analysis
model-tags: continuous models, gaussian distribution
model-language: church
---

Gaussian and gamma priors here set the mean and variance of a one-dimensional Gaussian distribution, which is then conditioned on eight observed data points. Metropolis-Hastings inference produces the posterior over the mean.

~~~~
(define xdata '(1 1.2 1.5 0.8 0.9 1 1.3 0.9))

(define samples
  (mh-query
   1000 10
   (define mu (gaussian 0 2))
   (define sigma-squared (gamma 1 1))
   
   mu

   (all
    (map (lambda (x) (equal? (gaussian mu sigma-squared x) x)) xdata)
    )))

(density samples "Posterior over mu" #t)
~~~~
