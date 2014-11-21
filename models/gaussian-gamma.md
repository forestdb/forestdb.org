---
layout: model
title: 1D Gaussian with Gaussian/gamma Hyperparameters
model-category: Miscellaneous
model-tags: continuous models, gaussian distribution
---

One-dimensional Gaussian distribution with mean and variance
sampled from Gaussian/gamma priors

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
