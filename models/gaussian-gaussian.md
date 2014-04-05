---
layout: model
title: 1D Gaussian with Gaussian Hyperparameters
model-status: code-fail
model-status-verbose: The MH chain cannot be initialized.
model-category: Miscellaneous
model-tags: continuous models, gaussian distribution
---

One-dimensional Gaussian distribution with mean and variance
sampled from a Gaussian distribution.

    (define observed-data
      '(4.18 5.36 7.54 2.47 8.83 6.21 5.22 6.41))
    
    (define num-observations (length observed-data))
    
    (query
    
     (define mean (gaussian 0 10))
     (define var  (abs (gaussian 0 5)))
     (define sample-gaussian (lambda () (gaussian mean var)))
    
     (list mean var)
    
     (equal? observed-data
             (repeat num-observations sample-gaussian)))
