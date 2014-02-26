---
layout: model
title: Hierarchical urn model
model-status: code
model-category: Miscellaneous
model-tags: hierarchical models
---

This model first samples a bias, splits the bias into red and
black, and then defines how the urns' proportions of red balls
depend on the biases.

    (query
    
     ;; model
     (define bias (uniform 0 10))
     (define red-bias (uniform 0 bias))
     (define black-bias (- bias red-bias))
     (define urn->proportion-red
       (mem
        (lambda (urn)
          (first (beta (+ .4 red-bias) (+ .4 black-bias))))))
     (define (sample-urn urn)
       (if (flip (urn->proportion-red urn))
           'red
           'black))
    
     ;; query expression
     (urn->proportion-red 3)
    
     ;; condition
     (equal? (repeat 15 (lambda () (sample-urn 1)))
             '(red red red red red red red red
                   red red red red red red black)))
