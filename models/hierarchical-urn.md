---
layout: model
title: Hierarchical Urn Model
model-status: code
model-category: Miscellaneous
model-tags: hierarchical models
---

This model first samples a bias, splits the bias into red and
black, and then defines how the urns' proportions of red balls
depend on the biases.

    ;;;fold: factor-equal?
    (define (factor-eq x y)
      (factor (if (equal? x y) 0.0 -1000))
      #t)
          
    (define (factor-equal? xs ys)
      (if (and (null? xs) (null? ys))
          #t
          (and (factor-eq (first xs) (first ys))
               (factor-equal? (rest xs) (rest ys)))))
    ;;;
    (define samples
      (mh-query 3000 10
                
                ;; model
                (define bias (uniform 0 10))
                (define red-bias (uniform 0 bias))
                (define black-bias (- bias red-bias))
                (define urn->proportion-red
                  (mem
                   (lambda (urn)
                     (beta (+ .4 red-bias) (+ .4 black-bias)))))
                (define (sample-urn urn)
                  (if (flip (urn->proportion-red urn))
                      'red
                      'black))
                
                ;; query expression
                (urn->proportion-red 3)
                
                ;; condition
                (factor-equal? (repeat 15 (lambda () (sample-urn 1)))
                               '(red red red red red red red red
                                     red red red red red red black))))
    
    (density samples "Proportion of red balls in urn 3" true)

References:

- Cite:Stuhlmueller2013aa
