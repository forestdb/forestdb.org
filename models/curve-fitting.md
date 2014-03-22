---
layout: model
title: Curve Fitting
model-status: code
model-category: Miscellaneous
model-tags: function learning, occam's razor
---

Bayesian inference can be used to simultaneously find the order of
a polynomial and its coefficients, trading off fit against prior
preference for simpler models. Model by @churchwiki.

    (define (make-poly c)
      (lambda (x) (apply + (map (lambda (a b) (* a (expt x b)))
                           c
                           (iota (length c))))))
    
    (define x-vals (map (lambda (x) (/ x 1)) (range -5 5)))
    
    (define true-coeffs '(-.5 1 .5))
    
    (define true-y-vals (map (make-poly true-coeffs) x-vals))
    
    (define obs-noise 0.2)
    
    (define obs-y-vals
      (map (lambda (x) (gaussian x obs-noise)) true-y-vals))
    
    (query
    
     (define poly-order (sample-integer 4))
     (define coefficients
       (repeat (+ poly-order 1)
               (lambda () (gaussian 0 2))))
     (define (noisy-equals? x y)
       (= 0.0 (gaussian (- x y) obs-noise)))
     (define y-vals
       (map (make-poly coefficients) x-vals))
    
     (list poly-order coefficients)
    
     (fold (lambda (x a) (and x a))
           true
           (map noisy-equals? y-vals obs-y-vals)))

References:

- Cite:ProbMods
