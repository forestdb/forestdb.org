---
layout: model
title: Curve Fitting
model-status: code-fail
model-status-verbose: The MH chain does not mix.
model-category: Miscellaneous
model-tags: function learning, occam's razor
model-language: church
---

Bayesian inference can be used to simultaneously find the order of
a polynomial and its coefficients, trading off fit against prior
preference for simpler models.

    ;;;fold: my-pi, glp, range
    (define my-pi 3.14159265358979323)
    
    (define (glp mean ssq smp)
      (let ([diff (- smp mean)])
        (- (- (/ (* diff diff) (* 2.0 ssq)))
           (* 0.5 (+ (log 2) (log my-pi) (log ssq))))))
    
    (define (xrange i j)
      (if (= i j)
          '()
          (cons i (range (+ i 1) j))))
    
    (define (range i j)
      (reverse (xrange i j)))
    ;;;
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
    
    (define (noisy-equals? x y)
      (factor (glp x obs-noise y))
      #t)
    
    (define samples
      (mh-query 
       
       10000 5
       
       (define poly-order (sample-integer 4))
       (define coefficients
         (repeat (+ poly-order 1)
                 (lambda () (gaussian 0 2))))
       (define y-vals
         (map (make-poly coefficients) x-vals))
       
       (list poly-order coefficients)
       
       (all (map noisy-equals? y-vals obs-y-vals))))
    
    (hist (map first samples))

References:

- Cite:ProbMods
