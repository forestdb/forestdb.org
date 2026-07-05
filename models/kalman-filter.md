---
layout: model
title: Kalman Filter
model-status: code
model-category: Time Series and Stochastic Processes
model-tags: gaussian
model-language: church
---

The Kalman filter (or Kalman smoother) is a Hidden Markov Model with Gaussian transition and observation functions. The model here is based on the [Kalman filter in Anglican](https://probprog.github.io/anglican/examples/viewer/?worksheet=kalman).

    ;; Gaussian log-density (variance parameterization)
    (define (glp mean var smp)
      (let ([diff (- smp mean)])
        (- (- (/ (* diff diff) (* 2.0 var)))
           (* 0.5 (+ (log 2) (log 3.141592653589793) (log var))))))
    
    ;; Soft observation: weight the trace by the likelihood of y
    (define (observe-gaussian mean var y)
      (factor (glp mean var y))
      #t)
    
    (define xs '(1 2 3 4 5    7 8 9 10))
    (define ys '(0.38 0.49 0.47 0.28 0.24   0.33 0.47 0.40 0.44))
    
    (define samples
      (mh-query
       3000 30
       ;; Measurement noise variance 
       (define m-noise 0.01)					
       ;; State transition model which is applied to the previous state		
       (define A 1) 								
       ;; Observation model which maps the true state space into the observed space
       (define H 1)
       ;; Process noise variance (large enough that the generic MH
       ;; kernel can move; with 1e-5 the chain never accepts a proposal)
       (define p-noise 0.01)
       ;; Transition function
       (define f 
         (mem 
          (lambda (t) 
            (if (<= t 1) 
                (gaussian 0 1) 
                (gaussian (* A (f (- t 1))) p-noise)))))
    
       ;; Query expression
       (f 6)
    
       ;; Condition (soft observations; exact equality on a continuous
       ;; draw has probability zero, so the chain could never move)
       (all
        (map (lambda (x y) (observe-gaussian (* H (f x)) m-noise y))
             xs
             ys))))
    
    (density samples "(f 6)" #t)

References:

- [Kalman filter in Anglican](https://probprog.github.io/anglican/examples/viewer/?worksheet=kalman)
