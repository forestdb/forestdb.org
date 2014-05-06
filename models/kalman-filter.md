---
layout: model
title: Kalman Filter
model-status: code-fail
model-status-verbose: The MCMC chain doesn't mix.
model-category: Miscellaneous
model-tags: gaussian
---

The Kalman filter (or Kalman smoother) is a Hidden Markov Model with Gaussian transition and observation functions. The model here is based on the [Kalman filter in Anglican](http://www.robots.ox.ac.uk/~fwood/anglican/examples/kalman_filter/).

    (define xs '(1 2 3 4 5    7 8 9 10))
    (define ys '(0.38 0.49 0.47 0.28 0.24   0.33 0.47 0.40 0.44))
    
    (define samples
      (mh-query
       10000 1
       ;; Measurement noise variance 
       (define m-noise 0.01)					
       ;; State transition model which is applied to the previous state		
       (define A 1) 								
       ;; Observation model which maps the true state space into the observed space
       (define H 1)
       ;; Process noise variance 											
       (define p-noise 0.00001)
       ;; Transition function
       (define f 
         (mem 
          (lambda (t) 
            (if (<= t 1) 
                (gaussian 0 1) 
                (gaussian (* A (f (- t 1))) p-noise)))))
    
       ;; Query expression
       (f 6)
    
       ;; Condition
       (all
        (map (lambda (x y) (= (gaussian (* H (f x)) m-noise y) y))
             xs
             ys))))
    
    (density samples "(f 6)" #t)

References:

- [Kalman filter in Anglican](http://www.robots.ox.ac.uk/~fwood/anglican/examples/kalman_filter/)
