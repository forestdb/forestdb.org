---
layout: model
title: Learning from Social and Direct Evidence
model-status: code
model-category: Reasoning about Reasoning
model-tags: social reasoning, ToM
model-language: church
---

A model of learning from social and direct evidence:

The learner is tasked with learning the weight of a coin. She observes some number *n-l* of flips. A second person, "Zach," sees *n-z* flips and then bets on the outcome of the next flip. The learner doesn't see the outcomes of Zach's *n-z* flips, but she does see how he bets.

    ;; Helper functions
    
    (define (iota n start)
      (define (helper n x)
        (if (> x n)
            '()
            (pair x (helper n (+ x 1)))))
      (helper n start))
    
    (define (factorial n)
      (apply * (iota n 1)))
    
    (define (combo n k) 
      (/ (factorial n) 
         (* (factorial k) (factorial (- n k)))))
    
    (define (binomial weight n) 
      (multinomial (iota n 0) 
                   (map (lambda (x) (bin_prob weight n x))
                        (iota n 0) )))
    
    (define (observe weight ndata obs)
      (flip (bin_prob weight ndata obs)))
    
    (define (bin_prob weight ndata obs)
      (define n ndata)
      (define k obs)
      (define p weight)
      (define nk (- n k))
      (define np (- 1 p))
      (* (combo n k) (expt p k) (expt np nk)))
    
    (define (guess weight alpha) 
      (if (flip (* alpha weight))
          1
          0))
    
    ;; Set data and priors
    
    (define reliable-prior .75)
    
    (define ss-nsamples 5)
    
    (define zach-nsamples 10)
    
    (define (act-prior) 
      (uniform-draw '(0 1)))
    
    (define (weight-prior) 
      (uniform-draw '(.1 .3 .5 .7 .9)))
    
    (define ss-obs 2)
    
    
    ;; Zach's model
    
    (define (zachs-choice z-obs ss-weight reliable)
      (rejection-query
    
       (define z-weight (weight-prior))
       (define act (act-prior))
       (define result (if reliable
                          (guess z-weight 100)
                          (guess z-weight 0)))
    
       act
    
       (and (observe z-weight zach-nsamples z-obs)
            (equal? act result))))
    
    
    ;; Learner's model
    
    (define (ss-model)
      (rejection-query
       (define ss-weight (weight-prior))
       (define zach-obs (binomial ss-weight zach-nsamples))
       (define zach-reliable (flip reliable-prior))
       (define zach (zachs-choice zach-obs ss-weight zach-reliable))
    
       ss-weight
    
       ;; Assuming our observations and Zach's choice:
       (and (observe ss-weight ss-nsamples ss-obs)
            (equal? 1  zach))))
    
    (hist (repeat 100 ss-model))
