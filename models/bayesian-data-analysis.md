---
layout: model
title: Bayesian Data Analysis
---

Here is the biased coin model presented in Chapter 5.

    (define bc-model (lambda (sequence biased-weight)
      (mh-query
       100 10
       
       (define fair-weight 0.5)
       
       (define isfair (flip))
       
       (define the-weight (if isfair 
       							fair-weight 
       							biased-weight))
       
       (define coin (lambda () 
       		(flip the-weight)))
       
       
       isfair
       
       (equal? sequence 
       	      (repeat 5 coin)))))
