---
layout: model
title: Bayesian Data Analysis
---

Here is the biased coin model presented in Chapter 5.

    (define biascoin-model (lambda (sequence bias-weight)
      (mh-query
       100 10
       
       (define fair-weight 0.5)
       
       (define isfair (flip))
       
       (define the-weight (if isfair 
       							fair-weight 
       							bias-weight))
       
       (define coin (lambda () 
       		(flip the-weight)))
       
       
       isfair
       
       (equal? sequence 
       	      (repeat 5 coin)))))


      (hist (biascoin-model (list false false false false true) 0.2) "00001 is fair?")

      (hist (biascoin-model (list false true true false true) 0.2) "01101 is fair?")

The only difference between this model and the one presented in Chapter 5 is that we've made  `bias-weight` an argument to the function `biascoin-model`. `bias-weight` was a parameter implicit in the model in Chapter 5; by making it into an argument, we have made it explicit.

Let's analyze the behavior of the model by looking at a number of different biases.
To compress out results, we'll compute the percentage endorsements for TRUE using `bool-sum`

      ;;;fold:
    (define biascoin-model (lambda (sequence bias-weight)
      (mh-query
       100 10
       
       (define fair-weight 0.5)
       
       (define isfair (flip))
       
       (define the-weight (if isfair 
                                                fair-weight 
                                                bias-weight))
       
       (define coin (lambda () 
                  (flip the-weight)))
       
       
       isfair
       
       (equal? sequence 
                  (repeat 5 coin)))))
      ;;;

      (define bool-sum (lambda (lst)
                  (/ 
                        (sum 
                              (map (lambda (x) (boolean->number x))     lst)) 
                        (length lst))))

      (define results-for-many-biases (map 
            (lambda (bias-weight) (bool-sum 
                  (biascoin-model 
                        (list false false false false false) 
                        bias-weight)))
      (list 0.1 0.2 0.3 0.4 0.6 0.7 0.8 0.9)))

      (hist results-for-many-biases)
      