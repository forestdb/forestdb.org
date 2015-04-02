---
layout: model
title: Bayesian Data Analysis
---

Here is the biased coin model presented in Chapter 5.

    (define biascoin-model (lambda (sequence bias-weight)
      (enumeration-query
       
       (define fair-weight 0.5)
       
       (define isfair (flip))
       
       (define the-weight (if isfair fair-weight bias-weight))
       
       (define coin (lambda () 
                  (flip the-weight)))
       
       
       isfair
       
       (equal? sequence 
                  (repeat 5 coin)))))

      (barplot (biascoin-model (list false false false false true) 0.2) "TTTTH is fair?")

      (barplot (biascoin-model (list false true true false true) 0.2) "THHTH is fair?")

The only difference between this model and the one presented in Chapter 5 is that we've made  `bias-weight` an argument to the function `biascoin-model`. `bias-weight` was a parameter implicit in the model in Chapter 5; by making it into an argument, we have made it explicit. Also, we have changed this from an `mh-query` model to an `enumeration-query` model.

Let's analyze the behavior of the model by looking at a number of different biases.
To compress out results, we're going to extract the `#t` probability; i.e. the probability that the sequence came from a fair coin.

      ;;;fold: (define biascoin-model (lambda (sequence bias-weight)
            (enumeration-query
             
             (define fair-weight 0.5)
             
             (define isfair (flip))
             
             (define the-weight (if isfair fair-weight bias-weight))
             
             (define coin (lambda () 
                        (flip the-weight)))
             
             
             isfair
             
             (equal? sequence 
                        (repeat 5 coin)))))
      ;;;

      (define get-probability-of-faircoin 
        (lambda (dist)
          (let ([index (list-index (first dist) #t)])
            (list-ref (second dist) index))))

      (define all-weights (list 0.1 0.2 0.3 0.4 0.6 0.7 0.8 0.9))

      (define results-for-many-biases 
            (map 
                  (lambda (bias-weight) 
                        (get-probability-of-faircoin 
                              (biascoin-model 
                                    (list false false false false false) 
                                    bias-weight)))
            all-weights))

      (barplot (list all-weights results-for-many-biases) "TTTTT is fair?, by bias-weight parameter")
      