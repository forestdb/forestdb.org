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

      ;;;fold:
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
      
We see that for lower values of `bias-weight`, we get the intuitive ifnerence. 

Now, let's suppose we've collected some data, and have judgements about whether or not certain sequences are biased. Could we use this data to infer the right value of `bias-weight'? 
One method to find out the best parameter value would be to "guess and check". We could plug in different values, and see if the predictions match our data

A more formal (and Bayesian) way to to do this is to say we (as scientists) have uncertainty about what the value of `bias-weight` is, but whatever it is, it should lead to good predictions of the data.

This sort of model is sometimes called "bayes in the notebook" (to contrast with the "bayes in the head" models we've dealt with so far).

As good scientists, we'll want to collect data for a number of sequences, and we'll want our model to predict the responses for all (or many) of them. 

      (define all-seqs 
            (list 
                  (list false false false false false)
                  (list false false false false true)
                  (list false false false true true)
                  (list false false true true true) 
                  (list false true true true true)
                  (list true true true true true)))

(define data-analysis 
  (lambda (experiment-data)
    (rejection-query

     (define biased-weight 
       (uniform-draw (list 0.1 0.2 0.3 0.4 0.6 0.7 0.8 0.9)))

     ; generate predictions for all sequences
     (define cognitive-model-predictions
       (map 
        (lambda (sequence) 
          (bc-model sequence biased-weight)) 
        all-seqs))

     ; what is the best biased-weight?
     biased-weight
     
     ; given that we've observed this data
     (all (flatten
     (map 
      (lambda (data-for-one-sequence model)
        ; map over data points in a given sequence
        (map (lambda (single-data-point)
               ;single-data-point)
               (equal? single-data-point (apply multinomial model)))
         data-for-one-sequence))
      (second experiment-data)
      cognitive-model-predictions)))
   

