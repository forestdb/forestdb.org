---
layout: model
title: Bayesian Data Analysis
---

By: Michael Henry Tessler and Noah D. Goodman

In this book we are primarily concerned with probabilistic models of cognition: understanding inferences that people draw as Bayesian conditioning given a generative model that captures a persons models of the world. Bayesian statistics are equally useful to us as scientists, when we are trying to understand what our data means about psychological hypotheses. This can become confusing: a particular modeling assumption can be something we hypothesize that people assume about the world, or can be something that we as scientists want to assume (but don't assume that people assume). A pithy way of saying this is that we can make assumptions about "Bayes in the head" or about "Bayes in the notebook". We will illustrate by considering cognitive models of randomness judgements, as explored in Chapter 5.

Imagine that you are asked to judge whether a sequence of coin flips came from a fair coin or a trick (weighted) coin. The sequence "TTTTT" probably strikes you as almost certainly trick, while the sequence "THHTH" probably strikes you as likely coming from a fair coin. The sequence "HHHTT" may seem more ambiguous.
Cognitive scientists like to confirm such intuitions, and explore the borderline cases in a quantitative way. So we did an experiment asking 30 participants to make this judgement for a number of different sequences. You can do this experiment yourself [here](http://stanford.edu/~mtessler/experiments/subjective-randomness/experiment/experiment.html) (your data won't be recorded).

In Chapter 5 we discussed several models of how people might make this judgement. First we'll recall the simplest such model, then we'll think about how to compare it to the data we have gathered.

#A simple model of randomness judgements

The simplest model assumes that the sequence comes from a fair coin (with heads probability 0.5) or a biased coin whose probability of landing Heads (the `bias-weight`) is a parameter of the model.

~~~~
(define (biascoin-model sequence bias-weight)
  (enumeration-query

   (define fair-weight 0.5)

   (define isfair (flip))

   (define the-weight (if isfair fair-weight bias-weight))

   (define coin (lambda () 
                  (flip the-weight)))

   isfair

   (equal? sequence (repeat 5 coin))))

(barplot (biascoin-model (list false false false false true) 0.2) "TTTTH is fair?")

(barplot (biascoin-model (list false true true false true) 0.2) "THHTH is fair?")
~~~~

We can gain some intuition for the possible predictions of this model by exploring different bias parameters.
To simplify the results, we make a function `get-probability-of-faircoin` to extract the `#t` probability (i.e. the probability that the sequence came from a fair coin).

~~~
;;;fold:
(define (biascoin-model sequence bias-weight)
  (enumeration-query

   (define fair-weight 0.5)

   (define isfair (flip))

   (define the-weight (if isfair fair-weight bias-weight))

   (define coin (lambda () 
                  (flip the-weight)))


   isfair

   (equal? sequence (repeat 5 coin))))
;;;

; takes in "dist": output from an enumeration-query
; and "selection": the element from the posterior that you want
; returns the probability of that selection
(define get-probability-of-faircoin
  (lambda (dist selection)
    (define index (list-index (first dist) selection))
      (list-ref (second dist) index)))

(define many-biases (list 0.1 0.2 0.3 0.4 0.6 0.7 0.8 0.9))

(define results-for-many-biases 
  (map 
   (lambda (bias-weight) 
     (get-probability-of-faircoin 
      (biascoin-model (list false false false false false) bias-weight)
      #t))
   many-biases))

(barplot (list many-biases results-for-many-biases) 
         "TTTTT is fair?, by bias-weight parameter")
~~~
      
We see that for lower values of `bias-weight`, we get the intuitive inference: TTTTT is not from a fair coin. The converse way of putting this is, if we imagine trying to enforce the judgement that TTTTT comes from a biased coin, we will be forced to assume the `bias-weight` is low. 

# Inferring model parameters

How can we ask more rigorously what the right value of `bias-weight` is, given the data we have collected? 
A standard method would be to optimize the fit (e.g. correlation) of data to model predictions, by adjusting the `bias-weight` parameter.
Another (more Bayesian) way to approach this is to say we (as scientists) have uncertainty about the true value of `bias-weight`, but we believe the data is the result of participants behaving according  to `biascoin-model` with some parameter value (This is our psychological theory). This naturally leads to a generative model based on sampling `bias-weight` and then using `biascoin-model` to predict the distribution from which responses are sampled; conditioning on the data then lets us form our (scientific) beliefs about the parameter after seeing the data.

Here is a sketch of this model (it doesn't run efficiently---we'll fix that shortly):

~~~
;;;fold:
(define biascoin-model 
  (lambda (sequence bias-weight)
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

;actual data from the experiment:
; organized as a list of two lists
; list 1: lists of sequences (stimuli)
; list 2: lists of responses
(define experiment-data
  (list
   (list
    (list #t #t #t #t #t) 
    (list #t #t #t #t #f) 
    (list #t #t #t #f #t) 
    (list #t #t #t #f #f) 
    (list #t #t #f #t #t) 
    (list #t #t #f #t #f) 
    (list #t #t #f #f #t) 
    (list #t #t #f #f #f) 
    (list #t #f #t #t #t) 
    (list #t #f #t #t #f) 
    (list #t #f #t #f #t) 
    (list #t #f #t #f #f) 
    (list #t #f #f #t #t) 
    (list #t #f #f #t #f) 
    (list #t #f #f #f #t) 
    (list #t #f #f #f #f) 
    (list #f #t #t #t #t) 
    (list #f #t #t #t #f) 
    (list #f #t #t #f #t) 
    (list #f #t #t #f #f) 
    (list #f #t #f #t #t) 
    (list #f #t #f #t #f) 
    (list #f #t #f #f #t) 
    (list #f #t #f #f #f)
    (list #f #f #t #t #t) 
    (list #f #f #t #t #f) 
    (list #f #f #t #f #t) 
    (list #f #f #t #f #f) 
    (list #f #f #f #t #t) 
    (list #f #f #f #t #f) 
    (list #f #f #f #f #t) 
    (list #f #f #f #f #f))
   (list 
    (list #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #t #f #f #t #f) 
    (list #f #t #f #t #t #t #f #f #f #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #f #t #f #f #t #t #f #f #f #t #f #f #f #t #f #f #t #f #f #f #t #f #t #t #f #f #f #f #t #f) 
    (list #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #t #f #t) 
    (list #t #t #t #t #t #t #t #f #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #t #t #t #t #t #t #f #f #t #t #t #t #t #f #t #f #t #f #t #t #t #t #f #t #t #t #t #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #t #f #t #f #f #f #f #f #t #f) 
    (list #t #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #f #t #t #t #t #f #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #f #t #t #t #t #t #t #t #t #t #t #f #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #f #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f) 
    (list #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #t #t #f #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #f #t #f #f #t #f #f #f #f #f #f #f #f #f #f #t #t #f #f #f #t #f #f #f #f #f #f #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #t #t #t #t #t #t #f #f #t #f #t #t #f #t #f #t #t #t #t #t #f #t #t #t #t #f #f #t #f) 
    (list #f #t #t #t #t #t #t #f #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #f) 
    (list #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #f #t #t #t #t #f #t #t #t #t #f #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #f #t #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #f #t #f #f #t #t #t #f #t #f #t #t #f #f #t #f #t #f #f #t #t #f #f #t #f #f #t #f #t #t) 
    (list #f #t #f #t #t #t #f #f #f #t #t #t #t #f #t #f #t #t #t #f #t #t #f #t #t #t #t #f #t #f) 
    (list #f #t #f #t #t #t #t #f #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #f #f #t #t #f #f #t #f #f #f #t #f #t #f #t #f #t #f #t #f #t #t #f #f #t #f #t #t) 
    (list #t #t #t #t #t #t #f #f #t #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #t #t #f #f #t #f #f #f #t #t #f #f #f #f #f #f #t #f #f #f #t #f #f #t #f #f #f #f #t #f) 
    (list #f #t #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f))))

; list of sequences (stimuli)
(define all-seqs (first experiment-data))
; list of responses
(define all-responses (second experiment-data))


(define data-analysis 
  (lambda (experiment-data)
    (query

    ; this is a model parameter, but we scientists don't know the right parameter value
     (define biased-weight 
       (uniform-draw (list 0.1 0.2 0.3 0.4 0.6 0.7 0.8 0.9)))

     ; generate predictions for all sequences
     (define cognitive-model-predictions
       (map 
        (lambda (sequence) (biascoin-model sequence biased-weight)) 
        all-seqs))

     ; query for: what is the best biased-weight?
     biased-weight

     ; given that we've observed all these data frmo our experiment
     (condition 
      (all (flatten
            ; map over sequences 
            ; (all-responses, and cognitive-model-predictions are organized by sequence)
            (map 
             (lambda (data-for-one-sequence model-for-one-sequence)
               ; map over data points in a given sequence
               (map 
                (lambda (single-data-point)
                  ; condition on data = model
                  (equal? single-data-point (apply multinomial model)))
                data-for-one-sequence))
             all-responses
             cognitive-model-predictions))))))
~~~

Notice that there are two queries: one as part of the cognitive model ('in the head') and one as part of the data analysis model ('in the notebook').

As written, this model is very inefficient, because for each response, it *samples* a model prediction (by way of `(apply multinomial model)`, which produces a sample from the result of an `enumeration-query`) and checks to see if all model predictions match up with the observed data. We can re-write this in a more efficient way by computing the probability of the responses directly (using our handy `get-probability-of-faircoin` function from before), and adding them with a factor statement (instead of a condition statement).

~~~
;;;fold:
(define biascoin-model 
  (mem (lambda (sequence bias-weight)
         (enumeration-query

          (define fair-weight 0.5)

          (define isfair (flip))

          (define the-weight (if isfair fair-weight bias-weight))

          (define coin (lambda () 
                         (flip the-weight)))


          isfair

          (equal? sequence (repeat 5 coin))))))


(define get-probability-of-faircoin
  (lambda (dist selection)
    (define index (list-index (first dist) selection))
      (list-ref (second dist) index)))

(define experiment-data
  (list
   (list
    (list #t #t #t #t #t) 
    (list #t #t #t #t #f) 
    (list #t #t #t #f #t) 
    (list #t #t #t #f #f) 
    (list #t #t #f #t #t) 
    (list #t #t #f #t #f) 
    (list #t #t #f #f #t) 
    (list #t #t #f #f #f) 
    (list #t #f #t #t #t) 
    (list #t #f #t #t #f) 
    (list #t #f #t #f #t) 
    (list #t #f #t #f #f) 
    (list #t #f #f #t #t) 
    (list #t #f #f #t #f) 
    (list #t #f #f #f #t) 
    (list #t #f #f #f #f) 
    (list #f #t #t #t #t) 
    (list #f #t #t #t #f) 
    (list #f #t #t #f #t) 
    (list #f #t #t #f #f) 
    (list #f #t #f #t #t) 
    (list #f #t #f #t #f) 
    (list #f #t #f #f #t) 
    (list #f #t #f #f #f)
    (list #f #f #t #t #t) 
    (list #f #f #t #t #f) 
    (list #f #f #t #f #t) 
    (list #f #f #t #f #f) 
    (list #f #f #f #t #t) 
    (list #f #f #f #t #f) 
    (list #f #f #f #f #t) 
    (list #f #f #f #f #f))
   (list 
    (list #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #t #f #f #t #f) 
    (list #f #t #f #t #t #t #f #f #f #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #f #t #f #f #t #t #f #f #f #t #f #f #f #t #f #f #t #f #f #f #t #f #t #t #f #f #f #f #t #f) 
    (list #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #t #f #t) 
    (list #t #t #t #t #t #t #t #f #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #t #t #t #t #t #t #f #f #t #t #t #t #t #f #t #f #t #f #t #t #t #t #f #t #t #t #t #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #t #f #t #f #f #f #f #f #t #f) 
    (list #t #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #f #t #t #t #t #f #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #f #t #t #t #t #t #t #t #t #t #t #f #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #f #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f) 
    (list #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #t #t #f #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #f #t #f #f #t #f #f #f #f #f #f #f #f #f #f #t #t #f #f #f #t #f #f #f #f #f #f #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #t #t #t #t #t #t #f #f #t #f #t #t #f #t #f #t #t #t #t #t #f #t #t #t #t #f #f #t #f) 
    (list #f #t #t #t #t #t #t #f #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #f) 
    (list #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #f #t #t #t #t #f #t #t #t #t #f #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #f #t #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #f #t #f #f #t #t #t #f #t #f #t #t #f #f #t #f #t #f #f #t #t #f #f #t #f #f #t #f #t #t) 
    (list #f #t #f #t #t #t #f #f #f #t #t #t #t #f #t #f #t #t #t #f #t #t #f #t #t #t #t #f #t #f) 
    (list #f #t #f #t #t #t #t #f #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #f #f #t #t #f #f #t #f #f #f #t #f #t #f #t #f #t #f #t #f #t #t #f #f #t #f #t #t) 
    (list #t #t #t #t #t #t #f #f #t #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #t #t #f #f #t #f #f #f #t #t #f #f #f #f #f #f #t #f #f #f #t #f #f #t #f #f #f #f #t #f) 
    (list #f #t #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f))))

; list of sequences
(define all-seqs (first experiment-data))
; list of responses 
(define all-responses (second experiment-data))
;;;

(define data-analysis 
  (lambda (experiment-data)
    (enumeration-query

    ; this is a model parameter, but we scientists don't know the right parameter value
     (define biased-weight 
       (uniform-draw (list 0.1 0.2 0.3 0.4 0.6 0.7 0.8 0.9)))

     ; generate predictions for all sequences
     (define cognitive-model-predictions
       (map 
        (lambda (sequence) 
          (biascoin-model sequence biased-weight)) 
        all-seqs))

     ; query for: what is the best parameter setting?
     biased-weight

     ; given that we've observed this data
     (factor (sum (flatten (map 
                            ; map over sequences 
                            (lambda (data-for-one-sequence model)
                              ; map over data points in a given sequence
                              (map (lambda (single-data-point)
                                     ; compute log p(data | model)
                                     (log (get-probability-of-faircoin model single-data-point)))
                                   data-for-one-sequence))       
                            all-responses
                            cognitive-model-predictions)))))))

(barplot (data-analysis experiment-data) "inferred bias weight")
~~~

We've taken some data, and written a cognitive model with some parameters (in this case, one parameter: bias-weight), and asked what the most likely value of that parameter is.
Our inferred parameter distribution reflects the beliefs we should have as scientists given the data and the assumptions that go into the model. 


# Posterior predictive

We often want to explore how well the predictions of the model match the actual data. When the model has parameters, the fit to data will depend on parameters... it is common to maximize the parameters and then look at model-data correlation. This can give a good sense of how well the model *could* do, but doesn't take into account our uncertainty (as scientists) about what values the parameters actually take on.
Another way to explore how well a model does is to look at the predictions of the model under the "inferred" parameter settings. 
This is called the *posterior predictive* distribution: it is the data that the model *actually* predicts, accounting for our beliefs about parameters after seeing the actual data. 

Here we look at the posterior predictive of the coin flip model, summarizing both the model predictions and the data by the condition means (i.e. the percent 'yes' responses to each coin sequence).

~~~
;;;fold:
(define biascoin-model 
  (mem (lambda (sequence bias-weight)
         (enumeration-query

          (define fair-weight 0.5)

          (define isfair (flip))

          (define the-weight (if isfair fair-weight bias-weight))

          (define coin (lambda () 
                         (flip the-weight)))


          isfair

          (equal? sequence (repeat 5 coin))))))

(define experiment-data
  (list
   (list
    (list #t #t #t #t #t) 
    (list #t #t #t #t #f) 
    (list #t #t #t #f #t) 
    (list #t #t #t #f #f) 
    (list #t #t #f #t #t) 
    (list #t #t #f #t #f) 
    (list #t #t #f #f #t) 
    (list #t #t #f #f #f) 
    (list #t #f #t #t #t) 
    (list #t #f #t #t #f) 
    (list #t #f #t #f #t) 
    (list #t #f #t #f #f) 
    (list #t #f #f #t #t) 
    (list #t #f #f #t #f) 
    (list #t #f #f #f #t) 
    (list #t #f #f #f #f) 
    (list #f #t #t #t #t) 
    (list #f #t #t #t #f) 
    (list #f #t #t #f #t) 
    (list #f #t #t #f #f) 
    (list #f #t #f #t #t) 
    (list #f #t #f #t #f) 
    (list #f #t #f #f #t) 
    (list #f #t #f #f #f)
    (list #f #f #t #t #t) 
    (list #f #f #t #t #f) 
    (list #f #f #t #f #t) 
    (list #f #f #t #f #f) 
    (list #f #f #f #t #t) 
    (list #f #f #f #t #f) 
    (list #f #f #f #f #t) 
    (list #f #f #f #f #f))
   (list 
    (list #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #t #f #f #t #f) 
    (list #f #t #f #t #t #t #f #f #f #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #f #t #f #f #t #t #f #f #f #t #f #f #f #t #f #f #t #f #f #f #t #f #t #t #f #f #f #f #t #f) 
    (list #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #t #f #t) 
    (list #t #t #t #t #t #t #t #f #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #t #t #t #t #t #t #f #f #t #t #t #t #t #f #t #f #t #f #t #t #t #t #f #t #t #t #t #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #t #f #t #f #f #f #f #f #t #f) 
    (list #t #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #f #t #t #t #t #f #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #f #t #t #t #t #t #t #t #t #t #t #f #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #f #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f) 
    (list #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #t #t #f #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #f #t #f #f #t #f #f #f #f #f #f #f #f #f #f #t #t #f #f #f #t #f #f #f #f #f #f #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #t #t #t #t #t #t #f #f #t #f #t #t #f #t #f #t #t #t #t #t #f #t #t #t #t #f #f #t #f) 
    (list #f #t #t #t #t #t #t #f #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #f) 
    (list #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #f #t #t #t #t #f #t #t #t #t #f #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #f #t #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #f #t #f #f #t #t #t #f #t #f #t #t #f #f #t #f #t #f #f #t #t #f #f #t #f #f #t #f #t #t) 
    (list #f #t #f #t #t #t #f #f #f #t #t #t #t #f #t #f #t #t #t #f #t #t #f #t #t #t #t #f #t #f) 
    (list #f #t #f #t #t #t #t #f #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #f #f #t #t #f #f #t #f #f #f #t #f #t #f #t #f #t #f #t #f #t #t #f #f #t #f #t #t) 
    (list #t #t #t #t #t #t #f #f #t #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #t #t #f #f #t #f #f #f #t #t #f #f #f #f #f #f #t #f #f #f #t #f #f #t #f #f #f #f #t #f) 
    (list #f #t #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f))))

; list of sequences
(define all-seqs (first experiment-data))
; list of responses 
(define all-responses (second experiment-data))

(define get-probability-of-faircoin
  (lambda (dist selection)
    (define index (list-index (first dist) selection))
      (list-ref (second dist) index)))

; compute mean "fair-ness" for each sequence (human data)
(define summarize-data 
  (lambda (dataset)
    (list (first dataset)
          (map 
           (lambda (lst) (mean (map boolean->number lst)))
           (second dataset)))))

; compute mean "fair-ness" for each sequence (model predictions)
(define summarize-model
  (lambda (modelpreds)
    (list 
     all-seqs
     (map 
      (lambda (dist) 
        (get-probability-of-faircoin dist #t))
      modelpreds))))

; compute expected value of posterior distribution
(define expval-from-enum-analysis-of-enum-model 
  (lambda (results)
    (map sum 
         (transpose (map 
                     (lambda (lst prob)
                       (map (lambda (x)
                              (* prob x))
                            (second lst)))
                     (first results) 
                     (second results))))))
;;;

(define data-analysis 
  (lambda (experiment-data)
    (enumeration-query

     (define biased-weight 
       (uniform-draw (list 0.1 0.2 0.3 0.4 0.6 0.7 0.8 0.9)))

     ; generate predictions for all sequences
     (define cognitive-model-predictions
       (map 
        (lambda (sequence) 
          (biascoin-model sequence biased-weight)) 
        all-seqs))


     ; query for: what are the predictions of the model?
     (summarize-model cognitive-model-predictions)

     ; given that we've observed this data
     (factor (sum (flatten (map 
                            (lambda (data-for-one-sequence model)
                              (map (lambda (single-data-point)
                                     (log (get-probability-of-faircoin model single-data-point)))
                                   data-for-one-sequence))       
                            all-responses
                            cognitive-model-predictions)))))))

(define results (data-analysis experiment-data))

(define posterior-predictive (expval-from-enum-analysis-of-enum-model results))

(scatter 
 (zip 
  posterior-predictive
  (second (summarize-data experiment-data)))
 "data vs. model")

(barplot (list all-seqs posterior-predictive) 
  "model: probability of fair?")

(barplot (list all-seqs (second (summarize-data experiment-data))) 
  "data: proportion of fair responses")
~~~

Our model provides an ok explanation of the data set, but we see from the scatter plot that there are some clear mismatches: the sequences that the model thinks are most likely fair are ones that people think are not fair. 
Looking more closely, these are sequences with many T values, such as TTTTT.

To gain more intuition, play with the following data set, adjusting the responses (second list) as needed:

~~~
(define experiment-data
  (list 
   (list 
    (list false false false false false)
    (list false false false false true)
    (list false false false true true)
    (list false false true true true) 
    (list false true true true true)
    (list true true true true true))

   (list 
    (list #f #f #f)
    (list #f #f #t)
    (list #f #t #t)
    (list #f #t #t)
    (list #f #f #t)
    (list #f #f #f))))
~~~

What is the posterior over the `bias weight`? (Query for: `biased-weight` and call `barplot` and the output). How does the posterior predictive look? What can you conclude about our bias coin model (with respect to this data)?

# Response noise

Perhaps the cognitive model differs from the data in ways that aren't 'central' to the theory, that is in ways that we wouldn't want to include in the cognitive model per se, but would like to account for. A common case is *random guessing*: participants sometimes respond randomly, instead of attending to the task. 
We can capture this *response noise* by simply extending our model with the possibility that each response came from a random guess:

~~~
(define data-analysis
  (query
    (define cognitive-model-predictions (bc-model ...))
    (define guessing-parameter (uniform 0 1))

    (define thinking-plus-guessing 
      (lambda (guessing-parameter)
        (if (flip guessing-parameter)
            (flip 0.5)
            cognitive-model-predictions)))


    query-statement

    (condition 
      (equal? data (thinking-plus-guessing guessing-parameter)))))
~~~

This pseudo-code is saying there is some probability (or, equivalently, proportion of responses) that is attributable to response noise, or guessing; 
this probability is captured by `guessing-parameter`. It is the amount of the data that is better captured by guessing behavior than our cognitive model predictions.
Thus, the estimate of `guessing-parameter` can be thought of as a measure of the amount of data that the model can account for.

## Data analysis model with response noise

~~~
;;;fold:
(define (get-indices needle haystack)
  (define (loop rest-of-haystack index)
    (if (null? rest-of-haystack) '()
        (let ((rest-of-indices (loop (rest rest-of-haystack) (+ index 1))))
          (if (equal? (first rest-of-haystack) needle)
              (pair index rest-of-indices)
              rest-of-indices))))
  (loop haystack 1))

;; takes in the output of enumeration (a joint posterior)
;; and outputs the marginals
(define (marginalize output)
  (let ([states (first output)])
    (map (lambda (sub-output) 
           (let* ([probs (second output)]
                  [unique-states (unique sub-output)]
                  [unique-state-indices 
                   (map 
                    (lambda (x) (list x (get-indices x sub-output))) 
                    unique-states)])

             (list (map first unique-state-indices)
                   (map 
                    (lambda (y) (sum (map 
                                      (lambda (x) (list-elt probs x)) 
                                      (second y)))) 
                    unique-state-indices))))
         (transpose states))))

(define bc-model 
  (mem
   (lambda (sequence bias-weight)
     (enumeration-query

      (define fair-weight 0.5)
      (define isfair (flip))
      (define the-weight (if isfair fair-weight bias-weight))
      (define coin (lambda () (flip the-weight)))

      isfair

      (equal? sequence 
              (repeat 5 coin))))))

(define experiment-data
  (list
   (list
    (list #t #t #t #t #t) 
    (list #t #t #t #t #f) 
    (list #t #t #t #f #t) 
    (list #t #t #t #f #f) 
    (list #t #t #f #t #t) 
    (list #t #t #f #t #f) 
    (list #t #t #f #f #t) 
    (list #t #t #f #f #f) 
    (list #t #f #t #t #t) 
    (list #t #f #t #t #f) 
    (list #t #f #t #f #t) 
    (list #t #f #t #f #f) 
    (list #t #f #f #t #t) 
    (list #t #f #f #t #f) 
    (list #t #f #f #f #t) 
    (list #t #f #f #f #f) 
    (list #f #t #t #t #t) 
    (list #f #t #t #t #f) 
    (list #f #t #t #f #t) 
    (list #f #t #t #f #f) 
    (list #f #t #f #t #t) 
    (list #f #t #f #t #f) 
    (list #f #t #f #f #t) 
    (list #f #t #f #f #f)
    (list #f #f #t #t #t) 
    (list #f #f #t #t #f) 
    (list #f #f #t #f #t) 
    (list #f #f #t #f #f) 
    (list #f #f #f #t #t) 
    (list #f #f #f #t #f) 
    (list #f #f #f #f #t) 
    (list #f #f #f #f #f))
   (list 
    (list #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #t #f #f #t #f) 
    (list #f #t #f #t #t #t #f #f #f #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #f #t #f #f #t #t #f #f #f #t #f #f #f #t #f #f #t #f #f #f #t #f #t #t #f #f #f #f #t #f) 
    (list #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #t #f #t) 
    (list #t #t #t #t #t #t #t #f #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #t #t #t #t #t #t #f #f #t #t #t #t #t #f #t #f #t #f #t #t #t #t #f #t #t #t #t #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #t #f #t #f #f #f #f #f #t #f) 
    (list #t #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #f #t #t #t #t #f #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #f #t #t #t #t #t #t #t #t #t #t #f #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #f #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f) 
    (list #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #t #t #f #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #f #t #f #f #t #f #f #f #f #f #f #f #f #f #f #t #t #f #f #f #t #f #f #f #f #f #f #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #t #t #t #t #t #t #f #f #t #f #t #t #f #t #f #t #t #t #t #t #f #t #t #t #t #f #f #t #f) 
    (list #f #t #t #t #t #t #t #f #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #f) 
    (list #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #f #t #t #t #t #f #t #t #t #t #f #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #f #t #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #f #t #f #f #t #t #t #f #t #f #t #t #f #f #t #f #t #f #f #t #t #f #f #t #f #f #t #f #t #t) 
    (list #f #t #f #t #t #t #f #f #f #t #t #t #t #f #t #f #t #t #t #f #t #t #f #t #t #t #t #f #t #f) 
    (list #f #t #f #t #t #t #t #f #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #f #f #t #t #f #f #t #f #f #f #t #f #t #f #t #f #t #f #t #f #t #t #f #f #t #f #t #t) 
    (list #t #t #t #t #t #t #f #f #t #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #t #t #f #f #t #f #f #f #t #t #f #f #f #f #f #f #t #f #f #f #t #f #f #t #f #f #f #f #t #f) 
    (list #f #t #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f))))

; list of sequences
(define all-seqs (first experiment-data))
; list of responses 
(define all-responses (second experiment-data))

(define get-probability-of-faircoin
  (lambda (dist selection)
    (define index (list-index (first dist) selection))
    (list-ref (second dist) index)))

; takes the mean "true" responses for each sequence
(define summarize-data 
  (lambda (dataset)
    (list (first dataset)
          (map 
           (lambda (lst) (mean (map boolean->number lst)))
           (second dataset)))))

(define summarize-model
  (lambda (modelpreds)
    (list 
     all-seqs
     (map 
      (lambda (dist) 
        (get-probability-of-faircoin dist #t))
      modelpreds))))

(define expval-from-enum-analysis-of-enum-model 
  (lambda (results)
    (map sum 
         (transpose (map 
                     (lambda (lst prob)
                       (map (lambda (x)
                              (* prob x))
                            (second lst)))
                     (first results)
                     (second results))))))
;;;


(define thinking-and-guessing 
  (lambda (sequence bias-weight guessing-parameter)
    (enumeration-query
     (define thinking (bc-model sequence bias-weight))
     (define guessing (list '(#t #f) '(0.5 0.5)))
     (define response
       (if (flip guessing-parameter)
           guessing
           thinking))

     (apply multinomial response)

     true)))

(define data-analysis 
  (lambda (experiment-data)
    (enumeration-query

     (define biased-weight 
       (uniform-draw (list 0.1 0.2 0.3 0.4 0.6 0.7 0.8 0.9)))

     (define response-noise 
       (uniform-draw (list 0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1)))

     ; generate predictions for all sequences
     (define cognitive-model-predictions
       (map 
        (lambda (sequence) 
          (bc-model sequence biased-weight)) 
        all-seqs))

     (define cognitive-plus-noise-predictions
       (map 
        (lambda (sequence)
          (thinking-and-guessing sequence biased-weight response-noise))
        all-seqs))

     ; joint query: 
     ; what are the model predictions? including our model of noise
     ; what are the cognitive model predictions? (idealized; no noise)
     ; what is the response noise?
     ; what is the biased-weight?
     (list 
      (summarize-model cognitive-plus-noise-predictions)
      (summarize-model cognitive-model-predictions)
      response-noise
      biased-weight)

     ; given that we've observed this data
     (factor (sum (flatten 
                   ; map over all of the predictions over our cogmodel+noise
                   (map 
                    (lambda (data-for-one-sequence model)
                      ; map over data points in a given sequence
                      (map (lambda (single-data-point)
                             (log (get-probability-of-faircoin model single-data-point)))
                           data-for-one-sequence))
                    all-responses
                    cognitive-plus-noise-predictions)))))))

(define results (marginalize (data-analysis experiment-data)))

(define posterior-predictive-withNoise-results (first results))
(define posterior-predictive-sansNoise-results (second results))
(define posterior-noise (third results))
(define posterior-bias (fourth results))


(define posterior-predictive-withNoise
  (expval-from-enum-analysis-of-enum-model posterior-predictive-withNoise-results))

(define posterior-predictive-sansNoise
  (expval-from-enum-analysis-of-enum-model posterior-predictive-sansNoise-results))

(scatter 
 (zip 
  posterior-predictive-withNoise
  (second (summarize-data experiment-data)))
 "data vs. cognitive model (including noise)")

(barplot (list all-seqs posterior-predictive-withNoise) 
         "model (with noise): probability of fair?")
(barplot (list all-seqs posterior-predictive-sansNoise) 
         "model (sans noise): probability of fair?")

(barplot (list all-seqs (second (summarize-data experiment-data))) 
         "data: proportion of fair responses")

(barplot posterior-noise "posterior on response noise")

(barplot posterior-bias "posterior on biased-weight")
~~~

The posterior on response noise is peaked around 0.5. Our best guess is that fifty percent of the data comes from noise---does this seem high to you? 
What is the difference between the model with noise and the model without noise? (Theoretically, but also how do the predictions differ?)

Notice that while the scatter plot looks a bit better, our problem isn't really solved by factoring in response noise (though it is useful and informative to do so).
What is our problem again? Our model makes good predictions for most of these sequences, but is failing with: TTTTT, TTTTH, and so on.
Why might this be the case? To gain an intuition, let's reexamine the bias-weight parameter value. 
The biased-weight is peaked at 0.9 now. What does this mean in terms of our cognitive model?
Recall the biased-coin-model: it is a psychological theory that says subjects compare the sequence a fair coin would generate vs. one that a biased-coin would generate.
The biased-coin sequence has it's own weight, in this case the sequences it prefers are going to be sequences with lots of Heads (since our inferred weight is = 0.9).
This hints at a fundamental flaw of this model: it can only predict biased-sequences in one direction; 'unfair coin' responses for sequences going the other way have to get attributed to random response noise! How could we get around this issue? 


# Moving the coin-weight "into" the cognitive model 

We can posit that people don't entertain just one biased-weight value, but rather a distribution of possible values.
(Note: this is what Griffiths & Tenenbaum (2001) did.)
Thus, while our above data analysis model put uncertainty over the biased-weight, but assumed that people used only one weight, whatever it was, we now want to place this uncertainty *within* the cognitive model.
Forgetting about the data analysis model for the time being, we move the `biased-weight` parameter from outside the model to inside the model.
Let's examine the behavior of this model with respect to two sequences of interest.

~~~
(define enriched-biascoin-model 
  (lambda (sequence)
    (enumeration-query

     (define fair-weight 0.5)

     ; the subject has only a vague notion of what the bias in this biased-coin would be
     (define biased-weight 
       (uniform-draw (list 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9)))

     (define isfair (flip))

     (define the-weight (if isfair 
                            fair-weight 
                            biased-weight))

     (define coin (lambda () 
                    (flip the-weight)))


     isfair

     (equal? sequence (repeat 5 coin)))))

(barplot (enriched-biascoin-model (list true true true true true)) "HHHHH is fair?")
(barplot (enriched-biascoin-model (list false false false false false)) "TTTTT is fair?")
~~~

This model matches our intuition for the fairness of these sequences. Do you see why?
To gain more insight, let's look at the posterior over the bias weight for different observed sequences:

~~~
(define enriched-biascoin-model 
  (lambda (sequence)
    (enumeration-query

     (define fair-weight 0.5)
     (define biased-weight 
       (uniform-draw (list 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9)))

     (define isfair (flip))


     (define the-weight (if isfair 
                            fair-weight 
                            biased-weight))

     (define coin (lambda () 
                    (flip the-weight)))


     biased-weight

     (equal? sequence 
             (repeat 5 coin)))))

(barplot (enriched-biascoin-model (list true true true true true)) "what weight generated HHHHH")
(barplot (enriched-biascoin-model (list false false false false false)) "what weight generated TTTTT")
~~~

The model has the flexibility to infer different biased coin weights for different sequences. Let's now add back in the data analysis model to compare this to our data set.

~~~
;;;fold:
(define enriched-biascoin-model 
  (mem (lambda (sequence)
    (enumeration-query

     (define fair-weight 0.5)
     (define biased-weight 
       (uniform-draw (list 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9)))

     (define isfair (flip))


     (define the-weight (if isfair 
                            fair-weight 
                            biased-weight))

     (define coin (lambda () 
                    (flip the-weight)))


     isfair

     (equal? sequence (repeat 5 coin))))))

(define experiment-data
  (list
   (list
    (list #t #t #t #t #t) 
    (list #t #t #t #t #f) 
    (list #t #t #t #f #t) 
    (list #t #t #t #f #f) 
    (list #t #t #f #t #t) 
    (list #t #t #f #t #f) 
    (list #t #t #f #f #t) 
    (list #t #t #f #f #f) 
    (list #t #f #t #t #t) 
    (list #t #f #t #t #f) 
    (list #t #f #t #f #t) 
    (list #t #f #t #f #f) 
    (list #t #f #f #t #t) 
    (list #t #f #f #t #f) 
    (list #t #f #f #f #t) 
    (list #t #f #f #f #f) 
    (list #f #t #t #t #t) 
    (list #f #t #t #t #f) 
    (list #f #t #t #f #t) 
    (list #f #t #t #f #f) 
    (list #f #t #f #t #t) 
    (list #f #t #f #t #f) 
    (list #f #t #f #f #t) 
    (list #f #t #f #f #f)
    (list #f #f #t #t #t) 
    (list #f #f #t #t #f) 
    (list #f #f #t #f #t) 
    (list #f #f #t #f #f) 
    (list #f #f #f #t #t) 
    (list #f #f #f #t #f) 
    (list #f #f #f #f #t) 
    (list #f #f #f #f #f))
   (list 
    (list #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #t #f #f #t #f) 
    (list #f #t #f #t #t #t #f #f #f #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #f #t #f #f #t #t #f #f #f #t #f #f #f #t #f #f #t #f #f #f #t #f #t #t #f #f #f #f #t #f) 
    (list #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #t #f #t) 
    (list #t #t #t #t #t #t #t #f #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #t #t #t #t #t #t #f #f #t #t #t #t #t #f #t #f #t #f #t #t #t #t #f #t #t #t #t #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #t #f #t #f #f #f #f #f #t #f) 
    (list #t #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #f #t #t #t #t #f #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #f #t #t #t #t #t #t #t #t #t #t #f #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #f #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f) 
    (list #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #t #t #f #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #f #t #f #f #t #f #f #f #f #f #f #f #f #f #f #t #t #f #f #f #t #f #f #f #f #f #f #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #t #t #t #t #t #t #f #f #t #f #t #t #f #t #f #t #t #t #t #t #f #t #t #t #t #f #f #t #f) 
    (list #f #t #t #t #t #t #t #f #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #f) 
    (list #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #f #t #t #t #t #f #t #t #t #t #f #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #f #t #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #f #t #f #f #t #t #t #f #t #f #t #t #f #f #t #f #t #f #f #t #t #f #f #t #f #f #t #f #t #t) 
    (list #f #t #f #t #t #t #f #f #f #t #t #t #t #f #t #f #t #t #t #f #t #t #f #t #t #t #t #f #t #f) 
    (list #f #t #f #t #t #t #t #f #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #f #f #t #t #f #f #t #f #f #f #t #f #t #f #t #f #t #f #t #f #t #t #f #f #t #f #t #t) 
    (list #t #t #t #t #t #t #f #f #t #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #t #t #f #f #t #f #f #f #t #t #f #f #f #f #f #f #t #f #f #f #t #f #f #t #f #f #f #f #t #f) 
    (list #f #t #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f))))

(define all-seqs (first experiment-data))
(define all-responses (second experiment-data))

(define get-probability-of-faircoin
  (lambda (dist selection)
    (let ([index (list-index (first dist) selection)])
      (list-ref (second dist) index))))

(define summarize-data 
  (lambda (dataset)
    (list (first dataset)
          (map 
           (lambda (lst) (mean (map boolean->number lst)))
           (second dataset)))))

(define summarize-model
  (lambda (modelpreds)
    (list 
     all-seqs
     (map 
      (lambda (dist) 
        (get-probability-of-faircoin dist #t))
      modelpreds))))

(define expval-from-enum-analysis-of-enum-model 
  (lambda (results)
    (map sum 
         (transpose (map 
                     (lambda (lst prob)
                       (map (lambda (x)
                              (* prob x))
                            (second lst)))
                     (first results) 
                     (second results))))))
;;;

(define data-analysis 
  (lambda (experiment-data)
    (enumeration-query


     ; generate predictions for all sequences
     (define cognitive-model-predictions
       (map 
        (lambda (sequence) 
          (enriched-biascoin-model sequence)) 
        all-seqs))


     ; what are the model predictions?
     (summarize-model cognitive-model-predictions)

     ; given that we've observed this data
     (factor (sum (flatten (map 
                            (lambda (data-for-one-sequence model)
                              ; map over data points in a given sequence
                              (map (lambda (single-data-point)
                                     (log (get-probability-of-faircoin model single-data-point)))
                                   data-for-one-sequence))       
                            (second experiment-data)
                            cognitive-model-predictions)))))))

(define results (data-analysis experiment-data))

(define posterior-predictive (expval-from-enum-analysis-of-enum-model results))

(scatter 
 (zip 
  posterior-predictive
  (second (summarize-data experiment-data)))
 "data vs. model")

(barplot (list all-seqs posterior-predictive) 
  "model: probability of fair?")

(barplot (list all-seqs (second (summarize-data experiment-data))) 
  "data: proportion of fair responses")
~~~

This is great. The model doesn't suffer from the same *lower-bias* flaw that it did previously.
Notice that our cognitive model now has no parameters for the data analysis model to infer; the posterior predictive is thus the same as what's known as the 'prior predictive', that is, what we would expect before seeing any data. 

## Generalized, enriched bias coin model 

We can further extend the above cognitive model. For instance, while it appears that people consider different weights for biased coins, do they have an overall bias to prefer heads-weighted or tails-weighted coins? We can explore this hypothesis by generalizing the uniform distribution to some distribution of coin weights, whose mean and variance have uncertainty. If the uncertain mean and variance are outside the cognitive model, then they are scientific hypotheses about the bias people bring to this problem; if they are inside, they represent the hypothesis that people reason about the mean and variance of the prior distribution.

Here's a model extended to capture uncertainty about the biases of participants. Note: This will take about 30 seconds to run.

~~~
;;;fold:
(define discretize-beta 
  (lambda (gamma delta bins)
    (define shape_alpha (* gamma delta))
    (define shape_beta (* (- 1 gamma) delta))
    (define beta-pdf (lambda (x) 
                       (*
                        (pow x (- shape_alpha 1))
                        (pow (- 1 x) (- shape_beta 1)))))
    (map beta-pdf bins)))

(define bins '(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9))

(define (get-indices needle haystack)
  (define (loop rest-of-haystack index)
    (if (null? rest-of-haystack) '()
        (let ((rest-of-indices (loop (rest rest-of-haystack) (+ index 1))))
          (if (equal? (first rest-of-haystack) needle)
              (pair index rest-of-indices)
              rest-of-indices))))
  (loop haystack 1))

(define (list-map lst)
  (if (all (map null? (map rest lst))) 
      lst
      (list (map first lst) (list-map (map rest lst)))))

(define (marginalize output)
  (let ([states (first output)])
    (map (lambda (sub-output) 
           (let* ([probs (second output)]
                  [unique-states (unique sub-output)]
                  [unique-state-indices 
                   (map 
                    (lambda (x) (list x (get-indices x sub-output))) 
                    unique-states)])

             (list (map first unique-state-indices)
                   (map 
                    (lambda (y) (sum (map 
                                      (lambda (x) (list-elt probs x)) 
                                      (second y)))) 
                    unique-state-indices))))

         (transpose states))))


(define experiment-data
  (list
   (list
    (list #t #t #t #t #t) 
    (list #t #t #t #t #f) 
    (list #t #t #t #f #t) 
    (list #t #t #t #f #f) 
    (list #t #t #f #t #t) 
    (list #t #t #f #t #f) 
    (list #t #t #f #f #t) 
    (list #t #t #f #f #f) 
    (list #t #f #t #t #t) 
    (list #t #f #t #t #f) 
    (list #t #f #t #f #t) 
    (list #t #f #t #f #f) 
    (list #t #f #f #t #t) 
    (list #t #f #f #t #f) 
    (list #t #f #f #f #t) 
    (list #t #f #f #f #f) 
    (list #f #t #t #t #t) 
    (list #f #t #t #t #f) 
    (list #f #t #t #f #t) 
    (list #f #t #t #f #f) 
    (list #f #t #f #t #t) 
    (list #f #t #f #t #f) 
    (list #f #t #f #f #t) 
    (list #f #t #f #f #f)
    (list #f #f #t #t #t) 
    (list #f #f #t #t #f) 
    (list #f #f #t #f #t) 
    (list #f #f #t #f #f) 
    (list #f #f #f #t #t) 
    (list #f #f #f #t #f) 
    (list #f #f #f #f #t) 
    (list #f #f #f #f #f))
   (list 
    (list #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #t #f #f #t #f) 
    (list #f #t #f #t #t #t #f #f #f #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #f #t #f #f #t #t #f #f #f #t #f #f #f #t #f #f #t #f #f #f #t #f #t #t #f #f #f #f #t #f) 
    (list #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #t #f #t) 
    (list #t #t #t #t #t #t #t #f #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #t #t #t #t #t #t #f #f #t #t #t #t #t #f #t #f #t #f #t #t #t #t #f #t #t #t #t #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #t #f #t #f #f #f #f #f #t #f) 
    (list #t #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #f #t #t #t #t #f #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #f #t #t #t #t #t #t #t #t #t #t #f #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #f #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f) 
    (list #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #t #t #f #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #f #t #f #f #t #f #f #f #f #f #f #f #f #f #f #t #t #f #f #f #t #f #f #f #f #f #f #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #t #t #t #t #t #t #f #f #t #f #t #t #f #t #f #t #t #t #t #t #f #t #t #t #t #f #f #t #f) 
    (list #f #t #t #t #t #t #t #f #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #f) 
    (list #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #f #t #t #t #t #f #t #t #t #t #f #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #f #t #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #f #t #f #f #t #t #t #f #t #f #t #t #f #f #t #f #t #f #f #t #t #f #f #t #f #f #t #f #t #t) 
    (list #f #t #f #t #t #t #f #f #f #t #t #t #t #f #t #f #t #t #t #f #t #t #f #t #t #t #t #f #t #f) 
    (list #f #t #f #t #t #t #t #f #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #f #f #t #t #f #f #t #f #f #f #t #f #t #f #t #f #t #f #t #f #t #t #f #f #t #f #t #t) 
    (list #t #t #t #t #t #t #f #f #t #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #t #t #f #f #t #f #f #f #t #t #f #f #f #f #f #f #t #f #f #f #t #f #f #t #f #f #f #f #t #f) 
    (list #f #t #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f))))


; list of sequences
(define all-seqs (first experiment-data))
; list of responses 
(define all-responses (second experiment-data))

(define get-probability-of-faircoin
  (lambda (dist selection)
    (define index (list-index (first dist) selection))
    (list-ref (second dist) index)))

(define summarize-data 
  (lambda (dataset)
    (list (first dataset)
          (map 
           (lambda (lst) (mean (map boolean->number lst)))
           (second dataset)))))

(define summarize-model
  (lambda (modelpreds)
    (list 
     all-seqs
     (map 
      (lambda (dist) 
        (get-probability-of-faircoin dist #t))
      modelpreds))))

(define expval-from-enum-analysis-of-enum-model 
  (lambda (results)
    (map sum 
         (transpose (map 
                     (lambda (lst prob)
                       (map (lambda (x)
                              (* prob x))
                            (second lst)))
                     (first results)
                     (second results))))))
;;;

(define generalized-biascoin-model 
  (mem (lambda (sequence gamma delta)
         (enumeration-query

          (define fair-weight 0.5)

          (define biased-weight
            (multinomial bins (discretize-beta gamma delta bins)))

          ; (define biased-weight 
          ;    (uniform-draw (list 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9)))

          (define isfair (flip))


          (define the-weight (if isfair 
                                 fair-weight 
                                 biased-weight))

          (define coin (lambda () 
                         (flip the-weight)))

          isfair

          (equal? sequence 
                  (repeat 5 coin))))))

(define data-analysis 
  (lambda (experiment-data)
    (enumeration-query

     (define gamma (uniform-draw (list 0.1 0.3 0.5 0.7 0.9)))
     (define delta (uniform-draw (list 0.1 0.5 1 3 7 15)))

     ; generate predictions for all sequences
     (define cognitive-model-predictions
       (map 
        (lambda (sequence) 
          (generalized-biascoin-model sequence gamma delta)) 
        all-seqs))


     ; what are the model predictions?
     (list 
      (summarize-model cognitive-model-predictions)
      gamma
      delta)

     ; given that we've observed this data
     (factor (sum (flatten (map 
                            (lambda (data-for-one-sequence model)
                              ; map over data points in a given sequence
                              (map (lambda (single-data-point)
                                     (log (get-probability-of-faircoin model single-data-point)))
                                   data-for-one-sequence))       
                            (second experiment-data)
                            cognitive-model-predictions)))))))



(define results (marginalize (data-analysis experiment-data)))


(define posterior-predictive-results (first results))
(define posterior-gamma (second results))
(define posterior-delta (third results))
(define posterior-predictive (expval-from-enum-analysis-of-enum-model posterior-predictive-results))

(scatter 
 (zip 
  posterior-predictive
  (second (summarize-data experiment-data)))
 "data vs. cognitive model")

(barplot (list all-seqs posterior-predictive) 
         "cognitive model: probability of fair?")

(barplot (list all-seqs (second (summarize-data experiment-data))) 
         "data: proportion of fair responses")

(barplot posterior-gamma "posterior on mean biased-weight")
(barplot posterior-delta "posterior on variance of biased-weight")
~~~

Does it appear that participants in our experiment have an overall bias toward heads or tails?
While this model does even better at predicting the data, it is not perfect.
Let's see what happens when we factor in response noise to determine whether thats a better explanation for some of the data.
Note: This will also take a while. Chrome may ask you to kill the page; power through.

~~~
;;;fold:
(define discretize-beta 
  (lambda (gamma delta bins)
    (define shape_alpha (* gamma delta))
    (define shape_beta (* (- 1 gamma) delta))
    (define beta-pdf (lambda (x) 
                       (*
                        (pow x (- shape_alpha 1))
                        (pow (- 1 x) (- shape_beta 1)))))
    (map beta-pdf bins)))

(define bins '(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9))

(define (get-indices needle haystack)
  (define (loop rest-of-haystack index)
    (if (null? rest-of-haystack) '()
        (let ((rest-of-indices (loop (rest rest-of-haystack) (+ index 1))))
          (if (equal? (first rest-of-haystack) needle)
              (pair index rest-of-indices)
              rest-of-indices))))
  (loop haystack 1))

(define (list-map lst)
  (if (all (map null? (map rest lst))) 
      lst
      (list (map first lst) (list-map (map rest lst)))))

(define (marginalize output)
  (let ([states (first output)])
    (map (lambda (sub-output) 
           (let* ([probs (second output)]
                  [unique-states (unique sub-output)]
                  [unique-state-indices 
                   (map 
                    (lambda (x) (list x (get-indices x sub-output))) 
                    unique-states)])

             (list (map first unique-state-indices)
                   (map 
                    (lambda (y) (sum (map 
                                      (lambda (x) (list-elt probs x)) 
                                      (second y)))) 
                    unique-state-indices))))

         (transpose states))))


(define experiment-data
  (list
   (list
    (list #t #t #t #t #t) 
    (list #t #t #t #t #f) 
    (list #t #t #t #f #t) 
    (list #t #t #t #f #f) 
    (list #t #t #f #t #t) 
    (list #t #t #f #t #f) 
    (list #t #t #f #f #t) 
    (list #t #t #f #f #f) 
    (list #t #f #t #t #t) 
    (list #t #f #t #t #f) 
    (list #t #f #t #f #t) 
    (list #t #f #t #f #f) 
    (list #t #f #f #t #t) 
    (list #t #f #f #t #f) 
    (list #t #f #f #f #t) 
    (list #t #f #f #f #f) 
    (list #f #t #t #t #t) 
    (list #f #t #t #t #f) 
    (list #f #t #t #f #t) 
    (list #f #t #t #f #f) 
    (list #f #t #f #t #t) 
    (list #f #t #f #t #f) 
    (list #f #t #f #f #t) 
    (list #f #t #f #f #f)
    (list #f #f #t #t #t) 
    (list #f #f #t #t #f) 
    (list #f #f #t #f #t) 
    (list #f #f #t #f #f) 
    (list #f #f #f #t #t) 
    (list #f #f #f #t #f) 
    (list #f #f #f #f #t) 
    (list #f #f #f #f #f))
   (list 
    (list #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #t #f #f #t #f) 
    (list #f #t #f #t #t #t #f #f #f #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #f #t #f #f #t #t #f #f #f #t #f #f #f #t #f #f #t #f #f #f #t #f #t #t #f #f #f #f #t #f) 
    (list #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #t #f #t) 
    (list #t #t #t #t #t #t #t #f #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #t #t #t #t #t #t #f #f #t #t #t #t #t #f #t #f #t #f #t #t #t #t #f #t #t #t #t #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #t #f #t #f #f #f #f #f #t #f) 
    (list #t #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #f #t #t #t #t #f #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #f #t #t #t #t #t #t #t #t #t #t #f #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #f #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f) 
    (list #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #t #t #f #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #f #t #f #f #t #f #f #f #f #f #f #f #f #f #f #t #t #f #f #f #t #f #f #f #f #f #f #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #t #t #t #t #t #t #f #f #t #f #t #t #f #t #f #t #t #t #t #t #f #t #t #t #t #f #f #t #f) 
    (list #f #t #t #t #t #t #t #f #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #f) 
    (list #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #f #t #t #t #t #f #t #t #t #t #f #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #f #t #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #f #t #f #f #t #t #t #f #t #f #t #t #f #f #t #f #t #f #f #t #t #f #f #t #f #f #t #f #t #t) 
    (list #f #t #f #t #t #t #f #f #f #t #t #t #t #f #t #f #t #t #t #f #t #t #f #t #t #t #t #f #t #f) 
    (list #f #t #f #t #t #t #t #f #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #f #f #t #t #f #f #t #f #f #f #t #f #t #f #t #f #t #f #t #f #t #t #f #f #t #f #t #t) 
    (list #t #t #t #t #t #t #f #f #t #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #t #t #f #f #t #f #f #f #t #t #f #f #f #f #f #f #t #f #f #f #t #f #f #t #f #f #f #f #t #f) 
    (list #f #t #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f))))

; list of sequences
(define all-seqs (first experiment-data))
; list of responses 
(define all-responses (second experiment-data))

(define get-probability-of-faircoin
  (lambda (dist selection)
    (define index (list-index (first dist) selection))
    (list-ref (second dist) index)))

(define summarize-data 
  (lambda (dataset)
    (list (first dataset)
          (map 
           (lambda (lst) (mean (map boolean->number lst)))
           (second dataset)))))

(define summarize-model
  (lambda (modelpreds)
    (list 
     all-seqs
     (map 
      (lambda (dist) 
        (get-probability-of-faircoin dist #t))
      modelpreds))))


(define expval-from-enum-analysis-of-enum-model 
  (lambda (results)
    (map sum 
         (transpose (map 
                     (lambda (lst prob)
                       (map (lambda (x)
                              (* prob x))
                            (second lst)))
                     (first results)
                     (second results))))))
;;;


(define bc-model 
  (mem 
   (lambda (sequence gamma delta)
     (enumeration-query

      (define fair-weight 0.5)

      (define biased-weight
        (multinomial bins (discretize-beta gamma delta bins)))

      ; (define biased-weight 
      ;    (uniform-draw (list 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9)))

      (define isfair (flip))


      (define the-weight (if isfair 
                             fair-weight 
                             biased-weight))

      (define coin (lambda () 
                     (flip the-weight)))


      isfair

      (equal? sequence 
              (repeat 5 coin))))))

(define thinking-and-guessing 
  (lambda (sequence gamma delta guessing-parameter)
    (enumeration-query
     (define thinking (bc-model sequence gamma delta))
     (define guessing (list '(#t #f) '(0.5 0.5)))
     (define response
       (if (flip guessing-parameter)
           guessing
           thinking))

     (apply multinomial response)

     true)))

(define data-analysis 
  (lambda (experiment-data)
    (enumeration-query

     (define gamma (uniform-draw (list 0.1 0.3 0.5 0.7 0.9)))
     (define delta (uniform-draw (list 0.1 0.5 1 3 7 15)))

     ; generate predictions for all sequences
     (define cognitive-model-predictions
       (map 
        (lambda (sequence) 
          (bc-model sequence gamma delta)) 
        all-seqs))

     (define response-noise (uniform-draw (list 0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1)))


     (define cognitive-plus-noise-predictions
       (map 
        (lambda (sequence)
          (thinking-and-guessing sequence gamma delta response-noise))
        all-seqs))


     ; what are the model predictions?
     (list 
      (summarize-model cognitive-plus-noise-predictions)
      (summarize-model cognitive-model-predictions)
      response-noise
      gamma
      delta)

     ; given that we've observed this data
     (factor (sum (flatten (map 
                            (lambda (data-for-one-sequence model)
                              ; map over data points in a given sequence
                              (map (lambda (single-data-point)
                                     (log (get-probability-of-faircoin model single-data-point)))
                                   data-for-one-sequence))       
                            all-responses
                            cognitive-plus-noise-predictions)))))))



(define results (marginalize (data-analysis experiment-data)))

(define posterior-predictive-withNoise-results (first results))
(define posterior-predictive-sansNoise-results (second results))
(define posterior-noise (third results))
(define posterior-gamma (fourth results))
(define posterior-delta (fifth results))

(define posterior-predictive-withNoise 
  (expval-from-enum-analysis-of-enum-model posterior-predictive-withNoise-results))

(define posterior-predictive-sansNoise 
  (expval-from-enum-analysis-of-enum-model posterior-predictive-sansNoise-results))

(scatter 
 (zip 
  posterior-predictive-withNoise
  (second (summarize-data experiment-data)))
 "data vs. cognitive model")

(barplot (list all-seqs posterior-predictive-withNoise) 
         "cognitive model (with noise): probability of fair?")
(barplot (list all-seqs posterior-predictive-sansNoise) 
         "cognitive model (sans noise): probability of fair?")
(barplot (list all-seqs (second (summarize-data experiment-data))) 
         "data: proportion of fair responses")

(barplot posterior-noise "posterior on noise parameter")
(barplot posterior-gamma "posterior on mean biased-weight")
(barplot posterior-delta "posterior on variance of biased-weight")
~~~

How much of the data must be explained as noise in this extended model?

The posteriors over the mean and variance of the biased-weight are interesting. These are parameters of a [beta](http://en.wikipedia.org/wiki/Beta_distribution) distribution. The mean value tells us that the there might be a bias towards seeing more Heads as more biased.  The variance value is small, which indicates that the resulting distribution is U-shaped, with peaks at high values and low values. This should match your intuition for the underlying cognitive model. The true prior over biased-coin rates is peaked at the extremes; and we've backed that out from our data!

# Model selection

We've explored a number of different models and seen that some seem better, explaining more of the data, though they differ in their complexity. How can we quantify which model is  better? We can set up the question like this: we, as scientists, are a priori uncertain which cognitive model actually gave rise to the data we have collected; after seeing the data, how do our beliefs about the correct model change? In a way, this is no different than the inference problems we've faced before.
In pseudocode this might look like:

~~~
(define model-comparion
  (query
    (define model-1 (simple-bc-model ...))
    (define model-2 (complex-bc-model ...))

    (define is-model-1? (flip 0.5))

    (define best-model
        (if is-model1?
            model-1
            model-2))

    is-model-1?

    (condition 
      (equal? data (best-model)))))
~~~


Let's try to write this in full:

~~~
;;;fold:
(define experiment-data
  (list
   (list
    (list #t #t #t #t #t) 
    (list #t #t #t #t #f) 
    (list #t #t #t #f #t) 
    (list #t #t #t #f #f) 
    (list #t #t #f #t #t) 
    (list #t #t #f #t #f) 
    (list #t #t #f #f #t) 
    (list #t #t #f #f #f) 
    (list #t #f #t #t #t) 
    (list #t #f #t #t #f) 
    (list #t #f #t #f #t) 
    (list #t #f #t #f #f) 
    (list #t #f #f #t #t) 
    (list #t #f #f #t #f) 
    (list #t #f #f #f #t) 
    (list #t #f #f #f #f) 
    (list #f #t #t #t #t) 
    (list #f #t #t #t #f) 
    (list #f #t #t #f #t) 
    (list #f #t #t #f #f) 
    (list #f #t #f #t #t) 
    (list #f #t #f #t #f) 
    (list #f #t #f #f #t) 
    (list #f #t #f #f #f)
    (list #f #f #t #t #t) 
    (list #f #f #t #t #f) 
    (list #f #f #t #f #t) 
    (list #f #f #t #f #f) 
    (list #f #f #f #t #t) 
    (list #f #f #f #t #f) 
    (list #f #f #f #f #t) 
    (list #f #f #f #f #f))
   (list 
    (list #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #t #f #f #t #f) 
    (list #f #t #f #t #t #t #f #f #f #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #f #t #f #f #t #t #f #f #f #t #f #f #f #t #f #f #t #f #f #f #t #f #t #t #f #f #f #f #t #f) 
    (list #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #t #f #t) 
    (list #t #t #t #t #t #t #t #f #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #t #t #t #t #t #t #f #f #t #t #t #t #t #f #t #f #t #f #t #t #t #t #f #t #t #t #t #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #t #f #t #f #f #f #f #f #t #f) 
    (list #t #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #f #t #t #t #t #f #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #f #t #t #t #t #t #t #t #t #t #t #f #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #f #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f) 
    (list #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #t #t #f #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #f #t #f #f #t #f #f #f #f #f #f #f #f #f #f #t #t #f #f #f #t #f #f #f #f #f #f #f #t #f) 
    (list #f #t #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #t #t #t #t #t #t #f #f #t #f #t #t #f #t #f #t #t #t #t #t #f #t #t #t #t #f #f #t #f) 
    (list #f #t #t #t #t #t #t #f #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #f) 
    (list #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #f #t #t #t #t #f #t #t #t #t #f #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #f #t #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #f #t #f #f #t #t #t #f #t #f #t #t #f #f #t #f #t #f #f #t #t #f #f #t #f #f #t #f #t #t) 
    (list #f #t #f #t #t #t #f #f #f #t #t #t #t #f #t #f #t #t #t #f #t #t #f #t #t #t #t #f #t #f) 
    (list #f #t #f #t #t #t #t #f #t #t #t #t #t #f #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t #t) 
    (list #t #t #f #f #t #t #f #f #t #f #f #f #t #f #t #f #t #f #t #f #t #f #t #t #f #f #t #f #t #t) 
    (list #t #t #t #t #t #t #f #f #t #t #t #t #t #t #t #f #t #f #t #t #t #t #t #t #t #t #t #f #t #f) 
    (list #t #t #f #f #t #f #f #f #t #t #f #f #f #f #f #f #t #f #f #f #t #f #f #t #f #f #f #f #t #f) 
    (list #f #t #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f) 
    (list #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #t #f #f #f #f #f #f #f #f #f #f #f #t #f))))

; list of sequences
(define all-seqs (first experiment-data))
; list of responses 
(define all-responses (second experiment-data))

(define get-probability-of-faircoin
  (lambda (dist selection)
    (define index (list-index (first dist) selection))
    (list-ref (second dist) index)))

(define summarize-data 
  (lambda (dataset)
    (list (first dataset)
          (map 
           (lambda (lst) (mean (map boolean->number lst)))
           (second dataset)))))

(define summarize-model
  (lambda (modelpreds)
    (list 
     all-seqs
     (map 
      (lambda (dist) 
        (get-probability-of-faircoin dist #t))
      modelpreds))))

;;;

(define complex-bc-model 
  (mem (lambda (sequence)
         (enumeration-query

          (define fair-weight 0.5)
          (define biased-weight 
            (uniform-draw (list 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9)))

          (define isfair (flip))
          (define the-weight (if isfair 
                                 fair-weight 
                                 biased-weight))

          (define coin (lambda () 
                         (flip the-weight)))


          isfair

          (equal? sequence (repeat 5 coin))))))


(define simple-bc-model 
  (mem
   (lambda (sequence bias-weight)
     (enumeration-query

      (define fair-weight 0.5)
      (define isfair (flip))
      (define the-weight (if isfair fair-weight bias-weight))
      (define coin (lambda () (flip the-weight)))

      isfair

      (equal? sequence (repeat 5 coin))))))



(define model-comparison 
  (lambda (experiment-data)
    (enumeration-query

     (define biased-weight 
       (uniform-draw (list 0.1 0.2 0.3 0.4 0.6 0.7 0.8 0.9)))



     (define is-model1? (flip 0.5))

     ; model1: generate predictions for all sequences
     (define model1
       (map 
        (lambda (sequence) 
          (simple-bc-model sequence biased-weight)) 
        all-seqs))

     ; model2: generate predictions for all sequences
     (define model2
       (map 
        (lambda (sequence) 
          (complex-bc-model sequence))
        all-seqs))

     (define best-model (if is-model1?
                            model1
                            model2))

     ; which is the best model?
     is-model1?

     ; given that we've observed this data
     (factor (sum (flatten (map 
                            (lambda (data-for-one-sequence model)
                              ; map over data points in a given sequence
                              (map (lambda (single-data-point)
                                     (log (get-probability-of-faircoin model single-data-point)))
                                   data-for-one-sequence))       
                            (second experiment-data)
                            best-model)))))))

(define results (model-comparison experiment-data))

(barplot results "is model 1 the best?")
~~~

Our data strongly favors the more complex model: we can be very confident in the more complex model being the better of the two, given our data and analysis assumptions.


# Exercises

**1. Subjective randomness digest.** We saw in this chapter how to analyze our Bayesian models of cognition by using Bayesian statistical techniques. Pick either the enriched bias coin model or the generalized enriched bias coin model. What phenomena was it able to capture? What wasn't it able to capture? How can you tell? Do you have an idea for a better model?

**2. Bayes in the head vs. Bayes in the notebook.** We've seen in this chapter how we can precisely separate assumptions about our computational-level theory of cognition from the assumptions that go into analyzing our data (and our theory). In this exercise, we will try to go between the two ways of looking at these things: by going from a theory and analysis in words, to a theory and analysis in Church (and back).
	
Consider the [reflectance and luminance model](https://probmods.org/patterns-of-inference.html#a-case-study-in-modularity-visual-perception-of-surface-lightness-and-color) from Chapter 4. This model captured the illusion of increased reflectance in terms of explaining away the observed luminance by the observed decrease in illumination (caused by the shadow). Here is the model again
	
~~~
(define observed-luminance 3.0)

(define samples
   (mh-query
    1000 10

    (define reflectance (gaussian 1 1))
    (define illumination (gaussian 3 0.5))
    (define luminance (* reflectance illumination))

    reflectance

    ;true))
    (condition (= luminance (gaussian observed-luminance 0.1))))) ; luminance is a property of the scene
    ;(condition (= illumination (gaussian 0.5  0.1))) this illumination represents the shadow

(display (list "Mean reflectance:" (mean samples)))
(hist samples "Reflectance")
~~~
	
Here I have included a commented `true` condition to make it easy for you to explore this model. 
	
**A. Warmup: What does the prior for reflectance look like? How does the prior change when we condition on the observed luminance? What happens when you condition on the observed shadow as well?**
	
Just as a reminder, the illusion is observed in the model when we condition on this statement about illumination  `(condition (= illumination (gaussian 0.5  0.1)))`, which is a stand-in for the effect of the shadow from the cylinder on the scene.
	
**B. How many parameters does this model of perception have?** (Hint: Go through each `define` and `condition`: Are the constituent variables of the statements (a) modeling assumptions or (b) part of the experimental setup / manipulation) **For all of the variables you've categorized as (a), which ones do you think refer to aspects of the perceptual system and which refer to aspects of the environment? What do you think these parameters represent in terms of the perceptual system or environment? (Feel free to use super general, even colloquial, terms to answer this.)**
	
**C.** Replace the hard-coded parameters of this model with variables, defined outside the query. Give them the most intuitive names you can fashion. Use this starter (pseudo) code.
	
	
~~~
(define parameter1 ...)
(define parameter2 ...)
;...

(define observed-luminance 3.0)

(query

 (define reflectance (gaussian 1 1))
 (define illumination (gaussian 3 0.5))
 (define luminance (* reflectance illumination))

 reflectance

 (= luminance (gaussian observed-luminance 0.1))))
~~~
	
**D. Are all of these parameters independent?** (If you had to specify values for them, would you have to consider values of other parameters when specifying them?) If two are not independent, can you think of a reparameterization that would be more independent? (Hint: If you have two non-independent parameters, you could keep only one of them and introduce a parameter specifying the relation between the two. E.g., two points that are linearly associated can be expressed as an one of them and the distance between them).
	
**E.** Writing data analysis models requires specifying priors over parameters. Without much prior knowledge in a domain, we want to pick priors that make the fewest assumptions. A good place to start is to think about the possible values the parameter could take on. **For each parameter, write down what you know about the possible values it could take on.
	
**F.** We're now in a position to write a data analysis model. The most common distributional forms for priors are [uniform](http://en.wikipedia.org/wiki/Uniform_distribution_(continuous)), [gaussian](http://en.wikipedia.org/wiki/Normal_distribution), [beta](http://en.wikipedia.org/wiki/Beta_distribution), and [exponential](http://en.wikipedia.org/wiki/Exponential_distribution). Put priors on your parameters from part C. Use this starter (pseudo) code.
	
~~~
(define perceptual-model
  (lambda (parameter1 parameter2 ...))
  (query

   ; fill in, copying where appropriate from the original model specification
   (define reflectance ...)
   (define illumination ...)
   (define luminance (* reflectance illumination))

   reflectance

   (= luminance ...))))



(define data-analysis-model
  (query
   ; replace with parameters you specified in Part C
   ; put priors over parameters
   (define parameter1 ...)
   (define parameter2 ...)
   (define ...)
   (define perceptual-model-predictions 
     (perceptual-model parameter1 parameter2 ...))
    
   ;;; what are you going to query for?
   ...  
   
   (condition (= experimental-data perceptual-model-predictions))))


~~~ 
	
**G.** What are you going to query for? Add it to your pseudocode above. What do each of things that you are querying for in the data analysis model represent?


**3. Parameter fitting vs. Parameter integration** One of the strongest motivations for using Bayesian techniques for model-data evaluation is in how "nuisance" parameters are treated. "Nuisance" parameters are parameters of no theoretical interest; their only purpose is to fill in a necessary slot in the model. Classically, the most prominant technique (from the frequentist tradition) for dealing with these parameters is to fit them to the data, i.e., to set their value equal to whatever value maximizes the model-data fit (or, equivalently, minimizes some cost function). 

The Bayesian approach is different. Since we have *a priori* uncertainty about the value of our parameter (e.g. as you specified in Part F of Exercise 2), we will also have *a posteriori* uncertainty about the value (though hopefully the uncertainty will be a little less). What the Bayesian does is *integrate* over her posterior distribution of parameter values to make predictions. Intuitively, rather than taking the value corresponding to the peak of the distribution, she's considering all values with their respective probabilites.
	
Why might this be important for model assessment? Imagine the following situation. You are piloting a task. You think that the task you've design is a little too difficult for subjects. (Let's imagine that you're a psychophysicist, and your task pertains to contrast discriminiation in the periphery.) You think the current task design is too difficult, but you're uncertain. It may well be that it's fine for subjects. We're going to think about this in terms of subjects ability with respect to your task. Here is your prior.

~~~
;; Prior on task diffuclty is uniform on 0..0.9, with a spike on 0.9

(define (task-difficulty-prior)
  (if (flip) .9 (/ (sample-integer 10) 10)))

(barplot (enumeration-query (task-difficulty-prior) true) 
         "Prior on task difficulty")
~~~

You have a model of how subjects perform on your task. You could have a structured, probabilistic model here. For simplicity, let's assume you have the simplest model of task performance: it is a direct function of task-difficulty `(define subject-perform-well? (not (flip task-difficulty)))`. Subjects perform well if the task isn't too difficult. This is just a proxy for a more complicated model of inference we could have. For example, you could imagine having some notion of task-difficulty for the model used in Exercise 2.

Let's say there's a lot of training involved in your task, such that it's very time consuming for you to collect data. You run one subject through your training regime and have them do the task. That subject performs well. The same day, your adviser (or funding agency) wants you to make a decision to collect more data or not (or switch up something about your paradigm). You thought beforehand that your task was too difficult. Do you still think your task is too hard? 

One way to address this is to look at the posterior over your `task-difficulty` parameter.  How does your degree of belief in `subject-ability` change as a result of your one pilot subject performing well?

~~~
;;;fold:
(define (expectation ps vs)
  (if (= (length ps) 0)
      0      
      (+ (* (first ps) (first vs))
         (expectation (rest ps) (rest vs)))))

(define (%most-probable-value vs ps best-v best-p)
  (if (= (length ps) 0)
      best-v
      (if (> (first ps) best-p)
          (%most-probable-value (rest vs) (rest ps) (first vs) (first ps))
          (%most-probable-value (rest vs) (rest ps) best-v best-p))))

(define (most-probable-value vs ps)
  (%most-probable-value vs ps 0 0))
;;;

;; Prior on task diffuclty is uniform on 0..0.9, with a spike on 0.9

(define (task-difficulty-prior)
  (if (flip) .9 (/ (sample-integer 10) 10)))

(barplot (enumeration-query (task-difficulty-prior) true) 
         "Prior on task difficulty")


;; Compute posterior after seeing one subject perform well on the task 

(define task-difficulty-posterior-dist
  (enumeration-query
   (define task-difficulty (task-difficulty-prior))   
   ; subject will perform well if the task is not too difficult
   (define subject-performs-well? (not (flip task-difficulty)))

   task-difficulty

   (condition (equal? subject-performs-well? #t))))


;; Most likely task-difficulty is still .9

(display "Most probable task-difficult after seeing 'one subject pass':" 
         (apply most-probable-value task-difficulty-posterior-dist))


;; But a lot of probability mass is on higher values

(barplot task-difficulty-posterior-dist
         "Posterior task-difficulty after observing 'one subject perform well'")


;; Indeed, the expected subject ability is around .5

(display "Expected coin weight after seeing 'one subject perform well':" 
         (apply expectation task-difficulty-posterior-dist))
~~~

**A. Would you proceed with more data collection or would you change your paradigm? How did you come to this conclusion?**

**B.**  In this example, we are using the value of the parameter `task-difficulty` to decide about whether to continue data collection or tweak the paradigm. We find ourselves in a similar situation when we have models of psychological phenomena and we want to decide whether or not the model has fit the data (or, equivalently, whether our psychological theory is capturing the phenomenon). The traditional approach is to fit the parameter by taking a point estimate (e.g. by using least-squares or maximum-likelihood estimation; here, we could take the *Maximum A Posteriori (or, MAP)* estimate, which would be 0.9). **Why might this not be a good idea? Provide two answers. One that applies to the data collection situation, and on that applies to the metaphor of model or theory evaluation.**

**4**. Let's continue to explore the inferences you (as a scientist) can draw from the posterior over parameter values. This posterior can give you an idea of whether or not your model is well-behaved. In other words, do the predictoins of your model depend heavily on the exact parameter value?

To help us understand how to examine posteriors over parameter settings, we're going to revisit the [example of the blicket detector](https://probmods.org/patterns-of-inference.html#example-of-blickets-and-blocking) from Chapter 4.

Here is the model, with slightly different names than the original example, and written in a parameter-friendly way. It is set up to display the ["backwards blocking"](http://en.wikipedia.org/wiki/Blocking_effect#Backward_blocking) phenomenon. 

~~~
(define blicket-base-rate 0.2)
(define blicket-power 0.9)
(define non-blicket-power 0.05)
(define machine-spontaneously-goes-off 0.05)

(define detecting-blickets
  (lambda 
    (evidence)

    (enumeration-query

     ; some objects are blickets
     (define blicket (mem (lambda (block) (flip blicket-base-rate))))

     ; some blocks have the power to make the box go off
     (define block-power (lambda (block) (if (blicket block) blicket-power non-blicket-power)))

     ; sometimes the machine goes off spontaneously
     ; otherwise, goes off if one of the blocks has the ability to make it go off (sequentially evaluated)

     (define machine-goes-off
       (lambda (blocks)
         (if (null? blocks)
             (flip machine-spontaneously-goes-off)
             (or (flip (block-power (first blocks)))
                 (machine-goes-off (rest blocks))))))

     (blicket 'A)

     ; all checks to make sure all are true; i.e. all the of the lists of blickets made the machine-go-off
     (all (map machine-goes-off evidence)))))

; A&B make the blicket-detector go off
(barplot (detecting-blickets (list (list 'A 'B))) 
         "Is A a blicket, given A&B works?")
; A&B make the blicket-detector go off, and then B makes the blicket detector go off
(barplot (detecting-blickets (list (list 'A 'B) (list 'B))) 
         "Is A a blicket, given A&B works, and B works?")
     
~~~

**A. What are the parameters of this model?** In the plainest English you can muster, interpret the current values of the parameters. What do they mean?

Let's analyze this model with respect to some data. First, we'll put priors on these parameters, and then we'll do inference, conditioning on some data we might have collected in an experiment on 4 year olds, a la Sobel, Tenenbaum, & Gopnik (2004). [The data used in this exercise is schematic data].

~~~
;;;fold:
(define (get-indices needle haystack)
  (define (loop rest-of-haystack index)
    (if (null? rest-of-haystack) '()
        (let ((rest-of-indices (loop (rest rest-of-haystack) (+ index 1))))
          (if (equal? (first rest-of-haystack) needle)
              (pair index rest-of-indices)
              rest-of-indices))))
  (loop haystack 1))

(define discretize-beta 
  (lambda (gamma delta bins)
    (define shape_alpha (* gamma delta))
    (define shape_beta (* (- 1 gamma) delta))
    (define beta-pdf (lambda (x) 
                       (*
                        (pow x (- shape_alpha 1))
                        (pow (- 1 x) (- shape_beta 1)))))
    (map beta-pdf bins)))

(define get-probability
  (lambda (dist selection)
    (define index (list-index (first dist) selection))
    (list-ref (second dist) index)))

(define expval-from-enum-analysis-of-enum-model 
  (lambda (results)
    (map sum 
         (transpose (map 
                     (lambda (lst prob)
                       (map (lambda (x)
                              (* prob x))
                            (second lst)))
                     (first results)
                     (second results))))))


(define expval-from-mh-analysis-of-enum-model 
  (lambda (results)
    (map mean 
         (transpose 
          (map second results)))))

(define make-bins
  (lambda (lowerbound upperbound gap)
    (define effective-ub (+ 1 (round (/ (- upperbound lowerbound) gap))))
    (define effective-range (range (round (* 10 lowerbound)) effective-ub))
    (define binned-range (map (lambda (x) (+ (* (position effective-range x)
                                                gap)
                                             lowerbound))
                              effective-range))
    (filter (lambda (x) (<= x 1)) binned-range)))

; returns a sample from a discretized beta with mean gamma and stdev delta
; (disc-beta 0.5 2 bins) = (uniform-draw bins)
(define disc-beta 
  (lambda (gamma delta bins)
    (multinomial bins (discretize-beta gamma delta bins))))


(define (marginalize output)
  (let ([states (first output)])
    (map (lambda (sub-output) 
           (let* ([probs (second output)]
                  [unique-states (unique sub-output)]
                  [unique-state-indices 
                   (map 
                    (lambda (x) (list x (get-indices x sub-output))) 
                    unique-states)])

             (list (map first unique-state-indices)
                   (map 
                    (lambda (y) (sum (map 
                                      (lambda (x) (list-elt probs x)) 
                                      (second y)))) 
                    unique-state-indices))))

         (transpose states))))

(define summarize-data 
  (lambda (dataset)
    (list (first dataset)
          (map 
           (lambda (lst) (mean (map boolean->number lst)))
           (second dataset)))))


(define summarize-model
  (lambda (modelpreds)
    (list 
     possible-evidence-streams
     (map 
      (lambda (dist) 
        (get-probability dist #t))
      modelpreds))))

;;;
(define detecting-blickets
  (mem 
   (lambda 
     (evidence
      blicket-base-rate 
      blicket-power 
      non-blicket-power 
      machine-spontaneously-goes-off)

     (enumeration-query

      ; some objects are blickets
      (define blicket (mem (lambda (block) (flip blicket-base-rate))))

      ; some blocks have the power to make the box go off
      (define block-power (lambda (block) (if (blicket block) blicket-power non-blicket-power)))

      ; sometimes the machine goes off spontaneously
      ; otherwise, goes off if one of the blocks has the ability to make it go off (sequentially evaluated)

      (define machine-goes-off
        (lambda (blocks)
          (if (null? blocks)
              (flip machine-spontaneously-goes-off)
              (or (flip (block-power (first blocks)))
                  (machine-goes-off (rest blocks))))))

      (blicket 'A)

      ; all checks to make sure all are true; i.e. all the of the lists of blickets made the machine-go-off
      (all (map machine-goes-off evidence))))))


; 5 experiment conditions / stimuli
(define possible-evidence-streams
  (list 
   (list (list 'A))
   (list (list 'A 'B))
   (list (list 'A 'B) (list 'B))
   (list (list 'A 'B) (list 'A 'B))
   (list '())))


; note: always the query "is A a blicket?"
(define data
  (list 
   (list #t #t #t #t #t #t #t #t #t #t #f) 
   (list #t #t #t #t #t #t #f #f #f #f #f)
   (list #t #t #t #t #f #f #f #f #f #f #f)
   (list #t #t #t #t #t #t #t #t #f #f #f)
   (list #t #t #f #f #f #f #f #f #f #f #f)))


(define data-analysis
  (mh-query 100 10

            ; make-bins takes arguments: lower-bound, upper-bound, step
            (define blicket-base-rate (uniform-draw (make-bins 0.1 0.9 0.1)))

            (define blicket-power (uniform-draw (make-bins 0.1 0.9 0.1)))
            (define non-blicket-power (uniform-draw (make-bins 0.1 0.9 0.1)))

            (define machine-spontaneously-goes-off (uniform-draw (make-bins 0.1 0.9 0.1)))

            (define cognitive-model-predictions
              (map (lambda (evidence) 
                     (detecting-blickets evidence blicket-base-rate blicket-power 
                                         non-blicket-power machine-spontaneously-goes-off))
                   possible-evidence-streams))


            ; query statement
            (list 
             (summarize-model cognitive-model-predictions)
             blicket-base-rate
             blicket-power
             non-blicket-power
             machine-spontaneously-goes-off)

            ; factor statement (in leiu of the condition statement)
            (factor (sum (flatten (map 
                                   (lambda (data-for-one-stimulus model)
                                     ; map over data points in a given stimulus
                                     (map (lambda (single-data-point)
                                            (log (get-probability model single-data-point)))
                                          data-for-one-stimulus))
                                   data
                                   cognitive-model-predictions))))))


(define results (transpose data-analysis))

;;;fold:
(define posterior-predictive (list possible-evidence-streams 
                                   (expval-from-mh-analysis-of-enum-model (first results))))

(define posterior-blicket-br (second results))
(define posterior-blicket-pow (third results))
(define posterior-nonblicket-pow (fourth results))
(define posterior-machine (fifth results))
(define data-summary  (summarize-data (list possible-evidence-streams data)))
(define model-data (zip (second posterior-predictive) (second data-summary)))
;;;

(hist posterior-blicket-br "posterior on blicket base rate")
(hist posterior-blicket-pow "posterior on blicket power")
(hist posterior-nonblicket-pow "posterior on nonblicket power")
(hist posterior-machine "posterior on machine sponatenously going off")

(scatter model-data "data vs. cognitive model")

(barplot posterior-predictive "cognitive model: probability of blicket?")
(barplot data-summary "data: proportion of 'A is a Blicket!' responses")
~~~

Before running this program, answer the following question:

**B.** What does the query statement in `data-analysis` return? What does the query statement in `detecting-blickets` return? Why are there two queries in this program?

**C.** Now, run the program. [Note: This will take between 15-30 seconds to run.] **Interpret each of the resulting plots.**

**D.** How do your interpretations relate to the parameter values that were set in the original program?

**E.** Look carefully at the priors (in the code) and the posteriors (in the plots) over `blicket-power` and `non-blicket-power`. Did we impose any *a priori* assumptions about the relationship between these parameters? Think about the experimental setup. Do you think we would be justified in imposing any assumptions? Why or why not? What do the posteriors tell you? How was the data analysis model able to arrive at this conclusion? 

**F. Do you notice anything about the scatter plot?** How would you interpret this? Is there something we could add to the data analysis model to account for this?

**G.** Now, we're going to examine the predictions of the model if we had done a more traditional analysis of point-estimates of parameters (i.e. fitting parameters).
Examine your histograms and determine the "maximum a posteriori" (MAP) value for each parameter. Plug those into the code below and run it.

~~~
;;;fold:
(define (get-indices needle haystack)
  (define (loop rest-of-haystack index)
    (if (null? rest-of-haystack) '()
        (let ((rest-of-indices (loop (rest rest-of-haystack) (+ index 1))))
          (if (equal? (first rest-of-haystack) needle)
              (pair index rest-of-indices)
              rest-of-indices))))
  (loop haystack 1))


(define get-probability
  (lambda (dist selection)
    (define index (list-index (first dist) selection))
    (list-ref (second dist) index)))


(define summarize-data 
  (lambda (dataset)
    (list (first dataset)
          (map 
           (lambda (lst) (mean (map boolean->number lst)))
           (second dataset)))))


(define summarize-model
  (lambda (modelpreds)
    (list 
     possible-evidence-streams
     (map 
      (lambda (dist) 
        (get-probability dist #t))
      modelpreds))))

;;;
(define detecting-blickets
  (mem 
   (lambda 
     (evidence
      blicket-base-rate 
      blicket-power 
      non-blicket-power 
      machine-spontaneously-goes-off)

     (enumeration-query

      ; some objects are blickets
      (define blicket (mem (lambda (block) (flip blicket-base-rate))))

      ; some blocks have the power to make the box go off
      (define block-power (lambda (block) (if (blicket block) blicket-power non-blicket-power)))

      ; sometimes the machine goes off spontaneously
      ; otherwise, goes off if one of the blocks has the ability to make it go off (sequentially evaluated)

      (define machine-goes-off
        (lambda (blocks)
          (if (null? blocks)
              (flip machine-spontaneously-goes-off)
              (or (flip (block-power (first blocks)))
                  (machine-goes-off (rest blocks))))))

      (blicket 'A)

      ; all checks to make sure all are true; i.e. all the of the lists of blickets made the machine-go-off
      (all (map machine-goes-off evidence))))))


; 5 experiment conditions / stimuli
(define possible-evidence-streams
  (list 
   (list (list 'A))
   (list (list 'A 'B))
   (list (list 'A 'B) (list 'B))
   (list (list 'A 'B) (list 'A 'B))
   (list '())))

(define data
  (list 
   (list #t #t #t #t #t #t #t #t #t #t #f) 
   (list #t #t #t #t #t #t #f #f #f #f #f)
   (list #t #t #t #t #f #f #f #f #f #f #f)
   (list #t #t #t #t #t #t #t #t #f #f #f)
   (list #t #t #f #f #f #f #f #f #f #f #f)))

; fill in with your "maximum a posteriori" parameter values from Part C.
(define blicket-base-rate ...)
(define blicket-power ...)
(define non-blicket-power ...)
(define machine-spontaneously-goes-off ...)

(define best-fit-model-predictions 
  (map (lambda (evidence) 
         (get-probability 
          (detecting-blickets evidence blicket-base-rate blicket-power 
                              non-blicket-power machine-spontaneously-goes-off) 
          #t))
       possible-evidence-streams))

(define data-summary  (summarize-data (list possible-evidence-streams data)))
(define model-data (zip best-fit-model-predictions (second data-summary)))
(scatter model-data "data vs. cognitive model")
(barplot (list possible-evidence-streams best-fit-model-predictions) "cognitive model: probability of blicket?")
(barplot data-summary "data: proportion of 'A is a Blicket!' responses")
~~~

**H.** What can you conclude about the two ways of looking at parameters in this model's case? Do you think the model is relatively robust to different parameter settings?

