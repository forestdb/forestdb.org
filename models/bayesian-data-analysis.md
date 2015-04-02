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

# Inferring model parameters

As good scientists, we'll want to collect data for a number of sequences, and we'll want our model to predict the responses for all (or many) of them. 


     (define bc-model (lambda (sequence bias-weight)
          (enumeration-query
           
           (define fair-weight 0.5)
           
           (define isfair (flip))
           
           (define the-weight (if isfair fair-weight bias-weight))
           
           (define coin (lambda () 
                      (flip the-weight)))
           
           
           isfair
           
           (equal? sequence 
                      (repeat 5 coin)))))

    (define all-seqs 
          (list 
                (list false false false false false)
                (list false false false false true)
                (list false false false true true)
                (list false false true true true) 
                (list false true true true true)
                (list true true true true true)))


      (define experiment-data
      (list 
              (list 
                (list false false false false false)
                (list false false false false true)
                (list false false false true true)
                (list false false true true true) 
                (list false true true true true)
                (list true true true true true))
       
       (list (list #f #f #f)
             (list #f #f #t)
             (list #f #t #t)
             (list #t #t #t)
             (list #f #t #t)
             (list #f #t #t))))
       

    (define data-analysis 
      (lambda (experiment-data)
        (mh-query 10 10

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
          cognitive-model-predictions)))))


This model is super inefficient. It samples a bias-weight, generates predictions from the biased-coin model with the weight, *samples* a response from the posterior of that model, and see's if it matches up with the observed data. 
   
Here is it done with factor statement, instead of a condition statement.

     (define bc-model (lambda (sequence bias-weight)
          (enumeration-query
           
           (define fair-weight 0.5)
           
           (define isfair (flip))
           
           (define the-weight (if isfair fair-weight bias-weight))
           
           (define coin (lambda () 
                      (flip the-weight)))
           
           
           isfair
           
           (equal? sequence 
                      (repeat 5 coin)))))

    (define all-seqs 
          (list 
                (list false false false false false)
                (list false false false false true)
                (list false false false true true)
                (list false false true true true) 
                (list false true true true true)
                (list true true true true true)))


      (define experiment-data
      (list 
              (list 
                (list false false false false false)
                (list false false false false true)
                (list false false false true true)
                (list false false true true true) 
                (list false true true true true)
                (list true true true true true))
       
       (list (list #f #f #f)
             (list #f #f #t)
             (list #f #t #t)
             (list #t #t #t)
             (list #f #t #t)
             (list #f #t #t))))
       
    ; takes in "dist": output from an enumeration-query
    ; and "selection": the element from the posterior that you want
    ; returns the probability of that selection
    (define get-probability
      (lambda (dist selection)
        (let ([index (list-index (first dist) selection)])
          (list-ref (second dist) index))))


    (define data-analysis 
      (lambda (experiment-data)
        (mh-query 10 10

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
         (factor (sum (flatten (map 
          (lambda (data-for-one-sequence model)
            ; map over data points in a given sequence
             (map (lambda (single-data-point)
                    (log (get-probability model single-data-point)))
                  data-for-one-sequence))       
          (second experiment-data)
          cognitive-model-predictions)))))))

    (hist (data-analysis experiment-data) "inferred bias weight")


This is cool. We've taken some data, and written a cognitive model with some parameters (in this case, one parameter: bias-weight), and asked what the most likely value of that parameter is.

Our inferred parameter setting makes sense given this made-up data. Our made-up data said the coin wasn't fair when mostly Tails came up. 

Sometimes parameter values aren't so easily interpreted as in our case here. Another way to test how well your model does is to look at the predictions of the model under these "inferred" parameter settings. This is called the "posterior predictive" distribution: it is the data that the model *actually* predicts. 

# Posterior predictive

     (define bc-model (lambda (sequence bias-weight)
          (enumeration-query
           
           (define fair-weight 0.5)
           
           (define isfair (flip))
           
           (define the-weight (if isfair fair-weight bias-weight))
           
           (define coin (lambda () 
                      (flip the-weight)))
           
           
           isfair
           
           (equal? sequence 
                      (repeat 5 coin)))))

    (define all-seqs 
          (list 
                (list false false false false false)
                (list false false false false true)
                (list false false false true true)
                (list false false true true true) 
                (list false true true true true)
                (list true true true true true)))


      (define experiment-data
      (list 
              (list 
                (list false false false false false)
                (list false false false false true)
                (list false false false true true)
                (list false false true true true) 
                (list false true true true true)
                (list true true true true true))
       
       (list (list #f #f #f)
             (list #f #f #t)
             (list #f #t #t)
             (list #t #t #t)
             (list #f #t #t)
             (list #f #t #t))))
       
    ; takes in "dist": output from an enumeration-query
    ; and "selection": the element from the posterior that you want
    ; returns the probability of that selection
    (define get-probability
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
       (get-probability dist #t))
     modelpreds))))
   

    (define data-analysis 
      (lambda (experiment-data)
        (mh-query 10 10

         (define biased-weight 
           (uniform-draw (list 0.1 0.2 0.3 0.4 0.6 0.7 0.8 0.9)))

         ; generate predictions for all sequences
         (define cognitive-model-predictions
           (map 
            (lambda (sequence) 
              (bc-model sequence biased-weight)) 
            all-seqs))
                  

         ; what are the model predictions?
         (summarize-model cognitive-model-predictions)
         
         ; given that we've observed this data
         (factor (sum (flatten (map 
          (lambda (data-for-one-sequence model)
            ; map over data points in a given sequence
             (map (lambda (single-data-point)
                    (log (get-probability model single-data-point)))
                  data-for-one-sequence))       
          (second experiment-data)
          cognitive-model-predictions)))))))

    (map second (data-analysis experiment-data))





