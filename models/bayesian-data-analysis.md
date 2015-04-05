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

##  Bad model (conditioning) DOESN'T RUN

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

## Better model (using factors)

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
        (enumeration-query

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

    (barplot (data-analysis experiment-data) "inferred bias weight")


This is cool. We've taken some data, and written a cognitive model with some parameters (in this case, one parameter: bias-weight), and asked what the most likely value of that parameter is.

Our inferred parameter setting makes sense given this made-up data. Our made-up data said the coin wasn't fair when mostly Tails came up. 

Sometimes parameter values aren't so easily interpreted as in our case here. Another way to test how well your model does is to look at the predictions of the model under these "inferred" parameter settings. This is called the "posterior predictive" distribution: it is the data that the model *actually* predicts. 

# Posterior predictive

      (define bc-model 
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
          (enumeration-query

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

      (define results (data-analysis experiment-data))

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

      (define posterior-predictive (expval-from-enum-analysis-of-enum-model results))

      (scatter 
       (zip 
        posterior-predictive
        (second (summarize-data experiment-data)))
       "data vs. model")

      (barplot (list all-seqs posterior-predictive) "model: probability of fair?")

      (barplot (list all-seqs (second (summarize-data experiment-data))) "data: proportion of fair responses")


Our model provides a pretty good fit to the data set. There are some mismatches, however. 
The model thinks HHHHH is a fair sequence, whereas our data suggest otherwise.

Try the following data set

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
             (list #f #t #t)
             (list #f #f #t)
             (list #f #f #f))))

What is the posterior over the `bias weight`? How does the posterior predictive look? What can you conclude about our bias coin model (with respect to this data))?

# Response noise

You may already have an intuition for what is going wrong here. 
Often, it's difficult to establish a feeling for why some analysis is going wrong. 
One common culprit is *response noise*, that is, data points that you've collected that don't reflect the subject doing the task. 
Let's call this behavior "guessing" (i.e. picking responses at random) and try to formalize this:

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

This pseudo-program is saying there is some probability (or, equivalently, proportion of responses) that is attributable to response noise, or guessing; 
this probability is captured by `guessing-parameter`. It is the amount of the data that is better captured by guessing behavior than our cognitive model predictions.
It is simultaneously a measure of fit of your cognitive model, as well as the reliability of the data.


## Data analysis model with response noise

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
             (list 
              (list #f #f #f)
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
                  (get-probability dist #t))
                modelpreds))))


          (define data-analysis 
            (lambda (experiment-data)
              (enumeration-query

               (define biased-weight 
                 (uniform-draw (list 0.1 0.2 0.3 0.4 0.6 0.7 0.8 0.9)))

               (define response-noise (uniform-draw (list 0 0.1 0.2 0.3 0.4 0.6 0.7 0.8 0.9 1)))

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
                                       (log (get-probability model single-data-point)))
                                     data-for-one-sequence))
                              (second experiment-data)
                              cognitive-plus-noise-predictions)))))))

          (define results (marginalize (data-analysis experiment-data)))

          (define posterior-predictive-withNoise-results (first results))
          (define posterior-predictive-sansNoise-results (second results))
          (define posterior-noise (third results))
          (define posterior-bias (fourth results))

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

          (define posterior-predictive-withNoise
            (expval-from-enum-analysis-of-enum-model posterior-predictive-withNoise-results))

          (define posterior-predictive-sansNoise
            (expval-from-enum-analysis-of-enum-model posterior-predictive-sansNoise-results))

          (scatter 
           (zip 
            posterior-predictive-withNoise
            (second (summarize-data experiment-data)))
           "data vs. cognitive model (including noise)")

          (barplot (list all-seqs posterior-predictive-withNoise) "model (with noise): probability of fair?")
          (barplot (list all-seqs posterior-predictive-sansNoise) "model (sans noise): probability of fair?")

          (barplot (list all-seqs (second (summarize-data experiment-data))) "data: proportion of fair responses")

          (barplot posterior-noise "posterior on response noise")

          (barplot posterior-bias "posterior on biased-weight")


Our posterior on response noise is peaked around 0.3. Can you make this value go up? 

(Hint: What would it mean for there to be a lot of guessing in our data set?)

What is the difference betweent the model with noise and the model without noise? (Theoretically, but also how do the predictions differ?)

Notice that our initial problem isn't really solved by factoring in response noise (though it is useful and informative to do so).
What is our problem again? Our model makes good predictions for most of these sequences, but is failing with the following two:

THHHH
HHHHH

Why might this be the case? To gain an intuition, let's reexamine the bias-weight paramter value. 
This can easily be done in the full model by using the *query statement* to return the `bias-weight` instead of, say, response-noise. 
(This helper functions here only play nice with 2 outputs at this point in time. In principle, of course, you can query for any number of arguments, by adding them to the list).

So try this query statement

                ; joint query: 
                ; what are the model predictions?
                ; what is the biased-weight?
                (list (summarize-model cognitive-model-predictions)
                      biased-weight)

And change the final `barplot` to say

      (barplot posterior-noise "posterior on biased-weight")

Of course, you can change the intermediate variable name `posterior-noise` to `posterior-weight` if you'd like.


The biased-weight is probably below 0.5. What does this mean in terms of our cognitive model?

Recall the biased-coin-model: it's comparing the sequence a fair coin would generate vs. one that a biased-coin would generate.
The biased-coin sequence has it's own weight, which means the sequences it prefers are going to sequences with lots of Tails (since our inferred weight is < 0.5).

This hints at a fundamental flaw of this model: it can only predict biased-sequences in one direction. How could we get around this issue? 


# Moving the coin-weight "into" the cognitive model 

To get around this issue, we're going to have to posit that this biased-weight doesn't take on just one value, but rather a distribution of possible values.
(Note: this is what Griffiths & Tenenbaum (2001) did.)


## Enriched cognitive model: Theta ~ Uniform (0 , 1)

We now revisit our cognitive model (forgetting about the data analysis model for the time being). We move the `biased-weight` parameter from outside the model to inside the model.

Let's examine the behavior of this model with respect to two sequences of interest.

    (define bc-model (lambda (sequence)
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
       
       (equal? sequence 
              (repeat 5 coin)))))

    (barplot (bc-model (list true true true true true)) "HHHHH is fair?")
    (barplot (bc-model (list false false false false false)) "TTTTT is fair?")


This model matches our intuition for the fairness of these sequences. Do you see why?

To gain more insight, we could query for the biased-weight variable, just like we did before in the data analysis model previously.


    (define bc-model (lambda (sequence)
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

    (barplot (bc-model (list true true true true true)) "what weight generated HHHHH")
    (barplot (bc-model (list false false false false false)) "what weight generated TTTTT")

The model has the flexibility to infer different biased coin weights for different sequences. Let's now compare this to our data set.

## Bayesian data analysis of enriched cognitive model


      (define bc-model (lambda (sequence)
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
          (enumeration-query


                    ; generate predictions for all sequences
                    (define cognitive-model-predictions
                      (map 
                       (lambda (sequence) 
                         (bc-model sequence)) 
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

      (define results (data-analysis experiment-data))

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

      (define posterior-predictive (expval-from-enum-analysis-of-enum-model results))

      (scatter 
       (zip 
        posterior-predictive
        (second (summarize-data experiment-data)))
       "data vs. model")

      (barplot (list all-seqs posterior-predictive) "model: probability of fair?")

      (barplot (list all-seqs (second (summarize-data experiment-data))) "data: proportion of fair responses")

This is great. The model doesn't suffer from the same *lower-bias* flaw that it did previously.
Note that right now, our congitive model has 0 parameters, so we're really just looking at the predictions of the model (as opposed to the posterior predictive).

## v2: Theta ~ Beta (gamma, delta) [gamma,delta in data analysis]

Are there actually 0 parameters to our model? In a sense, yes: there are no variables that take on particular values that we're uncertain about.
At the same time, we do have an assumption about the `biased-weight' distribution that exists inside the cognitive model. 
We have a uniform distribution over coin weights. It's conceiveable, however, that there is some global bias to detecting bias-sequences with heads or tails specifically.
That is, sequences of predominantly H (or, T) might be more confusable with fair sequences.

We can capture this idea by generalizing the uniform distribution to some distribution of coin weights, and we're unsure of the mean and variance of that distribution.

The canonical distribution over coin weights is the Beta distribution. 
(Coin-weights are formally Binomial parameters. 
  This higher-order distribution is called the Beta distribution. 
  You may (or may not) have heard of Beta-Binomial priors. This is it.)

Note: This will take about 10 seconds to run.

    (define discretize-beta (lambda (gamma delta bins)
        (define shape_alpha (* gamma delta))
        (define shape_beta (* (- 1 gamma) delta))
        (define beta-pdf (lambda (x) 
          (*
            (pow x (- shape_alpha 1))
            (pow (- 1 x) (- shape_beta 1)))))
      (map beta-pdf bins)))

    (define bins '(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9))

      (define bc-model (mem (lambda (sequence gamma delta)
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
          (enumeration-query

                    (define gamma (uniform-draw (list 0.1 0.3 0.5 0.7 0.9)))
                    (define delta (uniform-draw (list 0.1 0.5 1 3 7 15)))

                    ; generate predictions for all sequences
                    (define cognitive-model-predictions
                      (map 
                       (lambda (sequence) 
                         (bc-model sequence gamma delta)) 
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
                                                    (log (get-probability model single-data-point)))
                                                  data-for-one-sequence))       
                                           (second experiment-data)
                                           cognitive-model-predictions)))))))



      (define results (marginalize (data-analysis experiment-data)))


      (define posterior-predictive-results (first results))
      (define posterior-gamma (second results))
      (define posterior-delta (third results))

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

      (define posterior-predictive (expval-from-enum-analysis-of-enum-model posterior-predictive-results))

      (scatter 
       (zip 
        posterior-predictive
        (second (summarize-data experiment-data)))
       "data vs. cognitive model")

      (barplot (list all-seqs posterior-predictive) "cognitive model: probability of fair?")

      (barplot (list all-seqs (second (summarize-data experiment-data))) "data: proportion of fair responses")

      (barplot posterior-gamma "posterior on mean biased-weight")
      (barplot posterior-delta "posterior on varaince of biased-weight")







Still, our model predictions look very idyllic, while our observed data is a little more messy. 
Can we account for this with response noise?
Let's see what happens when we factor in response noise.

## Noise reduction

    (define discretize-beta (lambda (gamma delta bins)
        (define shape_alpha (* gamma delta))
        (define shape_beta (* (- 1 gamma) delta))
        (define beta-pdf (lambda (x) 
          (*
            (pow x (- shape_alpha 1))
            (pow (- 1 x) (- shape_beta 1)))))
      (map beta-pdf bins)))

    (define bins '(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9))

      (define bc-model (mem (lambda (sequence gamma delta)
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
          (enumeration-query

                    (define gamma (uniform-draw (list 0.1 0.3 0.5 0.7 0.9)))
                    (define delta (uniform-draw (list 0.1 0.5 1 3 7 15)))

                    ; generate predictions for all sequences
                    (define cognitive-model-predictions
                      (map 
                       (lambda (sequence) 
                         (bc-model sequence gamma delta)) 
                       all-seqs))

                (define response-noise (uniform-draw (list 0 0.1 0.2 0.3 0.4 0.6 0.7 0.8 0.9 1)))


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
                                                    (log (get-probability model single-data-point)))
                                                  data-for-one-sequence))       
                                           (second experiment-data)
                                           cognitive-plus-noise-predictions)))))))



      (define results (marginalize (data-analysis experiment-data)))

      (define posterior-predictive-withNoise-results (first results))
      (define posterior-predictive-sansNoise-results (second results))
      (define posterior-noise (third results))
      (define posterior-gamma (fourth results))
      (define posterior-delta (fifth results))

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

      (define posterior-predictive-withNoise 
        (expval-from-enum-analysis-of-enum-model posterior-predictive-withNoise-results))

      (define posterior-predictive-sansNoise 
        (expval-from-enum-analysis-of-enum-model posterior-predictive-sansNoise-results))

      (scatter 
       (zip 
        posterior-predictive-withNoise
        (second (summarize-data experiment-data)))
       "data vs. cognitive model")

      (barplot (list all-seqs posterior-predictive-withNoise) "cognitive model (with noise): probability of fair?")
      (barplot (list all-seqs posterior-predictive-sansNoise) "cognitive model (sans noise): probability of fair?")
      (barplot (list all-seqs (second (summarize-data experiment-data))) "data: proportion of fair responses")

      (barplot posterior-noise "posterior on noise parameter")
      (barplot posterior-gamma "posterior on mean biased-weight")
      (barplot posterior-delta "posterior on varaince of biased-weight")




# Model selection




