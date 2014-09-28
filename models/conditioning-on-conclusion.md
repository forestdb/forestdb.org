---
layout: model
title: Conditioning on the conclusion model issue
model-status: code
---

This is a discussion relating to Rational Speech-act models. It sprang out of the [syllogism model](http://forestdb.org/models/syllogisms-cogsci14.html). 
The observation was that for the argument-strength / pragamtic-reasoner queries, sampling a conclusion and then conditioning on the truth of the conclusion via ((meaning conclusion) A C), may be a strange thing to do. In particular, it may distort the distribution over situations (represented in the syllogism model as the properties A, B, C). This is evident in the following, simplified model.

      (define (meaning word)
        (case word
              (('all) (lambda (x) (equal? x 3)))
              (('some) (lambda (x) (> x 0)))))
      
      (define possible-conclusions (list 'all 'some))
      (define (conclusion-prior) (uniform-draw possible-conclusions))
      
      ;samples number of objects with property A
      (define (state-prior) (uniform-draw (list 1 2 3)))
      
      (define (reasoner)
        (enumeration-query
         (define state (state-prior))
         (define conclusion (conclusion-prior))  ;; sample conclusion from a uniform prior over the conclusions
      
         state
      
         ((meaning conclusion) state))) ; condition on the conclusion
         
      (reasoner)

The reasoner hasn't learned anything at this point, so their posterior should be the same as their prior: uniform over the three states. The posterior for this model, however, is non-uniform over the three states.
  
There is a more intuitive way of writing this model, however. Instead of sampling the conclusion uniformly and without regard to the truth of the conclusion, we can sample uniformly over the conclusions true of the world.

      ; helper function to uniformly sample over the conclusions TRUE of the state
      (define (sample-conclusion state)
            (let* ([conclusion-truth-values (map (lambda (x) (list x ((meaning x) state))) possible-conclusions)];sees which conclusions are true
                [true-conclusions (filter (lambda (y) (second y)) conclusion-truth-values)] ; returns true conclusion (and their truth-value i.e. true)
                )
            (first (uniform-draw true-conclusions)))) ;returns random conclusion, from true conclusions
      
      (define (meaning word)
      (case word
          (('all) (lambda (x) (equal? x 3)))
          (('some) (lambda (x) (> x 0)))))
      
      (define possible-conclusions (list 'all 'some))
      
      ;samples number of objects with property A
      (define (state-prior) (uniform-draw (list 1 2 3)))
      
      (define (reasoner) (enumeration-query
            (define state (state-prior))
            (define conclusion (sample-conclusion state)) ;; sampling the conclusion depends on the state
      
            state
      
            true)) ;;nothing to condition on 
      
      (reasoner)
        
Of course, in this model, the prior over states is equal to the posterior over states, since we aren't conditioning on any evidence. See what happens when you query for the conclusion, instead of the state. What you should see is that it's not immediately obvious from the posterior over conclusions, which is the right model. 


But what about the syllogism model? The syllogism model has a different set of possible conclusions. In fact, the conclusion set has a very particular structure ("The Square of Opposition"). The conclusion set is composed of 2 pairs of contradictory relations: All & Not all; Some & None. What will happen to these two model if we use this "symmetric" conclusion-set?

First, the "conditioning on the conclusion" model:

      (define (meaning word)
        (case word
              (('all) (lambda (x) (equal? x 3)))
              (('some) (lambda (x) (> x 0)))
              (('none) (lambda (x) (equal? x 0)))
              (('not-all) (lambda (x) (not (equal? x 3))))))
      
      (define possible-conclusions (list 'all 'some 'none 'notall))
      (define (conclusion-prior) (uniform-draw possible-conclusions))
      
      ;samples number of objects with property A
      (define (state-prior) (uniform-draw (list 0 1 2 3)))
      
      (define (reasoner)
        (enumeration-query
         (define state (state-prior))
         (define conclusion (conclusion-prior))  ;; sample conclusion from a uniform prior over the conclusions
      
         state
      
         ((meaning conclusion) state))) ; condition on the conclusion
         
      (reasoner)
      
One minor difference between this model and the previous one above is that the state-prior has state 0 (to make "none" true of at least one state). 

And the "sample true conclusions" model:

      (define (meaning word)
        (case word
              (('all) (lambda (x) (equal? x 3)))
              (('some) (lambda (x) (> x 0)))
              (('none) (lambda (x) (equal? x 0)))
              (('not-all) (lambda (x) (not (equal? x 3))))))
      
      (define possible-conclusions (list 'all 'some 'none 'not-all))
      
      (define (conclusion-prior) (uniform-draw possible-conclusions))
      
      ;samples number of objects with property A
      (define (state-prior) (uniform-draw (list 0 1 2 3)))
      
      (define (sample-conclusion state)
        (let* ([conclusion-truth-values (map (lambda (x) (list x ((meaning x) state))) possible-conclusions)];sees which conclusions are true
              [true-conclusions (filter (lambda (y) (second y)) conclusion-truth-values)] ; returns true conclusion (and their truth-value i.e. true)
              )
        (first (uniform-draw true-conclusions)))) ;returns random conclusion, from true conclusions
      
      (define (reasoner)
        (enumeration-query
         (define state (state-prior))
         (define conclusion (sample-conclusion state))
      
         state
      
         true))
      
      (reasoner)
