---
layout: model
title: Blue-eyed Islanders
model-status: code
model-category: Nested Inference
model-tags: theory of mind, game theory
---

The blue-eyed islanders puzzle (also known as the *muddy children*
and *cheating husbands* problem) is a well-known problem in
epistemic logic [@gamow:1958wh; @tao:2009uu]. The setup is as
follows: There is a tribe on a remote island. Out of the *n* people
in this tribe, *m* have blue eyes. Their religion forbids them to
know their own eye color, or even to discuss the topic. Therefore,
everyone sees the eye color of every other islander, but does not
know their own eye color. If an islander discovers their eye color,
they have to publicly announce this the next day at noon. All
islanders are highly logical. One day, a foreigner comes to the
island and---speaking to the entire tribe---he truthfully says: "At
least one of you has blue eyes."  What happens next?


    (define num-agents 4)
    
    (define baserate .045)
    
    (define (! thunk n)
      (if (= n 0)
          true
          (and (thunk) (! thunk (- n 1)))))
    
    (define (%sum-repeat proc n s)
      (if (= n 0)
          s
          (%sum-repeat proc
                       (- n 1)
                       (+ s (proc)))))
    
    (define (sum-repeat proc n)
      (%sum-repeat proc n 0))
    
    (define (agent t raised-hands others-blue-eyes)
      (query
       (define my-blue-eyes (if (flip baserate) 1 0))
       (define total-blue-eyes (+ my-blue-eyes others-blue-eyes))
       my-blue-eyes
       (and (> total-blue-eyes 0)
            (! (lambda () (= raised-hands (run-game 0 t 0 total-blue-eyes)))
               2))))
    
    (define (get-raised-hands t raised-hands true-blue-eyes)
      (+ (sum-repeat (lambda () (agent t raised-hands (- true-blue-eyes 1)))
                     true-blue-eyes)
         (sum-repeat (lambda () (agent t raised-hands true-blue-eyes))
                     (- num-agents true-blue-eyes))))
    
    (define (run-game start end raised-hands true-blue-eyes)
      (if (>= start end)
          raised-hands
          (run-game (+ start 1)
                    end
                    (get-raised-hands start raised-hands true-blue-eyes)
                    true-blue-eyes)))
    
    (run-game 0 4 0 4)

To run a noisy version of the model, use a `get-raised-hands` function that allows for accidentally raised hands:
    
    (define (get-raised-hands/twitch t raised-hands true-blue-eyes)
      (+ (sum-repeat (lambda () (agent t raised-hands (- true-blue-eyes 1)))
                     (- true-blue-eyes 1))
         (sum-repeat (lambda () (agent t raised-hands true-blue-eyes))
                     (- num-agents (+ true-blue-eyes)))
         (if (flip .1) 1 (agent t raised-hands (- true-blue-eyes 1)))))
