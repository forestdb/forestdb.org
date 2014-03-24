---
layout: model
title: Red-light game
model-status: code-fail
model-category: Miscellaneous
model-tags: planning
---

    (define (transition state-action)
      (pair
       (forward-model state-action)
       (action-prior)))
    
    (define (terminal? symbol) (flip gamma))
    
    (define (reward-pred rewards)
      (flip ((/ (sum rewards) (length rewards)))))
    
    (define (forward-model s-a)
      (pair
       (if (flip 0.5) 'red-light 'green-light)
       (let ((light (first (first s-a)))
             (position (last (first s-a)))
             (action (last s-a)))
         (if (eq? action 'go)
             (if (and (eq? light 'red-light)
                      (flip cheat-det))
                 0
                 (+ position 1))
             position))))
    
    (define (action-prior)
      (if (flip 0.5) 'go 'stop))
    
    (define (sp1 state)
      (if (> (last state) 5) 1 0))
    
    (rejection-query
     (define first-action (action-prior))
     (define final-state
       (first (unfold transition
                      (pair start-state first-action))))
     (define reward-list
       (list (sp1 final-state)
             (sp2 final-state)
             ..etc..))
     first-action
     (reward-pred reward-list))

References:

- Cite:Goodman2008uq
- Cite:Toussaint2006tea
