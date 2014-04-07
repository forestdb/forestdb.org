---
layout: model
title: Red-light Game
model-status: code
model-category: Miscellaneous
model-tags: planning
---

    (define (last l)
      (cond ((null? (rest l)) (first l))
            (else (last (rest l)))))
    
    ;; states have format (pair world-state agent-position)
    (define (sample-action trans start-state goal? ending)
      (rejection-query
       (define first-action (action-prior))
       (define state-action-seq 
         (rollout trans (pair start-state first-action) ending))
       state-action-seq
       (goal? state-action-seq)))
    
    ;; input and output are state-action pairs so we can run rollout
    (define (transition state-action)
      (pair (forward-model state-action) (action-prior)))
    
    (define (rollout next init end)
      (if (end init)
          (list init)
          (append (list init) (rollout next (next init) end))))
    
    (define cheat-det .9)
    
    (define (forward-model state-action)
      (pair
       (if (flip 0.5) 'red-light 'green-light)
       (let ((light (first (first state-action)))
             (position (rest (first state-action)))
             (action (rest state-action)))
         (if (eq? action 'go)
             (if (and (eq? light 'red-light)
                      (flip cheat-det))
                 0
                 (+ position 1))
             position))))
    
    (define discount .95)
    
    (define (ending? symbol) 
      (flip (- 1 discount)))
    
    (define goal-pos 5)
    
    (define (goal-function state-action-seq)
      (> (rest (first (last state-action-seq))) goal-pos))
    
    (define (action-prior) (if (flip 0.5) 'go 'stop))
    
    (define states-and-actions
      (sample-action transition (pair 'green-light 1) goal-function ending?))
    
    (for-each display states-and-actions)

References:

- Cite:ProbMods
- Cite:Goodman2008uq
- Cite:Toussaint2006tea
