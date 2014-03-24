---
layout: model
title: HMM-Ising
model-status: code
model-category: Undirected Constraints
model-tags: language, nested conditioning
---

A doubly-intractable HMM, meant as a toy model for speech
recognition with articulatory contraints.

    (define (hmm state N)
      (if (= N 0)
          '()
          (pair (observe state)
                (hmm (transition state) (- N 1)))))
    
    (define (observe state)
      (map (lambda (x) (if (flip .9) x (not x)))
           state))
    
    (define (language-transition state)
      (map (lambda (x) (if (flip .8) x (not x)))
           state))
    
    (define (factor a b)
      (flip (if (equal? a b) 1.0 0.5)))
    
    (define (transition state)
      (query
       (define new-state (language-transition state))
       new-state
       (all (map factor (drop new-state 1) (drop-right new-state 1)))))
    
    (hmm '(#f #f #f) 4)
