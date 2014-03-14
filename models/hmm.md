---
layout: model
title: Hidden Markov Model
model-status: code
model-category: Machine Learning
model-tags: temporal models
---

A simple model of a sequence of unobserved states, each of which
depending only on the previous one, and each of which gives rise to
an observation.

Version 1:

    (define (sample-state state)
      (cond
       ((eq? state 0) (sample-discrete '(0.7 0.2 0.1)))
       ((eq? state 1) (sample-discrete '(0.3 0.3 0.4)))
       ((eq? state 2) (sample-discrete '(0.3 0.65 0.05)))))
    
    (define (observe-state state)
      (cond
       ((eq? state 0) (sample-integer 3))
       ((eq? state 1) (+ (sample-integer 3) 1))
       ((eq? state 2) (+ (sample-integer 2) 2))))
    
    (define (hmm state n)
      (if (= n 0)
          '()
          (pair (observe-state state)
                (hmm (sample-state state) (- n 1)))))
    
    (hmm 0 5)

Version 2:

    (define states '(s1 s2 s3 s4 s5 s6 s7 s8 stop))
    
    (define vocabulary '(chef omelet soup eat work bake))
    
    (define state->observation-model
      (mem (lambda (state) (dirichlet (make-list (length vocabulary) 1)))))
    
    (define (observation state)
      (multinomial vocabulary (state->observation-model state)))
    
    (define state->transition-model
      (mem (lambda (state) (dirichlet (make-list (length states) 1)))))
    
    (define (transition state)
      (multinomial states (state->transition-model state)))
    
    
    (define (sample-words last-state)
      (if (equal? last-state 'stop)
          '()
          (pair (observation last-state) (sample-words (transition last-state)))))
    
    (sample-words 'start)

Source: [probmods.org](https://probmods.org/observing-sequences.html#hidden-markov-models)

See also:

- [HMM-Ising](/models/hmm-ising.html)
- [Infinite HMM](/models/infinite-hmm.html)
