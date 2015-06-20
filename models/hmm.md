---
layout: model
title: Discrete-Time Hidden Markov Model
model-status: code
model-category: PPAML Challenge Problems
model-tags: temporal models
model-language: church
---

A model of a sequence of unobserved states, each of which depends only on the previous one, and each of which gives rise to an observation.

Fixed transition and observation probabilities:

    (define (transition state)
      (cond
       ((eq? state 'a) (multinomial '(a b c) '(0.7 0.2 0.1)))
       ((eq? state 'b) (multinomial '(a b c) '(0.3 0.3 0.4)))
       ((eq? state 'c) (multinomial '(a b c) '(0.3 0.65 0.05)))))
    
    (define (observe-state state)
      (cond
       ((eq? state 'a) (sample-integer 3))
       ((eq? state 'b) (+ (sample-integer 3) 1))
       ((eq? state 'c) (+ (sample-integer 2) 2))))
    
    (define (hmm state n)
      (if (= n 0)
          '()
          (pair (observe-state state)
                (hmm (transition state) (- n 1)))))
    
    (hmm 'a 5)

Uncertain transition and observation probabilities (basic version):

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

Uncertain transition and observation probabilities (using `unfold`):

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
    
    (define (unfold p f g seed)
      (if (p seed)
          '()
          (pair (f seed)
                (unfold p f g (g seed)))))
    
    (define (stop? state)
      (eq? state 'stop))
       
    (define (sample-words)
      (unfold stop? observation transition 'S))
    
    (sample-words)

See also:

- [Markov Model](/models/markov.html)
- [HMM-Ising](/models/hmm-ising.html)
- [Infinite HMM](/models/infinite-hmm.html)

References:

- Cite:ProbMods
