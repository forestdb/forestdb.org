---
layout: model
title: Markov Model
model-status: code
model-category: Machine Learning
model-tags: temporal models
model-language: church
---

A Markov model is a model of a sequence of unobserved states. Each state depends only on the previous state.

    (define (transition state)
      (cond
       ((eq? state 'a) (multinomial '(a b c) '(0.7 0.2 0.1)))
       ((eq? state 'b) (multinomial '(a b c) '(0.3 0.3 0.4)))
       ((eq? state 'c) (multinomial '(a b c) '(0.3 0.65 0.05)))))
    
    (define (markov state n)
      (if (= n 0)
          '()
          (pair state (markov (transition state) (- n 1)))))
    
    (markov 'a 10)

We can put a prior on transition probabilities:

    (define states '(a b c))
    
    (define state->transition-model
      (mem (lambda (state) (dirichlet (make-list (length states) 1)))))
    
    (define (transition state)
      (multinomial states (state->transition-model state)))
    
    (define (markov state n)
      (if (= n 0)
          '()
          (pair state (markov (transition state) (- n 1)))))
    
    (markov 'a 10)

The number of states can be infinite:

    (define theta 0.7)
    
    (define (transition state)
      (if (= state 3)
          (multinomial (list 3 4)
                       (list (- 1 (* 0.5 theta)) (* 0.5 theta)))
          (multinomial (list (- state 1) state (+ state 1))
                       (list 0.5 (- 0.5 (* 0.5 theta)) (* 0.5 theta)))))
      
    (define (chain state n)
      (if (= n 0)
          state
          (chain (transition state) (- n 1))))
    
    (hist (repeat 2000 (lambda () (chain 3 20))) "markov chain")

See also:

- [Hidden Markov Model](/models/hmm.html)
