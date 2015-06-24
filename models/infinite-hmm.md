---
layout: model
title: Infinite Hidden Markov Model
model-status: code
model-category: Nonparametric Models
model-tags: mem, nonparametrics, mixture, hmm
model-language: church
---

The Infinite Hidden Markov model (also known as HDP-HMM) has a potentially infinite number of latent states:

    (define vocabulary '(chef omelet soup eat work bake))
    
    (define (get-state) (DPmem 0.5 gensym))
    
    (define state->transition-model 
      (mem 
        (lambda (state) 
          (DPmem 1.0 (get-state)))))
    
    (define (transition state) 
      (sample (state->transition-model state)))
    
    (define state->observation-model 
      (mem 
        (lambda (state) 
          (dirichlet (make-list (length vocabulary) 1)))))
    
    (define (observation state) 
      (multinomial vocabulary (state->observation-model state)))
    
    (define (sample-words last-state) 
      (if (flip 0.2) 
          '() 
          (pair (observation last-state) 
                (sample-words (transition last-state)))))
    
    (sample-words 'start) 

Compare to a version using `unfold`:

    (define vocabulary '(chef omelet soup eat work bake))
    
    (define (get-state) 
      (DPmem 0.5 gensym))
    
    (define state->transition-model 
      (mem 
        (lambda (state) 
          (DPmem 1.0 (get-state)))))
    
    (define (transition state)
      (if (flip .2)
          'stop
          (state->transition-model state)))
    
    (define state->observation-model 
      (mem 
        (lambda (state) 
          (dirichlet (make-list (length vocabulary) 1)))))
    
    (define (observation state) 
      (multinomial vocabulary (state->observation-model state)))
    
    (define (unfold p f g seed)
      (if (p seed)
          '()
          (pair (f seed)
                (unfold p f g (g seed)))))
    
    (define (stop? state)
      (eq? state 'stop))
       
    (define (sample-ihmm)
      (unfold stop? observation transition 'S))
    
    (sample-ihmm)

See also:

- [Hidden Markov Model](/models/hmm.html)

References:

- Cite:Beal2002infinite
- Cite:ProbMods
- Cite:Goodman2008uq
