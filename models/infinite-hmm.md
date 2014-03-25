---
layout: model
title: Infinite Hidden Markov Model
model-status: code-fail
model-category: Nonparametric Models
model-tags: mem, nonparametrics, mixture, hmm
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

Compare to the version in Ref:Goodman2008uq:

    (define get-symbol (DPmem 1.0 gensym))
    
    (define get-observation-model
      (mem (lambda (symbol) (make-100-sided-die))))
    
    (define ihmm-transition
      (DPmem 1.0 (lambda (state)
                   (if (flip) 'stop (get-symbol)))))
    
    (define (ihmm-expander symbol)
      (list ((get-observation-model symbol))
            (ihmm-transition symbol)))
    
    (define (sample-ihmm)
      (unfold ihmm-expander ’S))

See also:

- [Hidden Markov Model](/models/hmm.html)

References:

- Cite:Beal2002infinite
- Cite:ProbMods
- Cite:Goodman2008uq
