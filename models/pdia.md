---
layout: model
title: Probabilistic Deterministic Infinite Automata
model-status: code
model-category: Nonparametric Models
model-tags: mem, nonparametrics, hdp, sequence, automata
---

The Probabilistic Deterministic Infinite Automaton (PDIA) is a nonparametric model that defines a prior distribution over all probablistic deterministic finite automata (PDFA). Similar to a Hidden Markov Model, a PDFA sampled from the PDIA defines a prior probability over sequences. The key difference from Hidden Markov Models is that, conditioned on data, there is no uncertainty in the latent state sequence.

    (define vocabulary '(chef omelet soup eat work bake))
    
    (define top-level (DPmem 10.0 gensym))
    
    (define symbol->state-distribution
      (mem (lambda (symbol) (DPmem 10.0 top-level))))
    
    (define state/symbol->next-state
      (mem (lambda (state symbol) ((symbol->state-distribution symbol)))))
    
    (define state->observation-model
      (mem (lambda (state) 
        (dirichlet (make-list (length vocabulary) 1.0)))))
    
    (define (observation state) 
      (multinomial vocabulary (state->observation-model state)))
    
    (define (sample-words last-state) 
      (if (flip 0.1) 
        '()
        (let ((word (observation last-state)))
          (pair word 
            (sample-words (state/symbol->next-state last-state word))))))
    
    (sample-words 'start) 

References:

- Cite:pfau2010automata
