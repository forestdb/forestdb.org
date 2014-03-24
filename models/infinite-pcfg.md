---
layout: model
title: Infinite Probabilistic Context-Free Grammar
model-status: code-fail
model-category: Nonparametric Models
model-tags: language, pcfg
---

    (define terms '(a b c d))
    
    (define term-probs '(.1 .2 .2 .5))
    
    (define rule-type
      (mem (lambda symbol)
           (if (flip) 'terminal 'binary-production)))
           
    (define ipcfg-expander
      (DPmem 1.0
             (lambda (symbol)
               (if (eq? (rule-type symbol) 'terminal)
                   (multinomial terms term-probs)
                   (list (get-symbol) (get-symbol))))))
    
    (define (sample-ipcfg) (unfold ipcfg-expander 'S))

References:

- Cite:Goodman2008uq
