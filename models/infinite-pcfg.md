---
layout: model
title: Infinite Probabilistic Context-Free Grammar
model-status: code
model-category: Nonparametric Models
model-tags: language, pcfg
model-language: church
---

    (define terms '(a b c d))
    
    (define (terminal? y)
      (any
       (map (lambda (x) (equal? x y))
            terms)))
    
    (define get-nonterminal
      (DPmem 0.5 gensym))
    
    (define term-probs '(.1 .2 .2 .5))
    
    (define rule-type
      (mem 
       (lambda (symbol)
         (if (flip) 'terminal 'binary-production))))
           
    (define ipcfg-expander
      (DPmem 1.0
             (lambda (symbol)
               (if (eq? (rule-type symbol) 'terminal)
                   (list (multinomial terms term-probs))
                   (list (get-nonterminal) (get-nonterminal))))))
    
    (define (tree-unfold transition start-symbol)
      (if (terminal? start-symbol)
          start-symbol
          (pair start-symbol 
                (map (lambda (symbol) 
                       (tree-unfold  transition symbol)) 
                     (transition start-symbol)))))
    
    (define (sample-ipcfg) 
      (tree-unfold ipcfg-expander 'S))
    
    (sample-ipcfg)

See also:

- [Dirichlet Process](/models/dpmem.html)
- [Probabilistic Context-Free Grammar](/models/pcfg.html)

References:

- Cite:Goodman2008uq
