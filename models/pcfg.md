---
layout: model
title: Probabilistic Context-Free Grammar
model-status: code
model-category: Miscellaneous
model-tags: language
---

Direct style:

    (define (terminal t) (lambda () t))
    
    (define (sample thunk) (thunk))
    
    (define D (lambda ()
                (map sample
                     (multinomial
                      (list (list (terminal 'the) ) 
                            (list (terminal 'a)))
                      (list (/ 1 2) (/ 1 2))))))
    (define N (lambda ()
                (map sample 
                     (multinomial
                      (list (list (terminal 'chef)) 
                            (list (terminal 'soup)) 
                            (list (terminal 'omelet)))
                      (list (/ 1 3) (/ 1 3) (/ 1 3))))))
    (define V (lambda ()
                (map sample
                     (multinomial
                      (list (list (terminal 'cooks)) 
                            (list (terminal 'works)))
                      (list (/ 1 2) (/ 1 2))))))                
    (define A (lambda ()
                (map sample
                     (multinomial
                      (list (list (terminal 'diligently)))
                      (list (/ 1 1))))))
    (define AP (lambda ()
                 (map sample
                      (multinomial
                       (list (list A))
                       (list (/ 1 1))))))
    (define NP (lambda ()
                 (map sample
                      (multinomial
                       (list (list D N))
                       (list (/ 1 1))))))
    (define VP (lambda ()
                 (map sample
                      (multinomial
                       (list (list V AP) 
                             (list V NP))
                       (list (/ 1 2) (/ 1 2))))))
    (define S (lambda ()
                (map sample 
                     (multinomial
                      (list (list NP VP))
                      (list (/ 1 1))))))
    (S)

Unfold style:

    (define (terminal t) (list 'terminal t))
    
    (define (unwrap-terminal t) (second t))
    
    (define (tree-unfold transition start-symbol)
      (if (terminal? start-symbol)
          (unwrap-terminal start-symbol)   
          (pair start-symbol 
                (map (lambda (symbol) 
                       (tree-unfold  transition symbol)) 
                     (transition start-symbol)))))
    
    (define (terminal? symbol)
      (if (list? symbol)
          (equal? (first symbol) 'terminal)
          false))
    
    (define (transition nonterminal)
      (case nonterminal
            (('D) (multinomial(list (list (terminal 'the)) 
                                    (list (terminal 'a)))
                              (list (/ 1 2) (/ 1 2))))
            (('N) (multinomial (list (list (terminal 'chef))
                                     (list (terminal 'soup)) 
                                     (list (terminal 'omelet)))
                               (list (/ 1 3) (/ 1 3) (/ 1 3))))
            (('V) (multinomial (list (list (terminal 'cooks)) 
                                     (list (terminal 'works)))
                               (list (/ 1 2) (/ 1 2))))                
            (('A) (multinomial (list (list (terminal 'diligently)))
                               (list (/ 1 1))))
            (('AP) (multinomial (list (list 'A))
                                (list (/ 1 1))))
            (('NP) (multinomial (list (list 'D 'N))
                                (list (/ 1 1))))
            (('VP) (multinomial (list (list 'V 'AP) 
                                      (list 'V 'NP))
                                (list (/ 1 2) (/ 1 2))))
            (('S) (multinomial (list (list 'NP 'VP))
                               (list (/ 1 1))))
            (else 'error)))
    
    
    (tree-unfold transition 'S)

References:

- Cite:Goodman2008uq
- Cite:ProbMods
