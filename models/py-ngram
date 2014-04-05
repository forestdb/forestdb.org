---
layout: model
title: Pitman-Yor N-Gram
model-status: code-fail
model-category: Nonparametric Models
model-tags: dp, nonparametrics, language
---

This is a hierarchical Bayesian language model based on the Pitman-Yor process.

    (define (pick-a-stick sticks J)
      (if (flip (sticks J))
          J
          (pick-a-stick sticks (+ J 1))))
    
    (define (make-PYP a b)
      (let ((sticks (mem (lambda (x) (beta (- 1.0 a) (+ b (* a x)))))))
        (lambda () (pick-a-stick sticks 1))))
    
    (define (PYmem a b proc)
      (let ((augmented-proc (mem (lambda (args part) (apply proc args))))
            (crps (mem (lambda (args) (make-PYP a b)))))
        (lambda argsin (augmented-proc argsin ((crps argsin))))))
    
    (define dictionary '(a b c d e f g))
    (define max-ngram-order 4)
    (define doc-length-hyper-param 10)
    
    (define (kth-order-markov-memory K)
      (lambda (history update)
        (if (< (length history) K)
            (pair update history)
            (pair update (take history (- (length history) 1))))))
    
    (define prefix->a (mem (lambda (prefix-length) (beta 1.0 1.0))))
    (define prefix->b (mem (lambda (prefix-length) (gamma 1.0 1.0))))
    (define draw-word (lambda () (uniform-draw dictionary)))
    
    (define prefix->next-word-distribution
      (mem 
       (lambda (prefix) 
         (PYmem
          (prefix->a (length prefix)) (prefix->b (length prefix))
          (lambda ()
            (if (null? prefix)
                (draw-word)
                (prefix->next-word (rest prefix))))))))
    
    (define prefix->next-word
      (lambda (prefix)
        ((prefix->next-word-distribution prefix))))
    
    (define (unfold-N N expander update-memory memory)
      (if (< N 1)
          '()
          (let ((next-symbol (expander memory)))
            (pair next-symbol
                  (unfold-N (- N 1) 
                            expander
                            update-memory
                            (update-memory memory next-symbol))))))
    
    
    (define doc->num-words 
      (mem 
       (lambda (doc) 
         (poisson doc-length-hyper-param))))
    
    (define doc->words 
      (mem 
       (lambda (doc) 
         (unfold-N 
          (doc->num-words doc) 
          prefix->next-word 
          (kth-order-markov-memory max-ngram-order) 
          '()))))
    
    (doc->words 'doc1)
    
See also:

- [Pitman-Yor Process](/models/pymem.html)

References:

- Cite:teh2006hierarchical
- Cite:Goodman2008uq    
