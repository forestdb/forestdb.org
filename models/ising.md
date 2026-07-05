---
layout: model
title: 1D Ising
model-status: code
model-category: Graphical Models and Causality
model-tags: 
model-language: church
---

This Church model draws samples from a 1D Ising-like chain of ten to twenty binary spins, using mh-query with a noisy or factor-based equality constraint between neighboring bits. Version 1 rejects mismatched neighbors probabilistically while version 2 scores mismatches with an explicit log-probability factor, and both favor long runs of matching bits.

Version 1:
    
    (define (all-but-last xs)
      (cond ((null? xs) (error "all-but-last got empty list!"))
            ((null? (rest xs)) '())
            (else (pair (first xs) (all-but-last (rest xs))))))
    
    (define (all xs)
      (if (null? xs)
          #t
          (and (first xs)
               (all (rest xs)))))
    
    (define (noisy-equal? a b)
      (flip (if (equal? a b) 1.0 0.2)))
    
    (define samples
      (mh-query 10 5
                (define bits (repeat 10 (lambda () (if (flip) 1 0))))
                bits
                (all (map noisy-equal? (rest bits) (all-but-last bits)))))
    
    (for-each display samples)    
    

Version 2:

    (define ising-length 20)
    (define non-match-cost (log 0.1))

    ;; abuses map
    (define (bigram xs) 
      (map list xs (cdr xs)))

    (define samples
      (mh-query 
        50 50
        (define (eql x y) (factor (if (= x y) 0.0 non-match-cost)))
        (define xs (repeat ising-length (lambda () (if (flip) 0 1))))
        (for-each (lambda (xy) (eql (car xy) (car (cdr xy)))) (bigram xs))
        xs
        #t))

    (car (reverse samples))

Source: [shred](https://web.archive.org/web/2015/https://github.com/LFY/shred/blob/master/tests/ising.ss)      
