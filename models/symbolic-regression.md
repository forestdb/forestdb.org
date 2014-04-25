---
layout: model
title: Symbolic Regression
model-status: code
model-category: Miscellaneous
model-tags: shred, benchmark
---


    (define (zip xs1 xs2)
      (if (or (null? xs1) (null? xs2)) '()
        (pair
          (pair (first xs1) (pair (first xs2) '()))
          (zip (cdr xs1) (cdr xs2)))))
    
    (define my-pi 3.14159265358979323)
    
    (define (glp mean ssq smp)
      (let ([diff (- smp mean)])
        (- (- (/ (* diff diff) (* 2.0 ssq)))
           (* 0.5 (+ (log 2) (log my-pi) (log ssq))))))
    
    (define samples
      (mh-query 10 100
        (define (randint l h) (+ l (sample-integer (+ (- h l) 1))))
        (define order (randint 1 4))
        (define (my-range n) (if (equal? n 0) '() (pair (- n 1) (my-range (- n 1)))))
        (define (my-range2 n) (reverse (my-range n)))
        (define (pow n x) (if (equal? n 0) 1.0 (* x (pow (- n 1) x))))
        (define expts (my-range2 order))
        (define coefs (repeat order (lambda () (uniform -3.0 3.0))))
        (define data
          '((-4.0 16.0) (-3.0 9.0) (-2.0 4.0) (-1.0 1.0)
             (0.0 0.0) (1.0 1.0) (2.0 4.0) (3.0 9.0) (4.0 16.0)))
        (define (poly-fx x)
            (sum (map (lambda (c-n) (* (first c-n) (pow (second c-n) x)))
                      (zip coefs expts))))
        (define soft-eq (lambda (x y) (factor (glp x 0.1 y))))
        (define pairfactors (map (lambda (x-y) (soft-eq (second x-y) (poly-fx (first x-y)))) data))
        (define sample (pair order coefs))
        sample
        #t))
    samples

References 

- Cite:shred2014
- Source: [shred](https://github.com/LFY/shred/blob/master/benchmarks/sym-reg.ss)
