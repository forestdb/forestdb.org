---
layout: model
title: Infinite Mixture Model
model-status: code
model-category: Bayesian Nonparametrics
model-tags: shred, benchmark, machine learning
model-language: church
---

Four objects get assigned here to an open-ended number of categories drawn from a Dirichlet process, rather than a fixed finite set. Each category carries its own emission weight, and Metropolis-Hastings conditions on noisy observed features to infer how the objects cluster.

    (define (zip xs1 xs2) 
      (if (or (is_null xs1) (is_null xs2)) '() 
        (pair 
          (pair (first xs1) (pair (first xs2) '()))
          (zip (rest xs1) (rest xs2)))))
    
    (define (randint l h) 
      (+ l (sample-integer (+ 1 (- h l)))))
    
    (define samples
      (mh-query 10 100
        (define (soft-eq x y) (factor (if (equal? x y) 0.0 (log 0.1))))
        (define objects '(bob jane mary steve))
        (define observed-features '(#t #t #f #f))
        (define make-category (DPmem 0.4 (lambda () (randint 0 10000))))
        (define obj-cats (repeat (length objects) (lambda () (make-category))))
        (define observe-cat
          (mem
            (lambda (cat)
              (let* ((weight (beta 1.0 1.0))
                     (res (flip weight)))
                res))))
        (define observations
          (map (lambda (c) (observe-cat c)) obj-cats))
        (define constr
          (map
            (lambda (obs12) (soft-eq (first obs12) (second obs12)))
            (zip observations observed-features)))
        (define sample (pair obj-cats constr)) sample #t))
    
    samples

References 

- Cite:shred2014
- Source: [shred](https://web.archive.org/web/2015/https://github.com/LFY/shred/blob/master/benchmarks/imm.ss)
