---
layout: model
title: Finite Mixture Model
model-status: code
model-category: Regression and Statistical Learning
model-tags: shred, benchmark, machine learning
model-language: church
---

Four objects get sorted here into a handful of latent categories, each carrying its own emission weight, and noisy observed features are matched to them through a soft equality constraint. Metropolis-Hastings inference then recovers a posterior over how the objects cluster.

    (define (zip xs1 xs2) 
      (if (or (is_null xs1) (is_null xs2)) '() 
          (pair 
           (pair (first xs1) (pair (first xs2) '()))
           (zip (rest xs1) (rest xs2)))))
    
    (define (soft-eq x y) 
      (factor (if (equal? x y) 0.0 (log 0.1))))
    
    (define samples
      (mh-query 10 100
       (define objects '(bob jane mary steve))
       (define object-indices (iota (length objects)))
       (define observed-features '(#t #t #f #f))
       (define categories '(a b c))
       (define cat-indices (iota (length categories)))
       (define pseudo-counts (repeat (length categories) (lambda () 1.0)))
       (define cats-dist (dirichlet pseudo-counts))
       (define cat-weights 
         (map (lambda (cat) (beta 1.0 1.0)) 
              categories))
       (define types 
         (map (lambda (obj) (multinomial cat-indices cats-dist)) 
              objects))
       (define observations 
         (map (lambda (obj-index) 
                (flip (list-ref cat-weights (list-ref types obj-index)))) 
              object-indices))
       (define res 
         (map (lambda (cat-index) (list-ref categories cat-index)) 
              types))
       (define constr 
         (map (lambda (obs12) 
                (soft-eq (first obs12) (second obs12))) 
              (zip observations observed-features)))
       (define sample (pair res constr)) 
       sample 
       #t))
    
    samples

References:

- Cite:shred2014
- Source: [shred](https://web.archive.org/web/2015/https://github.com/LFY/shred/blob/master/benchmarks/mixture.ss)
