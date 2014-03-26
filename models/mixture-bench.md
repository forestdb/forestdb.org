---
layout: model
title: Bayesian Mixture Model (benchmark)
model-status: code
model-category: Machine Learning
model-tags: shred, benchmark, machine learning
---

    (define (zip xs1 xs2) 
      (if (or (is_null xs1) (is_null xs2)) '() 
          (pair 
           (pair (first xs1) (pair (first xs2) '()))
           (zip (rest xs1) (rest xs2)))))
    
    (define (pick-a-stick sticks J)
      (if (flip (sticks J))
          J
          (pick-a-stick sticks (+ J 1))))
    
    (define (make-sticks alpha)
      (let ((sticks (mem (lambda (x) (beta 1.0 alpha)))))
        (lambda () (pick-a-stick sticks 1))))
    
    (define (DPmem alpha base-dist)
      (let ((augmented-proc
             (mem (lambda (args stick-index) (apply base-dist args))))
            (DP (mem (lambda (args) (make-sticks alpha)))))
        (lambda argsin
          (let ((stick-index ((DP argsin))))
            (augmented-proc argsin stick-index)))))
    
    (define my-pi 3.14159265358979323)
    
    (define (glp mean ssq smp)
      (let ([diff (- smp mean)])
        (- (- (/ (* diff diff) (* 2.0 ssq)))
           (* 0.5 (+ (log 2) (log my-pi) (log ssq))))))
    
    (define (randint l h) (+ l (sample-integer (+ 1 (- h l)))))
    
    (define (index-in x xs)
      (define (loop x k rst)
        (if (is_null rst) k
            (if (equal? (first rst) x) k
                (loop x (+ k 1) (rest rst)))))
      (loop x 0 xs))
    
    (define (my-length xs)
      (if (is_null xs) 0
          (+ 1 (my-length (rest xs)))))
    
    (define (soft-eq x y) (factor (if (equal? x y) 0.0 (log 0.1))))
    
    (define samples
      (mh-query 10 100
       (define objects '(bob jane mary steve))
       (define object-indices (iota (my-length objects)))
       (define observed-features '(#t #t #f #f))
       (define categories '(a b c))
       (define cat-indices (iota (my-length categories)))
       (define pseudo-counts (repeat (my-length categories) (lambda () 1.0)))
       (define cats-dist (dirichlet '(0.3 0.3 0.3)))
       (define cat-weights (map (lambda (cat) (beta 1.0 1.0)) categories))
       (define types (map (lambda (obj) (multinomial cat-indices cats-dist)) objects))
       (define observations (map (lambda (obj-index) (flip (list-ref cat-weights (list-ref types obj-index)))) object-indices))
       (define res (map (lambda (cat-index) (list-ref categories cat-index)) types))
       (define constr (map (lambda (obs12) (soft-eq (first obs12) (second obs12))) (zip observations observed-features)))
       (define sample (pair res constr)) 
       sample 
       #t))
    
    samples
    
References 

- Cite:shred2014
- Source: [shred](https://github.com/LFY/shred/blob/master/benchmarks/mixture.ss)
