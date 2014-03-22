---
layout: model
title: Model Selection (benchmark)
model-status: code
model-category: Miscellaenous
model-tags: shred, benchmark
---

    (define (zip xs1 xs2) 
      (if (or (is_null xs1) (is_null xs2)) '() 
        (pair 
          (pair (first xs1) (pair (first xs2) '()))
          (zip (rest xs1) (rest xs2)))))
    
    (define my-pi 3.14159265358979323)
    
    (define (glp mean ssq smp)
      (let ([diff (- smp mean)])
        (- (- (/ (* diff diff) (* 2.0 ssq)))
           (* 0.5 (+ (log 2) (log my-pi) (log ssq))))))
    
    (define (randint l h) (+ l (sample-integer (+ 1 (- h l)))))
    
    (define samples
      (mh-query 10 100
        (define which-model (randint 0 1))
        (define (model1)
          (let* ((x (uniform 1 3))
                 (y (uniform 1 3)))
            (pair x (pair y (pair (* x y) '())))))
        (define model2
          (lambda ()
            (let* ((x (uniform 1 3))
                   (y (uniform 1 3)))
              (pair x (pair y (pair (+ x y) '()))))))
        (define data '(2 2 4))
        (define (soft-eq x y) (factor (glp x 0.1 y)))
        (define obs
          (if (= 0 which-model)
            (pair 'm1 (model1))
            (pair 'm2 (model2))))
        (define pairtr
          (map (lambda (xy) (soft-eq (first xy) (second xy)))
               (zip (rest obs) data)))
        (define sample (pair obs pairtr)) 
        sample 
        #t))
    samples
    
    
Source: [shred](https://github.com/LFY/shred/tree/master/benchmarks/model-selection.ss)
