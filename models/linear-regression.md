---
layout: model
title: Bayesian Linear Regression (benchmark)
model-status: code
model-category: Machine Learning
model-tags: shred, benchmark, machine learning
---

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
    
    (define (data-loop n)
      (if (= n 51) '()
        (pair (list n n)
              (data-loop (+ n 1)))))
    
    (define data (data-loop 0))
    
    (define xbar (/ (sum (map first data)) (my-length data)))
    
    (define samples
      (mh-query 10 100
        (define alpha (gaussian 0.0 1e4))
        (define beta (gaussian 0.0 1e4))
        (define tau (abs (gaussian 0.0 1e4)))
        (define (gauss-factor m v x) (factor (glp m v x)))
        (define y-constrs (map (lambda (xy) (gauss-factor (+ alpha (* beta (- (first xy) xbar))) (/ 1.0 tau) (second xy)))
            data))
        (define sample (list alpha beta (/ 1.0 (expt tau 0.5)) tau))
        sample #t))
    samples

      
Source: [shred](https://github.com/LFY/shred/blob/master/benchmarks/line.ss)
