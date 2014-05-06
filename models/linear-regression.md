---
layout: model
title: Bayesian Linear Regression
model-status: code
model-category: PPAML Challenge Problems
model-tags: shred, benchmark, machine learning
---

Version 1 (based on [Anglican](http://www.robots.ox.ac.uk/~fwood/anglican/examples/linear_regression/)):

    (define xs '(0 1 2 3))
    (define ys '(0 1 4 6))
    
    (define samples
      (mh-query
    
       1000 10
    
       (define m (gaussian 0 2))
       (define b (gaussian 0 2))
       (define sigma-squared (gamma 1 1))
    
       (define (f x)
         (+ (* m x) b))
    
       (f 4)
    
       (all
        (map (lambda (x y) (equal? (gaussian (f x) sigma-squared y) y))
             xs
             ys))))
    
    (density samples "Predicted y for x=4" #t)

Version 2 (based on [shred](http://lfy.github.io/shred.pdf)):

    (define my-pi 3.14159265358979323)
    
    (define (glp mean ssq smp)
      (let ([diff (- smp mean)])
        (- (- (/ (* diff diff) (* 2.0 ssq)))
           (* 0.5 (+ (log 2) (log my-pi) (log ssq))))))
    
    (define (data-loop n)
      (if (= n 51) '()
          (pair (list n n)
                (data-loop (+ n 1)))))
    
    (define data (data-loop 0))
    
    (define xbar (/ (sum (map first data)) (length data)))
    
    (define samples
      (mh-query 
       10 100
       (define alpha (gaussian 0.0 1e4))
       (define beta (gaussian 0.0 1e4))
       (define tau (abs (gaussian 0.0 1e4)))
       (define (gauss-factor m v x) (factor (glp m v x)))
       (define y-constrs 
         (map (lambda (xy) 
                (gauss-factor (+ alpha (* beta (- (first xy) xbar))) (/ 1.0 tau) (second xy)))
              data))
       (define sample (list alpha beta (/ 1.0 (expt tau 0.5)) tau))
       sample #t))
    samples

References 

- Cite:shred2014
- [Linear regression in Anglican](http://www.robots.ox.ac.uk/~fwood/anglican/examples/linear_regression/)
