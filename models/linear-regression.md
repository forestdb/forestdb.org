---
layout: model
title: Bayesian Linear Regression
model-status: code
model-category: Machine Learning
model-tags: machine learning
---

    (letrec 
        ([xys '((1 1) (2 3) (3 3) (4 3) (5 5))]
         [xbar 3]
         [alpha (xrp+init gaussian-scorer gaussian gaussian-prop 0 0 10000)]
         [beta (xrp+init gaussian-scorer gaussian gaussian-prop 0 0 10000)]
         [tau (abs (xrp+init gaussian-scorer gaussian gaussian-prop 1 0 10000))]
         [gauss-factor (factor (m v x) (gauss-log-pdf m v x))]
         [y-constrs 
          (map (lambda (xy) 
                 (gauss-factor (+ alpha (* beta (- (car xy) xbar))) (/ 1.0 tau) (cadr xy))) 
               xys)])
      (list alpha beta (/ 1.0 (sqrt tau)) tau)))
      
Source: [shred](https://github.com/LFY/shred/blob/master/tests/line.ss)
