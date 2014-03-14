---
layout: model
title: Dirichlet Process Memoizer
model-status: code
model-category: Nonparametric Models
model-tags: mem, nonparametrics
---

Using memoization (`mem`), we can implement the Dirichlet process:

    (define (pick-a-stick sticks J)
      (if (flip (sticks J))
          J
          (pick-a-stick sticks (+ J 1))))
    
    (define (make-sticks alpha)
      (let ((sticks (mem (lambda (x) (first (beta 1.0 alpha))))))
        (lambda () (pick-a-stick sticks 1))))
    
    (define my-sticks (make-sticks 1))

    (hist (repeat 1000 my-sticks) "Dirichlet Process")
    
Based on the Dirichlet Process, we can write a stochastic memoizer for any function (`DPmem`):

    (define (pick-a-stick sticks J)
      (if (flip (sticks J))
          J
          (pick-a-stick sticks (+ J 1))))
    
    (define (make-sticks alpha)
      (let ((sticks (mem (lambda (x) (first (beta 1.0 alpha))))))
        (lambda () (pick-a-stick sticks 1))))
    
    (define (DPmem alpha base-dist)
      (let ((augmented-proc (mem (lambda (args stick-index) (apply base-dist args))))
            (DP (mem (lambda (args) (make-sticks alpha)))))
        (lambda argsin
          (let ((stick-index (sample (DP argsin))))
            (augmented-proc argsin stick-index)))))

    (define memoized-gaussian (DPmem 1.0 gaussian))
    
    (hist (repeat 1000 (lambda () (memoized-gaussian 0.0 1.0))) "Dirichlet Process")

Source: [probmods.org - nonparametric models](https://probmods.org/non-parametric-models.html)
