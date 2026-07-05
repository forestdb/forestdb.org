---
layout: model
title: Geometric Distribution
model-status: code
model-category: Probability and Bayesian Data Analysis
model-tags: recursion
model-language: church
---

Repeated coin flips build a geometric distribution over the non-negative integers here, stopping and returning the current count on the first success in a purely recursive definition.

    (define (geometric p)
      (if (flip p)
          0
          (+ 1 (geometric p))))
    
    (hist (repeat 300 (lambda () (geometric .5))))
    
Alternatively, we can write the geometric distribution in tail-recursive form:
    
    (define (geometric-tail p n)
      (if (flip p)
          n
          (geometric-tail p (+ n 1))))
    
    (hist (repeat 300 (lambda () (geometric-tail .5 0))))
