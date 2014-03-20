---
layout: model
title: Geometric Distribution
model-status: code
model-category: Miscellaneous
model-tags: recursion
---

A simple recursively defined distribution on the integers.

    (define (geometric p)
      (if (flip p)
          0
          (+ 1 (geometric p))))
    
    (hist (repeat 300 (lambda () (geometric .5))))
    
Alternatively, we can write the geometric distribution in tail-recursive form:
    
    (define (geometric-alt p n)
      (if (flip p)
          n
          (geometric-alt p (+ n 1))))
    
    (hist (repeat 300 (lambda () (geometric-alt .5 0))))
