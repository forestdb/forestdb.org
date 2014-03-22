---
layout: model
title: Inducing Arithmetic Functions
model-status: code
model-category: Concept Learning
model-tags: concepts, program induction
---

This program induces a deterministic arithmetic function from input-output examples.

    (define (random-arithmetic-fn)
      (if (flip 0.3)
          (random-combination (random-arithmetic-fn) 
                              (random-arithmetic-fn))
          (if (flip) 
              (lambda (x) x) 
              (random-constant-fn))))
    
    (define (random-combination f g)
      (define op (uniform-draw (list + -)))
      (lambda (x) (op (f x) (g x))))
    
    (define (random-constant-fn)
      (define i (sample-integer 10))
      (lambda (x) i))
    
    (define (sample)
      (rejection-query
       (define my-proc (random-arithmetic-fn))
       (my-proc 2)
       (= (my-proc 1) 3)))

    (sample)

References:

- Cite:ProbMods
