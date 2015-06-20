---
layout: model
title: Inducing Arithmetic Functions
model-status: code
model-category: Concept Learning
model-tags: concepts, program induction
model-language: church
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
       (and (= (my-proc 0) 2)
            (= (my-proc 1) 3))))
    
    (sample)

To induce stochastic functions, it can be useful to condition on the evidence -- our input-output examples -- having high marginal likelihood under the induced function:

    (define (random-arithmetic-fn)
      (if (flip 0.3)
          (random-combination (random-arithmetic-fn) 
                              (random-arithmetic-fn))
          (if (flip) 
              (lambda (x) x) 
              (random-constant-fn))))
    
    (define (random-combination f g)
      (if (flip 0.5)
        ((lambda (op) (lambda (x) (op (f x) (g x))))
          (uniform-draw (list + -)))
        (lambda (x) (sample-integer 10))))
    
    (define (random-constant-fn)
      (define i (sample-integer 10))
      (lambda (x) i))
    
    (define (find-prob x xs ps)
      (if (null? xs)
          0.0
          (if (equal? (first xs) x)
              (first ps)
              (find-prob x (rest xs) (rest ps)))))
    
    (define (likelihood fn x)
      (let ([dist (enumeration-query (define _ 1) (fn) #t)])
        (find-prob x (first dist) (second dist))))
    
    (define (sample)
      (rejection-query
       (define my-proc (random-arithmetic-fn))
       (define (my-proc-likelihood x y)
         (likelihood (lambda () (my-proc x)) y))
       (my-proc 2)
       (and (flip (my-proc-likelihood 0 2)) 
            (flip (my-proc-likelihood 1 3)))))
    
    (hist (repeat 100 sample))

This is semantically equivalent to conditioning on input-output examples directly, but can improve convergence rates when MCMC is used in the outer query.

References:

- Cite:ProbMods
