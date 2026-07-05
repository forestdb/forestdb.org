---
layout: model
title: Pitman-Yor Process
model-status: code
model-category: Bayesian Nonparametrics
model-tags: dp, nonparametrics
model-language: church
---

The Pitman-Yor process, a two-parameter generalization of the Dirichlet process's stick-breaking construction, gets implemented here as a stochastic memoizer for arbitrary functions. Applying it to the Gaussian distribution produces repeated draws whose clustering pattern follows the Pitman-Yor process's characteristic power-law tail.

    (define (pick-a-stick sticks J)
      (if (flip (sticks J))
          J
          (pick-a-stick sticks (+ J 1))))
    
    (define (make-PYP a b)
      (let ((sticks (mem (lambda (x) (beta (- 1.0 a) (+ b (* a x)))))))
        (lambda () (pick-a-stick sticks 1))))
    
    (define (PYmem a b proc)
      (let ((augmented-proc (mem (lambda (args part) (apply proc args))))
            (crps (mem (lambda (args) (make-PYP a b)))))
        (lambda argsin (augmented-proc argsin ((crps argsin))))))
    
    (define PY-gaussian (PYmem 0.5 1.0 gaussian))
    
    (hist (repeat 1000 (lambda () (PY-gaussian 0.0 1.0))) "PY-gaussian")

See also:

- [Dirichlet Process](/models/dpmem.html)

References:

- Cite:Goodman2008uq
