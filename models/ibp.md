---
layout: model
title: Indian Buffet Process
model-status: code-fail
model-status-verbose: The code doesn't halt.
model-category: Nonparametric Models
model-tags: dp, nonparametrics
---

The Indian Buffet Process defines a distribution on binary matrices with a finite number of rows (e.g. objects) and an infinite number of columns (e.g. features).

From Ref:roy2008stochastic:

~~~~
(define (ibp-stick-breaking-process concentration base-measure)
  (let ((sticks (mem (lambda j (random-beta 1.0 concentration))))
        (atoms (mem (lambda j (base-measure)))))
    (lambda ()
      (let loop ((j 1) (dualstick (sticks 1))}
                 (append (if (flip dualstick) ;; with prob. dualstick
                             (atoms j) ;; add feature j
                             ’()) ;; otherwise, next stick
                         (loop (+ j 1) (* dualstick (sticks (+ j 1))))))))))
~~~~

> This procedure does not halt, and therefore does not induce a well-defined distribution on values, although the original IBP does. This raises the question of whether the IBP has a computable de Finetti representation and may have implications for sampler design.

References:

- Cite:thibaux2007hierarchical
- Cite:griffiths2005infinite
- Cite:teh2007stick
- Cite:roy2008stochastic
