---
layout: model
title: Indian Buffet Process
model-status: static
model-status-verbose: This procedure deliberately does not halt; it is shown as an illustration, not a runnable model.
model-category: Bayesian Nonparametrics
model-tags: dp, nonparametrics
model-language: church
---

The Indian Buffet Process defines a distribution on binary matrices with a finite number of rows (e.g. objects) and an infinite number of columns (e.g. features).

The procedure below is shown for illustration only — it deliberately does **not** halt, so it is rendered as static code rather than a runnable box.

From Ref:roy2008stochastic:

~~~~ norun
(define (ibp-stick-breaking-process concentration base-measure)
  (let ((sticks (mem (lambda (j) (random-beta 1.0 concentration))))
        (atoms (mem (lambda (j) (base-measure)))))
    (lambda ()
      (let loop ((j 1) (dualstick (sticks 1)))
        (append (if (flip dualstick)  ;; with prob. dualstick
                    (atoms j)          ;; add feature j
                    '())               ;; otherwise, next stick
                (loop (+ j 1) (* dualstick (sticks (+ j 1)))))))))
~~~~

> This procedure does not halt, and therefore does not induce a well-defined distribution on values, although the original IBP does. This raises the question of whether the IBP has a computable de Finetti representation and may have implications for sampler design.

Contrast this with the [Pitman-Yor Process](/models/pymem.html), whose stick-breaking weights sum to one almost surely, so its sampler *does* halt with probability one.

See also:

- [Dirichlet Process](/models/dpmem.html)
- [Pitman-Yor Process](/models/pymem.html)

References:

- Cite:thibaux2007hierarchical
- Cite:griffiths2005infinite
- Cite:teh2007stick
- Cite:roy2008stochastic
