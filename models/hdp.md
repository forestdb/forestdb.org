---
layout: model
title: Hierarchical Dirichlet Process
model-status: code
model-category: Nonparametric Models
model-tags: mem, nonparametrics, dp
model-language: church
---

In a Hierarchical Dirchlet Process (HDP), multiple DPs share another (top-level) DP as their base measure.

    (define base-measure (lambda () (poisson 20)))
    (define top-level  (DPmem 10.0 base-measure))
    (define sample-observation
      (DPmem 1.0
             (lambda (component)
               (top-level))))
    
    (hist (repeat 1000 base-measure) "Draws from Base Measure (poisson 20)")
    (hist (repeat 1000 (lambda () (sample-observation 'component1))) "Draws from Component DP 1")
    (hist (repeat 1000 (lambda () (sample-observation 'component2))) "Draws from Component DP 2")
    (hist (repeat 1000 (lambda () (sample-observation 'component3))) "Draws from Component DP 3")
    (hist (repeat 1000 top-level) "Draws from Top Level DP")
    'done

References:

- Cite:Teh2004sharing
- Cite:ProbMods
