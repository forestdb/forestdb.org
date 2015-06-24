---
layout: model
title: Finite Gaussian Mixture
model-status: code
model-category: Miscellaneous
model-tags: mixture, gaussian, simple
model-language: church
---

For each class, we draw and remember a mean and variance. For each object, we draw (and remember) one of the classes, then draw from a Gaussian with the corresponding mean and variance.

    (define (cls-distribution)
      (uniform-draw (list 'cls-a 'cls-b 'cls-c)))
    
    (define object->cls
      (mem (lambda (object) (cls-distribution))))
    
    (define cls->gaussian-parameters
      (mem (lambda (cls) (list (gaussian 65 10) (uniform 0 8)))))
    
    (define (observe object)
      (apply gaussian (cls->gaussian-parameters (object->cls object))))
    
    (map observe '(tom dick harry bill fred))

See also:

- [Infinite Gaussian Mixture](/models/infinite-gaussian-mixture.html)

References:

- Cite:ProbMods
