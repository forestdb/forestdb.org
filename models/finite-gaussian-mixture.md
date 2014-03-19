---
layout: model
title: Finite Gaussian Mixture
model-status: code
model-category: Miscellaneous
model-tags: mixture, gaussian, simple
---

For each class, we draw and remember a mean and variance. For each object, we draw (and remember) one of the classes, then draw from a Gaussian with the corresponding mean and variance.

    (define class-distribution 
      (uniform-draw 'class-a 'class-b 'class-c))
    
    (define object->class
      (mem (lambda (object) (class-distribution))))
    
    (define class->gaussian-parameters
      (mem (lambda (class) (list (gaussian 65 10) (gaussian 0 8)))))
    
    (define (observe object)
      (apply gaussian (class->gaussian-parameters (object->class object))))
    
    (map observe '(tom dick harry bill fred))

See also:

- [Infinite Gaussian Mixture](/models/infinite-gaussian-mixture.html)
