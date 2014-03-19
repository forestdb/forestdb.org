---
layout: model
title: Infinite Gaussian Mixture
model-status: code
model-category: Nonparametric Models
model-tags: mem, nonparametrics, mixture, gaussian
---

Using [DPmem](/models/dpmem.html), we can create an infinite mixture model with arbitrary observation distributions. In the following model, the observation function is Gaussian:

    (define class-distribution (DPmem 1.0 gensym))
    
    (define object->class
      (mem (lambda (object) (class-distribution))))
    
    (define class->gaussian-parameters
      (mem (lambda (class) (list  (gaussian 65 10) (uniform 0 8)))))
    
    (define (observe object)
      (apply gaussian (class->gaussian-parameters (object->class object))))
    
    (map observe '(tom dick harry bill fred))
    
Source: 

- [probmods.org - nonparametric models](https://probmods.org/non-parametric-models.html)    

See also:

- [Finite Gaussian Mixture](/models/finite-gaussian-mixture.html)
