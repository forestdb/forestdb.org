---
layout: model
title: Infinite Gaussian Mixture
model-status: code
model-category: Nonparametric Models
model-tags: mem, nonparametrics, mixture, gaussian
---

Using [DPmem](/models/dpmem.html), we can create an infinite mixture model with arbitrary observation distributions. In the following model, the observation function is Gaussian:

    (define cls-distribution (DPmem 1.0 gensym))
    
    (define object->cls
      (mem (lambda (object) (cls-distribution))))
    
    (define cls->gaussian-parameters
      (mem (lambda (cls) (list  (gaussian 65 10) (uniform 0 8)))))
    
    (define (observe object)
      (apply gaussian (cls->gaussian-parameters (object->cls object))))
    
    (map observe '(tom dick harry bill fred))
    
Source: 

- [probmods.org - nonparametric models](https://probmods.org/non-parametric-models.html)    

See also:

- [Finite Gaussian Mixture](/models/finite-gaussian-mixture.html)
