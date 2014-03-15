---
layout: model
title: Nested Chinese Restaurant Process
model-status: code
model-category: Nonparametric Models
model-tags: mem, nonparametrics, crp
---

In the Nested Chinese Restaurant Process (nCRP), each table refers to another restaurant that represents a lower-level category:

    (define top-level-category  (DPmem 1.0 gensym))
    
    (define subordinate-category
      (DPmem 1.0
             (lambda (parent-category)
               (pair (gensym) parent-category))))
    
    (define (sample-category) (subordinate-category (top-level-category)))
    
    (repeat 10 sample-category)

We can use these nested categories to generate observations:

    (define possible-observations '(a b c d e f g))
    
    (define top-level-category  (DPmem 1.0 gensym))
    
    (define top-level-category->parameters
      (mem 
        (lambda (cat) 
          (dirichlet (make-list (length possible-observations) 1.0)))))
        
    (define subordinate-category
      (DPmem 1.0
             (lambda (parent-category)
               (pair (gensym) parent-category))))
    
    (define subordinate-category->parameters
      (mem  
        (lambda (cat) 
          (dirichlet (top-level-category->parameters (rest cat))))))
        
    (define (sample-category) 
      (subordinate-category (top-level-category)))
    
    (define (sample-observation) 
      (multinomial possible-observations 
                   (subordinate-category->parameters (sample-category))))
    
    (repeat 10 sample-observation)

Source:
- Blei, D. M., Griffiths, T. L., Jordan, M. I., and Tenenbaum, J. B. (2004) Hierarchical topic models and the nested chinese restaurant process. In Advances in Neural Information Processing Systems 16.
- [probmods.org - nonparametric models](https://probmods.org/non-parametric-models.html)
