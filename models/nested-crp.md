---
layout: model
title: Nested Chinese Restaurant Process
model-status: code-fail
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

References:

- Cite:Blei2003hierarchical
- Cite:ProbMods
