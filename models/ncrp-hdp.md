---
layout: model
title: Nested CRP + HDP
model-status: code
model-category: Nonparametric Models
model-tags: mem, nonparametrics, dp
---

We can use the [Nested Chinese-Restaurant Process](/models/nested-crp.html) to sample a hierarchy of categories, and the [Hierarchical Dirichlet Process](/models/hdp.html) to make these categories share information:

    (define top-level-category  (DPmem 1.0 gensym))
    
    (define root-category (DPmem 10.0 (lambda () (poisson 20))))
    
    (define sample-from-top-level-category  
      (DPmem 1.0 (lambda (cat) (root-category))))
    
    (define subordinate-category
      (DPmem 1.0
             (lambda (parent-category)
               (pair (gensym) parent-category))))
    
    (define (sample-category) (subordinate-category (top-level-category)))
    
    (define sample-observation
      (DPmem 1.0
             (lambda (cat)
               (sample-from-top-level-category (rest cat)))))
    
    (repeat 10
     (lambda ()
       (let ((category (sample-category)))
         (hist (repeat 1000 (lambda () (sample-observation category)))
               (string-append  "Top Level: " (symbol->string (rest category))
                               ", Subordinate Level: " (symbol->string (first category))))
         (hist (repeat 1000 (lambda () (sample-from-top-level-category (rest category))))
               (string-append  "Top Level: " (symbol->string (rest category))))
         (hist (repeat 1000 (lambda () (sample-observation category)))
               "Root Category"))))
    'done

Source:

- [probmods.org - nonparametric models](https://probmods.org/non-parametric-models.html)
