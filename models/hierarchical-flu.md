---
layout: model
title: Hierarchical Flu/Cough Model
model-status: code
model-category: Miscellaneous
model-tags: hierarchical, medicine, mem
model-language: church
---

    (define flu-dist
      (repeat 
       1000
       (lambda ()
         (rejection-query
          
          ;; model
          (define flu-probability (uniform 0 1))
          (define flu (mem (lambda (person) (flip flu-probability))))
          (define cough
            (mem 
             (lambda (person)
               (if (flu person)
                   (flip .85)
                   (flip .1)))))
          
          ;; query
          (flu 'jim)
          
          ;; condition
          (and (cough 'bob) 
               (flu 'mary)
               (flu 'jane))))))
    
    (hist flu-dist)
