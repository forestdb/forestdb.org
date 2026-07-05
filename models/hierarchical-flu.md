---
layout: model
title: Hierarchical Flu/Cough Model
model-status: code
model-category: Probability and Bayesian Data Analysis
model-tags: hierarchical, medicine, mem
model-language: church
---

This Church model asks how likely Jim is to have the flu, given that Bob is coughing and both Mary and Jane have the flu. It repeatedly samples a shared flu-probability, a mem'd flu status per person, and a symptom cough that is more likely under flu, then uses rejection-query to build a posterior histogram over Jim's flu status.

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
