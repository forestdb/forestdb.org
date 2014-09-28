---
layout: model
title: "Conditioning on the conclusion" model issue (Leon & MH)
model-status: code
---

This is a discussion relating to Rational Speech-act models. It sprang out of the [syllogism model]{http://forestdb.org/models/syllogisms-cogsci14.html}. 
The observation was that for the argument-strength / pragamtic-reasoner queries, sampling a conclusion and then conditioning on the truth of the conclusion via ((meaning conclusion) A C), may be a strange thing to do. In particular, it may distort the distribution over situations (represented in the syllogism model as the properties A, B, C). This is evident in the following, simplified model.

      (define (meaning word)
        (case word
              (('all) (lambda (x) (equal? x 3)))
              (('some) (lambda (x) (> x 0)))))
      
      (define possible-conclusions (list 'all 'some))
      
      (define (conclusion-prior) (uniform-draw possible-conclusions))
      
      ;samples number of objects with property A
      (define (state-prior)
        (uniform-draw (list 1 2 3)))
      
      (define (reasoner)
        (enumeration-query
         (define state (state-prior))
         (define conclusion (conclusion-prior))  ;; sample conclusion from a uniform prior over the conclusions
      
         state
      
         ((meaning conclusion) state))) ; condition on the conclusion
         
      (reasoner)
  
