---
layout: model
title: Counterfactuals
---

    (define (lookup key list-of-pairs)
      (rest (assoc key list-of-pairs)))
    
    
    (define (run-world world conditioner epsilon)
    
      (define (wrap f)
        (lambda args
          (let* ([name (first args)]
                 [func-args (rest args)])
            (if (or (= epsilon 1.0) (flip epsilon))
                (apply f func-args)
                (lookup name world)))))
              
      (define flip0 (wrap flip))
    
      (enumeration-query
    
       (define A (flip0 'A .2))
       (define B (flip0 'B .8))   
       (define E (or A B))
    
       (make-world A B E)
    
       (conditioner A B E)))
    
    
    ;; Helper function
    
    (define (make-world A B E)
      (list (pair 'A A)
            (pair 'B B)
            (pair 'E E)))
    
    
    ;; Prior on worlds
    
    (define (empty-condition A B E) 
      #t)
    
    (barplot (run-world '() empty-condition 1.0)
             "Prior on worlds")
    
    
    ;; Conditioning on the actual world
    
    (define (observation-condition A B E)
      (and (not A) (not B) (not E))) ;; Could use noisy conditioning here
    
    (barplot (run-world '() observation-condition 1.0)
             "Conditioned on actual world (A=0 B=0 E=0)")
    
    
    ;; Counterfactuals
    
    (define actual-world (make-world #f #f #f))
    
    (define (intervention-condition A B E)
      E)
    
    (define epsilon .05)
    
    (barplot (run-world actual-world intervention-condition epsilon)
             "Counterfactual worlds for intervention E=1")
