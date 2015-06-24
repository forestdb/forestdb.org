---
layout: model
title: Causal Support
model-status: code
model-category: Concept Learning
model-tags: concepts 
model-language: church
---

This is a Church implementation of a model in Ref:Griffiths2005.

In principle, the likelihood evaluation involves many `flip`s, but for efficiency we collapse them into a single `flip` whose weight is the product of all the weights of the individual `flip`s. 
        
    ;; data is an association list that maps pairs of (c,e) of 0/1-valued tuples onto frequencies
    ;; e.g., '((0 1) 8) indicates that the event of cause-off-with-event-on happened 8 times 
    (define (output data)
      (define samples
        (mh-query
         10000 1
         
         ;; strengths
         (define w0 (uniform 0 1))
         (define w1 (uniform 0 1))
    
         ;; graph likelihood functions for p(e+ | c)
         ;; noisy-and
         ;;(define (p1 c) (* w0 (expt w1 c)))
    
         ;; noisy-or
         (define (p1 c) (- 1 (* (- 1 w0) (expt (- 1 w1) c))  ) )
    
         (define (p0 c) w0)
         
         ;; prior doesn't really matter
         (define graph0? (flip) ) 
    
         (define likelihood-fn (if graph0? p0 p1))
    
         ;; collapse all flips into a single flip
         ;; by multiplying their individual probabilities
         (define weight
           (prod 
            (map
             (lambda (lst)
               (let* ((c (first (first lst)))
                      (e (second (first lst)))
                      (N (second lst))
                      (pe+ (likelihood-fn c))
                      (p (if (= e 1) pe+ (- 1 pe+))))
                 (expt p N)))
             data)))
    
         ;; desired: whether or not we believe there's a causal relationship
         ;; between c and e
         (if graph0? 0 1)
    
         ;; condition: flip weight ~ flip likelihood (product of lik over {c+, c-} x {e+, e-})
         (flip weight)
         ))
      (display (mean samples)))
    
    (output '([(0 0) 0] [(0 1) 8] [(1 0) 0] [(1 1) 8] ))
    (output '([(0 0) 2] [(0 1) 6] [(1 0) 2] [(1 1) 6] ))
    (output '([(0 0) 4] [(0 1) 4] [(1 0) 4] [(1 1) 4] ))
    (output '([(0 0) 6] [(0 1) 2] [(1 0) 6] [(1 1) 2] ))
    (output '([(0 0) 8] [(0 1) 0] [(1 0) 8] [(1 1) 0] ))
    
    (output '([(0 0) 0] [(0 1) 6] [(1 0) 0] [(1 1) 8] ))
    (output '([(0 0) 2] [(0 1) 4] [(1 0) 2] [(1 1) 6] ))
    (output '([(0 0) 4] [(0 1) 2] [(1 0) 4] [(1 1) 4] ))
    (output '([(0 0) 6] [(0 1) 0] [(1 0) 6] [(1 1) 2] ))
    
    (output '([(0 0) 4] [(0 1) 4] [(1 0) 0] [(1 1) 8] ))
    (output '([(0 0) 6] [(0 1) 2] [(1 0) 2] [(1 1) 6] ))
    (output '([(0 0) 8] [(0 1) 0] [(1 0) 4] [(1 1) 4] ))
    
    (output '([(0 0) 6] [(0 1) 2] [(1 0) 0] [(1 1) 8] ))
    (output '([(0 0) 8] [(0 1) 0] [(1 0) 2] [(1 1) 6] ))
    (output '([(0 0) 8] [(0 1) 0] [(1 0) 0] [(1 1) 8] ))

References:

- Cite:Griffiths2005
