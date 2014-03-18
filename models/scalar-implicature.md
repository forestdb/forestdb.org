---
layout: model
title: Scalar Implicature
model-status: code
model-category: Nested Inference
model-tags: lingustics, pragmatics, theory of mind
---

A model of pragmatics: The speaker chooses a sentence conditioned on the listener inferring the intended state of the world when hearing this sentence; the listener chooses an interpretation conditioned on the speaker selecting the given utterance when intending this meaning. @stuhlmueller:2012aa; @ndg+ast:cogsci2012; @Frank:2012fe

    (define (belief actual-state access)
      (map (lambda (ac st pr) (if ac st (sample pr)))
           access
           actual-state
           (substate-priors)))
    
    (define (baserate) 0.8)
    
    (define (substate-priors)
      (list (lambda () (flip (baserate)))
            (lambda () (flip (baserate)))
            (lambda () (flip (baserate)))))
    
    (define (state-prior)
      (map sample (substate-priors)))
    
    (define (sentence-prior)
      (uniform-draw (list all-p some-p none-p)))
    
    (define (all-p state) (all state))
    (define (some-p state) (any state))
    (define (none-p state) (not (some-p state)))
    
    (define (speaker access state depth)
      (query
       (define sentence (sentence-prior))
       sentence
       (equal? (belief state access)
               (listener access sentence depth))))
    
    (define (listener speaker-access sentence depth)
      (query
       (define state (state-prior))
       state
       (if (= 0 depth)
           (sentence state)
           (equal? sentence
                   (speaker speaker-access state (- depth 1))))))
    
    (define (num-true state)
      (sum (map (lambda (x) (if x 1 0)) state)))
    
    ;; without full knowledge:
    (num-true (listener '(#t #t #f) some-p 5))
    
    ;; with full knowledge:
    (num-true (listener '(#t #t #t) some-p 5))
