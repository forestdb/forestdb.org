---
layout: model
title: Scalar Implicature
model-status: code
model-category: Reasoning about Reasoning
model-tags: linguistics, pragmatics, theory of mind
---

A model of pragmatic language interpretation: 

The speaker chooses a sentence conditioned on the listener inferring the intended state of the world when hearing this sentence; the listener chooses an interpretation conditioned on the speaker selecting the given utterance when intending this meaning.

    (define (state-prior) (uniform-draw '(0 1 2 3)))
    
    (define (sentence-prior) (uniform-draw (list all-sprouted some-sprouted none-sprouted)))
    
    (define (all-sprouted state) (= 3 state))
    (define (some-sprouted state) (< 0 state))
    (define (none-sprouted state) (= 0 state))
    
    (define (speaker state depth)
      (rejection-query
       (define words (sentence-prior))
       words
       (equal? state (listener words depth))))
    
    (define (listener words depth)
      (rejection-query
       (define state (state-prior))
       state
       (if (= depth 0)
           (words state)
           (equal? words (speaker state (- depth 1))))))
    
    (define depth 1)
    
    (hist (repeat 300 (lambda () (listener some-sprouted depth))))

A more complex version of the model takes into account the listener's knowledge about the speaker's access to the items the speaker is referring to. In this version, lack of access can lead to a cancelled implicature (i.e. "some" does not imply "not all"):

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
      (rejection-query
       (define sentence (sentence-prior))
       sentence
       (equal? (belief state access)
               (listener access sentence depth))))
    
    (define (listener speaker-access sentence depth)
      (rejection-query
       (define state (state-prior))
       state
       (if (= 0 depth)
           (sentence state)
           (equal? sentence
                   (speaker speaker-access state (- depth 1))))))
    
    (define (num-true state)
      (sum (map (lambda (x) (if x 1 0)) state)))
    
    (define (show thunk)
      (hist (repeat 100 thunk)))
    
    ;; without full knowledge:
    (show (lambda () (num-true (listener '(#t #t #f) some-p 1))))
    
    ;; with full knowledge:
    (show (lambda () (num-true (listener '(#t #t #t) some-p 1))))

References:

- Cite:Goodman2013xz
- Cite:Frank2012fe
- Cite:Stuhlmueller2013aa
- Cite:ProbMods
