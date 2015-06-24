---
layout: model
title: Schelling Coordination Game with False Belief
model-status: code
model-category: Reasoning about Reasoning
model-tags: theory of mind, game theory
model-language: church
---

Bob believes that Alice wants to meet him, and Alice knows this,
but in fact Alice wants to avoid Bob.

    (define (sample-location)
      (if (flip .55)
          'good-bar
          'bad-bar))
    
    (define (alice* depth)
      (rejection-query
       (define alice*-location (sample-location))
       alice*-location
       (not (equal? alice*-location (bob (- depth 1))))))
    
    (define (alice depth)
      (rejection-query
       (define alice-location (sample-location))
       alice-location
       (equal? alice-location (bob (- depth 1)))))
    
    (define (bob depth)
      (rejection-query
       (define bob-location (sample-location))
       bob-location
       (if (= depth 0)
           #t
           (equal? bob-location (alice depth)))))
    
    (hist (repeat 10 (lambda () (alice* 3))))

References:

- Cite:Schelling1960
- Cite:Stuhlmueller2013aa
