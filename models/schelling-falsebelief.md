---
layout: model
title: Schelling coordination game with false belief
model-status: code
model-category: Nested Inference
model-tags: theory of mind, game theory
---

Bob believes that Alice wants to meet him, and Alice knows this,
but in fact Alice wants to avoid Bob [@schelling.t:1960].

    (define (sample-location)
      (if (flip .55)
          'popular-bar
          'unpopular-bar))
    
    (define (alice* depth)
      (query
       (define alice*-location (sample-location))
       alice*-location
       (not (equal? alice*-location (bob (- depth 1))))))
    
    (define (alice depth)
      (query
       (define alice-location (sample-location))
       alice-location
       (equal? alice-location (bob (- depth 1)))))
    
    (define (bob depth)
      (query
       (define bob-location (sample-location))
       bob-location
       (or (= depth 0)
           (equal? bob-location (alice depth)))))
    
    (alice* 5)
