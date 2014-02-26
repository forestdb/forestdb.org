---
layout: model
title: Schelling coordination game
model-status: code
model-category: Nested Inference
model-tags: theory of mind, game theory
---

Two agents, Alice and Bob, want to meet. They choose which bar to
go to by recursively reasoning about one another
[@schelling.t:1960].

    (define (sample-location)
      (if (flip .55)
          'popular-bar
          'unpopular-bar))
    
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
