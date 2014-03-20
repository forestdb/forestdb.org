---
layout: model
title: Schelling Coordination Game
model-status: code
model-category: Nested Inference
model-tags: theory of mind, game theory
---

Two agents, Alice and Bob, want to meet. They choose which bar to
go to by recursively reasoning about one another
[@schelling.t:1960].

    (define (sample-location)
      (if (flip .6)
          'good-bar
          'bad-bar))
    
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
    
    (hist (repeat 100 (lambda () (bob 1))))
