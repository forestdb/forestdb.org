---
layout: model
title: Nested Number Guessing
model-status: code
model-category: Nested Inference
model-tags: theory of mind
---

Two agents play a game in which each agent needs to name a number
between *0* and *9* and they win if their numbers add up to
*13*. The first player knows this, and he knows that the second
player gets to see the number the first player chooses, but the
second player mistakenly thinks that the two win if their numbers
add up to any number greater than *8* (and the first player knows
this as well). What number should the first player choose?

    (define (sample)
      (rejection-query
       (define a (sample-integer 10))
       (define b
         (rejection-query
          (define c (sample-integer 10))
          c
          (> (+ a c) 8)))
       a
       (= (+ a b) 13)))
    
    (hist (repeat 1000 sample))

References:

- Cite:Stuhlmueller2013aa
