---
layout: model
title: Birthday Problem
model-status: code
model-category: Miscellaneous
model-tags: toy
model-language: church
---

What is the probability that in a room filled with 23 people, at least one pair of people has the same birthday?

    (define N 23)
    
    (define (two-birthdays-match?)
      (rejection-query
       
       (define birthday 
         (mem (lambda (i) (+ (sample-integer 365) 1))))
       
       (define (pair-equal i j)
         (if (> i N) 
             false
             (if (> j N)
                 (pair-equal (+ i 1) (+ i 2))
                 (if (= (birthday i) (birthday j)) 
                     true
                     (pair-equal i (+ j 1))))))
       
       (pair-equal 1 2)
       
       #t))
    
    (hist (repeat 1000 two-birthdays-match?))
    
References:

- [Birthday problem in Anglican](http://www.robots.ox.ac.uk/~fwood/anglican/examples/birthday/)
