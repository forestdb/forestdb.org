---
layout: model
title: Monty Hall Problem
model-status: code
model-category: Miscellaneous
model-tags: toy
model-language: church
---

In a game show, a contestant is presented three doors. One door has a car behind it; the other two doors only have goats. The contestant chooses a door, which remains closed. The game host reveals one of the remaining two, and the contestant has the option to switch. On average, should the contestant switch?

    ; choose a random index of a list
    (define (random-list-index l)
      (sample-discrete (make-list (length l) (div 1 (length l)))))
    
    ; remove a value from a list
    (define (filter-out a-list value)
      (filter (lambda (x) (if (equal? x value) #f #t)) a-list))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define door-options (list 0 1 2))
    
    (define (pick-door)
      (random-list-index door-options))
    
    (define (gen-doors car-door)
      (let [(doors (list 'goat 'goat 'goat))]
        (update-list doors car-door 'car)))
    
    ; reveal a door after the contestant chooses one
    (define (show-door first-choice car-door)
      (if (= first-choice car-door)
          (random-list-index (filter-out door-options car-door))
          (first (filter-out (filter-out door-options first-choice) car-door))))
    
    (define samples
      (mh-query 
       10000 1
       
       (define car-door (pick-door))
       (define doors (gen-doors car-door))
       
       (define first-choice (pick-door))
       (define shown-door (show-door first-choice car-door))
         
       (define switch-doors (flip .5))
         
       (define second-choice
         (if switch-doors
             (first (filter-out (filter-out door-options first-choice) shown-door))
             first-choice))
       
       (equal? second-choice car-door)
       
       switch-doors))

    (hist samples "Chance of winning if you switch")
    
References:

- [The Monty Hall problem](https://en.wikipedia.org/wiki/Monty_Hall_problem)
