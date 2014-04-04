---
layout: model
title: 1D Ising
model-status: code
model-category: Undirected Constraints
model-tags: 
---

Version 1:

    ;; just a hack - implement properly to avoid fp hell
    (define (log-flip lp) (flip (exp lp)))

    (define num-spins 4)

    (define J '((0 1 0 0)
                (0 0 1 0)
                (0 0 0 1)
                (0 0 0 0)))

    (define samples
      (mh-query
       50
       50
       (define spins (repeat num-spins (lambda () (if (flip) 1 -1))))
       spins
       (all
        (map
         (lambda (J-row x)
           (all
            (map
             (lambda (energy y)
               (log-flip (- (* energy x y) (abs energy))))
             J-row spins)))
         J 
         spins))))

    (car (reverse samples))
    

Version 2:

    (define ising-length 20)
    (define non-match-cost (log 0.1))

    ;; abuses map
    (define (bigram xs) (map list xs (cdr xs)))

    (define samples
      (mh-query
        50
        50
        (define (eql x y) (factor (if (= x y) 0.0 non-match-cost)))
        (define xs (repeat ising-length (lambda () (if (flip) 0 1))))
        (for-each (lambda (xy) (eql (car xy) (car (cdr xy)))) (bigram xs))
        xs
        #t))

    (car (reverse samples))

Source: [shred](https://github.com/LFY/shred/blob/master/tests/ising.ss)      
