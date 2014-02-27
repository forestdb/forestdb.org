---
layout: model
title: 1D Ising
model-status: code
model-category: Undirected Constraints
model-tags: 
---

Version 1:

    (define num-spins 4)
    
    (define J '((0 1 0 0)
                (0 0 1 0)
                (0 0 0 1)
                (0 0 0 0)))
    
    (query
     (define spins (repeat num-spins (lambda () (if (flip) 1 -1))))
     spins
     (all
      (map (lambda (J-row x)
             (map (lambda (energy y)
                    (log-flip (- (* energy x y) (abs energy))))
                  J-row spins))
           J spins)))

Version 2:

    (letrec 
        ([eq (factor (x y) (if (= x y) 0.0 (log 0.1)))]
         [xs (repeat 10 (lambda () (xrp+init randint-scorer randint randint-prop 0 0 1)))]
         [constr (map (lambda (xy) (eq (car xy) (car (cdr xy)))) (bigram xs))])
      xs))
      
Source: [shred](https://github.com/LFY/shred/blob/master/tests/ising.ss)      
