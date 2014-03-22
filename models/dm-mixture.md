---
layout: model
title: Finite Dirichlet-Multinomial Mixture
model-status: code
model-category: Miscellaneous
model-tags: mixture, multinomial, dirichlet
---

We construct a mixture distribution on draws from three bags of marbles. Each bag is associated with a discrete distribution on the three colors `blue`, `green`, and `red`. We draw marbles, with each marble drawn randomly from one of the bags.

    (define colors '(blue green red))
    
    (define samples
     (mh-query
       200 100
    
       (define phi (dirichlet '(1 1 1)))
       (define alpha 0.1)
       (define prototype (map (lambda (w) (* alpha w)) phi))
    
       (define bag->prototype (mem (lambda (bag) (dirichlet prototype))))
    
       ;;the probability that an observation will come from each bag:
       (define bag-mixture (dirichlet '(1 1 1)))
    
       ;;each observation (which is named for convenience) comes from one of three bags:
       (define obs->bag
         (mem (lambda (obs-name)
                (multinomial '(bag1 bag2 bag3) bag-mixture))))
    
       (define draw-marble
         (mem (lambda (obs-name)
                (multinomial colors (bag->prototype (obs->bag obs-name))))))
    
       ;;did obs1 and obs2 come from the same bag? obs1 and obs3?
       (list (equal? (obs->bag 'obs1) (obs->bag 'obs2))
             (equal? (obs->bag 'obs1) (obs->bag 'obs3)))
    
       (and
        (equal? 'red (draw-marble 'obs1))
        (equal? 'red (draw-marble 'obs2))
        (equal? 'blue (draw-marble 'obs3))
        (equal? 'blue (draw-marble 'obs4))
        (equal? 'red (draw-marble 'obs5))
        (equal? 'blue (draw-marble 'obs6))
        )))
    
    (hist (map first samples) "obs1 and obs2 same category?")
    (hist (map second samples) "obs1 and obs3 same category?")
    'done

References:

- Cite:ProbMods
