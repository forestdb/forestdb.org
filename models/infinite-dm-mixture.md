---
layout: model
title: Infinite Dirichlet-Multinomial Mixture
model-status: code-fail
model-status-verbose: Unknown error.
model-category: Nonparametric Models
model-tags: mem, nonparametrics, mixture
---

We use the Dirichlet Process to construct a distribution on a potentially infinite number of bags. Each bag is associated with a discrete distribution on the three colors `blue`, `green`, and `red`. We draw marbles, with each marble drawn randomly from either a previously seen or a new bag.

    (define colors '(blue green red))
    
    (define samples
     (mh-query
       200 100
    
       (define phi (dirichlet '(1 1 1)))
       (define alpha 0.1)
       (define prototype (map (lambda (w) (* alpha w)) phi))
    
       (define bag->prototype (mem (lambda (bag) (dirichlet prototype))))
    
       ;;the prior distribution on bags is simply a DPmem of gensym:
       (define get-bag (DPmem 1.0 gensym))
    
       ;;each observation comes from one of the bags:
       (define obs->bag (mem (lambda (obs-name) (get-bag))))
    
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
