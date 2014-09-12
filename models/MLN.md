---
layout: model
title: Markov Logic Network
model-status: code
model-category: Miscellaneous
model-tags: mln
---

    (define (implies x y) (or (not x) y))
    
    (define samples
      (mh-query 1000 100
        (define people (list 'anna 'bob))
        (define smokes (mem (lambda (person) (flip))))
        (define cancer (mem (lambda (person) (flip))))
        (define friends (mem (lambda (x y) (flip))))
    
        (define smokes-cancer (sum (map 
          (lambda (x) (if (implies (smokes x) (cancer x)) 0 -1.5)) 
          people
        )))
        (define friends-smoke (sum (map
            (lambda (x) (sum (map
              (lambda (y) (if (implies (friends x y) (eq? (smokes x) (smokes y))) 0 -1.1))
              people
              )))
            people
        )))    
        (define model (+ smokes-cancer friends-smoke))
        (define evidence (and (smokes 'anna) (friends 'anna 'bob)))
    
        (cancer 'bob)
    
        (and evidence (< (uniform 0 1) (exp model)))
      )
    )
    (hist samples)
