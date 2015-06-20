---
layout: model
title: Tug of War
model-status: code
model-category: Miscellaneous
model-tags: hierarchical models
model-language: church
---

The "tug of war" model is a hierarchical model that can be used to illustrate the broad range of inferences that can be drawn from a relatively simple compositional model.

    (define num-people 4)
    
    (define alice 0)
    (define bob 1)
    (define sue 2)
    (define tom 3)
    
    (define team1 (list alice bob))
    (define team2 (list sue tom))
    
    (define (sample-strength)
      (if (flip) 10 5))
    
    (define (sample)
      (rejection-query
       
       (define strengths
         (repeat num-people sample-strength))
       
       (define (strength person)
         (list-ref strengths person))
       
       (define lazy (lambda (person) (flip (/ 1 3))))
       
       (define (total-pulling team)
         (sum
          (map (lambda (person)
                 (if (lazy person) (/ (strength person) 2) (strength person)))
               team)))
       
       (define (winner team1 team2)
         (if (< (total-pulling team1)
                (total-pulling team2))
             'team2
             'team1))
       
       (list (strength alice) (strength bob))
       
       (and (eq? 'team1 (winner team1 team2))
            (eq? 'team2 (winner team1 team2))
            (eq? 'team1 (winner team1 team2))
            (eq? 'team1 (winner team1 team2))
            (eq? 'team1 (winner team1 team2))
            (eq? 'team1 (winner team1 team2))
            (eq? 'team1 (winner team1 team2))
            (eq? 'team1 (winner team1 team2)))))
    
    (hist (repeat 100 sample))

References:

- Cite:Gerstenberg2012ping
- Cite:ProbMods
