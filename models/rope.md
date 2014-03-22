---
layout: model
title: Rope-Pulling
model-status: code
model-category: Miscellaneous
model-tags: hierarchical models
---

The rope-pulling model is a simple hierarchical model that illustrates compositionality in human reasoning.

    (define num-people 4)
    
    (define alice 0)
    (define bob 1)
    (define sue 2)
    (define tom 3)
    
    (define team1 (list alice bob))
    (define team2 (list sue tom))
    
    (define (sample-strength)
      (if (flip) 10 5))
    
    (query
    
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
       (if (&lt; (total-pulling team1)
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
          (eq? 'team1 (winner team1 team2))))

References:

- Cite:ProbMods
