---
layout: model
title: Pedagogy
model-status: code
model-category: Reasoning about Reasoning
model-tags: communication, pedagogy
model-language: church
---

A teacher chooses examples for a learner, who tries to infer the correct hypotheses from the teacher's examples:

    (define (teacher die depth)
      (rejection-query
       (define side (side-prior))
       side
       (equal? die (learner side depth))))
    
    (define (learner side depth)
      (rejection-query
       (define die (die-prior))
       die
       (if (= depth 0)
           (equal? side (roll die))
           (equal? side (teacher die (- depth 1))))))
    
    (define (die->probs die)
      (case die
        (('A) '(0.0 0.2 0.8))
        (('B) '(0.1 0.3 0.6))
        (else 'uhoh)))
    
    (define (side-prior) (uniform-draw '(red green blue)))
    (define (die-prior) (if (flip) 'A 'B))
    (define (roll die) (multinomial '(red green blue) (die->probs die)))
    
    (define depth 1)
    
    (hist (repeat 500 (lambda () (learner 'green depth))))

References:

- Cite:shafto2012learning
- Cite:ProbMods
