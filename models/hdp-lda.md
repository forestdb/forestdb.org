---
layout: model
title: Latent Dirichlet Allocation with Hierarchical Dirichlet Prior
model-status: code-fail
model-status-verbose: Code is only a sketch so far.
model-category: PPAML Challenge Problems
model-tags: benchmark
---

    (define num-words 20)
    
    (define corpus-word-ind (iota (length corpus-words)))
     
    (define word-hypers (make-list num-words 1.0))
        
    (mh-query
      (define topic (DPmem 1.0 gensym))
      (define word-from-topic (mem (lambda (topic) (make-dirichlet-discrete word-hypers))))
      (define topic-from-doc  (mem (lambda (doc) (DPmem 1.0 topic))))
      (define word-topic (mem (lambda (doc word-index) (sample (topic-from-doc doc)))))
      (define word (mem (lambda (doc word-index) (sample (word-from-topic (word-topic doc word-index))))))
      (map word-topic corpus-docs corpus-word-ind)
      (equal? (map word corpus-docs corpus-word-ind) corpus-observed-words))

References:

- Cite:ProbMods
