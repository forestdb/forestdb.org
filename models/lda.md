---
layout: model
title: Latent Dirichlet Allocation
model-status: code
model-category: Machine Learning
model-tags: language, hierarchical models
---

LDA is a model that can be used to discover the topics of documents.

    ;;;fold: factor-equal?
    (define (factor-eq x y)
      (factor (if (equal? x y) 0.0 -1000))
      #t)
    
    (define (factor-equal? xs ys)
      (if (and (null? xs) (null? ys))
          #t
          (and (factor-eq (first xs) (first ys))
               (factor-equal? (rest xs) (rest ys)))))
    ;;;
    (define vocabulary (append '(bear wolf)'(python prolog)))
    
    (define topics '(topic1 topic2))
    
    (define doc-length 10)
    
    (define doc1 '(bear wolf bear wolf bear wolf python wolf bear wolf))
    (define doc2 '(python prolog python prolog python prolog python prolog python prolog))
    (define doc3 '(bear wolf bear wolf bear wolf bear wolf bear wolf))
    (define doc4 '(python prolog python prolog python prolog python prolog python prolog))
    (define doc5 '(bear wolf bear python bear wolf bear wolf bear wolf))
    
    (define samples
      (mh-query
       100 100
       
       (define document->mixture-params
         (mem (lambda (doc-id) (dirichlet (make-list (length topics) 1.0)))))
       
       (define topic->mixture-params
         (mem (lambda (topic) (dirichlet (make-list (length vocabulary) 0.1)))))
       
       (define document->topics
         (mem (lambda (doc-id)
                (repeat doc-length
                        (lambda () (multinomial topics (document->mixture-params doc-id)))))))
       
       (define document->words
         (mem (lambda (doc-id)
                (map (lambda (topic)
                       (multinomial vocabulary (topic->mixture-params topic)))
                     (document->topics doc-id)))))
       
       (map topic->mixture-params topics)
       
       (and
        (factor-equal? (document->words 'doc1) doc1)
        (factor-equal? (document->words 'doc2) doc2)
        (factor-equal? (document->words 'doc3) doc3)
        (factor-equal? (document->words 'doc4) doc4)
        (factor-equal? (document->words 'doc5) doc5))))
    
    (car (reverse samples))

References:

- Cite:Blei2003tn
- Cite:ProbMods
