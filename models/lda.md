---
layout: model
title: Latent Dirichlet Allocation
model-status: code
model-category: Machine Learning
model-tags: language, hierarchical models
---

LDA is a model that can be used to discover the topics of documents.

    (define vocabulary (append '(bear wolf)'(python prolog)))
    
    (define topics '(topic1 topic2))
    
    (define doc-length 10)
    
    (define doc1 '(bear wolf bear wolf bear wolf python wolf bear wolf))
    (define doc2 '(python prolog python prolog python prolog python prolog python prolog))
    (define doc3 '(bear wolf bear wolf bear wolf bear wolf bear wolf))
    (define doc4 '(python prolog python prolog python prolog python prolog python prolog))
    (define doc5 '(bear wolf bear python bear wolf bear wolf bear wolf))
    
    (query
    
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
      (equal? (document->words 'doc1) doc1)
      (equal? (document->words 'doc2) doc2)
      (equal? (document->words 'doc3) doc3)
      (equal? (document->words 'doc4) doc4)
      (equal? (document->words 'doc5) doc5)))

References:

- Cite:Blei2003tn
- Cite:ProbMods
