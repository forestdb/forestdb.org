---
layout: model
title: Latent Dirichlet Allocation
model-status: code
model-category: Machine Learning
model-tags: language, hierarchical models, shred, benchmark, machine learning
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

The following is a version of the code closer to the one used in Ref:shred2014:

    (define (index-in x xs)
      (define (loop x k rst)
        (if (is_null rst) k
          (if (equal? (first rst) x) k
            (loop x (+ k 1) (rest rst)))))
        (loop x 0 xs))
    
    (define (word-factor i distr) 
      (let ((p (list-ref distr i)))
        (factor (log p))))
    
    (define number-of-topics 2)
    
    (define vocabulary '("A" "B" "C" "D"))
    
    (define documents
      '(("A" "A" "A") ("A" "C" "A") ("C" "C" "A")
        ("A" "A" "A") ("A" "C" "A") ("C" "C" "A")
        ("A" "A" "A") ("A" "C" "A") ("C" "C" "A")
        ("A" "A" "A") ("A" "C" "A") ("C" "C" "A")
        ("D" "D" "D") ("B" "D" "B") ("B" "B" "D")))
    
    (define samples
      (mh-query    
       100 100   
       
       (define topic-word-distributions
         (repeat number-of-topics 
                 (lambda () (dirichlet '(0.4 0.4 0.4 0.4)))))
       
       (define process
         (map
          (lambda (document)
            (let* ((topic-selection-distr (dirichlet '(0.3 0.3))))
              (map (lambda (word)
                     (let* ((sampled-topic (multinomial '(0 1) topic-selection-distr))
                            (idx (index-in word vocabulary)))
                       (word-factor idx (list-ref topic-word-distributions sampled-topic))
                       (pair 'topic (list-ref topic-word-distributions sampled-topic))))
                   document)))
          documents))
       
       topic-word-distributions
    
       #t))
    
    (define last-sample (car (reverse samples)))
    
    (barplot (list vocabulary (first last-sample)) "Topic 1")
    (barplot (list vocabulary (second last-sample)) "Topic 2")

References:

- Cite:Blei2003tn
- Cite:ProbMods
- Cite:shred2014
