---
layout: model
title: Latent Dirichlet Allocation (benchmark)
model-status: code
model-category: Machine Learning
model-tags: shred, benchmark, machine learning
---

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
    
References 

- Cite:shred2014
- Source: [shred](https://github.com/LFY/shred/blob/master/benchmarks/topic-lda.ss)
 
