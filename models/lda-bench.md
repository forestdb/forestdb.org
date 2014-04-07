---
layout: model
title: Latent Dirichlet Allocation (benchmark)
model-status: code-fail
model-category: Machine Learning
model-tags: shred, benchmark, machine learning
model-status-verbose: Unknown error.
---


    (define (zip xs1 xs2) 
      (if (or (is_null xs1) (is_null xs2)) '() 
        (pair 
          (pair (first xs1) (pair (first xs2) '()))
          (zip (rest xs1) (rest xs2)))))
    
    (define (pick-a-stick sticks J)
      (if (flip (sticks J))
          J
          (pick-a-stick sticks (+ J 1))))
    
    (define (make-sticks alpha)
      (let ((sticks (mem (lambda (x) (beta 1.0 alpha)))))
        (lambda () (pick-a-stick sticks 1))))
    
    (define (DPmem alpha base-dist)
      (let ((augmented-proc
              (mem (lambda (args stick-index) (apply base-dist args))))
            (DP (mem (lambda (args) (make-sticks alpha)))))
        (lambda argsin
          (let ((stick-index ((DP argsin))))
            (augmented-proc argsin stick-index)))))
    
    (define my-pi 3.14159265358979323)
    
    (define (glp mean ssq smp)
      (let ([diff (- smp mean)])
        (- (- (/ (* diff diff) (* 2.0 ssq)))
           (* 0.5 (+ (log 2) (log my-pi) (log ssq))))))
    
    (define (randint l h) (+ l (sample-integer (+ 1 (- h l)))))
    
    (define (index-in x xs)
      (define (loop x k rst)
        (if (is_null rst) k
          (if (equal? (first rst) x) k
            (loop x (+ k 1) (rest rst)))))
        (loop x 0 xs))
    
    (define (my-length xs)
      (if (is_null xs) 0
        (+ 1 (my-length (rest xs)))))
    
    (define samples
      (mh-query 10 100
        (define (word-prob i distr) (factor (log (list-ref distr i))))
        (define number-of-topics 2)
        (define vocabulary '("A" "B" "C" "D"))
        (define documents
          '(("A" "A" "A") ("A" "C" "A") ("C" "C" "A")
             ("A" "A" "A") ("A" "C" "A") ("C" "C" "A")
             ("A" "A" "A") ("A" "C" "A") ("C" "C" "A")
             ("A" "A" "A") ("A" "C" "A") ("C" "C" "A")
             ("D" "D" "D") ("B" "D" "B") ("B" "B" "D")))
        (define topic-word-distributions (repeat number-of-topics (lambda () (dirichlet '(0.4 0.4 0.4 0.4)))))
        (define process
          (map
            (lambda (document)
              (let* ((topic-selection-distr (dirichlet '(0.3 0.3))))
                (map (lambda (word)
                       (let* ((sampled-topic (multinomial '(0 1) topic-selection-distr))
                              (idx (index-in word vocabulary)))
                         (pair (pair 'topic (list-ref topic-word-distributions sampled-topic))
                               (word-prob idx (list-ref topic-word-distributions sampled-topic)))))
                     document)))
            documents))
        (define total-score
          (sum
            (map
              (lambda (proc)
                (sum (map (lambda (item) (rest item)) proc)))
              process)))
        (define sample (pair total-score process)) 
        sample 
        #t))
    samples
    
References 

- Cite:shred2014
- Source: [shred](https://github.com/LFY/shred/blob/master/benchmarks/topic-lda.ss)
 
