---
layout: model
title: Gaussian Process Classifier
model-status: code
model-category: Nonparametric Models
model-tags: gp, nonparametrics, classifier
---

    ;; Matrix utility functions
    
    (define (split-list-first split lst n)
      (if (= n 0)
          (cons split lst)
          (split-list-first (append split (list (car lst))) (cdr lst) (- n 1))))
    
    (define (zero-vector n)
      ; list of n zeros
      (make-list n 0) )
    
    ;; Basic matrix functions
    
    (define (m-reshape-col elements m n)
      ; Appends elements to matrix
      ; m rows, n columns
      ; Produces matrix as list of columns
      ; Assumes elements are column ordered i.e. (1 1) (2 1) ...
      (let* ([split (split-list-first '() elements m)]
             [new-column (car split)]
             [remaining-elements (cdr split)] )
        (if (= n 1)
            (cons new-column '())
            (cons new-column (m-reshape-col remaining-elements m (- n 1))) ) ) )
    
    (define (m-reshape-row elements m n)
      ; Appends elements to matrix
      ; m rows, n columns
      ; Produces matrix as list of rows
      ; Assumes elements are row ordered i.e. (1 1) (1 2) ...
      (let* ([split (split-list-first '() elements n)]
             [new-row (car split)]
             [remaining-elements (cdr split)] )
        (if (= m 1)
            (cons new-row '())
            (cons new-row (m-reshape-row remaining-elements (- m 1) n)) ) ) )
    
    (define (m->el A i j)
      ; Returns element (i j) of matrix A
      (list-ref (list-ref A (- i 1)) (- j 1)) )
    
    (define (mr*v mr v)
      ; Matrix in row format right multiplied by vector
      (let ([dot-prod (sum (map * (car mr) v))])
        (if (= (length mr) 1)
            (list dot-prod)
            (cons dot-prod (mr*v (cdr mr) v)) ) ) )
    
    (define (matrix-map-1 proc A)
      ; Map proc onto each element of A
      (map (lambda (a) (map proc a)) A) )
    
    (define (matrix-map-2 proc A B)
      ; Map proc onto each element of A and B
      (map (lambda (a b) (map proc a b)) A B) )
    
    ;; Cholesky decomposition
    
    ;; A crude and slow implementation of the Cholesky-Banachiewicz algorithm
    
    (define (chol A)
      ; Lower triangular Cholesky decomposition of A
      ; Assumes that A is a matrix of rows - but it might not matter
      (let ([L11 (sqrt (car (car A)))])
        (chol-square (chol-iter A (cons 2 1) (list (list L11)))) ) )
    
    (define (chol-iter A ip L)
      ; Top level iteration of cholesky
      (if (> (car ip) (length A))
          L
          (chol-iter A (chol-next-index-pair ip) (chol-add-element A L (car ip) (cdr ip))) ) )
    
    (define (chol-add-element P L i j)
      ; if j = 1 then add the new element into a new row
      ; else add the new element at the end of the last row
      (let ([new-el (chol-get-element P L i j)]
            [previous-rows (chol-truncate L)]
            [last-row (m->last-row/col L)])
        (cond
         ((= j 1) (append L (list (list new-el))))
         (else (append previous-rows (list (append last-row (list new-el))))) ) ) )
    
    (define (chol-next-index-pair ip)
      ; Defines the order of iteration in Cholesky algorithm
      ; ip = (i . j)
      ; if i = j then return (i+1 . 1)
      ; else return (i . j+1)
      (cond
       ((= (car ip) (cdr ip)) (cons (+ (car ip) 1) 1))
       (else (cons (car ip) (+ (cdr ip) 1))) ) )
    
    (define (chol-get-element P L i j)
      ; The Cholesky formula
      (cond
       ((= i j) (sqrt (- (m->el P i i) (chol-ps L i i))))
       (else (/ (- (m->el P i j) (chol-ps L i j)) (m->el L j j))) ) )
    
    (define (chol-ps L i j)
      ; The sum in the formula for L_{ij}
      ; \sum_{k=1}^{j-1} L_{ik}L_{jk}
      (chol-ps-iter (- j 1) L i j) )
    
    (define (chol-ps-iter k L i j)
      ; The main iterative loop for the Cholesky partial sum
      (cond
       ((= k 0) 0)
       ((= k 1) (* (m->el L i 1) (m->el L j 1)))
       (else (+ (* (m->el L i k) (m->el L j k))
                (chol-ps-iter (- k 1) L i j) )) ) )
    
    ;; Cholesky utilities
    
    (define (chol-square A)
      ; pad the rows of A with zeros to make square
      (map (lambda (v) (append v (zero-vector (- (length A) (length v))))) A) )
    
    ;; GPs
    
    (define (sqr x)
      (expt x 2) )
    
    (define (mv-gaussian mean sigma)
      ; mean + chol(sigma) * gaussians
      (map + mean (mr*v (chol sigma) (repeat (length mean) (lambda () (gaussian 0 1))))) )
    
    (define (euclidean-squared-distance X Y)
      (sum (map (lambda (x y) (sqr (- x y))) X Y)) )
    
    (define (euclidean-squared-distances X Ys)
      (map (lambda (Y) (euclidean-squared-distance X Y)) Ys) )
    
    (define (cov-se-iso-row x xs ell sf)
      (map (lambda (a) (* (exp (- 0 a)) (sqr sf))) (map (lambda (b) (/ b (* 2 (sqr ell)))) (euclidean-squared-distances x xs))) )
    
    (define (cov-se-iso x ell sf)
      (map (lambda (a) (cov-se-iso-row a x ell sf)) x) )
    
    (define (gp-se-iso x mean ell sf)
      (mv-gaussian mean (cov-se-iso x ell sf)) )
    
    (define (sigmoid x)
      (/ (exp x) (+ 1 (exp x))) )
    
    (define (m->last-row/col A)
      ; Returns the last row or column of a matrix
      ; Last row or column depends on the format of the matrix
      (car (reverse A)) )
    
    (define (chol-truncate A)
      ; Returns everything but the last row
      (if (null? (cdr A)) 
          '()
          (reverse (cdr (reverse A))) ) )
    
    (define input '((1) (2) (3) (4) (5) (6) (7) (8) (9) (10) (11) (12) (13) (14) (15) (16) (17) (18) (19) (20)))
    (define observations '(#t #t #t #t #t #f #f #f #f #f #t #t #t #t #t #f #f #f #f #f))
    
    (define samples
      (mh-query 
       
       10 10
       
       ;; Define the generative model and the likelihood of observation
       
       ;; Setup
       
       (define N 20)
       (define mu (zero-vector N))
       
       ; Broad priors on hyperparameters
       (define ell (+ 1 (min 2 (exp 0.1))))
       (define sf (+ 1 (exp 0.1)))
       
       ;; The model
       
       (define latent-gp (gp-se-iso input mu ell sf))
       
       ;; Likelihood calculations
       
       (define (bernoulli-likelihood p b)
         (if b p (- 1 p)) )
       
       (define (gp-class-log-lik gp obs)
         (sum (map (lambda (l b) (log (bernoulli-likelihood (sigmoid l) b))) gp obs)) )
       
       (define log-likelihood (gp-class-log-lik latent-gp observations))
       
       (define _ (factor log-likelihood))
       
       ;; Quantity to sample
       
       latent-gp
       
       #t
       
       ); mh-query
      ); define samples
    
    samples

References:

- James Lloyd (2012)
