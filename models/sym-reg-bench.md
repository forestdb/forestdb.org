

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
    
    
    (define (pairs xs)
                  (cond [(is_null xs) '()]
                        [(is_null (rest xs)) '()]
                        [else
                          (append (map (lambda (i) (pair (first xs) (pair i '()))) 
                                       (rest xs))
                                  (pairs (rest xs)))]))
    
    (define (citation-paper-score c p)
      (let* ([c-auth (first c)]
               [c-title (second c)]
               [p-auth (first (rest p))]
               [p-title (rest (rest p))])
        (+ 
          (if (equal? c-auth p-auth) 0.0 (log 0.1))
          (if (equal? c-title p-title) 0.0 (log 0.1)))))
    
    (define
      (repulsion p1 p2)
      (let* ([p1-id (first p1)]
               [p2-id (first p2)])
        (if (equal? p1-id p2-id) 0.0
          (let* ([p1-auth (first (rest p1))]
                   [p2-auth (first (rest p2))]
                   [p1-title (rest (rest p1))]
                   [p2-title (rest (rest p2))])
            (+ 
              (if (equal? p1-auth p2-auth) (log 0.1) 0.0)
              (if (equal? p1-title p2-title) (log 0.1) 0.0))))))
    
    (define samples
      (mh-query 10 100
                (define citations
                  '(("A" "T") ("A" "U") ("B" "X") ("A" "Y") ("B" "X")))
                (define sample-paper-id (lambda () (uniform 0 1)))
                (define sample-author
                  (lambda ()
                    (multinomial '("A" "B" "C" "D") '(0.25 0.25 0.25 0.25))))
                (define sample-title
                  (lambda ()
                    (multinomial '("T" "U" "V" "W" "X")
                                 '(0.2 0.2 0.2 0.2 0.2))))
                (define sample-paper
                  (lambda ()
                    (pair (sample-paper-id)
                          (pair (sample-author) (sample-title)))))
                (define paper-distr (DPmem 0.4 (lambda () (sample-paper))))
                (define factor-citation-paper
                  (lambda (c p) (factor (citation-paper-score c p))))
                (define factor-repulsion
                  (lambda (p1 p2) (factor (repulsion p1 p2))))
                (define papers+factors
                  (map
                    (lambda (c)
                      (let* ((paper (paper-distr))
                               (paper-citation-factor
                                 (factor-citation-paper c paper)))
                        (pair paper paper-citation-factor)))
                    citations))
                (define repel-factors
                  (map
                    (lambda (p1p2)
                      (let* ((p1 (first (first p1p2)))
                               (p2 (first (second p1p2))))
                        (factor-repulsion p1 p2)))
                    (pairs papers+factors)))
                (define sample (zip citations papers+factors))
                sample #t ))
    
    samples
    
