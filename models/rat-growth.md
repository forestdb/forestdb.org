---
layout: model
title: Rat Growth
model-status: code
model-category: Undirected Constraints
model-tags: 
---

    (define (zip xs1 xs2) 
      (if (or (is_null xs1) (is_null xs2)) '() 
        (pair 
          (pair (first xs1) (pair (first xs2) '()))
          (zip (rest xs1) (rest xs2)))))
    
    (define my-pi 3.14159265358979323)
    
    (define (glp mean ssq smp)
      (let ([diff (- smp mean)])
        (- (- (/ (* diff diff) (* 2.0 ssq)))
           (* 0.5 (+ (log 2) (log my-pi) (log ssq))))))
    
    (define ratsdata
      '((151.0 199.0 246.0 283.0 320.0) (145.0 199.0 249.0 293.0 354.0)
        (147.0 214.0 263.0 312.0 328.0) (155.0 200.0 237.0 272.0 297.0)
        (135.0 188.0 230.0 280.0 323.0) (159.0 210.0 252.0 298.0 331.0)
        (141.0 189.0 231.0 275.0 305.0) (159.0 201.0 248.0 297.0 338.0)
        (177.0 236.0 285.0 350.0 376.0) (134.0 182.0 220.0 260.0 296.0)
        (160.0 208.0 261.0 313.0 352.0) (143.0 188.0 220.0 273.0 314.0)
        (154.0 200.0 244.0 289.0 325.0) (171.0 221.0 270.0 326.0 358.0)
        (163.0 216.0 242.0 281.0 312.0) (160.0 207.0 248.0 288.0 324.0)
        (142.0 187.0 234.0 280.0 316.0) (156.0 203.0 243.0 283.0 317.0)
        (157.0 212.0 259.0 307.0 336.0) (152.0 203.0 246.0 286.0 321.0)
        (154.0 205.0 253.0 298.0 334.0) (139.0 190.0 225.0 267.0 302.0)
        (146.0 191.0 229.0 272.0 302.0) (157.0 211.0 250.0 285.0 323.0)
        (132.0 185.0 237.0 286.0 331.0) (160.0 207.0 257.0 303.0 345.0)
        (169.0 216.0 261.0 295.0 333.0) (157.0 205.0 248.0 289.0 316.0)
        (137.0 180.0 219.0 258.0 291.0) (153.0 200.0 244.0 286.0 324.0)))
    
    
    (define samples
      (mh-query 10 100
        (define alphac (gaussian 0.0 1e4))
        (define betac (gaussian 0.0 1e4))
        (define tauc (gamma 1e-3 1e3))
        (define taualpha (gamma 1e-3 1e3))
        (define taubeta (gamma 1e-3 1e3))
        (define xbar 22.0)
        (define gauss-factor (lambda (m v x) (factor (glp m v x))))
        (define x '(8.0 15.0 22.0 29.0 36.0))
        (define void-local
          (map
            (lambda (s)
              (let* ((alpha-rat
                       (gaussian alphac
                                 (/ 1.0 taualpha)))
                     (beta-rat
                       (gaussian betac
                                 (/ 1.0 taubeta)))
                     (y-constrs
                       (map
                         (lambda (x-w)
                           (gauss-factor
                             (+ alpha-rat
                                (* beta-rat (- (first x-w) xbar)))
                             tauc (second x-w)))
                         (zip x s))))
                1.0))
            ratsdata))
        (define sample (- alphac (* betac xbar))) 
        sample 
        #t))
    samples


 
Source: [shred](https://github.com/LFY/shred/blob/master/tests/rats.church)
