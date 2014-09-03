---
layout: model
title: Surprisingly Rational, Costello & Watts (2014)
---

Unbiased probability estimation for p=.5:

~~~~
(define (bool-sum lst)
  (sum (map boolean_to_number lst)))

(define (noisify flag noise-prob)
  (or 
   (and flag (flip (- 1 noise-prob)))
   (and (not flag) (flip noise-prob))))

(define (noisy-probability episodes noise true-prob)
  (/ (bool-sum 
      (repeat episodes 
              (lambda () (noisify (flip true-prob) noise))))
     episodes))

(define (expected-noisy-probability episodes noise true-prob)
  (mean
   (repeat 1000 
           (lambda () (noisy-probability episodes noise true-prob)))))

(define estimate
  (noisy-probability 10000 0.25 0.5))
  
(barplot
 (list
  (list "estimated p" "estimated 1-p")
  (list estimate (- 1 estimate)))
 "Unbiased probability estimate when p=0.5")
~~~~

Conservatism towards the ends of the scale:

~~~~
;;;fold:

(define (bool-sum lst)
  (sum (map boolean_to_number lst)))

(define (noisify flag noise-prob)
  (or 
   (and flag (flip (- 1 noise-prob)))
   (and (not flag) (flip noise-prob))))

(define (noisy-probability episodes noise true-prob)
  (/ (bool-sum 
      (repeat episodes 
              (lambda () (noisify (flip true-prob) noise))))
     episodes))

(define (expected-noisy-probability episodes noise true-prob)
  (mean
   (repeat 1000 
           (lambda () (noisy-probability episodes noise true-prob)))))
;;;

(barplot
 (list
  (list .1 .5 .9) 
  (map (lambda (p) (expected-noisy-probability 100 0.25 p))
       (list .1 .5 .9)))
 "Conservatism")
~~~~

Subadditivity:

~~~~
;;;fold:

(define (bool-sum lst)
  (sum (map boolean_to_number lst)))

(define (noisify flag noise-prob)
  (or 
   (and flag (flip (- 1 noise-prob)))
   (and (not flag) (flip noise-prob))))

(define (noisy-probability episodes noise true-prob)
  (/ (bool-sum 
      (repeat episodes 
              (lambda () (noisify (flip true-prob) noise))))
     episodes))

(define (expected-noisy-probability episodes noise true-prob)
  (mean
   (repeat 1000 
           (lambda () (noisy-probability episodes noise true-prob)))))
;;;

(define two-state-dist (list 0.3 0.7))
(define three-state-dist (list 0.3 0.4 0.3))
(define five-state-dist (list 0.3 0.2 0.05 0.2 0.25))

(define (sum-of-expectations dist)
  (sum (map (lambda (p) (expected-noisy-probability 100 0.25 p)) dist)))

(barplot
 (list 
  (list "two states" "three states" "five states")
  (map sum-of-expectations
       (list two-state-dist
             three-state-dist
             five-state-dist)))
 "Subadditivity")
~~~~

Conjunction fallacy:

Supposedly highest when P(A) is low, P(A|B) & P(B) is high, or highest when P(A) is almost the same as P(A^B).
                   
~~~~
;;;fold:

(define (bool-sum lst)
  (sum (map boolean_to_number lst)))

(define (noisify flag noise-prob)
  (or 
   (and flag (flip (- 1 noise-prob)))
   (and (not flag) (flip noise-prob))))

(define (noisy-probability episodes noise true-prob)
  (/ (bool-sum 
      (repeat episodes 
              (lambda () (noisify (flip true-prob) noise))))
     episodes))

(define (expected-noisy-probability episodes noise true-prob)
  (mean
   (repeat 1000 
           (lambda () (noisy-probability episodes noise true-prob)))))
;;;

(define (conjunction-fallacy? a a&b episodes noise)
  (< (noisy-probability episodes noise a)
     (noisy-probability episodes noise a&b)))

(define max-a|b (lambda (a b) (/ a b)))

(define a&b (lambda (a|b b) (* a|b b)))

(hist (repeat 1000 
              (lambda () 
                (conjunction-fallacy? 0.1 (a&b (max-a|b 0.1 0.9) 0.9) 100 0.25))) 
      "Conjunction fallacy ('optimal' conditions)")

(hist (repeat 1000 
              (lambda () 
                (conjunction-fallacy? 0.75 0.4 100 0.25)))
      "No conjunction fallacy ('bad' conditions)")
~~~~
