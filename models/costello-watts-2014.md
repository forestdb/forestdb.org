---
layout: model
title: Surprisingly Rational, Costello & Watts (2014)
---

This is a church version of their model and some of their derived effects. I wrote this up quickly so there may be bugs / unsightly code.

(define bool-sum (lambda (lst)
                   (sum (map boolean_to_number lst))))

(define noisy-probability
  (lambda (episodes noise true-prob)
    (repeat episodes 
            (lambda () 
              (let ([flag (flip true-prob)])
                (or 
                 (and flag (flip (- 1 noise)))
                 (and (not flag) (flip noise))))))))

(define (expected-noisy-prob episodes noise true-prob) 
  (repeat 1000 (lambda () (bool-sum (noisy-probability episodes noise true-prob)))))


; single subject

(hist (noisy-probability 10000 0.25 0.5) "true probability = 0.5")


;conservatism

;; (multiviz

;;  (hist (expected-noisy-prob 100 0.25 0.5) "true probability = 0.5")
;;  (hist (expected-noisy-prob 100 0.25 0.9) "true probability = 0.9")
;;  (hist (expected-noisy-prob 100 0.25 0.1) "true probability = 0.1")

;; )

;subadditivity

;; (define some-distribution (list 0.3 0.4 0.3))
;; (define some-other-distribution (list 0.3 0.2 0.05 0.2 0.25))
;; (define binary-complementarity (list 0.3 0.7))


;; ; note: expected-noisy-prob returns samples from noisy-probability "subjects" so MEAN

;; (define (sum-of-expectations dist)
;;   (sum (map (lambda (p) (mean (expected-noisy-prob 100 0.25 p))) dist)))

;; (list 
;;  (list "3 state dist" (sum-of-expectations some-distribution) )
;;  (list "5 state dist" (sum-of-expectations some-other-distribution))
;;  (list "2 state dist" (sum-of-expectations binary-complementarity)))


;conjunction fallacy

;; (define conjunction-fallacy? 
;;   (lambda (a a&b episodes noise)
;;     (< (bool-sum (noisy-probability episodes noise a))
;;        (bool-sum (noisy-probability episodes noise a&b)))))
   
;;   ;; supposedly highest when P(A) is low, P(A|B) & P(B) is high
;;   ;; or highest when P(A) is almost the same as P(A^B)
  
;; (define max-a|b (lambda (a b) (/ a b)))

;; (define a&b (lambda (a|b b) (* a|b b)))

;; (multiviz
;;  (hist (repeat 1000 (lambda () 
;;                       (conjunction-fallacy? 0.1 (a&b (max-a|b 0.1 0.9) 0.9) 100 0.25))) 
;;        "'optimal' conjunction fallacy conditions")
 
;;  (hist (repeat 1000 (lambda () 
;;                       (conjunction-fallacy? 0.75 0.4 100 0.25)))
;;        "bad conjunction fallacy conditions")
 
;;  (hist (repeat 1000 (lambda () 
;;                       (conjunction-fallacy? 0.75 (a&b (max-a|b 0.75 0.5) 0.5) 100 0.25)))
;;        "novel prediction?")
;;  )
