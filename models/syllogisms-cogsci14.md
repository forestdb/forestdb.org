---
layout: model
title: Syllogisms (CogSci14)
---

This is the model.

(define all-true (lambda (lst) (apply and lst)))
(define some-true (lambda (lst) (apply or lst)))

; assume at least one object has each of the properties
(define existential-import (lambda (A B objects) 
                             (and (some-true (map A objects)) (some-true (map B objects)))))

(define all (lambda (A B)
              (if (existential-import A B objects)
                  (all-true (map (lambda (x) (if (A x) (B x) true)) 
                                 objects))
                  false)))

(define some (lambda (A B)
               (if (existential-import A B objects)
                   (some-true (map (lambda (x) (if (A x) (B x) false)) 
                                   objects))
                   false)))

(define none (lambda (A B)
               (if (existential-import A B objects)
                   (all-true (map (lambda (x) (if (A x) (not (B x)) true)) 
                                  objects))
                   false)))

(define not-all (lambda (A B)
                 (if (existential-import A B objects)
                     (some-true (map (lambda (x) (if (A x) (not (B x)) false)) 
                                     objects))
                     false)))


(define (raise-to-power dist alph)
  (list (first dist) (map (lambda (x) (pow x alph)) (second dist))))


; pass strings, which then call functions of the same name
(define (meaning word)
  (case word
        (('all) all)
        (('some) some)
        (('not-all) not-all)
        (('none) none)))

; i have no prior beliefs about what the answer should be
(define (conclusion-prior) (uniform-draw (list 'all 'some 'not-all 'none)))
; i have no prior beliefs about what arguments the experimenter could give me
(define (premise-prior) (uniform-draw (list 'all 'some 'not-all 'none)))

; my prior beliefs about the rarity of properties
(define br 0.25)
; number of objects in the situation i'm imagining
(define objects (list 'o1 'o2 'o3))

(define experimenter
  (mem
  ; the experimenter takes in the conclusion as an argument (i.e. she has a conclusion in mind)
   (lambda (conclusion depth)
     (enumeration-query
      ;  (rejection-query
      ; the experimenter draws premises from the premise-prior (uniform) distribution
      (define premise-one (premise-prior))
      (define premise-two (premise-prior))
      
      ; the experimenter produces two premises
      (list premise-one premise-two)
      
      ; the experimenter wants the reasoner to draw a particular conclusion, gives the premises 
      (equal? conclusion (apply multinomial (raise-to-power 
                                             (reasoner premise-one premise-two depth) 4.75)))))))

(define reasoner 
  (mem
  ; the reasoner takes in two premises as arguments
   (lambda (premise-one premise-two depth)
     (enumeration-query
      ;(rejection-query
      ;(mh-query 1000 1
      ; the reasoner is uncertain about what the situation is
      (define A (mem (lambda (x) (flip br))))
      (define B (mem (lambda (x) (flip br))))
      (define C (mem (lambda (x) (flip br))))
      ; the reasoner is also uncertain about what conclusion is true
      (define conclusion (conclusion-prior))
      
      ;((meaning conclusion) A C)
      ; the reasoner produces a conclusion
      conclusion
      
      (and
      ; the conclusion quantifier is true of terms: A & C
       ((meaning conclusion) C A)
       (if (= depth 0)
           (and 
            ; the premise quantifiers apply to: A-B & B-C
            ; 
            ((meaning premise-one) B A)
            ((meaning premise-two) C B))
           ; if depth = 1, the reasoner assumes the experimenter produces particular premises, given the conclusion
           (equal? (list premise-one premise-two) (apply multinomial (experimenter conclusion (- depth 1)))))
       )))))

; (hist (repeat 100 (lambda () (reasoner 'all 'some 0))))
;(reasoner 'all 'some 0)
(reasoner 'none 'all 1)
