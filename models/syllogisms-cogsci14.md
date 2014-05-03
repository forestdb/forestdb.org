---
layout: model
title: Syllogisms (CogSci14)
---

A model of syllogistic reasoning as communication.

The reasoner imagines an experimenter who chooses premises conditioned on the reasoner inferring an intended conclusion.

A syllogism is a two-sentence (premise) argument. Each sentence consists of 2 terms, and between the two premises, 1 of the terms is shared.
E.g.

B - A

C - B

In this argument, B is the shared term. The task for the reasoner is to generate a conclusion relating the "end-terms"-- A & C.

The relations between the terms is a quantifier. 
In classical syllogisms, the quantifiers are those from the "square of opposition": {all, some, none, not-all}.

In this model, the reasoner imagines situations which are composed of objects with properties. Sentences (premises or conclusion) are then either true or false of a given situation.

    (define all-true (lambda (lst) (apply and lst)))
    (define some-true (lambda (lst) (apply or lst)))
    
    ; assume the situations constructed have at least one object with each of the properties
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
    
    ; the reasoner has uninformative prior beliefs about what the conclusion should be
    (define (conclusion-prior) (uniform-draw (list 'all 'some 'not-all 'none)))
    ; rhe reasoner has uninformative prior beliefs about what arguments the experimenter could give
    (define (premise-prior) (uniform-draw (list 'all 'some 'not-all 'none)))
    
    ; parameter 1: the reasoner's prior beliefs about the rarity of properties
    (define br 0.25)
    ; parameter 2: number of objects in the situation the reasoner imagines
    (define objects (list 'o1 'o2 'o3))
    
    (define experimenter
      (mem
      ; the experimenter takes in the conclusion as an argument (i.e. she has a conclusion in mind)
       (lambda (conclusion depth)
         (enumeration-query
          ; the experimenter draws premises from the premise-prior (uniform) distribution
          (define premise-one (premise-prior))
          (define premise-two (premise-prior))
          
          ; the experimenter produces two premises
          (list premise-one premise-two)
          
          ; the experimenter wants the reasoner to draw a particular conclusion, gives the premises 
          ; parameter 3: "optimality" -- the degree to which the experimenter's argument is optimal for the conclusion
         (equal? conclusion (apply multinomial (raise-to-power (reasoner premise-one premise-two depth) 4.75)))))))
    
    (define reasoner 
      (mem
      ; the reasoner takes in two premises as arguments
       (lambda (premise-one premise-two depth)
         (enumeration-query
          ; the reasoner is uncertain about what the situation is
          (define A (mem (lambda (x) (flip br))))
          (define B (mem (lambda (x) (flip br))))
          (define C (mem (lambda (x) (flip br))))
          ; the reasoner is also uncertain about what conclusion is true
          (define conclusion (conclusion-prior))
          
          ; the reasoner produces a conclusion
          conclusion
          
          (and
          ; the conclusion quantifier is true of terms: A & C
           ((meaning conclusion) A C)
           (if (= depth 0)
               (and 
                ; the premise quantifiers apply to: A-B & B-C
                ((meaning premise-one) A B)
                ((meaning premise-two) B C))
               ; if depth = 1, the reasoner assumes the experimenter produces premises 
               ; conditioned on the reasoner drawing a particular conclusion
               (equal? (list premise-one premise-two) (apply multinomial (experimenter conclusion (- depth 1)))))
           )))))
    
    ; All A are B
    ; Some B are C [paper, Fig 2 [3]]
    (reasoner 'all 'some 0)
    ; try changing the depth parameter to 1
    ; the reasoner then considers the space of possible arguments, 
    ; and concludes that "some" is the more likely intended conclusion
    
    ; All A are B
    ; No B are C [paper, Fig 2 [1]]
    
    ;(reasoner 'all 'none 0)
    
    ; this is a valid syllogism with 2 valid conclusions
    ; the literal reaonser (depth=0) has no preference among valid conclusions
    ; the pragmatic reasonser (depth=1) infers that "None" is the more likely intended conclusion
