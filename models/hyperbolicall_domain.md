---
layout: model
title: Hyperbolic quantifiers with domain restriction
model-status: code
model-language: church
---

;; Example model for universal quantifier interpretation with potential domain restriction inference

; There are 2 members of type A and 3 members of type B 
; (e.g. 2 peices of chocolate cake, 3 pieces of lemon cake)
(define num-A 2)
(define num-B 3)
(define num-total (+ num-A num-B))

; A state is a pair of numbers: (number of A's eaten, number of B's eaten)
(define states (list '(0 0) '(0 1) '(0 2) '(0 3)
                     '(1 0) '(1 1) '(1 2) '(1 3) 
                     '(2 0) '(2 1) '(2 2) '(2 3)))

; It's likely for Bob to eat 0, 1, or 2 of the things, and very unlikely for Bob to eat
; all 5 things.
(define (item-prior)
  (multinomial states
               '(0.2 0.3 0.1 0.05
                 0.3 0.1 0.05 0.01
                 0.1 0.05 0.01 0.001)))

; The probability that a speaker is upset at Bob for eating 0-5 things
(define (affect-prior)
  (list '(0 0.2) '(1 0.3) 
        '(2 0.4) '(3 0.6) 
        '(4 0.8) '(5 0.9)))

; A speaker can have the goal to communicate about the total number of things Bob ate,
; the number of Type A's Bob ate, or how she feels about Bob's eating behavior
(define goals
  (list
   'how-many-total?
   'how-many-in-domain?
   'affect?
))

; Prior probabilty of each goal can change with context and saliency of subset
(define goal-probs '(0.3 0.3 0.3))

(define (goal-prior) (multinomial goals goal-probs))

; Things speaker can say
(define utterances (list
                    'some
                    'all
                    'a
                    'no
                    ))

(define (utterance-prior) (multinomial utterances '(0.1 0.1 0.1 0.1)))

; Sample affect given total number of things eaten
(define (sample-affect total affect-prior)
  (let ((current-state-affect-pair (first affect-prior)))
    (if (equal? total (first current-state-affect-pair))
        (if (flip (second current-state-affect-pair))
            '1
            '0)
        (sample-affect total (rest affect-prior)))))

; Literal interpretation of words. This depends on the goal---
; If the goal is just to communicate about Type A, "all" is
; literally true when all of Type A's are eaten.
; TODO: think more about whether this is a reasonable thing to do
(define (literal-interpretation goal utterance state)
(case goal
      (('how-many-total?)
       (case utterance
             (('all) (equal? (sum state) num-total))
             (('some) (> (sum state) 0))
             (('no) (equal? (sum state) 0))
             (('a) (equal? (sum state) 1))))
      (('how-many-in-domain?)
       (case utterance
             (('all) (equal? (first state) num-A))
             (('some) (> (first state) 0))
             (('no) (equal? (first state) 0))
             (('a) (equal? (first state) 1))))
      (('affect?)
       (case utterance
             (('all) (equal? (sum state) num-total))
             (('some) (> (sum state) 0))
             (('no) (equal? (sum state) 0))
             (('a) (equal? (sum state) 1))))))


(define (goal-satisfied? goal listener-interp speaker-world)
(case goal
      (('how-many-total?) (equal? (sum (first listener-interp)) (sum (first speaker-world))))
      (('how-many-in-domain?) (equal? (first (first listener-interp)) (first (first speaker-world))))
      (('affect?) (equal? (second listener-interp) (second speaker-world)))
))

; Literal listener interprets utterance literally
(define L0
  (mem (lambda (utterance goal)
         (enumeration-query
          (define state (item-prior))
          (define total (sum state))
          (define affect (sample-affect total (affect-prior)))
          (list state affect)
          (literal-interpretation goal utterance state)))))

; Speaker chooses utterance for literal listener
(define speaker
  (mem (lambda (state affect goal)
         (enumeration-query
          (define utterance (utterance-prior))
          utterance
          (goal-satisfied? goal (apply multinomial (L0 utterance goal))
                           (list state affect))
          ))))

; Pragmatic listener infers number of A's and B's eaten and speaker's affect
(define L1
  (mem (lambda (utterance)
         (enumeration-query
          (define state (item-prior))
          (define total (sum state))
          (define affect (sample-affect total (affect-prior)))
          (define goal (goal-prior))

          (list state affect)

          (equal? utterance
                      (apply multinomial (raise-to-power (speaker state affect goal) alpha)))
              ))))

(define (raise-to-power speaker-dist alpha)
  (list (first speaker-dist) (map (lambda (x) (pow x alpha)) (second speaker-dist))))

(define alpha 1)

;(display (L1 'all))
(barplot (L1 'all) "all")
(barplot (L1 'some) "some")
;(barplot (L1 'no) "no")
;(barplot (L1 'a) "a")
