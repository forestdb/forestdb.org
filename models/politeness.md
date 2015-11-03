---
layout: model
title: Politeness
model-language: church
---

Draft

~~~~
(define (expectation dist)
  (define vs (first dist))
  (define ps (second dist))
  (if (= (length ps) 0)
      0      
      (+ (* (first ps) (first vs))
         (expectation (list (rest vs) (rest ps))))))


(define (get-indices needle haystack)
  (define (loop rest-of-haystack index)
    (if (null? rest-of-haystack) '()
        (let ((rest-of-indices (loop (rest rest-of-haystack) (+ index 1))))
          (if (equal? (first rest-of-haystack) needle)
              (pair index rest-of-indices)
              rest-of-indices))))
  (loop haystack 1))

(define (marginalize output)
  (let ([states (first output)])
    (map (lambda (sub-output) 
           (let* ([probs (second output)]
                  [unique-states (unique sub-output)]
                  [unique-state-indices 
                   (map 
                    (lambda (x) (list x (get-indices x sub-output))) 
                    unique-states)])

             (list (map first unique-state-indices)
                   (map 
                    (lambda (y) (sum (map 
                                      (lambda (x) (list-elt probs x)) 
                                      (second y)))) 
                    unique-state-indices))))
         (transpose states))))

;; Define evaluative states (also, the valence of each state)
;; terrible, bad, okay, good, amazing
(define states (list "TERRIBLE" "BAD" "OKAY" "GOOD" "AMAZING"))

;; mapping from state to p(feels good)
(define state-to-valence (lambda (state)
                           (case state
                                 (("TERRIBLE") 0.01)
                                 (("BAD") 0.25)
                                 (("OKAY") 0.5)
                                 (("GOOD") 0.75)
                                 (("AMAZING") 0.99))))

(define state-value (list 0.01 0.25 0.5 0.75 0.99))

;; Prior probability of each evaluative state
(define (state-prior) (multinomial states '(1 1 1 1 1))) ;; fixme: should be determined from expt?

;; QUD function
(define (qud-fn speaker-goals)
  (if (first speaker-goals) ;(equal? "honest" (multinomial (list "honest" "kind" "mean") speaker-goals))
      (if (second speaker-goals)
          (lambda (state valence) (list state valence))
          (lambda (state valence) state))
      (lambda (state valence) valence)))

;; words and states associated with them
(define (meaning words state)
  (case words
        (("terrible") (equal? state "TERRIBLE"))
        (("bad")  (equal? state "BAD"))
        (("okay") (equal? state "OKAY"))
        (("good") (equal? state "GOOD"))
        (("amazing") (equal? state "AMAZING"))))

(define (meaning words state)
  (case words
        (("terrible") (equal? state (multinomial states '(50 10 3 2 1))))
        (("bad")  (equal? state (multinomial states '(10 50 5 2 1))))
        (("okay") (equal? state (multinomial states '(1 5 50 5 1))))
        (("good") (equal? state (multinomial states '(1 2 5 50 10))))
        (("amazing") (equal? state (multinomial states '(1 2 3 10 50))))))

;; define utterance
(define utterances (list "terrible" "bad" "okay" "good" "amazing"))

(define (utterance-prior) (multinomial utterances '(1 1 1 1 1)))

(define valence-to-be-communicated 
  (lambda (kindness meanness)
    (if 
      kindness
     ; (equal? "kind" (multinomial (list "kind" "mean") (list kindness meanness)))
     (multinomial state-value state-value)
     (multinomial state-value (reverse state-value)))))



; Literal listener: 
; knows the qud value
(define literal-listener
  (mem
   (lambda (utterance qud)
     (enumeration-query
      (define state (state-prior))
      (define valence (state-to-valence state))

      ((qud-fn qud) state valence)
      ; state
      ; valence

      (meaning utterance state)))))


;; (literal-listener "amazing" (list 0.1 0.9))

(define speaker
  (mem
   (lambda (state speaker-goals)
     (enumeration-query
      (define utterance (utterance-prior))
      (define valence  (valence-to-be-communicated 
                          (second speaker-goals) 
                          (third speaker-goals)))
      (define qud-val ((qud-fn speaker-goals) state valence))

      utterance

      (equal? qud-val (apply multinomial 
                             (literal-listener utterance speaker-goals)))))))

(define results (speaker "BAD" (list false true false)))
(display (first results))
(display (second results))

; (literal-listener "bad" (list true false false))

; (define pragmatic-listener
;   (mem
;    (lambda (utterance)
;      (enumeration-query
;       (define state (state-prior))
;       (define valence (state-to-valence state))

;       (define speaker-honesty (flip 0.5))
;       (define speaker-kindness (flip 0.5))
;       ; (define speaker-kindness (uniform-draw '(0.1 0.9)))
;       (define speaker-meanness (not speaker-kindness)) ;;(uniform-draw '(0.3 0.5 0.7)))
;       (define speaker-goals (list speaker-honesty speaker-kindness speaker-meanness))

;       (define qud-val ((qud-fn speaker-goals) state valence))

;       (map (lambda (x) (if x 1 0)) speaker-goals)

;       (and 
;        (equal? utterance
;                (apply multinomial (speaker state speaker-goals)))
;        (equal? state "OKAY"))))))


; (define posterior (pragmatic-listener "terrible"))
; (zip (list "honesty" "politeness" "meanness") 
;      (map expectation (marginalize posterior)))

 ; (marginalize posterior)
; ;; define speaker2, based on pragmatic-listener
; (define speaker2
;   (mem
;    (lambda (state speaker-type)
;      (enumeration-query
;       (define utterance (utterance-prior))
;       (define how-good-is-the-state (valence-prior state))

;       utterance

;       (equal? (list state how-good-is-the-state) 
;               (apply multinomial (prag-listener utterance speaker-type)))))))



; (display (prag-listener "not great") "state given 'not great'")
; (display (prag-listener "not bad") "state given 'not bad'")
; (display (prag-listener "great") "state given 'great'")
; (display (prag-listener "bad") "state given 'bad'")

~~~~