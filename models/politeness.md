---
layout: model
title: Politeness
model-language: church
---

## Model 1: Bernoulli, mutually-exclusive goals in S1

This is a model for the QUD-inference task, where participants are given a true state of the world 
(e.g. the report was terrible) and an utterance (e.g. "It was okay"), and asked to infer the speaker's
goals. We model this as a pragmatic listener, who has uncertainty over the possible goals (`~ flip(0.5)`)
in addition to the true state (`~ uniformDraw([possible states])`).

Valence here is modeled as a (determinstic) function of the state. The interpretation of this variable is
"the speaker's perception of the quality of the work" (i.e. "what he thought about it"). For the speaker,
he can either have the goal of honesty (in which case he chooses words that best convey the true state) or
the goal of kindness. In the spirit of the "helper models", by being kind, the speaker adopts the utility of the listener
w.r.t. to the desired state. In this situations, he choose utterances in proportion to their subjective value (`state-value`). 

~~~~
;; helper functions
(define (raise-to-power speaker-dist alpha)
  (list (first speaker-dist)
        (map (lambda (x) (pow x alpha)) (second speaker-dist))))

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
;; end helpers

;; Define evaluative states
(define states (list "TERRIBLE" "BAD" "OKAY" "GOOD" "AMAZING"))
;; Prior (uniform) probability of each evaluative state
;; -- should be determined from expt?
(define (state-prior) (multinomial states '(1 1 1 1 1))) 

;; define utterance
(define utterances (list "terrible" "bad" "okay" "good" "amazing"))
(define (utterance-prior) (multinomial utterances '(1 1 1 1 1)))

;; (some) mapping from state to valence
;; right now, determinisic
(define state-to-valence (lambda (state)
                           (case state
                                 (("TERRIBLE") 0.01)
                                 (("BAD") 0.25)
                                 (("OKAY") 0.5)
                                 (("GOOD") 0.75)
                                 (("AMAZING") 0.99))))

(define state-value (list 0.01 0.25 0.5 0.75 0.99))



;; QUD function (goals either: honest or kind)
(define (qud-fn speaker-goals)
	;; if honest
  (if (first speaker-goals)
      (lambda (state valence) state)
      (lambda (state valence) valence)))

;; words and states associated with them (non vague words)
; (define (meaning words state)
;   (case words
;         (("terrible") (equal? state "TERRIBLE"))
;         (("bad")  (equal? state "BAD"))
;         (("okay") (equal? state "OKAY"))
;         (("good") (equal? state "GOOD"))
;         (("amazing") (equal? state "AMAZING"))))

;; words and states associated with them (with some flexibility in meanings)
(define (meaning words state)
  (case words
        (("terrible") (equal? state (multinomial states '(50 10 3 2 1))))
        (("bad")  (equal? state (multinomial states '(10 50 5 2 1))))
        (("okay") (equal? state (multinomial states '(1 5 50 5 1))))
        (("good") (equal? state (multinomial states '(1 2 5 50 10))))
        (("amazing") (equal? state (multinomial states '(1 2 3 10 50))))))

;; this function samples a state according to it's value (higher value, higher probability)
;; this is where the kindness rubber hits the road 
(define valence-to-be-communicated 
  (lambda (kindness)
    (if kindness
     (multinomial state-value state-value)
     ;; otherwise, goal is meanness
     (multinomial state-value (reverse state-value)))))

(define speaker-optimality 4)

; Literal listener: 
; knows the qud value
(define literal-listener
  (mem
   (lambda (utterance qud)
     (enumeration-query
      (define state (state-prior))
      (define valence (state-to-valence state)) ;; deterministic

      ((qud-fn qud) state valence)

      (meaning utterance state)))))


;; (literal-listener "amazing" (list true false))
;; Speaker
;; qud function returns either state or valence as the goal
;; if state: pick the utterance that best matches that state
;; if valence: pick the utterance that will (softmaximize) utility of the listener
(define speaker
  (mem
   (lambda (state speaker-goals)
     (enumeration-query
      (define utterance (utterance-prior))
      (define valence  (valence-to-be-communicated (second speaker-goals)))
      (define qud-val ((qud-fn speaker-goals) state valence))

      utterance

      (equal? qud-val (apply multinomial 
                             (literal-listener utterance speaker-goals)))))))

; (define results (speaker "BAD" (list false true)))
; (display (first results))
; (display (second results))

(define pragmatic-listener
  (mem
   (lambda (utterance)
     (enumeration-query
      (define state (state-prior))
      (define valence (state-to-valence state))

      (define speaker-honest (flip 0.5))
      (define speaker-kind (flip 0.5))
      (define speaker-goals (list speaker-honest speaker-kind))

      (define qud-val ((qud-fn speaker-goals) state valence))

      ;query for: speaker-goals
      (map (lambda (x) (if x 1 0)) speaker-goals)

      (and 
       (equal? utterance (apply multinomial 
          (raise-to-power (speaker state speaker-goals) speaker-optimality)))
       (equal? state "OKAY"))))))

(define posterior (pragmatic-listener "terrible"))
(zip (list "honesty" "politeness") 
     (map expectation (marginalize posterior)))

~~~~



## Model 2: Weights on goals.

This is a very similar model to the one above. Instead of having goals being bernoulli and mutually exlusive,
there are weights on the goals. 

There is one QUD fn in which the goals are mutually exclusive (by default). 
There is another in which multiple goals can be entertained (commented out by default).
I believe they make similar predictions.

~~~~
(define (raise-to-power speaker-dist alpha)
  (list (first speaker-dist)
        (map (lambda (x) (pow x alpha)) (second speaker-dist))))


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
(define state-value (list 0.01 0.25 0.5 0.75 0.99))

;; Prior probability of each evaluative state
(define (state-prior) (multinomial states '(1 1 1 1 1))) ;; fixme: should be determined from expt?

;; define utterance
(define utterances (list "terrible" "bad" "okay" "good" "amazing"))
(define (utterance-prior) (multinomial utterances '(1 1 1 1 1)))

;; mapping from state to p(feels good)
(define state-to-valence (lambda (state)
                           (case state
                                 (("TERRIBLE") 0.01)
                                 (("BAD") 0.25)
                                 (("OKAY") 0.5)
                                 (("GOOD") 0.75)
                                 (("AMAZING") 0.99))))


;; QUD function ;;; mutually exclusive goals
(define (qud-fn speaker-goals)
  (if (equal? "honest" (multinomial (list "honest" "kind" "mean") speaker-goals))
      (lambda (state valence) state)
      (lambda (state valence) valence)))


;; QUD function ;; potentially mixtures of goals
; (define (qud-fn speaker-goals)
;   (if (flip (first speaker-goals))
;       (if (flip (second speaker-goals))
;           (lambda (state valence) (list state valence)) ; if honest and kind
;           (if (flip (third speaker-goals)) 
;               (lambda (state valence) (list state valence)) ; if honest and not kind but mean
;               (lambda (state valence) state))) ; if honest and not kind nor mean
;       (lambda (state valence) valence))) ; if not honest

;; words and states associated with them (non vague words)
; (define (meaning words state)
;   (case words
;         (("terrible") (equal? state "TERRIBLE"))
;         (("bad")  (equal? state "BAD"))
;         (("okay") (equal? state "OKAY"))
;         (("good") (equal? state "GOOD"))
;         (("amazing") (equal? state "AMAZING"))))

;; words and states associated with them (with some flexibility in meanings)
(define (meaning words state)
  (case words
        (("terrible") (equal? state (multinomial states '(50 10 3 2 1))))
        (("bad")  (equal? state (multinomial states '(10 50 5 2 1))))
        (("okay") (equal? state (multinomial states '(1 5 50 5 1))))
        (("good") (equal? state (multinomial states '(1 2 5 50 10))))
        (("amazing") (equal? state (multinomial states '(1 2 3 10 50))))))


(define valence-to-be-communicated 
  (lambda (kindness meanness)
    (if 
     (equal? "kind" (multinomial (list "kind" "mean") (list kindness meanness)))
     (multinomial state-value state-value)
     (multinomial state-value (reverse state-value)))))


(define speaker-optimality 3)
; Literal listener: 
; knows the qud value
(define literal-listener
  (mem
   (lambda (utterance qud)
     (enumeration-query
      (define state (state-prior))
      (define valence (state-to-valence state))

      ((qud-fn qud) state valence)

      (meaning utterance state)))))


;; (literal-listener "amazing" (list 0.1 0.9 0.5))

(define speaker
  (mem
   (lambda (state speaker-goals)
     (enumeration-query
      (define utterance (utterance-prior))
      (define valence (valence-to-be-communicated (second speaker-goals) (third speaker-goals)))
      (define qud-val ((qud-fn speaker-goals) state valence))

      utterance

      (equal? qud-val (apply multinomial 
                             (literal-listener utterance speaker-goals)))))))

;; (define results (speaker "BAD" (list 0.7 0.1 0.1)))
;; (display (first results))
;; (display (second results))

(define pragmatic-listener
  (mem
   (lambda (utterance)
     (enumeration-query
      (define state (state-prior))
      (define valence (state-to-valence state))

      (define speaker-honesty (uniform-draw '(0.3 0.5 0.7)))
      (define speaker-kindness (uniform-draw '(0.3 0.5 0.7)))
      (define speaker-meanness (- 1 speaker-kindness)) ;;(uniform-draw '(0.3 0.5 0.7)))
      (define speaker-goals (list speaker-honesty speaker-kindness speaker-meanness))

      (define qud-val ((qud-fn speaker-goals) state valence))

      speaker-goals

      (and 
       (equal? utterance (apply multinomial 
          (raise-to-power (speaker state speaker-goals) speaker-optimality)))
       (equal? state "OKAY"))))))


(define posterior (pragmatic-listener "good"))

(zip (list "honesty" "politeness" "meanness") 
	 (map expectation (marginalize posterior)))

~~~~



