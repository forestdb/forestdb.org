---
layout: model
title: Spoken irony and common ground inference
model-status: code
model-language: church
---

~~~
;; Five possible weather states
(define states
  (list 'terrible 'bad 'ok 'good 'amazing))

;; Listener believes the weather state is most likely amazing, but plausibly good as well
(define (state-prior) 
  (multinomial states '(0.01 0.05 0.1 0.3 0.5)))

;; Probability of positive valence given each weather state
(define (valence-prior state)
  (if (flip (second (assoc state
                           (list (list 'terrible 0.01)
                                 (list 'bad 0.2)
                                 (list 'ok 0.5)
                                 (list 'good 0.8)
                                 (list 'amazing 0.99)))))
     'pos
     'neg))

;; Probability of high arousal given each weather state
(define (arousal-prior state)
  (if (flip (second (assoc state
                           (list (list 'terrible 0.9)
                                 (list 'bad 0.5)
                                 (list 'ok 0.1)
                                 (list 'good 0.5)
                                 (list 'amazing 0.9)))))
      'high
      'low
      ))

;; Probability of the speaker having each communicative goal
(define (goal-prior)
  (multinomial (list 'g-state 'g-valence 'g-arousal) '(0.1 0.1 0.1)))

;; Speaker's goal is satisfied if the goal dimension is communicated to listener
(define (goal-satisfied? goal listener-interp speaker-world)
  (case goal
    (('g-state) (equal? (first listener-interp) (first speaker-world)))
    (('g-valence) (equal? (second listener-interp) (second speaker-world)))
    (('g-arousal) (equal? (third listener-interp) (third speaker-world)))
        ))

;; List of utterances the speaker can say
(define utterances states)

;; Cost of each utterance is the same
(define (utterance-prior) (uniform-draw utterances))

;; List of possible tones
(define tones (list 'sarcastic 'plain))

;; Cost of each tone is the same
(define (tone-prior) (multinomial tones '(0.5 0.5)))

;; Prior probability of having the same prior as speaker
(define sameness-prior 0.5)

;; L2 is given an utterance and a tone. L2 is uncertain whether speaker has
;; same prior as he does.
(define L2
  (mem
   (lambda (utterance tone)
     (enumeration-query
      (define same? (flip sameness-prior))
      (define state 
        (if same? (state-prior)
            (uniform-draw states)))
      (define valence (valence-prior state))
      (define arousal (arousal-prior state))
      (list same?)
      (equal? (list utterance tone) (apply multinomial (S2 state valence arousal same?)))))))

;; Given a weather state and her valence and arousal towards it,
;; S2 chooses the utterance and tone such that L1 interprets
;; the utterance literally or sarcastically
(define S2
  (mem
   (lambda (state valence arousal same?)
     (enumeration-query
      (define utterance (utterance-prior))
      (define tone (tone-prior))
      (define interp (apply multinomial (L1 utterance tone same?)))
      (list utterance tone)
      ; If tone is sarcastic, condition on L1 interpreting utterance nonliterally
      (if (equal? tone 'sarcastic) (not (literal-interpretation utterance (first interp)))
          ; Else, condition on L1 interpreting utterance literally.
          ; (Should add some noise to this)
          (literal-interpretation utterance (first interp)))))))

;; Given the utterance, tone, and whether S1 has the same priors,
;; L1 reasons about S1's communicative goal
;; and infers S1's state, valence, and arousal
;; such that S1 would choose the utterance. L1 does not care about tone.
(define L1
  (mem
   (lambda (utterance tone same?)
     (enumeration-query
      (define state 
        (if same? (state-prior)
            (uniform-draw states)))
      (define valence (valence-prior state))
      (define arousal (arousal-prior state))
      (define goal (goal-prior))
      (list state valence arousal)
      (equal? utterance (apply multinomial (S1 state valence arousal goal same?)))
      ))))

;; Given state, valence, arousal, goal, and whether prior is shared,
;; S1 chooses utterance such that goal dimension is communicated to literal listener
(define S1
  (mem
   (lambda (state valence arousal goal same?)
     (enumeration-query
      (define utterance (utterance-prior))
      (define lit-interp (apply multinomial (L0 utterance same?)))
      utterance
          (goal-satisfied? goal lit-interp (list state valence arousal))))))

;; Given utterance and whether prior is shared,
;; L0 interprets utterance literally and produces speaker's valence and arousal
;; given utterance is literal
(define L0
  (mem
   (lambda (utterance same?)
     (enumeration-query
      (define state 
        (if same? (state-prior)
            (uniform-draw states)))
      (define valence (valence-prior state))
      (define arousal (arousal-prior state))
      (list state valence arousal)
      (literal-interpretation utterance state)))))

;; Literal interpretation "meaning" function, just check if uttered number reflects price state
(define (literal-interpretation utterance state)
  (equal? utterance state))

(barplot (L2 'terrible 'sarcastic))
(barplot (L2 'amazing 'plain))
~~~
