---
layout: model
title: Irony
model-status: code
model-category: Reasoning about Reasoning
model-tags: linguistics, pragmatics
---
;; There are three possible states the weather could be in: terrible, ok, or amazing
(define states (list 'terrible 'ok 'amazing))
;; Since we are in California, the prior over these states are the following.
;; Once could also imagine this being the prior in a certain context, e.g. when it's clearly
;; sunny and nice out.
(define (state-prior) (multinomial states '(0.01 0.5 0.5)))

;; Define valences. -1 is negative valence; 1 is positive. We assume valence is binary.
(define valences (list '-1 '1))

;; Valence prior defined in terms of negative valence. If the current state
;; is terrible, it's extremely likely that the valence associted is negative.
;; If it's ok, then the valence could be negative or positive with equal probability.
(define valence-prior (list 
                       (list 'terrible 0.99)
                       (list 'ok 0.5) 
                       (list 'amazing 0.01)))
    
;; Define arousals. Assuming arousal is binary, but could model as continuous.
(define arousals (list 'low 'high))

;; Define goals and goal priors. Could want to communicate state of the world,    
;; valence about it, or arousal (intensity of feeling) about it.
(define goals (list 'goal-state 'goal-valence 'goal-arousal))
    
(define (goal-prior) (multinomial goals '(0.1 0.1 0.1)))
    
;; Assume possible utterances are identical to possible states
(define utterances states)
    
;; Assume cost of utterances is uniform.
(define (utterance-prior) 
     (multinomial utterances '(0.1 0.1 0.1)))
    
;; Sample valence given a state.
(define (sample-valence state prior)
    (let ((current-state-valence-pair (first prior)))
    (if (equal? state (first current-state-valence-pair))
        (if (flip (second current-state-valence-pair))
            '-1
            '1)
        (sample-valence state (rest prior)))))
    
;; Sample arousal given a state.
(define (sample-arousal state)
  (case state
        (('terrible) (multinomial arousals '(0.1 0.9)))
        (('ok) (multinomial arousals '(0.9 0.1)))
        (('amazing) (multinomial arousals '(0.1 0.9)))))
    
; Literal interpretation is just when utterance equals state
(define (literal-interpretation utterance state)
  (equal? utterance state))
    
; A speaker's goal is satisfied if the listener infers the correct and relevant information.
(define (goal-satisfied? goal listener-state-valence-arousal speaker-state speaker-valence speaker-arousal)
  (case goal
    (('goal-state) (equal? (first listener-state-valence-arousal) speaker-state))
        (('goal-valence) (equal? (second listener-state-valence-arousal) speaker-valence))
        (('goal-arousal) (equal? (third listener-state-valence-arousal) speaker-arousal))))
    
;; The model is currently restricted to hardness=1
(define speaker
  (mem (lambda (state valence arousal goal depth)
  (enumeration-query
   (define utterance (utterance-prior))
   ; Choose utterance
   utterance
   ; Conditioned on goal being satisfied
    (goal-satisfied? goal (apply multinomial (listener utterance depth)) state valence arousal)
   ))))
    
(define listener
  (mem (lambda (utterance depth)
  (enumeration-query
   (define state (state-prior))
   (define valence (sample-valence state valence-prior))
   (define arousal (sample-arousal state))
   (define speaker-goal (goal-prior))
   ; Choose interpretation
   (list state valence arousal)
   ; Given speaker's utterance
   (if (equal? depth 0)
        (literal-interpretation utterance state)
        (equal? utterance
               (apply multinomial (speaker state valence arousal speaker-goal (- depth 1)))))
    ))))
    
(define depth 1)
(define hardness 1)
(define (interpret utterance) 
  (listener utterance depth))
    
; Interpretation given an utterance
(barplot (interpret 'terrible))
