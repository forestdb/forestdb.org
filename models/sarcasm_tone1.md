---
layout: model
title: Sarcasm with tone as goal signal
model-status: code
model-language: church
---
    ; Five possible movie states
    (define states
      (list 'terrible 'bad 'ok 'good 'amazing))
    
    ; Listener believes the movie is amazing, but plausibly good as well
    (define (state-prior) 
      (multinomial states '(0.01 0.05 0.1 0.3 0.5)))
    
    ; Probability of positive valence given each movie state
    (define (valence-prior state)
      (if (flip (second (assoc state
                               (list (list 'terrible 0.01)
                                     (list 'bad 0.2)
                                     (list 'ok 0.5)
                                     (list 'good 0.8)
                                     (list 'amazing 0.99)))))
         'pos
         'neg))
    
    ; Probability of high arousal given each movie state
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
    
    
    (define (goal-prior)
      (multinomial (list 'g-state 'g-valence 'g-arousal) '(0.1 0.1 0.1)))
    
    (define (goal-satisfied? goal listener-interp speaker-world)
      (case goal
        (('g-state) (equal? (first listener-interp) (first speaker-world)))
        (('g-valence) (equal? (second listener-interp) (second speaker-world)))
        (('g-arousal) (equal? (third listener-interp) (third speaker-world)))
            ))
    
    ;; Define list of possible utterances (same as price states)
    (define utterances states)
    
    (define (utterance-prior)
      (uniform-draw utterances))
    
    (define tones (list 'drawl 'plain))
    (define (tone-prior) (multinomial tones (list 0.1 0.9)))
    
    (define sameness-prior 0.5)
    
    ;; Literal interpretation "meaning" function, just check if uttered number reflects price state
    (define (literal-interpretation utterance state)
      (equal? utterance state))
    
    
    ;; Pragmatic listener, jointly infers the price state, speaker valence, and QUD
    (define L1
      (mem
       (lambda (utterance tone)
         (enumeration-query
          (define same? (flip sameness-prior))
          (define state 
            (if same? (state-prior)
                (uniform-draw states)))
          (define valence (valence-prior state))
          (define arousal (arousal-prior state))
          (define goal (goal-prior))
          (list state)
          (equal? (list utterance tone) (apply multinomial (S1 state valence arousal goal)))))))
    
    ;; Speaker, chooses an utterance to convey a particular value of the qud
    (define S1
      (mem
       (lambda (state valence arousal goal)
         (enumeration-query
          (define utterance (utterance-prior))
          (define tone (if (equal? goal 'g-arousal) 'drawl (tone-prior)))
          (define lit-interp (apply multinomial (L0 utterance)))
          (list utterance tone)
              (goal-satisfied? goal lit-interp 
                         (list state valence arousal))))))
    
    ;; Literal listener, infers the qud value assuming the utterance is true of the state
    (define L0
      (mem
       (lambda (utterance)
         (enumeration-query
          (define state (state-prior))
          (define valence (valence-prior state))
          (define arousal (arousal-prior state))
          (list state valence arousal)
          (literal-interpretation utterance state)))))
    
    
    (barplot (L1 'terrible 'plain) "terrible plain")
    (barplot (L1 'terrible 'drawl) "terrible drawl")
    (barplot (L1 'amazing 'plain) "amazing plain")
    (barplot (L1 'amazing 'drawl) "amazing drawl")

