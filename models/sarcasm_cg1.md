---
layout: model
title: Sarcasm with uncertainty about speaker's beliefs
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
      
      ; Probability of the speaker having each communicative goal
      (define (goal-prior)
        (multinomial (list 'g-state 'g-valence 'g-arousal) '(0.1 0.1 0.1)))
      
      ; Speaker's goal is satisfied if the goal dimension is communicated to listener
      (define (goal-satisfied? goal listener-interp speaker-world)
        (case goal
          (('g-state) (equal? (first listener-interp) (first speaker-world)))
          (('g-valence) (equal? (second listener-interp) (second speaker-world)))
          (('g-arousal) (equal? (third listener-interp) (third speaker-world)))
              ))
      
      ; List of utterances the speaker can say
      (define utterances states)
      
      ; Cost of each utterance is the same
      (define (utterance-prior) (uniform-draw utterances))
      
      ; Given the utterance and belief about whether S1 has the same priors,
      ; L1 reasons about S1's communicative goal
      ; and infers S1's state, valence, and arousal
      ; such that S1 would choose the utterance. L1 does not care about tone.
      (define L1
        (mem
         (lambda (utterance)
           (enumeration-query
            (define same? (flip sameness-prior))
            (define state 
              (if same? (state-prior)
                  (uniform-draw states)))
            (define valence (valence-prior state))
            (define arousal (arousal-prior state))
            (define goal (goal-prior))
            ;(list state valence arousal)
            (list state same?)
            (equal? utterance (apply multinomial (S1 state valence arousal goal)))
            ))))
      
      ; Given state, valence, arousal, goal, and whether prior is shared,
      ; S1 chooses utterance such that goal dimension is communicated to literal listener
      (define S1
        (mem
         (lambda (state valence arousal goal)
           (enumeration-query
            (define utterance (utterance-prior))
            (define lit-interp (apply multinomial (L0 utterance)))
            utterance
                (goal-satisfied? goal lit-interp (list state valence arousal))))))
      
      ; Given utterance and whether prior is shared,
      ; L0 interprets utterance literally and produces speaker's valence and arousal
      ; given utterance is literal
      (define L0
        (mem
         (lambda (utterance)
           (enumeration-query
            (define state utterance)
            (define valence (valence-prior state))
            (define arousal (arousal-prior state))
            (list state valence arousal)
            (literal-interpretation utterance state)))))
      
      ;; Literal interpretation "meaning" function, just check if uttered number reflects price state
      (define (literal-interpretation utterance state)
        (equal? utterance state))
      
      ; The listener is uncertain about the speaker's beliefs.
      ; Prior probability that the speaker shares same beliefs as listener.
      (define sameness-prior 0.5)
      
      (barplot (L1 'terrible) "terrible")
      (barplot (L1 'bad) "bad")
      (barplot (L1 'ok) "ok")
      (barplot (L1 'good) "good")
      (barplot (L1 'amazing) "amazing")

