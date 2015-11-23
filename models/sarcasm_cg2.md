---
layout: model
title: Sarcasm with uncertainty about speaker's beliefs about listener's beliefs
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
      
      ; L2 hears an utterance. L2 is uncertain whether speaker has
      ; same prior beliefs.
      (define L2
        (mem
         (lambda (utterance)
           (enumeration-query
            ; Do I (listener) believe we share the same beliefs?
            (define listener-same? (flip listener-sameness-prior))
            ; Does the speaker believe we share the same beliefs?
            (define speaker-same? (flip speaker-sameness-prior))
            ; If I believe we share the same beliefs, I draw
            ; speaker's state from my own prior distribution.
            ; Otherwise, I back off to uniform.
            (define state 
              (if listener-same? (state-prior) (uniform-draw states)))
            (define valence (valence-prior state))
            (define arousal (arousal-prior state))
            (define goal (goal-prior))
            ;(list state)
            (list state speaker-same?)
            ;(list listener-same? speaker-same?)
            (equal? utterance (apply multinomial (S2 state valence arousal goal speaker-same?)))))))
      
      ; Given a state, her valence and arousal towards it, and the speaker's
      ; beliefs about whether or not the listener shares beliefs,
      ; S2 chooses an utterance.
      (define S2
        (mem
         (lambda (state valence arousal goal speaker-same?)
           (enumeration-query
            (define utterance (utterance-prior))
            utterance
            ; How does the speaker think L1 will interpret the utterance?
            (define L1-interp (apply multinomial (L1 utterance speaker-same?)))
            (goal-satisfied? goal L1-interp (list state valence arousal))))))
      
      
      ; Given the utterance and whether speaker and listener have the same priors,
      ; L1 reasons about S1's communicative goal
      ; and infers S1's state, valence, and arousal
      ; such that S1 would choose the utterance.
      (define L1
        (mem
         (lambda (utterance speaker-same?)
           (enumeration-query
            (define state (if speaker-same? (state-prior) (uniform-draw states)))
            (define valence (valence-prior state))
            (define arousal (arousal-prior state))
            (define goal (goal-prior))
            (list state valence arousal)
            (equal? utterance (apply multinomial (S1 state valence arousal goal)))
            ))))
      
      ; Given state, valence, arousal, goal,
      ; S1 chooses utterance such that goal 
      ; dimension is communicated to literal listener.
      ; S1 and L0 don't care about shared priors.
      (define S1
        (mem
         (lambda (state valence arousal goal)
           (enumeration-query
            (define utterance (utterance-prior))
            (define lit-interp (apply multinomial (L0 utterance)))
            utterance
                (goal-satisfied? goal lit-interp (list state valence arousal))))))
      
      ; L0 interprets utterance literally and produces speaker's valence and arousal
      ; given utterance is literal
      (define L0
        (mem
         (lambda (utterance)
           (enumeration-query
            (define state (state-prior))
            (define valence (valence-prior state))
            (define arousal (arousal-prior state))
            (list state valence arousal)
            (equal? utterance state)))))
      
      ; The listener is uncertain about the speaker's beliefs.
      ; Prior probability that the speaker shares same beliefs as listener.
      (define listener-sameness-prior 0.5)
      
      ; The listener thinks the speaker is uncertain about the listener's beliefs.
      ; Prior probability that the listener believes the speaker thinks they 
      ; have the same beliefs. 
      (define speaker-sameness-prior 0.5)
      
      (barplot (L2 'terrible) "terrible")
      (barplot (L2 'amazing) "amazing")
      
