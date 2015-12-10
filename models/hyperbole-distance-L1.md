---
layout: model
title: Hyperbole with uncertainty about speaker's priors
model-status: code
model-language: church
---
	(define (raise-to-power speaker-distribution alpha)
	  (list (first speaker-distribution)
	    (map (lambda (x) (pow x alpha)) (second speaker-distribution))))
	
	(define alpha 10)
	
	(define states
	  (list 50 100 500 1000 5000 10000 1000000))
	
	(define statuses
	  (list 'working 'middle 'onepercent))
	
	; Different priors over watch prices
	(define (prior ses)
	  (case ses
	    (('working) (multinomial states '(0.5 0.2 0.1 0.05 0.05 0.0001 0.0001)))
	    (('middle) (multinomial states '(0.05 0.4 0.3 0.1 0.05 0.0001 0.0001)))
	    (('onepercent) (multinomial states '(0.0001 0.05 0.2 0.3 0.2 0.05 0.0001)))))
	
	  ; Probability of positive valence given each movie state
	  (define (valence-prior ses state)
	    (case ses
	      (('working) (if (flip (second (assoc state
	       (list (list 50 0.2)
	         (list 100 0.5)
	         (list 500 0.9)
	         (list 1000 0.99)
	         (list 5000 0.99)
	         (list 10000 0.99)
	         (list 1000000 0.99)))))
	      'expensive
	      'meh))
	      (('middle) (if (flip (second (assoc state
	       (list (list 50 0.1)
	         (list 100 0.3)
	         (list 500 0.5)
	         (list 1000 0.8)
	         (list 5000 0.99)
	         (list 10000 0.99)
	         (list 1000000 0.99)))))
	      'expensive
	      'meh))
	      (('onepercent) (if (flip (second (assoc state
	       (list (list 50 0.01)
	         (list 100 0.05)
	         (list 500 0.1)
	         (list 1000 0.2)
	         (list 5000 0.3)
	         (list 10000 0.4)
	         (list 1000000 0.99)))))
	      'expensive
	      'meh))))
	  
	
	  
	  
	  ; Probability of the speaker having each communicative goal
	  (define (goal-prior)
	    (multinomial (list 'g-state 'g-valence) '(0.1 0.1)))
	  
	  ; Speaker's goal is satisfied if the goal dimension is communicated to listener
	  (define (goal-satisfied? goal listener-interp speaker-world)
	    (case goal
	      (('g-state) (equal? (first listener-interp) (first speaker-world)))
	      (('g-valence) (equal? (second listener-interp) (second speaker-world)))
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
	     (lambda (utterance listener-ses type)
	       (enumeration-query
	        (define same? (flip sameness-prior))
	        (define speaker-ses (if same? 
	          listener-ses
	          (uniform-draw statuses)))
	        (define state (prior speaker-ses))
	        (define valence (valence-prior speaker-ses state))
	        (define goal (goal-prior))
	        (define interp-type
	          (if (equal? state utterance) 'literal
	            (if (< state utterance) 'hyperbolic 'other)))
	        ;(list same?)
	        (list same?)
	        ; Condition on utterance and interpretation type, probability that listener thinks
	        ; speaker is from the same SES
	        (and (equal? utterance (apply multinomial (raise-to-power (S1 speaker-ses state valence goal) alpha))) 
	         (equal? interp-type type))
	        ))))
	  
	  ; Given state, valence, arousal, goal, and whether prior is shared,
	  ; S1 chooses utterance such that goal dimension is communicated to literal listener
	  (define S1
	    (mem
	     (lambda (ses state valence goal)
	       (enumeration-query
	        (define utterance (utterance-prior))
	        (define lit-interp (apply multinomial (L0 utterance ses)))
	        utterance
	        (goal-satisfied? goal lit-interp (list state valence))))))
	  
	  ; Given utterance and whether prior is shared,
	  ; L0 interprets utterance literally and produces speaker's valence and arousal
	  ; given utterance is literal
	  (define L0
	    (mem
	     (lambda (utterance ses)
	       (enumeration-query
	        (define state utterance)
	        (define valence (valence-prior ses state))
	        (list state valence)
	        (literal-interpretation utterance state)))))
	  
	  ;; Literal interpretation "meaning" function, just check if uttered number reflects price state
	  (define (literal-interpretation utterance state)
	    (equal? utterance state))
	  
	  ; The listener is uncertain about the speaker's beliefs.
	  ; Prior probability that the speaker shares same beliefs as listener.
	  (define sameness-prior 0.5)
	  (barplot (L1 1000000 'working 'literal) "working 1000000 literal")
	  (barplot (L1 1000000 'working 'hyperbolic) "working 1000000 hyperbolic")
	  (barplot (L1 10000 'working 'literal) "working 10000 literal")
	  (barplot (L1 10000 'working 'hyperbolic) "working 10000 hyperbolic")
	  (barplot (L1 1000 'working 'literal) "working 1000 literal")
	  (barplot (L1 1000 'working 'hyperbolic) "working 1000 hyperbolic")
	  (barplot (L1 500 'working 'literal) "working 500 literal")
	  (barplot (L1 500 'working 'hyperbolic) "working 500 hyperbolic")
	  (barplot (L1 100 'working 'literal) "working 100 literal")
	  (barplot (L1 100 'working 'hyperbolic) "working 100 hyperbolic")
	  (barplot (L1 50 'working 'literal) "working 50 literal")
	  
	  (barplot (L1 1000000 'middle 'literal) "middle 1000000 literal")
	  (barplot (L1 1000000 'middle 'hyperbolic) "middle 1000000 hyperbolic")
	  (barplot (L1 10000 'middle 'literal) "middle 10000 literal")
	  (barplot (L1 10000 'middle 'hyperbolic) "middle 10000 hyperbolic")
	  (barplot (L1 1000 'middle 'literal) "middle 1000 literal")
	  (barplot (L1 1000 'middle 'hyperbolic) "middle 1000 hyperbolic")
	  (barplot (L1 500 'middle 'literal) "middle 500 literal")
	  (barplot (L1 500 'middle 'hyperbolic) "middle 500 hyperbolic")
	  (barplot (L1 100 'middle 'literal) "middle 100 literal")
	  (barplot (L1 100 'middle 'hyperbolic) "middle 100 hyperbolic")
	  (barplot (L1 50 'middle 'literal) "working 50 literal")
	  
	  (barplot (L1 1000000 'onepercent 'literal) "onepercent 1000000 literal")
	  (barplot (L1 1000000 'onepercent 'hyperbolic) "onepercent 1000000 hyperbolic")
	  (barplot (L1 10000 'onepercent 'literal) "onepercent 10000 literal")
	  (barplot (L1 10000 'onepercent 'hyperbolic) "onepercent 10000 hyperbolic")
	  (barplot (L1 50 'onepercent 'literal) "onepercent 50 literal")
	
