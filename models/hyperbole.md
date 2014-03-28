---
layout: model
title: Hyperbole
model-status: code
model-category: Reasoning about Reasoning
model-tags: linguistics, pragmatics
---

A model of hyperbole understanding as pragmatic reasoning:

The speaker chooses an utterance conditioned on the listener inferring information that is correct and relevant to the speaker's communicative goal (or QUD). The goal can either be to communicate the state of the  world, the speaker's attitude towards the state of the world (affect), or both. The listener chooses an interpretation conditioned on the speaker selecting the given utterance when intending to communicate this meaning. In this example the state of the world is how much an electric kettle cost.

	;; Prior of kettle prices (taken from human experiments)
	(define (state-prior) (multinomial states '(0.4205 0.3865 0.0533 0.0538 0.0223 0.0211 0.0112 0.0111 0.0083 0.0120)))
	
	;; Probability that given a price state, the speaker thinks it's too expensive (taken from human experiments)
	(define valence-prior (list (list '50 '0.3173) (list '51 '0.3173) (list '500 '0.7920)  (list '501 '0.7920) (list '1000 '0.8933) (list '1001 '0.8933)(list '5000 0.9524) (list '5001 0.9524) (list '10000 0.9864) (list '10001 0.9864)))
	
	;; Parameters
	(define depth 1)
	(define hardness 1)
	
	;; Define communicative goals and goal priors
	(define goals (list 'state-and-valence-precise 'state-and-valence-imprecise  'just-state-precise 'just-state-imprecise 'just-valence))
	(define (goal-prior) (multinomial goals '(0.17 0.17 0.17 0.17 0.32)))
	
	;; Define list of prices under consideration (possible price states)
	(define states (list '50 '51 '500 '501 '1000 '1001 '5000 '5001 '10000 '10001))
	
	;; Define list of possible utterances (same as price states)
	(define utterances states)
	
	;; Sharp numbers are costlier
	(define (utterance-prior) (multinomial utterances '(0.18 0.1 0.18 0.1 0.18 0.1 0.18 0.1 0.18 0.1)))
	
	;; Define valences. 0 is no valence; 1 is with valence
	(define valences (list '0 '1))
	
	(define (sample-valence state prior)
	  (let ((current-state-valence-pair (first prior)))
	    (if (equal? state (first current-state-valence-pair))
	        (if (flip (second current-state-valence-pair))
	            '1
	            '0)
	        (sample-valence state (rest prior)))))
	
	
	(define (literal-interpretation utterance state)
	  (equal? utterance state))
	
	(define (evaluate-state-goal listener-state speaker-state precision)
	  (if (equal? precision 'precise)
	      (equal? listener-state speaker-state)
	      (<= (abs (- listener-state speaker-state)) 1)))
	
	(define (goal-satisfied? goal listener-state-valence-pair speaker-state speaker-valence)
	  (case goal
	    (('state-and-valence-precise) (and (evaluate-state-goal (first listener-state-valence-pair) speaker-state 'precise)
	                               (equal? (second listener-state-valence-pair) speaker-valence)))
	    (('state-and-valence-imprecise) (and (evaluate-state-goal (first listener-state-valence-pair) speaker-state 'imprecise)
	                               (equal? (second listener-state-valence-pair) speaker-valence)))
	    (('just-state-precise) (evaluate-state-goal (first listener-state-valence-pair) speaker-state 'precise))
	    (('just-state-imprecise) (evaluate-state-goal (first listener-state-valence-pair) speaker-state 'imprecise))
	    (('just-valence) (equal? (second listener-state-valence-pair) speaker-valence))))
	
	;; Speaker model
	(define speaker
	  (mem (lambda (state valence goal depth)
	  (enumeration-query
	   (define utterance (utterance-prior))
	   utterance
	    (goal-satisfied? goal (apply multinomial (listener utterance depth)) state valence)
	   ))))
	
	;; Listener model
	(define listener
	  (mem (lambda (utterance depth)
	  (enumeration-query
	   (define state (state-prior))
	   (define valence (sample-valence state valence-prior))
	   (define speaker-goal (goal-prior))
	   
	   (list state valence)
	
	   (if (equal? depth 0)
	       (literal-interpretation utterance state)
	       (equal? utterance
	               (apply multinomial (speaker state valence speaker-goal (- depth 1)))))
	   ))))
	
	(define (sample-one utterance) 
	  (listener utterance depth))
	
	(list (sample-one '50) (sample-one '51) (sample-one '500) 
	      (sample-one '501) (sample-one '1000) (sample-one '1001)
	      (sample-one '5000) (sample-one '5001) (sample-one '10000) (sample-one '10001))
