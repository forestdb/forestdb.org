---
layout: model
title: Plural Predication
model-status: code
model-category: Reasoning about Reasoning
model-tags: language, ambiguity, plurality, interpretation noise
model-language: church
---

This is the model of ambiguity resolution in plural predication from Scontras and Goodman 2015.

	;; helper function to check list identity
	(define (naive-list-equals? as bs)
	  (if (not (= (length as) (length bs)))
	      false
	      (all (map equal? as bs))))
	
	;; helper function to get position of an element x in a haystack lst
	(define (position* lst x) 
	  (if (null? lst) 
	      -Infinity
	      (if (naive-list-equals? (first lst) x)
	          0
	          (+ 1 (position* (rest lst) x)))))
	
	;; helper functiont to compute the maximum support for KL
	(define max-support 
	  (lambda (dist maximal-support)
	    (map 
	     (lambda (x)
	       (if (> (position* (first dist) x) -1)
	           (list-ref (second dist) (position* (first dist) x))
	           0))
	     (first maximal-support))))
	
	;; helper function to compute KL divergence
	(define (KL P Q)
	  (sum (map (lambda (xp xq)
	              (if (= xp 0)
	                  0
	                  (*
	                   xp
	                   (log (/ xp
	                           xq)))))
	            P Q)))
	
	;; helper function for speaker optimality
	(define (power lst alpha) (map (lambda (x) (expt x alpha)) lst))
	(define (topower dist alpha) (list (first dist)
	                                   (power (second dist) alpha)))
	
	;; helper for erf(x)
	(define (t x)
	  (/ 1 
	     (+ 1 (* 0.3275911 x))
	     )
	  )
	(define (erf x)
	  (- 1 (* (+ (* 0.254829592 (t x))
	             (* -0.284496736 (expt (t x) 2))
	             (* 1.421413741 (expt (t x) 3))
	             (* -1.453152027 (expt (t x) 4))
	             (* 1.061405429 (expt (t x) 5)))          
	          (exp (* -1 (expt x 2))))
	     )
	  )
	
	;; the full model starts here
	(define (plural-predication number-of-objects
	                            noise
	                            knowledge
	                            )
	
	  ;; possible utterance 
	  (define utterances (list  
	                      'amb
	                      'collective 
	                      'distributive 
	                      ))
	
	  ;; ambiguous utterance is cheaper
	  (define (utterance-prior) (multinomial utterances '(2 1 1)))
	
	  ;; object come in two sizes, 3 and 4, with equal probability
	  (define object-prior '((3 4) (1 1)))
	
	  ;; states are n random draws from the obejct prior where n=number-of-objects
	  (define (state-prior) 
	    (repeat number-of-objects (lambda () (apply multinomial object-prior) 
	                                )))
	
	  ;; prior on thresholds
	  (define (dist-theta-prior) (apply multinomial object-prior))
	  (define (coll-theta-prior) (uniform-draw '(9 10 11 12)))
	
	  ;; contextual noise in collective interpretation
	  (define noise-variance (case noise
	                               (('no)
	                                0.01) ;; no noise                            
	                               (('low)
	                                .75) ;; low
	                               (('mid) 
	                                1) ;; middle
	                               (('high)
	                                1.25) ;; high
	                               ))
	
	  ;; interpretation semantics
	  (define collective-interpretation 
	    (lambda (state coll-theta collective-noise) 
	      (flip ;;additive noise
	       (- 1 (* (+ 1 (erf (/ (- (- coll-theta (sum state)) 0) 
	                            (* collective-noise (sqrt 2))))) 
	               0.5)))
	      ))
	  (define distributive-interpretation 
	    (lambda (state dist-theta) 
	      (if
	       (all (map (lambda (d) (> d dist-theta)) state))
	       #t
	       #f)))
	
	  ;; utterance semantics
	  (define (meaning utterance state dist-theta coll-theta collective? collective-noise)
	    (case utterance
	          (('amb)
	           (if collective?
	               (collective-interpretation state coll-theta collective-noise)
	               (distributive-interpretation state dist-theta)))
	          (('not-amb)
	           (if collective?
	               (not (collective-interpretation state coll-theta collective-noise))
	               (not (distributive-interpretation state dist-theta))))
	          (('distributive)
	           (distributive-interpretation state dist-theta))
	          (('collective)
	           (collective-interpretation state coll-theta collective-noise))))
	
	  ;; pragmatic listener L1
	  (define prag-listener
	    (mem
	     (lambda (utterance speakerknows)
	       (enumeration-query
	        (define state (state-prior))
	        (define collective? (flip 0.8)) ; prior on collective interpretation
	        (define dist-theta (dist-theta-prior))
	        (define coll-theta (coll-theta-prior))
	
	        ;;         (list collective? state)
	        collective?
	
	        (condition (equal? utterance 
	                           (apply multinomial 
	                                  (topower (speaker collective? state dist-theta coll-theta speakerknows)
	                                           7)))) ;; speaker optimality
	        ))))
	
	  ;; speaker belief function for epistemic manipulation
	  (define speakers-belief
	    (mem
	     (lambda (state knowledge)
	       (define (obs s) (if knowledge s (sum s)))
	       (enumeration-query
	        (define bstate (state-prior))
	        bstate
	        (equal? (obs bstate) (obs state))
	        ))))
	
	  ;; speaker S1
	  (define speaker 
	    (mem
	     (lambda (collective? state dist-theta coll-theta knowstate)
	       (enumeration-query
	        (define utterance (utterance-prior))
	        (define bstate-distribution (speakers-belief state knowstate))
	        (define listener-dist (listener utterance collective? dist-theta coll-theta))
	        (define speaker-dist 
	          (max-support bstate-distribution listener-dist))
	
	
	        utterance
	
	        (factor (- (KL speaker-dist (second listener-dist))))
	        ))))
	
	  ;; literal listener L0
	  (define listener
	    (mem
	     (lambda (utterance collective? dist-theta coll-theta)
	       (enumeration-query
	        (define collective-noise noise-variance)
	        (define state (state-prior))
	        state
	        (condition (meaning utterance state dist-theta coll-theta collective? collective-noise))
	        ))))
	
	  ;; model computes probability of collective for ambiguous utterance
	  (prag-listener 'amb knowledge) 
	
	  )
	
	;; model predictions for 3-object states
	(barplot (plural-predication 3 'no #T) "no noise, full knowledge")
	(barplot (plural-predication 3 'no #F) "no noise, sum knowledge")
	(barplot (plural-predication 3 'low #T) "low noise, full knowledge")
	(barplot (plural-predication 3 'low #F) "low noise, sum knowledge")
	(barplot (plural-predication 3 'mid #T) "mid noise, full knowledge")
	(barplot (plural-predication 3 'mid #F) "mid noise, sum knowledge")
	(barplot (plural-predication 3 'high #T) "high noise, full knowledge")
	(barplot (plural-predication 3 'high #F) "high noise, sum knowledge")
