---
layout: model
title: Exhaustivity
model-status: code
model-category: Reasoning about Reasoning
model-tags: linguistics, pragmatics
model-language: church
---

	(define (filter pred lst)
	  (fold (lambda (x y)
	          (if (pred x)
	              (pair x y)
	              y))
	        '()
	        lst))

	(define (my-iota n)
	  (define (helper n x)
	    (if (= x n)
	        '()
	        (pair x (helper n (+ x 1)))
	        )
	    )
	  (helper n 0))

	(define (my-sample-integer n)
	  (uniform-draw (my-iota n))
	  )

	(define (extend a b)
	  (if (equal? a '())
	      b
	      (extend (rest a) (pair (first a) b))))

	(define (flatten-nonrecursive a)
	  (fold (lambda (x y) (extend x y)) '() a))

	(define (generate-subsets elements)
	  (if (equal? (length elements) 0)
	      '()
	      (let ((first-element (first elements))
	            (rest-subsets (generate-subsets (rest elements))))
	        (let ((new-subsets (pair (list first-element)
	                                 (map (lambda (x) (pair first-element x)) rest-subsets))))
	          (extend new-subsets rest-subsets)))))

	(define (sample-nonempty-subset elements)
	  (let ((subsets (generate-subsets elements)))
	    (list-ref subsets (my-sample-integer (length subsets)))))

	(define (zip a b)
	  (if (equal? (length a) 0)
	      '()
	      (pair (list (first a) (first b)) (zip (rest a) (rest b)))))

	(define (member-of e elements)
	  (if (> (length elements) 0)
	      (if (equal? e (first elements))
	          #t
	          (member-of e (rest elements)))
	      #f))

	;; _______________________________________________________________________________

	(define states (list 'bob 'mary 'bobmary))
	;;either bob went to the restaurant alone, or mary did, or both bob and mary went

	;;the speaker either knows the exact state, or knows that at least bob went, or knows that at least mary went
	(define knowledge-states (list (list 'bob) (list 'mary) (list 'bobmary) (list 'bob 'bobmary) (list 'mary 'bobmary)))
	(define (knowledge-prior) (multinomial knowledge-states '(1 1 1 1 1)))

	(define knowledge-state-combinations
	  (flatten-nonrecursive (map (lambda (x) (map (lambda (y) (list x y)) x)) knowledge-states)))

	(define (sample-from-knowledge-state knowledge-state)
	  (uniform-draw knowledge-state))

	(define utterances '(bob mary bobandmary))
	(define (get-utterance-prob utterance)
	  (case utterance
	        (('bob 'mary) 1)
	        (('bobandmary) 0.5)
	        (('null) 0)))
	(define (utterance-prior) (multinomial utterances (map get-utterance-prob utterances)))

	(define prosodies (list #f #t))
	(define (get-prosody-prob prosody)
	  (case prosody
	        ((#f) 1)
	        ((#t) 1)))
	(define (prosody-prior) (multinomial prosodies (map get-prosody-prob prosodies)))

	(define (noise-model utterance prosody)
	  (if prosody
	      (lambda (x)
	        (case x
	              ((utterance) 0.99)
	              (('bob) 0.005)
	              (('mary) 0.005)
	              (else 0)))
	      (lambda (x)
	        (case x
	              ((utterance) 0.98)
	              (('bob) 0.01)
	              (('mary) 0.01)
	              (else 0)))))

	;;sample a new utterance from the noise model
	(define (sample-noise-model utterance prosody)
	  (let ((noise-dist (noise-model utterance prosody)))
	    (multinomial utterances (map noise-dist utterances))))

	(define (literal-meaning utterance)
	  (case utterance
	        (('bob) (list 'bob 'bobmary))
	        (('mary) (list 'mary 'bobmary))
	        (('bobandmary) (list 'bobmary))
	        (('null) (list 'bob 'mary 'bobmary))))

	(define (literal-evaluation utterance state)
	  (member-of state (literal-meaning utterance)))

	(define (find-state-prob listener-probs state)
	  (if (equal? listener-probs '())
	      0
	      (if (equal? state (second (first (first listener-probs))))
	          (second (first listener-probs))
	          (find-state-prob (rest listener-probs) state))))

	(define (get-expected-surprisal knowledge-state listener)
	  (let ((listener (zip (first listener) (second listener))))
	    (let ((relevant-listener-probs (filter (lambda (x) (equal? knowledge-state (first (first x))))
	                                           listener)))
	      (let ((state-probs (map (lambda (x) (find-state-prob relevant-listener-probs x)) knowledge-state)))
	        (sum (map (lambda (x) (* (/ 1 (length knowledge-state)) (log x))) state-probs))))))

	(define (speaker-utility knowledge-state utterance prosody depth)
	  (let ((utterance-dist (zip utterances (map (noise-model utterance prosody) utterances))))
	    (let ((utterance-dist (filter (lambda (x) (> (second x) 0)) utterance-dist)))
	      (let ((listeners (map (lambda (x) (listener (first x) prosody (- depth 1))) utterance-dist)))
	        (let ((surprisals (map (lambda (x) (get-expected-surprisal knowledge-state x)) listeners)))
	          (sum (map (lambda (x y) (* (second x) y)) utterance-dist surprisals)))))))

	(define (speaker-literal-evaluate knowledge-state utterance)
	  (let ((truth-value (all (map (lambda (x) (literal-evaluation utterance x)) knowledge-state))))
	    (if truth-value
	        1
	        0)))

	(define speaker
	  (mem (lambda (knowledge-state depth)
	         (enumeration-query
	          (define utterance (utterance-prior))
	          (define prosody (prosody-prior))
	          
	          (list (sample-noise-model utterance prosody) prosody)
	          
	          (factor (+ (* (- hardness 1) (log (get-utterance-prob utterance)))
	                     (* (- hardness 1) (log (get-prosody-prob prosody)))
	                     (* hardness (speaker-utility knowledge-state utterance prosody depth))))))))

	(define listener
	  (mem (lambda (utterance prosody depth)
	         (enumeration-query
	          (define knowledge-state (knowledge-prior))
	          (define state (sample-from-knowledge-state knowledge-state))
	          
	          (list knowledge-state state)
	          
	          (if (equal? depth 0)
	              (let ((intended-utterance (utterance-prior)))
	                (and (equal? utterance (sample-noise-model intended-utterance prosody))
	                     (literal-evaluation intended-utterance state)))
	              (equal? (list utterance prosody) (apply multinomial (speaker knowledge-state depth))))))))

	(define hardness 2)

	(map first (map (lambda (x) (second (listener 'bob #f x))) (list 0 1 2 3 4 5 6 7 8 9 10 11 12)))

References:

- Cite:Bergen2014prosody
