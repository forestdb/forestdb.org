---
layout: model
title: Ellipsis
model-status: code
model-category: Reasoning about Reasoning
model-tags: linguistics, pragmatics
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

	(define (list-product a b)
	  (flatten-nonrecursive (map (lambda (x) (map (lambda (y) (list x y)) b)) a)))

	;; _______________________________________________________________________________

	(define states (list-product (list 'bob 'mary 'nobody) (list 'restaurant)))

	(define knowledge-states (list (list (list 'bob 'restaurant))
	                               (list (list 'mary 'restaurant))
	                               (list (list 'nobody 'restaurant))))
	(define (knowledge-prior) (multinomial knowledge-states (list 1 1 1)))

	(define knowledge-state-combinations
	  (flatten-nonrecursive (map (lambda (x) (map (lambda (y) (list x y)) x)) knowledge-states)))

	(define (sample-from-knowledge-state knowledge-state)
	  (list-ref knowledge-state (my-sample-integer (length knowledge-state))))

	(define utterances (list-product (list 'null 'bob 'mary 'nobody) (list 'null 'restaurant)))
	(define grammatical-utterances (list-product (list 'bob 'mary 'nobody) (list 'restaurant)))
	(define (get-utterance-prob utterance)
	  (* (case (first utterance)
	       (('bob 'mary 'nobody) 1)
	       (('null) 1))
	     (case (second utterance)
	       (('restaurant) 1)
	       (('null) 1))))

	(define (utterance-prior) (multinomial utterances (map get-utterance-prob utterances)))
	(define (grammatical-utterance-prior) (multinomial grammatical-utterances (map get-utterance-prob grammatical-utterances)))

	(define (noise-model utterance)
	  (lambda (x)
	  (* (case (first x)
	                     (((first utterance)) 0.99)
	                     (('null) 0.01)
	                     (else 0))
	                   (case (second x)
	                     (((second utterance)) 0.99)
	                     (('null) 0.01)
	                     (else 0)))))

	(define (sample-noise-model utterance prosody)
	  (let ((noise-dist (noise-model utterance prosody)))
	    (multinomial utterances (map noise-dist utterances))))

	(define (literal-meaning utterance)
	  (list
	   (case (first utterance)
	     (('bob) (list 'bob))
	     (('mary) (list 'mary))
	     (('nobody) (list 'nobody))
	     (('null) (list 'bob 'mary)))
	   (case (second utterance)
	     (('restaurant) (list 'restaurant))
	     (('null) (list 'restaurant )))))

	(define (literal-evaluation utterance state)
	  (let ((lit (literal-meaning utterance)))
	    (and (member-of (first state) (first lit))
	         (member-of (second state) (second lit)))))

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

	 (define (speaker-utility knowledge-state utterance depth)
	   (let ((utterance-dist (zip utterances (map (noise-model utterance) utterances))))
	     (let ((utterance-dist (filter (lambda (x) (> (second x) 0)) utterance-dist)))
	       (let ((listeners (map (lambda (x) (listener (first x) (- depth 1))) utterance-dist)))
	         (let ((surprisals (map (lambda (x) (get-expected-surprisal knowledge-state x)) listeners)))
	           (sum (map (lambda (x y) (* (second x) y)) utterance-dist surprisals)))))))


	(define speaker
	  (mem (lambda (knowledge-state depth)
	         (enumeration-query
	          (define utterance (utterance-prior))

	          (list (sample-noise-model utterance))


	          (factor (+ (* (- hardness 1) (log (get-utterance-prob utterance)))
	                (* hardness (speaker-utility knowledge-state utterance depth))))))))

	(define listener
	  (mem (lambda (utterance depth)
	         (enumeration-query
	          (define knowledge-state (knowledge-prior))
	          (define state (sample-from-knowledge-state knowledge-state))

	          (list knowledge-state state)
	          
	          (if (equal? depth 0)
	              (let ((intended-utterance (grammatical-utterance-prior)))
	                (and (equal? utterance (sample-noise-model intended-utterance))
	                     (literal-evaluation intended-utterance state)))
	              (equal? (list utterance) (apply multinomial (speaker knowledge-state depth))))))))


	(define hardness  2)

	(barplot (speaker (list (list 'bob 'restaurant)) 1))

References:

- Cite:Bergen2014prosody
