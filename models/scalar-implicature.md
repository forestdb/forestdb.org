---
layout: model
title: Scalar Implicature
model-status: code
model-category: Reasoning about Reasoning
model-tags: linguistics, pragmatics, theory of mind
model-language: church
---

A model of pragmatic language interpretation: 

The speaker chooses a sentence conditioned on the listener inferring the intended state of the world when hearing this sentence; the listener chooses an interpretation conditioned on the speaker selecting the given utterance when intending this meaning.

    (define (state-prior) (uniform-draw '(0 1 2 3)))
    
    (define (sentence-prior) (uniform-draw (list all-sprouted some-sprouted none-sprouted)))
    
    (define (all-sprouted state) (= 3 state))
    (define (some-sprouted state) (< 0 state))
    (define (none-sprouted state) (= 0 state))
    
    (define (speaker state depth)
      (rejection-query
       (define words (sentence-prior))
       words
       (equal? state (listener words depth))))
    
    (define (listener words depth)
      (rejection-query
       (define state (state-prior))
       state
       (if (= depth 0)
           (words state)
           (equal? words (speaker state (- depth 1))))))
    
    (define depth 1)
    
    (hist (repeat 300 (lambda () (listener some-sprouted depth))))

A more complex version of the model takes into account the listener's knowledge about the speaker's access to the items the speaker is referring to. In this version, lack of access can lead to a cancelled implicature (i.e. "some" does not imply "not all"):

    (define (belief actual-state access)
      (map (lambda (ac st pr) (if ac st (sample pr)))
           access
           actual-state
           (substate-priors)))
    
    (define (baserate) 0.8)
    
    (define (substate-priors)
      (list (lambda () (flip (baserate)))
            (lambda () (flip (baserate)))
            (lambda () (flip (baserate)))))
    
    (define (state-prior)
      (map sample (substate-priors)))
    
    (define (sentence-prior)
      (uniform-draw (list all-p some-p none-p)))
    
    (define (all-p state) (all state))
    (define (some-p state) (any state))
    (define (none-p state) (not (some-p state)))
    
    (define (speaker access state depth)
      (rejection-query
       (define sentence (sentence-prior))
       sentence
       (equal? (belief state access)
               (listener access sentence depth))))
    
    (define (listener speaker-access sentence depth)
      (rejection-query
       (define state (state-prior))
       state
       (if (= 0 depth)
           (sentence state)
           (equal? sentence
                   (speaker speaker-access state (- depth 1))))))
    
    (define (num-true state)
      (sum (map (lambda (x) (if x 1 0)) state)))
    
    (define (show thunk)
      (hist (repeat 100 thunk)))
    
    ;; without full knowledge:
    (show (lambda () (num-true (listener '(#t #t #f) some-p 1))))
    
    ;; with full knowledge:
    (show (lambda () (num-true (listener '(#t #t #t) some-p 1))))

The following model takes into account prosody:

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

	(define states (list 'some 'all))
	;;either bob went to the restaurant alone, or mary did, or both bob and mary went

	;;the speaker either knows the exact state, or knows that at least bob went, or knows that at least mary went
	(define knowledge-states (list (list 'some) (list 'all) (list 'some 'all)))
	(define (knowledge-prior) (multinomial knowledge-states '(1 1 1)))

	(define knowledge-state-combinations
	  (flatten-nonrecursive (map (lambda (x) (map (lambda (y) (list x y)) x)) knowledge-states)))

	(define (sample-from-knowledge-state knowledge-state)
	  (uniform-draw knowledge-state))

	(define utterances '(some all))
	(define (get-utterance-prob utterance)
	  (case utterance
	        (('some 'all) 1)
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
	              (('some) 0.005)
	              (('all) 0.005)
	              (else 0)))
	      (lambda (x)
	        (case x
	              ((utterance) 0.98)
	              (('some) 0.01)
	              (('all) 0.01)
	              (else 0)))))

	;;sample a new utterance from the noise model
	(define (sample-noise-model utterance prosody)
	  (let ((noise-dist (noise-model utterance prosody)))
	    (multinomial utterances (map noise-dist utterances))))

	(define (literal-meaning utterance)
	  (case utterance
	        (('some) (list 'some 'all))
	        (('all) (list 'all))))

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
	(map third (map (lambda (x) (second (speaker (list 'some) x))) (list 1 2 3 4 5 6 7 8 9 10)))


This variant allows listeners to back off to a uniform prior over states when they think the world is 'wonky'. Also allows for including different sets of alternative utterances ("some", "all", "none" in the example call) and QUDs ("how-many" in the example call).


	(define (rep n val) (repeat n (lambda () val)))
	
	(define (power dist a) (list (first dist) 
	                           (map (lambda (x) (pow x a)) (second dist))))
	
	(define (meaning utterance marbles total-marbles)
	  (case utterance
	        (('all) (= marbles total-marbles))
	        (('none) (= marbles 0))        
	        (('some) (> marbles 0))
	        (('most) (> marbles (/ total-marbles 2)))
	        (('many) (> marbles (/ total-marbles 2)))     
		    (('morethanhalf) (> marbles (/ total-marbles 2)))             
	        (('one) (= marbles 1))        
	        (('two) (= marbles 2))          
	        (('three) (= marbles 3))
	        (('four) (= marbles 4))        
	        (('five) (= marbles 5))
	        (('six) (= marbles 6))        
	        (('seven) (= marbles 7))        
	        (('eight) (= marbles 8))
	        (('nine) (= marbles 9))
	        (('ten) (= marbles 10))
	        (('eleven) (= marbles 11))
	        (('twelve) (= marbles 12))
	        (('thirteen) (= marbles 13))
	        (('fourteen) (= marbles 14))
	        (('fifteen) (= marbles 15))        
	        (('almostall) (and (> marbles (* .75 total-marbles)) (< marbles total-marbles)))
	        (('half) (= marbles (/ total-marbles 2)))        
	        (('acouple) (= marbles 2))        
	        (('notone) (= marbles 0))        
	        (('onlyone) (= marbles 1))        
	        (('everyone) (= marbles total-marbles))        
	        (('notmany) (< marbles (/ total-marbles 2)))        
	        (('justone) (= marbles 1))        
	        (('almostnone) (and (> marbles 0) (< marbles (* .25 total-marbles))))
	        (('allbutone) (= marbles (- total-marbles 1)))
	        (('lessthanhalf) (< marbles (/ total-marbles 2)))          
	        (('overhalf) (> marbles (/ total-marbles 2)))  
	        (('several) (> marbles 2))
	        (('afew) (and (< marbles (* total-marbles .7)) (> marbles 1)))               
	        (('veryfew) (< marbles (/ total-marbles 2)))                         
	        (('alot) (> marbles (/ total-marbles 2)))
	        ))
	
	(define (QUD-cell QUD marbles total-marbles)
	  (case QUD
	        (('is-all) (= marbles total-marbles))
	        (('is-any) (> marbles 0))
	        (('how-many) marbles)))
	
	(define (quantifier quantifier-alternatives) (uniform-draw quantifier-alternatives))
	
	(define (marble-state s)
		(multinomial '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15) s))
	
	(define literal-listener 
	  (mem (lambda (utterance QUD total-marbles quantifier-alternatives supposed-prior)
	         (enumeration-query
	          (define marbles (marble-state supposed-prior))
	          
	          (QUD-cell QUD marbles total-marbles)
	          
	          (meaning utterance marbles total-marbles)))))
	
	(define pragmatic-speaker 
	  (mem (lambda (marbles QUD total-marbles quantifier-alternatives supposed-prior)
	         (enumeration-query
	          (define utterance (quantifier quantifier-alternatives))
	          utterance
	          (and
	           (equal? (QUD-cell QUD marbles total-marbles)
	                   (apply multinomial
	                          (literal-listener utterance QUD total-marbles quantifier-alternatives supposed-prior)))
	           (meaning utterance marbles total-marbles))))))
	
	
	(define (pragmatic-listener utterance QUD total-marbles quantifier-alternatives speaker-opt)
		(enumeration-query
			(define wonky-world? (flip wonkyworld-prior))
	        (define actualprior (theprior wonky-world?))		
			(define marbles (marble-state actualprior))
			
			;wonky-world?
			;marbles
			(list wonky-world? marbles)
	
			(equal? utterance (apply multinomial 
	                                 (power (pragmatic-speaker marbles QUD total-marbles quantifier-alternatives actualprior)
	                                        speaker-opt))))) 
	
	; set of alternatives used in the computation
	(define alternatives '(some all none))
	
	; prior wonkiness probability
	(define wonkyworld-prior .5)
	
	; 'empirical' prior distributions
	(define empiricalprior (list .1 .1 .1 .1 .1 .1 .1 .1 .1 .1 .1 .1 .1 .1 2 5))
	
	(define theprior (lambda (wonky-world?)
		(if wonky-world?
			(list .1 .1 .1 .1 .1 .1 .1 .1 .1 .1 .1 .1 .1 .1 .1 .1)
			empiricalprior)))
	
	; run pragmatic listener with an utterance, a QUD, a total number of marbles (this version requires it to be 15), the set of alternatives defined above, and a speaker optimality parameter
	(pragmatic-listener 'some 'how-many 15 alternatives 2) 



References:

- Cite:Goodman2013xz
- Cite:Frank2012fe
- Cite:Stuhlmueller2013aa
- Cite:ProbMods
- Cite:Bergen2014prosody
