---
layout: model
title: PLoT Example Code
model-language: church
---

# Section 22.2

double

	(define double (lambda (x) (+ x x)))
	(double 2)

symbols

	(display
	 (equal? 'bob 'bob)
	 (equal? 'bob 'jim))

randomness 0.0

	(flip 0.5)

randomness 1.0

	(and (flip 0.5) (flip 0.5))

eyecolor 0.0

	(define eyecolor (lambda (x) (if (flip) 'blue 'brown)))
	(eyecolor 'bob)

eyecolor 1.0

	(define eyecolor (mem (lambda (x) (if (flip) 'blue 'brown))))
	(equal? (eyecolor 'bob) (eyecolor 'bob))

eyecolor 2.0: **fix formatting in text!!!**

	;;;fold:
	(hist
	 (repeat
	  100
	  (lambda ()
	    (rejection-query

	     (define father
	       (lambda (x)
	         (string->symbol (string-append "father-of-" (symbol->string x)))))
	     (define mother
	       (lambda (x)
	         (string->symbol (string-append "mother-of-" (symbol->string x)))))
	;;;

	     (define eyecolor 
	       (mem (lambda (x) 
	              (if (flip 0.1) 
	                  (if (flip) 'blue 'brown) 
	                  (if (flip) (eyecolor (father x)) (eyecolor (mother x)))))))

	;;;fold:
	     (eyecolor 'bob)
	     (condition (equal? (eyecolor (mother 'bob)) 'brown))))) "Bob's eye color given that his mother has brown eyes")
	;;;

conditioning 0.0: **fix parens!!!**

	;;;fold:
	(define my-dist (lambda () (gaussian 0 1)))
	(define my-condition (lambda (x) (> x 0)))
	;;;

	(define my-conditional
	  (lambda ()
	    (define sample (my-dist))
	    (if (my-condition sample) sample (my-conditional))))

	;;;fold:
	(density (repeat 1000 my-conditional)
	      "Sample X from a standard gaussian, condition on X being positive" #t)
	;;;

conditioning 1.0

	;;;fold:
	(define father
	  (lambda (x)
	    (string->symbol (string-append "father-of-" (symbol->string x)))))
	(define mother
	  (lambda (x)
	    (string->symbol (string-append "mother-of-" (symbol->string x)))))

	(hist
	 (repeat
	  100
	  (lambda ()
	;;;

	(rejection-query
	 
	 (define eyecolor
	   (mem (lambda (x) 
	          (if (flip 0.1) 
	              (if (flip) 'blue 'brown) 
	              (if (flip) (eyecolor (father x)) (eyecolor (mother x)))))))
	 
	 (eyecolor (mother 'bob))
	 (equal? 'blue (eyecolor 'bob)))
	    
	;;;fold:
	    )) "Bob's mother's eye color given that Bob has blue eyes")
	;;;

# Section 22.3

ping-pong 0.0: **fix winning condition!!**

	;;;fold:
	(hist
	 (repeat
	  100
	  (lambda ()
	;;;
	     (define personstrength (mem (lambda (person) (gaussian 10 3))))
	     (define lazy (mem (lambda (person game) (flip 0.1))))
	     (define teamstrength 
	       (mem (lambda (team game) 
	              (sum (map (lambda(person) 
	                          (if (lazy person game) 
	                              (/ (personstrength person) 2) 
	                              (personstrength person))) 
	                        team)))))
	     (define winner
	       (mem (lambda (team1 team2 game) 
	              (if (> (teamstrength team1 game) (teamstrength team2 game))
	                  'team1 'team2))))

	     (winner '(bob alice) '(eve) 'game1)
	;;;fold:
	    )) "Who wins the Ping-Pong match when team1 (Bob and Alice) plays against team2 (Eve)?")
	;;;

ping-pong 1.0: **change = to equals? in condition**

	;;;fold:
	(density
	 (repeat
	  100
	  (lambda ()
	    (define personstrength (mem (lambda (person) (gaussian 10 3))))
	    (define lazy (mem (lambda (person game) (flip 0.1))))
	    (define teamstrength 
	      (mem (lambda (team game) 
	             (sum (map (lambda(person) 
	                         (if (lazy person game) 
	                             (/ (personstrength person) 2) 
	                             (personstrength person))) 
	                       team)))))
	    (define winner
	      (mem (lambda (team1 team2 game) 
	             (if (> (teamstrength team1 game) (teamstrength team2 game))
	                 'team1 'team2))))

	    ;The query: 
	    (personstrength 'TG)
	    )) "TG's strength a priori" #t)

	(density
	 (repeat
	  100
	  (lambda ()
	;;;

	    (rejection-query 

	     ;; . . . CONCEPTS . . . 

	;;;fold:
	     (define personstrength (mem (lambda (person) (gaussian 10 3))))
	     (define lazy (mem (lambda (person game) (flip 0.1))))
	     (define teamstrength 
	       (mem (lambda (team game) 
	              (sum (map (lambda(person) 
	                          (if (lazy person game) 
	                              (/ (personstrength person) 2) 
	                              (personstrength person))) 
	                        team)))))
	     (define winner
	       (mem (lambda (team1 team2 game) 
	              (if (> (teamstrength team1 game) (teamstrength team2 game))
	                  'team1 'team2))))
	;;;

	     ;The query: 
	     (personstrength 'TG) 

	     ;The evidence: 
	     (and 
	      (equal? 'team1 (winner '(TG) '(NG) 1)) 
	      (equal? 'team1 (winner '(NG) '(AS) 2)) 
	      (equal? 'team1 (winner '(NG) '(BL) 3))))

	;;;fold:
	    )) "TG's strength when he wins against an apparently strong NG" #t)
	;;;

ping-pong 2.0: **uncomment query!!**

	;;;fold:
	(density
	 (repeat
	  100
	  (lambda ()
	;;;
	    
	    (rejection-query 

	     ;; . . . CONCEPTS . . . 

	     ;;;fold:
	     (define personstrength (mem (lambda (person) (gaussian 10 3))))
	     (define lazy (mem (lambda (person game) (flip 0.1))))
	     (define teamstrength 
	       (mem (lambda (team game) 
	              (sum (map (lambda(person) 
	                          (if (lazy person game) 
	                              (/ (personstrength person) 2) 
	                              (personstrength person))) 
	                        team)))))
	     (define winner
	       (mem (lambda (team1 team2 game) 
	              (if (> (teamstrength team1 game) (teamstrength team2 game))
	                  'team1 'team2))))
	     ;;;

	     ;The query:
	     (personstrength 'TG) 

	     ;The evidence: 
	     (and 
	      (equal? 'team1 (winner '(TG) '(NG) 1)) 
	      (equal? 'team1 (winner '(NG) '(AS) 2))
	      (equal? 'team1 (winner '(NG) '(BL) 3)) 
	      (lazy 'NG 1))) ;additional kinds of evidence (Expt. 2)

	;;;fold:
	    )) "TG's strength when he wins against an apparently strong NG *when NG is lazy*" #t)
	;;;

# Section 22.4

ping-pong 3.0

	;; this doesn't run
	(define lazy (mem (lambda (person game) 
		(query 
			(define action (flip L)) 
			action 
			(= (teamof person)(winner (team1of game) (team2of game) game))))))

intuitive psychology

	(define choice (lambda (belief state goal?) 
	    (rejection-query 
	        (define action (action-prior)) 
	        action 
	        (goal? (belief state action)))))

	;;;fold:

	;; i can choose to sleep or play soccer.
	(define action-prior (lambda () (uniform-draw '(sleep soccer))))

	;; i have a theory of what will happen, depending on what action i take and how i currently feel
	(define my-theory
	  (lambda (state action)
	    (define well-rested
	      (if (equal? state 'i-am-tired)
	          ;; if i'm tired, i can be well-rested if and only if i sleep
	          (equal? action 'sleep)
	          ;; if i'm not tired, then i'm already well-rested
	          #t))
	    (define had-fun
	      (if (equal? action 'soccer)
	          (if well-rested
	              ;; if i play soccer and i'm energetic, i will likely have fun
	              (flip 0.9)
	              ;; if i play soccer but i'm tired, it's unlikely i will have fun
	              (flip 0.2))
	          ;; if i don't play soccer, it's even less likely i'll have fun. i really like soccer
	          (flip 0.1)))
	    (list well-rested had-fun)))

	(define did-i-have-fun?
	  (lambda (end-state)
	    (define well-rested (first end-state))
	    (define had-fun (second end-state))
	    had-fun))

	(hist (repeat 100 (lambda () (choice my-theory 'i-feel-energetic did-i-have-fun?)))
	      "If I'm energetic and I want to have fun, will I play soccer or sleep?")

	(hist (repeat 100 (lambda () (choice my-theory 'i-am-tired did-i-have-fun?)))
	      "If I'm tired, but I want to have fun, will I play soccer or sleep?")
	;;;

