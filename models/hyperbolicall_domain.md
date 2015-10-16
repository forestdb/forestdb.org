---
layout: model
title: Hyperbolic quantifiers with domain restriction
model-status: code
model-language: church
---

        (define num-A 3)
        (define num-B 6)
        (define states 
              (list 
                  '(0, 0) '(0, 1) '(0, 2) '(0, 3) '(0, 4) '(0, 5) '(0, 6) 
                  '(1, 0) '(1, 1) '(1, 2) '(1, 3) '(1, 4) '(1, 5) '(1, 6) 
                  '(2, 0) '(2, 1) '(2, 2) '(2, 3) '(2, 4) '(2, 5) '(2, 6) 
                  '(3, 0) '(3, 1) '(3, 2) '(3, 3) '(3, 4) '(3, 5) '(3, 6)))
                              
        (define (item-prior) (multinomial states 
                  '(0.147069152314 0.0980461015429 0.0653640676953 0.0435760451302 0.0290506967535 
                  0.019367131169 0.0129114207793 0.0980461015429 0.0653640676953 0.0435760451302 
                  0.0290506967535 0.019367131169 0.0129114207793 0.00860761385288 0.0653640676953 
                  0.0435760451302 0.0290506967535 0.019367131169 0.0129114207793 0.00860761385288 
                  0.00573840923525 0.0435760451302 0.0290506967535 0.019367131169 0.0129114207793 
                  0.00860761385288 0.00573840923525 0.00382560615683)))
                  
        (define (affect-prior) 
                        (list 
                              '(0, 0.01212998770738744) 
                              '(1, 0.05279266352637022) 
                              '(2, 0.14072860496657558) 
                              '(3, 0.2705545996562575) 
                              '(4, 0.4183148490476859) 
                              '(5, 0.5593309536848171) 
                              '(6, 0.678858462295646) 
                              '(1, 0.05279266352637022) 
                              '(2, 0.14072860496657558) 
                              '(3, 0.2705545996562575) 
                              '(4, 0.4183148490476859) 
                              '(5, 0.5593309536848171) 
                              '(6, 0.678858462295646) 
                              '(7, 0.7724187794670039) 
                              '(2, 0.14072860496657558) 
                              '(3, 0.2705545996562575) 
                              '(4, 0.4183148490476859) 
                              '(5, 0.5593309536848171) 
                              '(6, 0.678858462295646) 
                              '(7, 0.7724187794670039) 
                              '(8, 0.841850968819012) 
                              '(3, 0.2705545996562575) 
                              '(4, 0.4183148490476859) 
                              '(5, 0.5593309536848171) 
                              '(6, 0.678858462295646) 
                              '(7, 0.7724187794670039) 
                              '(8, 0.841850968819012) 
                              '(9, 0.8915728001645071)))
                              
          (define goal-probs '(0.5 0.3 0.05 0.15))
                  
          ; A speaker can have the goal to communicate about the total number of things Bob ate,
          ; the number of Type A's Bob ate, or how she feels about Bob's eating behavior
          (define goals
             (list
                'how-many-total?
                'how-many-A?
                'how-many-B?
                'affect?
              ))
                  
                  
          (define num-total (+ num-A num-B))
          (define (goal-prior) (multinomial goals goal-probs))
                  
          ; Things speaker can say
            (define utterances (list
                  	'some
                  	'all
                  	'none
            ))
                  
          (define (utterance-prior) (multinomial utterances '(0.1 0.1 0.1)))
                  
          ; Sample affect given total number of things eaten
                  (define (sample-affect total affect-prior)
                  	(let ((current-state-affect-pair (first affect-prior)))
                  		(if (equal? total (first current-state-affect-pair))
                  			(if (flip (second current-state-affect-pair))
                  				'1
                  				'0)
                  			(sample-affect total (rest affect-prior)))))
                  
                  ; Literal interpretation of words. 
          (define (literal-interpretation utterance state)
                  	(case utterance
                  		(('all) (equal? (sum state) num-total))
                  		(('some) (> (sum state) 0))
                  		(('none) (equal? (sum state) 0))))
            
                  
          (define (goal-satisfied? goal listener-interp speaker-world)
                  	(case goal
                  		(('how-many-total?) (equal? (sum (first listener-interp)) (sum (first speaker-world))))
                  		(('how-many-A?) (equal? (first (first listener-interp)) (first (first speaker-world))))
                  		(('how-many-B?) (equal? (second (first listener-interp)) (second (first speaker-world))))
                  		(('affect?) (equal? (second listener-interp) (second speaker-world)))
                  		))
                  
          ; Literal listener interprets utterance literally
            (define L0
                  	(mem (lambda (utterance goal)
                  		(enumeration-query
                  			(define state (item-prior))
                  			(define total (sum state))
                  			(define affect (sample-affect total (affect-prior)))
                  			(list state affect)
                  			(literal-interpretation utterance state)))))
                  
          ; Speaker chooses utterance for literal listener
            (define speaker
                  	(mem (lambda (state affect goal)
                  		(enumeration-query
                  			(define utterance (utterance-prior))
                  			utterance
                  			(goal-satisfied? goal (apply multinomial (L0 utterance goal))
                  				(list state affect))
                  			))))
                  
         ; Pragmatic listener infers number of A's and B's eaten and speaker's affect
            (define L1
                  	(mem (lambda (utterance)
                  		(enumeration-query
                  			(define state (item-prior))
                  			(define total (sum state))
                  			(define affect (sample-affect total (affect-prior)))
                  			(define goal (goal-prior))
                  
                  			(list state affect goal)
                  
                  			(equal? utterance
                  				(apply multinomial (raise-to-power (speaker state affect goal) alpha)))
                  			))))
                  
        (define (raise-to-power speaker-dist alpha)
                  	(list (first speaker-dist) (map (lambda (x) (pow x alpha)) (second speaker-dist))))
                  
        (define alpha 1)
                  
        (define all-interpretation (L1 'all))
        (define some-interpretation (L1 'some))
        (define none-interpretation (L1 'none))
                  
        all-interpretation
        some-interpretation
        none-interpretation
                  
