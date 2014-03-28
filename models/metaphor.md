---
layout: model
title: Metaphor
model-status: code
model-category: Reasoning about Reasoning
model-tags: linguistics, pragmatics
---

A model of metaphor interpretation as pragmatic reasoning:

The speaker chooses an utterance conditioned on the listener inferring a state of the world that is correct and relevant to the speaker's communicative goal (or QUD). The listener chooses an interpretation conditioned on the speaker selecting the given utterance when intending to communicate this meaning. Different animal categories are associated with different features with empirically measured priors. The speaker's goal may be to communicate different features; the listener is uncertain about the speaker's goal and performs joint inference on the goal and the intended meaning.


    (define (state-prior) (uniform-draw '(0 1 2 3)))
    
    
    ;; In this model, the possible utterances a speaker may choose are "John is a whale" and "John is a person."
    (define categories (list 'whale 'person))
    (define utterances (list 'whale 'person))
    ;; The utterances are equally costly.
    (define (utterance-prior) (multinomial utterances '(0.1 0.1)))
    ;; It is a priori extremely unlikely that John is actually a whale.
    (define (categories-prior) (multinomial categories '(0.01 0.99)))
    ;; The features being considered are "large", "graceful", "majestic"
    (define featureSets (list '(1 1 1) '(1 1 0) '(1 0 1) '(1 0 0) '(0 1 1) '(0 1 0) '(0 0 1) '(0 0 0)))
	(define featureSet-prior (list (list '0.30592786494628 '0.138078454222818 '0.179114768847673 '0.13098781834847 '0.0947267162507846 '0.0531420411185539 '0.0601520520596695 '0.0378702842057509) (list '0.11687632453038 '0.105787535267869 '0.11568145784997 '0.130847056136141 '0.15288225956497 '0.128098151176801 '0.114694702836614 '0.135132512637255) ))
	
	;; Speaker's possible goals are to communicate feature 1, 2, or 3
	(define goals (list 'goal-feature1 'goal-feature2 'goal-feature3))
	
	;; Prior probability of speaker's goal. Set to uniform but can change with context/QUD.
	(define (goal-prior) (multinomial goals '(0.33 0.33 0.33)))
	
	;; Speaker optimality parameter
	(define alpha 3.0)
	;; Recursive depth
	(define depth 1)
	
	;; Sample set of features given that John is a member of category
	(define (sample-featureSet category prior all-categories)
	(if (equal? category (first all-categories))
	(multinomial featureSets (first prior))
	(sample-featureSet category (rest prior) (rest all-categories))
	)
	)
	
	;; Literal interpretation returns true if interpreted categroy is identical to utterance
	(define (literal-interpretation utterance category)
	(equal? utterance category))
	
	;; Check if goal is satisfied
	(define (goal-satisfied? goal listener-category-feature1-feature2-feature3 speaker-category speaker-feature1 speaker-feature2 speaker-feature3)
	(case goal
	(('goal-feature1) (equal? (second listener-category-feature1-feature2-feature3) speaker-feature1))
	(('goal-feature2) (equal? (third listener-category-feature1-feature2-feature3) speaker-feature2))
	(('goal-feature3) (equal? (fourth listener-category-feature1-feature2-feature3) speaker-feature3))
	))
	
	
	;; Speaker model
	(define speaker
	(mem (lambda (category feature1 feature2 feature3 goal depth)
	(enumeration-query
	(define utterance (utterance-prior))
	utterance
	(goal-satisfied? goal (apply multinomial (listener utterance depth)) category feature1 feature2 feature3)
	))))
	
	;; Listener model
	(define listener
	(mem (lambda (utterance depth)
	(enumeration-query
	(define category (categories-prior))
	(define featureSet (sample-featureSet category featureSet-prior categories))
	(define feature1 (first featureSet))
	(define feature2 (second featureSet))
	(define feature3 (third featureSet))
	(define speaker-goal (goal-prior))
	
	(list category feature1 feature2 feature3)
	
	(if (equal? depth 0)
	(literal-interpretation utterance category)
	(equal? utterance
	(apply multinomial (raise-to-power (speaker category feature1 feature2 feature3 speaker-goal (- depth 1)) alpha))))
	))))
	
	(define (raise-to-power speaker-dist alpha)
	(list (first speaker-dist) (map (lambda (x) (pow x alpha)) (second speaker-dist))))
	
	(define (sample-one utterance)
	(listener utterance depth))
	
	;; Returns probability of category and features given the utterance "John is a whale."
	(sample-one 'whale)

References:
- Cite:Goodman2013xz
