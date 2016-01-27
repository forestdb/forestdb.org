---
layout: model
title: Spatial Implicature
model-status: code
model-category: Reasoning about Reasoning
model-tags: implicature, pragmatic reasoning
model-language: church
---

    ;;;; Helper functions ;;;
    
    ;; Grabbing coordinates
    (define (x-coordinate state) (first state))
    (define (y-coordinate state) (second state))
    
    ;; Calculate euclidian distance between two points
    (define (dist p1 p2) (sqrt (+ (pow (- (first p1) (first p2)) 2) 
                                  (pow (- (second p1) (second p2)) 2))))
    
    ;; Rounding to the second digit for less cluttered results
    (define (round-2 x)
      (/ (round (* x 100)) 100))
    
    ;; Epsilon parameter for approximate equality between points in the city. 
    (define epsilon 20)
    
    ;;;; City layout information ;;;
    
    ;; All cities
    (define cityW 500)
    (define cityH 300)
    
    (define redQuarterW 250)
    (define redQuarterH 150)
    
    (define blueQuarterW 100)
    (define blueQuarterH 70)
    
    (define plazaR 30)
    
    (define redQuarterBL '(20 20))
    (define blueQuarterBL '(330 170))
    
    (define city1Plaza '(120 130))
    (define city2Plaza '(370 205))
    (define city3Plaza '(150 170))
    (define city4Plaza '(285 205))
    
    ;; Specific city, change this to evaluate a new map
    (define plazaC city1Plaza)
    
    ;; Prior on where Lilies can grow, set to uniform over City
    (define (lily-prior) (list (uniform 0 cityW) (uniform 0 cityH)))
    
    
    ;;;; Building the lexicon ;;;
    
    ;; TODO: More general version would take in polygon borders and return 
    ;;       a function for checking relations on that. As is, functions 
    ;;       returned are specified to rectangles and circles. 
    
    ;; 'IN'
    ;; The constructor takes in a bottom-left position, a width and a height
    ;; and returns a function that takes in a state and returns true/false.
    ;; True is returned for points within the bounding box/circle.
    
    (define (in-constructor bl w h)
      (lambda (state) (and (< (first bl) (x-coordinate state)) 
                                (< (second bl) (y-coordinate state))
                                (> (+ (first bl) w) (x-coordinate state))
                                (> (+ (second bl) h) (y-coordinate state)))))
    
    ;; Define the specific meaning of various in relations using the constructor
    (define in-red (in-constructor redQuarterBL redQuarterW redQuarterH))
    (define in-blue (in-constructor blueQuarterBL blueQuarterW blueQuarterH))
    (define in-city (in-constructor '(0 0) cityW cityH))
    (define (in-plaza state) (> plazaR (dist state plazaC)))
    
    ;; 'NEAR-EDGE'
    ;; Similar to the 'IN' constructor, except points are evaluated as True
    ;; if they fall near the edge of the area within a certain distance, whether 
    ;; inside or outside the bounding box/circle. 
    ;; 'near-dist' is the tolerance distance. 
    
    (define (near-edge-constructor bl w h near-dist)
      (lambda (state)
        (let ((x (x-coordinate state))
              (y (y-coordinate state))
              (xbl (first bl))
              (ybl (second bl)))
          (or (and (< (abs (- x xbl)) near-dist) (< ybl y) (< y (+ ybl h)))
              (and (< (abs (- x (+ xbl w))) near-dist) (< ybl y) (< y (+ ybl h)))
              (and (< (abs (- y ybl)) near-dist) (< xbl x) (< x (+ xbl w)))
              (and (< (abs (- y (+ ybl h))) near-dist) (< xbl x) (< x (+ xbl w)))
              ))))
    
    
    ;;;;; Speaker and Listener models
    (define (speaker state depth)
      (rejection-query
       
       ;; Define a near-distance prior as uniform on 10-30
       (define (dist-prior) (uniform-draw (list 10 20 30)))
       
       ;; Sample the tolerance for each region from the prior
       (define near-dist-red (dist-prior))
       (define near-dist-blue (dist-prior))
       (define near-dist-city (dist-prior))
       (define near-dist-plaza (dist-prior))
       
       ;; 'NEAR-EDGE'
       ;; Define the utterances as far as the speaker understands them, using their choice of tolerances
       (define near-edge-red (near-edge-constructor redQuarterBL redQuarterW redQuarterH near-dist-red))
       (define near-edge-blue (near-edge-constructor blueQuarterBL blueQuarterW blueQuarterH near-dist-blue))
       (define near-edge-city (near-edge-constructor '(0 0) cityW cityH near-dist-city))
       (define (near-edge-plaza state) (< (abs (- plazaR (dist state plazaC))) near-dist-plaza))
    
       ;; 'NEAR'
       ;; Similar to the 'in' constructor, except point are evaluated as true
       ;; if they fall outside the box within a certain distance. 
       (define (near-red state) (and (near-edge-red state) (not (in-red state))))
       (define (near-blue state) (and (near-edge-blue state) (not (in-blue state))))
       (define (near-plaza state) (and (near-edge-plaza state) (not (in-plaza state))))
       
       ;; List the possible *meanings* of the utterances
       (define utterances (list in-red in-blue in-plaza in-city 
                                near-edge-red near-edge-blue near-edge-plaza near-edge-city
                                near-red near-blue near-plaza))
       
       ;; Sample pointer to utterance. Uniform before seeing Lily
       (define utterance-index (uniform-draw (list 0 1 2 3 4 5 6 7 8 9 10)))
       
       ; Query
       utterance-index
       
       ; Condition on the listener (approximately) matching the observed state
       (< (dist state (listener utterance-index depth)) epsilon)))
    
    (define (listener utterance-index depth)
      (rejection-query
       ;; Sample lily location from prior
       (define state (lily-prior))
       
       ;; Define a near-distance prior as uniform on 10-50
       (define (dist-prior) (uniform-draw (list 10 20 30)))
       
       ;; Sample the near-distance for each region
       (define near-dist-red (dist-prior))
       (define near-dist-blue (dist-prior))
       (define near-dist-city (dist-prior))
       (define near-dist-plaza (dist-prior))
       
       ;; 'NEAR-EDGE'
       ;; Define the utterances as far as the speaker understands them, using their choice of near-dists
       (define near-edge-red (near-edge-constructor redQuarterBL redQuarterW redQuarterH near-dist-red))
       (define near-edge-blue (near-edge-constructor blueQuarterBL blueQuarterW blueQuarterH near-dist-blue))
       (define near-edge-city (near-edge-constructor '(0 0) cityW cityH near-dist-city))
       (define (near-edge-plaza state) (< (abs (- plazaR (dist state plazaC))) near-dist-plaza))
    
       ;; 'NEAR'
       ;; Similar to the 'in' constructor, except point are evaluated as true
       ;; if they fall outside the box within a certain distance. 
       (define (near-red state) (and (near-edge-red state) (not (in-red state))))
       (define (near-blue state) (and (near-edge-blue state) (not (in-blue state))))
       (define (near-plaza state) (and (near-edge-plaza state) (not (in-plaza state))))
    
       
       ;; Define the possible *meaning* of the utterances, might be different from 
       ;; speaker in their tolerance
       (define utterances (list in-red in-blue in-plaza in-city 
                                near-edge-red near-edge-blue near-edge-plaza near-edge-city
                                near-red near-blue near-plaza))
    
       ;; The speaker-passed utterance-index chooses the relevant utterance
       (define utterance (list-ref utterances utterance-index))
    
       ;; Query on the position of the lily
       state
       
       ;; Conditioned on...
       (if (= depth 0)
           ;; ...The literal meaning evaluating to 'True' for the state (L0)
           (utterance state)
           ;; ...The utterance heard being what the speaker would say if 
           ;;    they had observed the sampled state (L1)
           (equal? utterance-index (speaker state (- depth 1))))))
    
    ;; Choose the depth of the listener:
    ;;   Depth 0 => Literal listener (L0)
    ;;   Dpeth 1 => Pragmatic listener (L1)
    (define depth 0)

References:

- Cite:ProbMods

;; Choose number of samples to take and utterance heard by listener
(define dat (repeat 5000 (lambda () (listener 8 depth))))

;; Round the results and output the data
(map (lambda (x) (list (round-2 (first x)) (round-2 (second x)))) dat)
