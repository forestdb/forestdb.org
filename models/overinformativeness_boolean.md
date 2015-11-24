---
layout: model
title: Overinformativeness model with Boolean properties
model-status: code
model-language: church
---

# A pragmatic speaker that can do basic composition and takes into account predicate noise. 

You can vary:

- color and size noise (fidelity)

- color and size cost

- speaker optimality

- context (lists of features)

This model is specifically to explore the interaction of noise, cost, and optimality parameters for a fixed context of interest from the Gatt et al. 2011 paper (big red, small red, small yellow)

~~~
(define (power dist a) (list (first dist) 
                             (map (lambda (x) (pow x a)) (second dist))))

(define color-fidelity .999)
(define size-fidelity .8)
(define cost_color .1)
(define cost_size .1)
(define speaker-opt 17)

; Free parameters:
(define color-fidelity .9) ; 1 - noise probability (color -- higher means less noise)
(define size-fidelity .7) ; 1 - noise probability (size -- higher meanss less noise)
(define cost_color .1) ; lower means less costly
(define cost_size .3) ; lower means less costly
(define speaker-opt 5) ; standard speaker optimality parameter
(define listener-opt 1) ; non-standard listener optimality parameter -- leave unchanged


; A context is a list of lists, where sub-lists represent objects with a name (unique ID), a size feature and a color feature (currently only implemented for two features -- TODO: extend!)
(define context (list (list 'o1 'big 'red)
                      (list 'o2 'small 'red)
                      (list 'o3 'small 'yellow)))

; Extracts a list of object IDs for the objects in the context
(define objs
  (map (lambda (obj)
         (first obj))
       context))

; Helper function for utterances: Strips the original context of object IDs
(define pruned-context 
  (map (lambda (cont) 
         (drop cont 1))
       context))		

; Helper function for utterances: concatenates an object's features to create a "_"-separated two-word utterance like "big_red"
(define (gen-utt obj)
  (append obj (list (string-append (first obj) '_ (second obj)))))

; Generates the set of alternatives for the context by taking all feature conjunctions as utterances as well as each individual feature (currently only implemented for two-feature objects)
(define utterances
  (union (flatten (map (lambda (obj) 
                         (gen-utt obj)) 
                       pruned-context))))

; Generates a cost vector for the utterances, with a fixed cost for an extra word (free parameter defined at beginning of file). One-word utterances have cost 1.
(define costs
  (map (lambda (utt)
         (sum (map (lambda (word) 
                           (if (or (equal? word 'red) 
                                   (equal? word 'yellow)) 
                               cost_color cost_size)) 
                         (regexp-split utt '_))))
       utterances))

; Helper function for utterance-atoms: Tests whether an utterance consists of just one word (multi-word utterances are separated by "_")
(define (is-atom? utt) (= (length (regexp-split utt '_)) 1))

; Extracts all the utterance atoms (one-word utterances) from the set of contextually generated utterances
(define utterance-atoms (first (partition is-atom? utterances)))

; The basic lexicon that encodes noisy semantics for words (ie correctly returns true/false with probability determined by fidelity parameter)
(define (lexicon utterance obj)
  (if (> (list-index (check-features obj) utterance) -1)
      (flip (get-fidelity utterance))
      (flip (- 1 (get-fidelity utterance)))))


; Helper function for lexicon: Returns a list of an object's features
(define (check-features obj)
  (list-ref pruned-context (list-index objs obj)))

; Helper function for lexicon: Retrieves the fidelity of a predicate (currently only implemented for color and size)
; Danger: assumes only the listed colors and will return size fidelity for ANY predicate that's not in the color list
(define (get-fidelity utterance)
  (if (> (list-index 
          '(red orange yellow green blue purple black brown pink white)
          utterance) -1)
      color-fidelity
      size-fidelity)) 

; Tests whether an object is in the extension of an utterance -- directly applies lexicon if the utterance is a one-word utterance atom; builds meaning compositionally otherwise (only works for at most two-word utterances)
(define (meaning utterance obj)
  (if (lexical-item? utterance)
      (lexicon utterance obj)
      (comp-meaning utterance obj)))

; Helper function for meaning: Splits a two-word utterance into its parts and returns the conjoined sub-meanings
(define (comp-meaning utterance obj)
  (define subs (regexp-split utterance '_))
  (and (meaning (first subs) obj) (meaning (second subs) obj)))


; Helper function for meaning: Checks whether an utterance is a lexical item (i.e. an utterance atom)
(define (lexical-item? utterance)
  (> (list-index utterance-atoms utterance) -1))

; The standard literal listener that infers a distribution over objects, given an utterance
(define literal-listener 
  (mem (lambda (utterance)
         (enumeration-query
          (define obj (uniform-draw objs))

          obj

          (meaning utterance obj)))))

; A pragmatic speaker that infers a distribution over utterances, given an object he has in mind -- TODO: should you be conditioning on truth of utterance?
; Maybe somewhat non-standard: softmax on literal listener
(define pragmatic-speaker 
  (mem (lambda (obj)
         (enumeration-query        
          (define utterance 
          			(multinomial utterances 
                              (map (lambda (c) (exp (- c))) costs)))

          utterance

          (equal? obj
                  (apply multinomial 
                         (literal-listener utterance)))))))



(multiviz (barplot (power (pragmatic-speaker 'o1) speaker-opt) (stringify (first context)))
          (barplot (power (pragmatic-speaker 'o2) speaker-opt) (stringify (second context)))
          (barplot (power (pragmatic-speaker 'o3) speaker-opt) (stringify (third context))))

~~~
# A pragmatic speaker that can do basic composition and takes into account predicate noise. 

You can vary:

- color and size noise (fidelity)

- color and size cost

- speaker optimality

- context (lists of features)

This model is specifically to explore the effect of varying the number of distractors.

~~~
;; Model created August 28 2015 by jdegen

(define (power dist a) (list (first dist) 
                             (map (lambda (x) (pow x a)) (second dist))))

; Free parameters:
(define color_fidelity .999) ; 1 - noise probability (color -- higher means less noise)
(define size_fidelity .8) ; 1 - noise probability (size -- higher meanss less noise)
(define color_cost .1) ; lower means less costly
(define size_cost .1) ; lower means less costly
(define speaker-opt 15) ; standard speaker optimality parameter
; A context is a labeled list of lists, where sub-lists represent objects with a name (unique ID), a size feature and a color feature (currently only implemented for two features -- TODO: extend!)
(define context_foursame
  (list 'four_same
        (list (list 'o1 'big 'red)
              (list 'o2 'small 'red)
              (list 'o3 'small 'red)
              (list 'o4 'small 'red)
              (list 'o5 'small 'red)              
              )))

(define context_fourvaried 
  (list 'four_varied
        (list (list 'o1 'big 'red)
              (list 'o2 'small 'red)
              (list 'o3 'small 'yellow)
              (list 'o4 'small 'yellow)
              (list 'o5 'small 'yellow)              
              )))

(define context_threevaried 
  (list 'three_varied
        (list (list 'o1 'big 'red)
              (list 'o2 'small 'red)
              (list 'o3 'small 'yellow)
              (list 'o4 'small 'yellow)
              )))

(define context_twovaried 
  (list 'two_varied
        (list (list 'o1 'big 'red)
              (list 'o2 'small 'red)
              (list 'o3 'small 'yellow)
              )))

(define listener-opt 1) ; non-standard listener optimality parameter -- leave unchanged

; The standard literal listener that infers a distribution over objects, given an utterance
(define literal-listener 
  (mem (lambda (utterance color_fidelity size_fidelity objs utterances pruned-context utterance-atoms)

         ; The basic lexicon that encodes noisy semantics for words (ie correctly returns true/false with probability determined by fidelity parameter)
         (define (lexicon utterance obj color_fidelity size_fidelity)
           (if (> (list-index (check-features obj) utterance) -1)
               (flip (get-fidelity utterance color_fidelity size_fidelity))
               (flip (- 1 (get-fidelity utterance color_fidelity size_fidelity)))))


         ; Helper function for lexicon: Returns a list of an object's features
         (define (check-features obj)
           (list-ref pruned-context (list-index objs obj)))

         ; Helper function for lexicon: Retrieves the fidelity of a predicate (currently only implemented for color and size)
         ; Danger: assumes only the listed colors and will return size fidelity for ANY predicate that's not in the color list
         (define (get-fidelity utterance color_fidelity size_fidelity)
           (if (> (list-index 
                   '(red orange yellow green blue purple black brown pink white)
                   utterance) -1)
               color_fidelity
               size_fidelity)) 

         ; Tests whether an object is in the extension of an utterance -- directly applies lexicon if the utterance is a one-word utterance atom; builds meaning compositionally otherwise (only works for at most two-word utterances)
         (define (meaning utterance obj color_fidelity size_fidelity)
           (if (lexical-item? utterance)
               (lexicon utterance obj color_fidelity size_fidelity)
               (comp-meaning utterance obj color_fidelity size_fidelity)))

         ; Helper function for meaning: Splits a two-word utterance into its parts and returns the conjoined sub-meanings
         (define (comp-meaning utterance obj color_fidelity size_fidelity)
           (define subs (regexp-split utterance '_))
           (and 
            (meaning (first subs) obj color_fidelity size_fidelity) 
            (meaning (second subs) obj color_fidelity size_fidelity)))


         ; Helper function for meaning: Checks whether an utterance is a lexical item (i.e. an utterance atom)
         (define (lexical-item? utterance)
           (> (list-index utterance-atoms utterance) -1))  
         (enumeration-query
          (define obj (uniform-draw objs))

          obj

          (meaning utterance obj color_fidelity size_fidelity)))))

; A pragmatic speaker that infers a distribution over utterances, given an object he has in mind -- TODO: should you be conditioning on truth of utterance?
; Maybe somewhat non-standard: softmax on literal listener
(define pragmatic-speaker 
  (mem (lambda (ob color_fidelity size_fidelity color_cost size_cost context)

         ; Extracts a list of object IDs for the objects in the context
         (define objs
           (map (lambda (obj)
                  (first obj))
                context))

         ; Helper function for utterances: Strips the original context of object IDs
         (define pruned-context 
           (map (lambda (cont) 
                  (drop cont 1))
                context))		

         ; Helper function for utterances: concatenates an object's features to create a "_"-separated two-word utterance like "big_red"
         (define (gen-utt obj)
           (append obj (list (string-append (first obj) '_ (second obj)))))

         ; Generates the set of alternatives for the context by taking all feature conjunctions as utterances as well as each individual feature (currently only implemented for two-feature objects)
         (define utterances
           (union (flatten (map (lambda (obj) 
                                  (gen-utt obj)) 
                                pruned-context))))

         ; Helper function for utterance-atoms: Tests whether an utterance consists of just one word (multi-word utterances are separated by "_")
         (define (is-atom? utt) (= (length (regexp-split utt '_)) 1))

         ; Extracts all the utterance atoms (one-word utterances) from the set of contextually generated utterances
         (define utterance-atoms (first (partition is-atom? utterances)))  

         ; Generates a cost vector for the utterances, with a fixed cost for an extra word (free parameter defined at beginning of file). One-word utterances have cost 1.
         (define (costs color_cost size_cost)
           (map (lambda (utt)
                  (sum (map (lambda (word) 
                              (if (or (equal? word 'red) 
                                      (equal? word 'yellow)) 
                                  color_cost size_cost)) 
                            (regexp-split utt '_))))
                utterances))

         (enumeration-query        
          (define utterance (multinomial utterances 
                                         (map (lambda (c) (exp (- c))) (costs color_cost size_cost))))

          utterance

          (equal? ob
                  (apply multinomial
                         (power 
                          (literal-listener utterance color_fidelity size_fidelity objs utterances pruned-context utterance-atoms) 
                          listener-opt)))
          ))))


(define run-speaker 
  (lambda (obj color_fidelity size_fidelity color_cost size_cost speaker-opt context)
    (define results (power 
                     (pragmatic-speaker obj color_fidelity size_fidelity color_cost size_cost (second context)) 
                     speaker-opt))
    (define utts (first results))
    (define probs (second results))
    (define context_label (first context))
    (list (flatten (list 'object 'color_fidelity 'size_fidelity 'color_cost 'size_cost 'speaker-opt 'context utts)) (flatten (list  obj color_fidelity size_fidelity color_cost size_cost speaker-opt context_label (map (lambda (p) (/ p (sum probs))) probs))))
    ))	

(multiviz
 (barplot
  (power 
   (pragmatic-speaker 'o1 color_fidelity size_fidelity color_cost size_cost (second context_twovaried)) 
   speaker-opt) "Two distractors, only 1 same color as target")
 (barplot
  (power 
   (pragmatic-speaker 'o1 color_fidelity size_fidelity color_cost size_cost (second context_threevaried)) 
   speaker-opt) "Three distractors, only 1 same color as target") 
 (barplot
  (power 
   (pragmatic-speaker 'o1 color_fidelity size_fidelity color_cost size_cost (second context_fourvaried)) 
   speaker-opt) "Four distractors, only 1 same color as target") 
 (barplot
  (power 
   (pragmatic-speaker 'o1 color_fidelity size_fidelity color_cost size_cost (second context_foursame)) 
   speaker-opt) "Four distractors, all same color as target") 
 )

~~~

# A pragmatic speaker that can do basic composition and takes into account predicate noise. 

You can vary:

- color, size, and type noise (fidelity)

- color, size, and type cost

- speaker optimality

- context (lists of features)

This model is specifically to explore the effect of varying the number of dimensions along which objects differ while keeping color constant as a non-distinguishing feature (as in Koolen et al 2013, but excluding orientation for the time being, ie just adding one extra dimension).

~~~
;; Model created September 7 2015 by jdegen

(define (power dist a) (list (first dist) 
                             (map (lambda (x) (pow x a)) (second dist))))

; Free parameters:
(define color_fidelity .999) ; 1 - noise probability (color -- higher means less noise)
(define size_fidelity .8) ; 1 - noise probability (size -- higher meanss less noise)
(define type_fidelity .95) ; 1 - noise probability (size -- higher meanss less noise)
(define color_cost .1) ; lower means less costly
(define size_cost .1) ; lower means less costly
(define type_cost .1) ; lower means less costly
(define speaker-opt 18) ; standard speaker optimality parameter
; A context is a labeled list of lists, where sub-lists represent objects with a name (unique ID), a size feature and a color feature (currently only implemented for two features -- TODO: extend!)
(define context_sevensame
  (list 'seven_same
        (list (list 'o1 'big 'green 'fan)
              (list 'o2 'big 'green 'tv)
              (list 'o3 'big 'green 'desk)
              (list 'o4 'big 'green 'couch)
              (list 'o5 'big 'green 'desk)
              (list 'o6 'big 'green 'chair)
              (list 'o7 'big 'green 'couch)                                          
              (list 'o8 'big 'green 'chair)              
              )))

(define context_sevenvaried
  (list 'seven_varied
        (list (list 'o1 'small 'blue 'couch)
              (list 'o2 'big 'gray 'chair)
              (list 'o3 'small 'green 'tv)
              (list 'o4 'big 'green 'fan)
              (list 'o5 'big 'red 'fan)
              (list 'o6 'big 'red 'desk)
              (list 'o7 'big 'brown 'tv)                                          
              (list 'o8 'big 'gray 'chair)              
              ))) 

(define context_sevensame_othercolor
  (list 'seven_same_othercolor
        (list (list 'o1 'big 'red 'chair)
              (list 'o2 'big 'red 'fan)
              (list 'o3 'small 'red 'desk)
              (list 'o4 'small 'red 'tv)
              (list 'o5 'big 'red 'desk)
              (list 'o6 'small 'red 'chair)
              (list 'o7 'big 'red 'fan)                                          
              (list 'o8 'big 'red 'couch)              
              )))

(define context_sevenvaried_othercolor
  (list 'seven_varied_othercolor
        (list (list 'o1 'small 'brown 'chair)
              (list 'o2 'big 'blue 'desk)
              (list 'o3 'small 'gray 'fan)
              (list 'o4 'big 'brown 'chair)
              (list 'o5 'small 'red 'couch)
              (list 'o6 'big 'gray 'desk)
              (list 'o7 'small 'green 'tv)                                          
              (list 'o8 'big 'blue 'couch)              
              )))                            


(define listener-opt 1) ; non-standard listener optimality parameter -- leave unchanged

; The standard literal listener that infers a distribution over objects, given an utterance
(define literal-listener 
  (mem (lambda (utterance color_fidelity size_fidelity type_fidelity objs utterances pruned-context utterance-atoms)

         ; The basic lexicon that encodes noisy semantics for words (ie correctly returns true/false with probability determined by fidelity parameter)
         (define (lexicon utterance obj color_fidelity size_fidelity type_fidelity)
           (if (> (list-index (check-features obj) utterance) -1)
               (flip (get-fidelity utterance color_fidelity size_fidelity type_fidelity))
               (flip (- 1 (get-fidelity utterance color_fidelity size_fidelity type_fidelity)))))


         ; Helper function for lexicon: Returns a list of an object's features
         (define (check-features obj)
           (list-ref pruned-context (list-index objs obj)))

         ; Helper function for lexicon: Retrieves the fidelity of a predicate (currently only implemented for color and size)
         ; Danger: assumes only the listed colors and will return size fidelity for ANY predicate that's not in the color list
         (define (get-fidelity utterance color_fidelity size_fidelity type_fidelity)
           (if (> (list-index 
                   '(red orange yellow green blue purple black brown pink white)
                   utterance) -1)
               color_fidelity
               (if (> (list-index
                       '(big small)
                       utterance) -1)
                   size_fidelity
                   type_fidelity)
               )) 

         ; Tests whether an object is in the extension of an utterance -- directly applies lexicon if the utterance is a one-word utterance atom; builds meaning compositionally otherwise (only works for at most two-word utterances)
         (define (meaning utterance obj color_fidelity size_fidelity type_fidelity)
           (if (lexical-item? utterance)
               (lexicon utterance obj color_fidelity size_fidelity type_fidelity)
               (comp-meaning utterance obj color_fidelity size_fidelity type_fidelity)))

         ; Helper function for meaning: Splits a two-word utterance into its parts and returns the conjoined sub-meanings
         (define (comp-meaning utterance obj color_fidelity size_fidelity type_fidelity)
           (define subs (regexp-split utterance '_))
           (if (= (length subs) 2)
               (and 
                (meaning (first subs) obj color_fidelity size_fidelity type_fidelity) 
                (meaning (second subs) obj color_fidelity size_fidelity type_fidelity))
               (and
                (meaning (first subs) obj color_fidelity size_fidelity type_fidelity) 
                (meaning (second subs) obj color_fidelity size_fidelity type_fidelity)
                (meaning (third subs) obj color_fidelity size_fidelity type_fidelity))
               ))


         ; Helper function for meaning: Checks whether an utterance is a lexical item (i.e. an utterance atom)
         (define (lexical-item? utterance)
           (> (list-index utterance-atoms utterance) -1))  

         (enumeration-query
          (define obj (uniform-draw objs))

          obj

          (meaning utterance obj color_fidelity size_fidelity type_fidelity)))))

; A pragmatic speaker that infers a distribution over utterances, given an object he has in mind -- TODO: should you be conditioning on truth of utterance?
; Maybe somewhat non-standard: softmax on literal listener
(define pragmatic-speaker 
  (mem (lambda (ob color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost context)

         ; Extracts a list of object IDs for the objects in the context
         (define objs
           (map (lambda (obj)
                  (first obj))
                context))

         ; Helper function for utterances: Strips the original context of object IDs
         (define pruned-context 
           (map (lambda (cont) 
                  (drop cont 1))
                context))		

         ; Helper function for utterances: concatenates an object's features to create a "_"-separated two-word utterance like "big_red" and three-word utterance like "big_red_chair"
         (define (gen-utt obj)
           (list
            (third obj)
            (string-append (first obj) '_ (third obj))
            (string-append (second obj) '_ (third obj))			 
            (string-append (first obj) '_ (second obj) '_ (third obj))))
         ;           (append obj (list (string-append (first obj) '_ (second obj) '_ (third obj)))))			

         ; Generates the set of alternatives for the context by taking all feature conjunctions as utterances as well as each individual feature (currently only implemented for two-feature objects)
         (define utterances
           (union (flatten (map (lambda (obj) 
                                  (gen-utt obj)) 
                                pruned-context))))

         ; Helper function for utterance-atoms: Tests whether an utterance consists of just one word (multi-word utterances are separated by "_")
         ;        (define (is-atom? utt) (= (length (regexp-split utt '_)) 1))

         ; Extracts all the utterance atoms (one-word utterances) from the set of contextually generated utterances
         (define utterance-atoms (union (flatten pruned-context))) ; (first (partition is-atom? utterances)))  


         ; Generates a cost vector for the utterances, with a fixed cost for an extra word (free parameter defined at beginning of file). One-word utterances have cost 1.
         (define (costs color_cost size_cost type_cost)
           (map (lambda (utt)
                  (sum (map (lambda (word) 
                              (if (or (equal? word 'red) 
                                      (equal? word 'blue)
                                      (equal? word 'gray)
                                      (equal? word 'green)
                                      (equal? word 'brown)) 
                                  color_cost 
                                  (if (or (equal? word 'big)
                                          (equal? word 'small))
                                      size_cost
                                      type_cost))) 
                            (regexp-split utt '_))))
                utterances))

         (enumeration-query        
          (define utterance (multinomial utterances 
                                         (map (lambda (c) (exp (- c))) (costs color_cost size_cost type_cost))))

          utterance

          (equal? ob
                  (apply multinomial
                         (power 
                          (literal-listener utterance color_fidelity size_fidelity type_fidelity objs utterances pruned-context utterance-atoms) 
                          listener-opt)))
          ))))


(define run-speaker 
  (lambda (obj color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost speaker-opt context)
    (define results (power 
                     (pragmatic-speaker obj color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context)) 
                     speaker-opt))
    (define utts (first results))
    (define probs (second results))
    (define context_label (first context))
    (list (flatten (list 'object 'color_fidelity 'size_fidelity 'type_fidelity 'color_cost 'size_cost 'type_cost 'speaker-opt 'context utts)) (flatten (list  obj color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost speaker-opt context_label (map (lambda (p) (/ p (sum probs))) probs))))
    ))	

(multiviz
 (barplot
  (power 
   (pragmatic-speaker 'o1 color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context_sevensame)) 
   speaker-opt) "Seven distractors, low variation, type necessary")
 (barplot
  (power 
   (pragmatic-speaker 'o1 color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context_sevenvaried)) 
   speaker-opt) "Seven distractors, high variation, type necessary")
 (barplot
  (power 
   (pragmatic-speaker 'o1 color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context_sevensame_othercolor)) 
   speaker-opt) "Seven distractors, low variation, size and type necessary")
 (barplot
  (power 
   (pragmatic-speaker 'o1 color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context_sevenvaried_othercolor)) 
   speaker-opt) "Seven distractors, high variation, size and type necessary (plus other object of same color!)")     
 )

~~~

# A pragmatic speaker that can do basic composition and takes into account predicate noise. 

You can vary:

- color, size, type, and "one" noise (fidelity)

- color, size, and type (includes "one") cost

- speaker optimality

- context (lists of features)

This model is specifically to explore the effect of varying the number of dimensions along which objects differ while keeping color constant as a non-distinguishing feature (as in Koolen et al 2013, but excluding orientation for the time being, ie just adding one extra dimension) -- it also adds "one" as an utterance alternative with its own noise parameter, which should reflect that one is on average a very noisy 'type'.

~~~

;; Model created September 10 2015 by jdegen

(define (power dist a) (list (first dist) 
                             (map (lambda (x) (pow x a)) (second dist))))

; Free parameters:
(define color_fidelity .999) ; 1 - noise probability (color -- higher means less noise)
(define size_fidelity .8) ; 1 - noise probability (size -- higher means less noise)
(define type_fidelity .95) ; 1 - noise probability (type ie category -- higher means less noise)
(define one_fidelity .7) ; 1 - noise probability ('one' -- higher means less noise)
(define color_cost .1) ; lower means less costly
(define size_cost .1) ; lower means less costly
(define type_cost .1) ; lower means less costly
(define speaker-opt 15) ; standard speaker optimality parameter
; A context is a labeled list of lists, where sub-lists represent objects with a name (unique ID), a size feature and a color feature (currently only implemented for two features -- TODO: extend!)
(define context_sevensame
  (list 'seven_same
        (list (list 'o1 'big 'green 'fan 'one)
              (list 'o2 'big 'green 'tv 'one)
              (list 'o3 'big 'green 'desk 'one)
              (list 'o4 'big 'green 'couch 'one)
              (list 'o5 'big 'green 'desk 'one)
              (list 'o6 'big 'green 'chair 'one)
              (list 'o7 'big 'green 'couch 'one)                                          
              (list 'o8 'big 'green 'chair 'one)              
              )))

(define context_sevenvaried
  (list 'seven_varied
        (list (list 'o1 'small 'blue 'couch 'one)
              (list 'o2 'big 'gray 'chair 'one)
              (list 'o3 'small 'green 'tv 'one)
              (list 'o4 'big 'green 'fan 'one)
              (list 'o5 'big 'red 'fan 'one)
              (list 'o6 'big 'red 'desk 'one)
              (list 'o7 'big 'brown 'tv 'one)                                          
              (list 'o8 'big 'gray 'chair 'one)              
              ))) 

(define context_sevensame_othercolor
  (list 'seven_same_othercolor
        (list (list 'o1 'big 'red 'chair 'one)
              (list 'o2 'big 'red 'fan 'one)
              (list 'o3 'small 'red 'desk 'one)
              (list 'o4 'small 'red 'tv 'one)
              (list 'o5 'big 'red 'desk 'one)
              (list 'o6 'small 'red 'chair 'one)
              (list 'o7 'big 'red 'fan 'one)                                          
              (list 'o8 'big 'red 'couch 'one)              
              )))

(define context_sevenvaried_othercolor
  (list 'seven_varied_othercolor
        (list (list 'o1 'small 'brown 'chair 'one)
              (list 'o2 'big 'blue 'desk 'one)
              (list 'o3 'small 'gray 'fan 'one)
              (list 'o4 'big 'brown 'chair 'one)
              (list 'o5 'small 'red 'couch 'one)
              (list 'o6 'big 'gray 'desk 'one)
              (list 'o7 'small 'green 'tv 'one)                                          
              (list 'o8 'big 'blue 'couch 'one)              
              )))                            


(define listener-opt 1) ; non-standard listener optimality parameter -- leave unchanged

; The standard literal listener that infers a distribution over objects, given an utterance
(define literal-listener 
  (mem (lambda (utterance color_fidelity size_fidelity type_fidelity one_fidelity objs utterances pruned-context utterance-atoms)

         ; The basic lexicon that encodes noisy semantics for words (ie correctly returns true/false with probability determined by fidelity parameter)
         (define (lexicon utterance obj color_fidelity size_fidelity type_fidelity one_fidelity)
           (if (> (list-index (check-features obj) utterance) -1)
               (flip (get-fidelity utterance color_fidelity size_fidelity type_fidelity one_fidelity))
               (flip (- 1 (get-fidelity utterance color_fidelity size_fidelity type_fidelity one_fidelity)))))


         ; Helper function for lexicon: Returns a list of an object's features
         (define (check-features obj)
           (list-ref pruned-context (list-index objs obj)))

         ; Helper function for lexicon: Retrieves the fidelity of a predicate (currently only implemented for color and size)
         ; Danger: assumes only the listed colors and will return size fidelity for ANY predicate that's not in the color list
         (define (get-fidelity utterance color_fidelity size_fidelity type_fidelity one_fidelity)
           (if (> (list-index 
                   '(red orange yellow green blue purple black brown pink white)
                   utterance) -1)
               color_fidelity
               (if (> (list-index
                       '(big small)
                       utterance) -1)
                   size_fidelity
                   (if (equal? utterance 'one)
                   one_fidelity
                   type_fidelity)
               ))) 

         ; Tests whether an object is in the extension of an utterance -- directly applies lexicon if the utterance is a one-word utterance atom; builds meaning compositionally otherwise (only works for at most two-word utterances)
         (define (meaning utterance obj color_fidelity size_fidelity type_fidelity one_fidelity)
           (if (lexical-item? utterance)
               (lexicon utterance obj color_fidelity size_fidelity type_fidelity one_fidelity)
               (comp-meaning utterance obj color_fidelity size_fidelity type_fidelity one_fidelity)))

         ; Helper function for meaning: Splits a two-word utterance into its parts and returns the conjoined sub-meanings
         (define (comp-meaning utterance obj color_fidelity size_fidelity type_fidelity one_fidelity)
           (define subs (regexp-split utterance '_))
           (if (= (length subs) 2)
               (and 
                (meaning (first subs) obj color_fidelity size_fidelity type_fidelity one_fidelity) 
                (meaning (second subs) obj color_fidelity size_fidelity type_fidelity one_fidelity))
               (and
                (meaning (first subs) obj color_fidelity size_fidelity type_fidelity one_fidelity) 
                (meaning (second subs) obj color_fidelity size_fidelity type_fidelity one_fidelity)
                (meaning (third subs) obj color_fidelity size_fidelity type_fidelity one_fidelity))
               ))


         ; Helper function for meaning: Checks whether an utterance is a lexical item (i.e. an utterance atom)
         (define (lexical-item? utterance)
           (> (list-index utterance-atoms utterance) -1))  

         (enumeration-query
          (define obj (uniform-draw objs))

          obj

          (meaning utterance obj color_fidelity size_fidelity type_fidelity one_fidelity)))))

; A pragmatic speaker that infers a distribution over utterances, given an object he has in mind -- TODO: should you be conditioning on truth of utterance?
; Maybe somewhat non-standard: softmax on literal listener
(define pragmatic-speaker 
  (mem (lambda (ob color_fidelity size_fidelity type_fidelity one_fidelity color_cost size_cost type_cost context)

         ; Extracts a list of object IDs for the objects in the context
         (define objs
           (map (lambda (obj)
                  (first obj))
                context))

         ; Helper function for utterances: Strips the original context of object IDs
         (define pruned-context 
           (map (lambda (cont) 
                  (drop cont 1))
                context))		

         ; Helper function for utterances: concatenates an object's features to create a "_"-separated two-word utterance like "big_red" and three-word utterance like "big_red_chair"
         (define (gen-utt obj)
           (list
            (third obj)
            (string-append (first obj) '_ (third obj))
            (string-append (second obj) '_ (third obj))			 
            (string-append (first obj) '_ (second obj) '_ (third obj))
            (string-append (first obj) '_ (fourth obj))
            (string-append (second obj) '_ (fourth obj))			 
            (string-append (first obj) '_ (second obj) '_ (fourth obj))            
            ))
         ;           (append obj (list (string-append (first obj) '_ (second obj) '_ (third obj)))))			

         ; Generates the set of alternatives for the context by taking all feature conjunctions as utterances as well as each individual feature (currently only implemented for two-feature objects)
         (define utterances
           (union (flatten (map (lambda (obj) 
                                  (gen-utt obj)) 
                                pruned-context))))

         ; Helper function for utterance-atoms: Tests whether an utterance consists of just one word (multi-word utterances are separated by "_")
         ;        (define (is-atom? utt) (= (length (regexp-split utt '_)) 1))

         ; Extracts all the utterance atoms (one-word utterances) from the set of contextually generated utterances
         (define utterance-atoms (union (flatten pruned-context))) ; (first (partition is-atom? utterances)))  


         ; Generates a cost vector for the utterances, with a fixed cost for an extra word (free parameter defined at beginning of file). One-word utterances have cost 1.
         (define (costs color_cost size_cost type_cost)
           (map (lambda (utt)
                  (sum (map (lambda (word) 
                              (if (or (equal? word 'red) 
                                      (equal? word 'blue)
                                      (equal? word 'gray)
                                      (equal? word 'green)
                                      (equal? word 'brown)) 
                                  color_cost 
                                  (if (or (equal? word 'big)
                                          (equal? word 'small))
                                      size_cost
                                      type_cost))) 
                            (regexp-split utt '_))))
                utterances))

         (enumeration-query        
          (define utterance (multinomial utterances 
                                         (map (lambda (c) (exp (- c))) (costs color_cost size_cost type_cost))))

          utterance

          (equal? ob
                  (apply multinomial
                         (power 
                          (literal-listener utterance color_fidelity size_fidelity type_fidelity one_fidelity objs utterances pruned-context utterance-atoms) 
                          listener-opt)))
          ))))


(define run-speaker 
  (lambda (obj color_fidelity size_fidelity type_fidelity one_fidelity color_cost size_cost type_cost speaker-opt context)
    (define results (power 
                     (pragmatic-speaker obj color_fidelity size_fidelity type_fidelity one_fidelity color_cost size_cost type_cost (second context)) 
                     speaker-opt))
    (define utts (first results))
    (define probs (second results))
    (define context_label (first context))
    (list (flatten (list 'object 'color_fidelity 'size_fidelity 'type_fidelity 'one_fidelity 'color_cost 'size_cost 'type_cost 'speaker-opt 'context utts)) (flatten (list  obj color_fidelity size_fidelity type_fidelity one_fidelity color_cost size_cost type_cost speaker-opt context_label (map (lambda (p) (/ p (sum probs))) probs))))
    ))	

(multiviz
 (barplot
  (power 
   (pragmatic-speaker 'o1 color_fidelity size_fidelity type_fidelity one_fidelity color_cost size_cost type_cost (second context_sevensame)) 
   speaker-opt) "Seven distractors, low variation, type necessary")
 (barplot
  (power 
   (pragmatic-speaker 'o1 color_fidelity size_fidelity type_fidelity one_fidelity color_cost size_cost type_cost (second context_sevenvaried)) 
   speaker-opt) "Seven distractors, high variation, type necessary")
 (barplot
  (power 
   (pragmatic-speaker 'o1 color_fidelity size_fidelity type_fidelity one_fidelity color_cost size_cost type_cost (second context_sevensame_othercolor)) 
   speaker-opt) "Seven distractors, low variation, size and type necessary")
 (barplot
  (power 
   (pragmatic-speaker 'o1 color_fidelity size_fidelity type_fidelity one_fidelity color_cost size_cost type_cost (second context_sevenvaried_othercolor)) 
   speaker-opt) "Seven distractors, high variation, size and type necessary (plus other object of same color!)")     
 )

~~~

# A pragmatic speaker that can do basic composition and takes into account predicate noise and color predictability via noun-specific color cost. This is brutal and ugly but works. 

You can vary:

- color, size, and type noise (fidelity, though size doesn't do anything)

- color, size, and type cost (though size doesn't do anything)

- speaker optimality

- context (lists of features)

This model is specifically to explore the effect of varying color predictability.

~~~


;; Model created September 15 2015 by jdegen
;; Model modified October 20 2015 by jdegen

(define (power dist a) (list (first dist) 
                             (map (lambda (x) (pow x a)) (second dist))))

; Free parameters:
(define color_fidelity .999) ; 1 - noise probability (color -- higher means less noise)
(define size_fidelity .8) ; 1 - noise probability (size -- higher meanss less noise)
(define type_fidelity .95) ; 1 - noise probability (size -- higher meanss less noise)
(define color_cost .1) ; lower means less costly
(define size_cost .1) ; lower means less costly
(define type_cost .1) ; lower means less costly
(define speaker-opt 18) ; standard speaker optimality parameter
; A context is a labeled list of lists, where sub-lists represent objects with a name (unique ID), a size feature and a color feature (currently only implemented for two features -- TODO: extend!)
(define context_typical
  (list 'typical
        (list (list 'o1 'red 'tomato)
              (list 'o2 'red 'pumpkin)
              (list 'o3 'green 'lemon)
              (list 'o4 'green 'cheese)
              (list 'o5 'yellow 'corn)
              (list 'o6 'yellow 'broccoli)
              )))

(define context_intermediate
  (list 'intermediate
        (list (list 'o1 'yellow 'apple)
              (list 'o2 'orange 'carrot)
              (list 'o3 'red 'corn)
              (list 'o4 'red 'banana)
              (list 'o5 'yellow 'pineapple)
              (list 'o6 'orange 'naranja)
              ))) 

(define context_atypical
  (list 'atypical
        (list (list 'o1 'blue 'pepper)
              (list 'o2 'green 'apple)
              (list 'o3 'blue 'lemon)
              (list 'o4 'yellow 'pineapple)
              (list 'o5 'yellow 'banana)
              (list 'o6 'green 'lettuce)
              )))


(define (noun_specific_adj_costs utt)
  (case utt
        (('red_tomato) .99)
        (('red_pumpkin) .01)
        (('green_lemon) .5)
        (('green_cheese) .01)
        (('yellow_corn) .99)
        (('yellow_broccoli) .5)
        (('yellow_apple) .5)
        (('orange_carrot) .99)
        (('red_corn) .01)
        (('red_banana) .01)
        (('yellow_pineapple) .5)
        (('orange_naranja) .99)
        (('blue_pepper) .01)
        (('green_apple) .5)
        (('blue_lemon) .01)
        (('yellow_pineapple) .5)
        (('yellow_banana) .99)
        (('green_lettuce) .99)
        ))              

(define listener-opt 1) ; non-standard listener optimality parameter -- leave unchanged

; The standard literal listener that infers a distribution over objects, given an utterance
(define literal-listener 
  (mem (lambda (utterance color_fidelity size_fidelity type_fidelity objs utterances pruned-context utterance-atoms)

         ; The basic lexicon that encodes noisy semantics for words (ie correctly returns true/false with probability determined by fidelity parameter)
         (define (lexicon utterance obj color_fidelity size_fidelity type_fidelity)
           (if (> (list-index (check-features obj) utterance) -1)
               (flip (get-fidelity utterance color_fidelity size_fidelity type_fidelity))
               (flip (- 1 (get-fidelity utterance color_fidelity size_fidelity type_fidelity)))))


         ; Helper function for lexicon: Returns a list of an object's features
         (define (check-features obj)
           (list-ref pruned-context (list-index objs obj)))

         ; Helper function for lexicon: Retrieves the fidelity of a predicate (currently only implemented for color and size)
         ; Danger: assumes only the listed colors and will return size fidelity for ANY predicate that's not in the color list
         (define (get-fidelity utterance color_fidelity size_fidelity type_fidelity)
           (if (> (list-index 
                   '(red orange yellow green blue purple black brown pink white)
                   utterance) -1)
               color_fidelity
               (if (> (list-index
                       '(big small)
                       utterance) -1)
                   size_fidelity
                   type_fidelity)
               )) 

         ; Tests whether an object is in the extension of an utterance -- directly applies lexicon if the utterance is a one-word utterance atom; builds meaning compositionally otherwise (only works for at most two-word utterances)
         (define (meaning utterance obj color_fidelity size_fidelity type_fidelity)
           (if (lexical-item? utterance)
               (lexicon utterance obj color_fidelity size_fidelity type_fidelity)
               (comp-meaning utterance obj color_fidelity size_fidelity type_fidelity)))

         ; Helper function for meaning: Splits a two-word utterance into its parts and returns the conjoined sub-meanings
         (define (comp-meaning utterance obj color_fidelity size_fidelity type_fidelity)
           (define subs (regexp-split utterance '_))
           (if (= (length subs) 2)
               (and 
                (meaning (first subs) obj color_fidelity size_fidelity type_fidelity) 
                (meaning (second subs) obj color_fidelity size_fidelity type_fidelity))
               (and
                (meaning (first subs) obj color_fidelity size_fidelity type_fidelity) 
                (meaning (second subs) obj color_fidelity size_fidelity type_fidelity)
                (meaning (third subs) obj color_fidelity size_fidelity type_fidelity))
               ))


         ; Helper function for meaning: Checks whether an utterance is a lexical item (i.e. an utterance atom)
         (define (lexical-item? utterance)
           (> (list-index utterance-atoms utterance) -1))  

         (enumeration-query
          (define obj (uniform-draw objs))

          obj

          (meaning utterance obj color_fidelity size_fidelity type_fidelity)))))

; A pragmatic speaker that infers a distribution over utterances, given an object he has in mind -- TODO: should you be conditioning on truth of utterance?
; Maybe somewhat non-standard: softmax on literal listener
(define pragmatic-speaker 
  (mem (lambda (ob color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost context)

         ; Extracts a list of object IDs for the objects in the context
         (define objs
           (map (lambda (obj)
                  (first obj))
                context))

         ; Helper function for utterances: Strips the original context of object IDs
         (define pruned-context 
           (map (lambda (cont) 
                  (drop cont 1))
                context))		

         ; Helper function for utterances: concatenates an object's features to create a "_"-separated two-word utterance like "big_red" and three-word utterance like "big_red_chair"
         (define (gen-utt obj)
           (list
            (second obj)
            (string-append (first obj) '_ (second obj))
            ))
         ;           (append obj (list (string-append (first obj) '_ (second obj) '_ (third obj)))))			

         ; Generates the set of alternatives for the context by taking all feature conjunctions as utterances as well as each individual feature (currently only implemented for two-feature objects)
         (define utterances
           (union (flatten (map (lambda (obj) 
                                  (gen-utt obj)) 
                                pruned-context))))

         ; Helper function for utterance-atoms: Tests whether an utterance consists of just one word (multi-word utterances are separated by "_")
         ;        (define (is-atom? utt) (= (length (regexp-split utt '_)) 1))

         ; Extracts all the utterance atoms (one-word utterances) from the set of contextually generated utterances
         (define utterance-atoms (union (flatten pruned-context))) ; (first (partition is-atom? utterances)))  


         ; Generates a cost vector for the utterances, with a fixed cost for an extra word (free parameter defined at beginning of file). One-word utterances have cost 1.
         (define (costs color_cost size_cost type_cost)
           (map (lambda (utt)
                  ;                  (sum (map (lambda (word) 
                  ;                              (if (or (equal? word 'red) 
                  ;                                      (equal? word 'orange)
                  ;                                      (equal? word 'yellow)                                                                    
                  ;                                      (equal? word 'green)
                  ;                                      (equal? word 'blue)
                  ;                                      (equal? word 'purple)                                      
                  ;                                      (equal? word 'pink)                                      
                  ;                                      (equal? word 'gray)
                  ;                                      (equal? word 'black)
                  ;                                      (equal? word 'white)                                                                            
                  ;                                      (equal? word 'brown)) 
                  (if (or (equal? utt 'red) 
                          (equal? utt 'orange)
                          (equal? utt 'yellow)                                                                    
                          (equal? utt 'green)
                          (equal? utt 'blue)
                          (equal? utt 'purple)                                      
                          (equal? utt 'pink)                                      
                          (equal? utt 'gray)
                          (equal? utt 'black)
                          (equal? utt 'white)                                                                            
                          (equal? utt 'brown)) 
                      color_cost 
                      ;                                  (if (or (equal? word 'big)
                      ;                                          (equal? word 'small))
                      ;                                      size_cost
                      (if (or (equal? utt 'tomato)
                              (equal? utt 'pumpkin)
                              (equal? utt 'lemon)
                              (equal? utt 'cheese)
                              (equal? utt 'corn)
                              (equal? utt 'broccoli)
                              (equal? utt 'apple)
                              (equal? utt 'carrot)
                              (equal? utt 'corn)
                              (equal? utt 'banana)
                              (equal? utt 'pineapple)
                              (equal? utt 'naranja)
                              (equal? utt 'pepper)
                              (equal? utt 'lettuce))                                          
                          ;)
                          type_cost
                          (noun_specific_adj_costs utt)
                          ))) 
                ;                            (regexp-split utt '_))))
                utterances))

         (enumeration-query        
          (define utterance (multinomial utterances 
                                         (map (lambda (c) (exp (- c))) (costs color_cost size_cost type_cost))))

          utterance

          (equal? ob
                  (apply multinomial
                         (power 
                          (literal-listener utterance color_fidelity size_fidelity type_fidelity objs utterances pruned-context utterance-atoms) 
                          listener-opt)))
          ))))


(define run-speaker 
  (lambda (obj color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost speaker-opt context)
    (define results (power 
                     (pragmatic-speaker obj color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context)) 
                     speaker-opt))
    (define utts (first results))
    (define probs (second results))
    (define context_label (first context))
    (list (flatten (list 'object 'color_fidelity 'size_fidelity 'type_fidelity 'color_cost 'size_cost 'type_cost 'speaker-opt 'context utts)) (flatten (list  obj color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost speaker-opt context_label (map (lambda (p) (/ p (sum probs))) probs))))
    ))	

(multiviz
 (barplot
  (power 
   (pragmatic-speaker 'o1 color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context_typical)) 
   speaker-opt) "typical color")
 (barplot
  (power 
   (pragmatic-speaker 'o1 color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context_intermediate)) 
   speaker-opt) "intermediate color typicality")
 (barplot
  (power 
   (pragmatic-speaker 'o1 color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context_atypical)) 
   speaker-opt) "atypical color")
 )

~~~

# A pragmatic speaker that can do basic composition and takes into account predicate noise and color predictability.

You can vary:

- color, size, and type noise/fidelity (tbut these don't do anything in this model)

- color, size, and type cost (though size cost doesn't do anything)

- speaker optimality

- context (lists of features)

- objects: 2-feature lists (color type) -- allows for manipulating color typicality. Currently three different objects are defined: yellow banana (typical), green banana (intermediate), red banana (atypical)

This model is specifically to explore the effect of varying color predictability.

~~~
; Created October 20 2015 by jdegen

(define (power dist a) (list (first dist) 
                             (map (lambda (x) (pow x a)) (second dist))))

; Free parameters:
(define color_fidelity .999) ; 1 - noise probability (color -- higher means less noise -- unused in this model)
(define size_fidelity .8) ; 1 - noise probability (size -- higher meanss less noise -- unused in this model) 
(define type_fidelity .95) ; 1 - noise probability (size -- higher meanss less noise -- unused in this model)
(define color_cost .1) ; lower means less costly
(define size_cost .1) ; lower means less costly
(define type_cost .1) ; lower means less costly
(define speaker-opt 18) ; standard speaker optimality parameter
; A context is a labeled list of lists, where sub-lists represent objects with a name (unique ID), a size feature and a color feature (currently only implemented for two features -- TODO: actually integrate!)
;(define context_typical
;  (list 'typical
;        (list (list 'o1 'red 'tomato)
;              (list 'o2 'red 'pumpkin)
;              (list 'o3 'green 'lemon)
;              (list 'o4 'green 'cheese)
;              (list 'o5 'yellow 'corn)
;              (list 'o6 'yellow 'broccoli)
;              )))
;
;(define context_intermediate
;  (list 'intermediate
;        (list (list 'o1 'yellow 'apple)
;              (list 'o2 'orange 'carrot)
;              (list 'o3 'red 'corn)
;              (list 'o4 'red 'banana)
;              (list 'o5 'yellow 'pineapple)
;              (list 'o6 'orange 'naranja)
;              ))) 
;
;(define context_atypical
;  (list 'atypical
;        (list (list 'o1 'blue 'pepper)
;              (list 'o2 'green 'apple)
;              (list 'o3 'blue 'lemon)
;              (list 'o4 'yellow 'pineapple)
;              (list 'o5 'yellow 'banana)
;              (list 'o6 'green 'lettuce)
;              )))

(define object_typical
  '(yellow banana)
  )

(define object_intermediate
  '(green banana)
  )

(define object_atypical
  '(red banana)
  )

(define listener-opt 1) ; non-standard listener optimality parameter -- leave unchanged

; The literal listener infers a distribution over features of an object, given an utterance -- retrieves the color prior for a simple type utterance and perfectly retrieves that true feature combination if color is mentioned
(define literal-listener 
  (mem (lambda (utterance color_fidelity size_fidelity type_fidelity)       

         (enumeration-query       
          (define sub-utts (regexp-split utterance '_))     

          (define banana-prior
            (multinomial '(red orange yellow green blue)
                         (list .0001 .0001 2 .1 .0001)))

          (define tomato-prior
            (multinomial '(red orange yellow green blue)
                         (list .8 .0001 .1 .1 .0001)))

          (define color-prior
            (lambda (obj-type)
              (case obj-type
                    (('banana) banana-prior)
                    (('tomato) tomato-prior)                 
                    )))

          (define (meaning sub-utts obj-color obj-type)
            (if (= (length sub-utts) 2)
                (equal? (first sub-utts) obj-color)
                true))          

          (define obj-type 
            (if (= (length sub-utts) 2)         
                (second sub-utts)
                (first sub-utts)))

          (define obj-color (color-prior obj-type))

          (list obj-color obj-type)

          (meaning sub-utts obj-color obj-type)))))

; A pragmatic speaker that infers a distribution over utterances, given an object he has in mind 
; Maybe somewhat non-standard: softmax on literal listener (but not used because listener-opt set to 1)
(define pragmatic-speaker 
  (mem (lambda (obj color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost)

         ; Helper function for utterances: concatenates an object's features to create a "_"-separated two-word utterance like "big_red" and three-word utterance like "big_red_chair"
         (define (gen-utt obj)
           (list
            (second obj)
            (string-append (first obj) '_ (second obj))
            ))	

         ; Generates the set of alternatives for the object by taking all feature conjunctions as utterances as well as each individual feature (currently only implemented for two-feature objects)
         (define utterances
           (gen-utt obj))

         ; Generates a cost vector for the utterances, with a fixed cost for an extra word (free parameter defined at beginning of file). 
         (define (costs color_cost size_cost type_cost)
           (map (lambda (utt)
                  (sum (map (lambda (word) 
                              (if (or (equal? word 'red) 
                                      (equal? word 'orange)
                                      (equal? word 'yellow)                                                                    
                                      (equal? word 'green)
                                      (equal? word 'blue)
                                      (equal? word 'purple)                                      
                                      (equal? word 'pink)                                      
                                      (equal? word 'gray)
                                      (equal? word 'black)
                                      (equal? word 'white)                                                                            
                                      (equal? word 'brown)) 
                                  color_cost 
                                  (if (or (equal? word 'big)
                                          (equal? word 'small))
                                      size_cost
                                      type_cost
                                      ))) 
                            (regexp-split utt '_))))
                utterances))

         (enumeration-query        
          (define utterance (multinomial utterances 
                                         (map (lambda (c) (exp (- c))) (costs color_cost size_cost type_cost))))

          utterance

          (equal? obj
                  (apply multinomial
                         (power 
                          (literal-listener utterance color_fidelity size_fidelity type_fidelity) 
                          listener-opt)))
          ))))


;(define run-speaker 
;  (lambda (obj color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost speaker-opt context)
;    (define results (power 
;                     (pragmatic-speaker obj color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context)) 
;                     speaker-opt))
;    (define utts (first results))
;    (define probs (second results))
;    (define context_label (first context))
;    (list (flatten (list 'object 'color_fidelity 'size_fidelity 'type_fidelity 'color_cost 'size_cost 'type_cost 'speaker-opt 'context utts)) (flatten (list  obj color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost speaker-opt context_label (map (lambda (p) (/ p (sum probs))) probs))))
;    ))	


 (multiviz
  (barplot
   (power 
    (pragmatic-speaker object_typical color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost) 
    speaker-opt) "typical color")
  (barplot
   (power 
    (pragmatic-speaker object_intermediate color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost) 
    speaker-opt) "intermediate color typicality")
  (barplot
   (power 
    (pragmatic-speaker object_atypical color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost) 
    speaker-opt) "atypical color")
  )

~~~

# Color predictability effect.

The pragmatic speaker tries to get the literal listener to choose the intended referent (one of six objects with a color and type feature). The literal listener samples objects depending on the object's color (either inferred from prior or explicitly given in utterance). For unpredictable colors, the probability of miscommunication is high, so the speaker is likely to mention color. But, confusing: the speaker is happy to mention color even when it's very predictable (and even when you strike the other object of the same color from the context). Why??

~~~
; Created October 31 2015 by jdegen

(define (power dist a) (list (first dist) 
                             (map (lambda (x) (pow x a)) (second dist))))

(define listener_n 1000) ;mh-query iterations in literal listener
(define speaker_n 1000) ;mh-query iterations in pragmatic speaker

; Free word fidelity and speaker optimality parameters (none of which are used in this model):
(define color_fidelity .999) ; 1 - noise probability (color -- higher means less noise -- unused in this model)
(define size_fidelity .8) ; 1 - noise probability (size -- higher meanss less noise -- unused in this model) 
(define type_fidelity .95) ; 1 - noise probability (size -- higher meanss less noise -- unused in this model)
(define speaker-opt 18) ; standard speaker optimality parameter

; Free word cost parameters (equal costs):
(define color_cost .1) ; lower means less costly
(define size_cost .1) ; lower means less costly
(define type_cost .1) ; lower means less costly

; A context is a labeled list of lists, where sub-lists represent objects with a color and a type feature. The following three contexts are taken directly from Westerbeek et al. 2014. The first object in each context represents the target object for that context, in descending order of color typicality (red tomato, yellow apple, blue pepper)
(define context_typical
  (list 'typical
        (list (list 'red 'tomato)
              (list 'red 'pumpkin)
              (list 'green 'lemon)
              (list 'green 'cheese)
              (list 'yellow 'corn)
              (list 'yellow 'broccoli)
              )))

(define context_intermediate
  (list 'intermediate
        (list (list 'yellow 'apple)
              (list 'orange 'carrot)
              (list 'red 'corn)
              (list 'red 'banana)
              (list 'yellow 'pineapple)
              (list 'orange 'naranja)
              ))) 

(define context_atypical
  (list 'atypical
        (list (list 'blue 'pepper)
              (list 'green 'apple)
              (list 'blue 'lemon)
              (list 'yellow 'pineapple)
              (list 'yellow 'banana)
              (list 'green 'lettuce)
              )))


; The literal listener infers a distribution over objects, given an utterance -- retrieves the color prior for a simple type utterance and perfectly retrieves that true feature combination if color is mentioned
(define literal-listener
  (mem   (lambda (utterance color_fidelity size_fidelity type_fidelity objects)       
           ;         (enumeration-query  

           (mh-query listener_n 5

                     ; split up the utterance into its sub-strings				           			
                     (define sub-utts (regexp-split utterance '_))     

                     ; made-up color priors for the objects in the contexts

                     (define carrot-prior
                       (multinomial '(red orange yellow green blue)
                                    (list .0001 .9 .097 .0001 .0001)))

                     (define pineapple-prior
                       (multinomial '(red orange yellow green blue)
                                    (list .0001 .0001 .9 .097 .0001)))

                     (define naranja-prior
                       (multinomial '(red orange yellow green blue)
                                    (list .0001 .9 .0001 .097 .0001)))

                     (define tomato-prior
                       (multinomial '(red orange yellow green blue)
                                    (list .9 .0001 .0001 .097 .0001)))
                                    
                     (define apple-prior
                       (multinomial '(red orange yellow green blue)
                                    (list .7 .0001 .2 .098 .0001)))
                                                                        
                     (define pepper-prior
                       (multinomial '(red orange yellow green blue)
                                    (list .6 .099 .15 .15 .0001))) 

                     (define lettuce-prior
                       (multinomial '(red orange yellow green blue)
                                    (list .0001 .0001 .0001 .996 .0001))) 

                     (define banana-prior
                       (multinomial '(red orange yellow green blue)
                                    (list .0001 .0001 .9 .097 .0001)))


                     (define pumpkin-prior
                       (multinomial '(red orange yellow green blue)
                                    (list .0001 .9 .0001 .097 .0001)))

                     (define lemon-prior
                       (multinomial '(red orange yellow green blue)
                                    (list .0001 .0001 .9 .097 .0001)))

                     (define cheese-prior
                       (multinomial '(red orange yellow green blue)
                                    (list .0001 .9 .0001 .0001 .097)))

                     (define broccoli-prior
                       (multinomial '(red orange yellow green blue)
                                    (list .0001 .0001 .097 .9 .0001))) 

                     (define corn-prior
                       (multinomial '(red orange yellow green blue)
                                    (list .0001 .0001 .9 .097 .0001)))          

                     ; helper function to get the right color prior depending on object type
                     (define color-prior
                       (lambda (obj-type)
                         (case obj-type
                               (('apple) apple-prior)
                               (('carrot) carrot-prior) 
                               (('pineapple) pineapple-prior)
                               (('naranja) naranja-prior)
                               (('pepper) pepper-prior) 
                               (('lettuce) lettuce-prior)                                
                               (('banana) banana-prior)
                               (('tomato) tomato-prior) 
                               (('pumpkin) pumpkin-prior)
                               (('lemon) lemon-prior)
                               (('cheese) cheese-prior) 
                               (('broccoli) broccoli-prior)
                               (('corn) corn-prior)                    
                               )))

                     ; meaning function that checks the literal semantics of the utterance -- currently UNUSED because the literal listener needs to be able to get things "wrong"
                     (define (meaning sub-utts obj-color obj-type)
                       (if (= (length sub-utts) 2)
                           (and (equal? (first sub-utts) obj-color) (equal? (second sub-utts) obj-type))
                           (equal? (first sub-utts) obj-type)))      

                     ; THE MEAT.
                     ; 1. Get the object's color -- if the utterance is a color_noun utterance, it's just the first word; otherwise, take a sample from the object's color prior
                     (define obj-color 
                       (if (> (list-index 
                               '(red orange yellow green blue)
                               (first sub-utts)) -1)
                           (first sub-utts)
                           (color-prior (first sub-utts))))


                     ; 2. Sample an object depending on the sampled color.

                     ; Helper function: check whether an object has a color and return true if it does, false otherwise
                     (define (check-color obj color)
                       (if (equal? (first obj) color)
                           1
                           0.00000001))

                     ; Sample an object from only that set of objects in context that has the sampled color.
                     (define sample-compatible-object
                       (multinomial objects
                                    (map (lambda (o) 
                                           (check-color o obj-color)) 
                                         objects)
                                    ))

                     (define obj sample-compatible-object)

                     obj

                     ;          (meaning sub-utts obj-color obj-type)
                     true))))


; A pragmatic speaker that infers a distribution over utterances, given an object from a context (plus knowledge of word fidelities and costs, but those are all either not used or set to be equal to each other here)

(define pragmatic-speaker 
  (mem (lambda (obj color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost context)

         ; Helper function for utterances: concatenates an object's features to create a "_"-separated two-word utterance like "red_apple"
         (define (gen-utt obj)
           (list
            (second obj)
            (string-append (first obj) '_ (second obj))
            ))	

         ; Generates the set of alternatives for the object by taking all feature conjunctions as utterances as well as each individual feature (currently only implemented for two-feature objects)
         (define utterances
           (gen-utt obj))

         ; Generates a cost vector for the utterances, with a fixed cost for an extra word (free parameter defined at beginning of file). 
         (define (costs color_cost size_cost type_cost)
           (map (lambda (utt)
                  (sum (map (lambda (word) 
                              (if (or (equal? word 'red) 
                                      (equal? word 'orange)
                                      (equal? word 'yellow)                                                                    
                                      (equal? word 'green)
                                      (equal? word 'blue))
                                  color_cost 
                                  (if (or (equal? word 'big)
                                          (equal? word 'small))
                                      size_cost
                                      type_cost
                                      ))) 
                            (regexp-split utt '_))))
                utterances))

         (mh-query speaker_n 5
                   ;         (enumeration-query        
                   (define utterance (multinomial utterances 
                                                  (map (lambda (c) (exp (- c))) (costs color_cost size_cost type_cost))))

                   utterance

                   (equal? obj
                           (uniform-draw 
                            (literal-listener utterance color_fidelity size_fidelity type_fidelity context))))
         )))


;(define run-speaker 
;  (lambda (obj color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost speaker-opt context)
;    (define results (power 
;                     (pragmatic-speaker obj color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context)) 
;                     speaker-opt))
;    (define utts (first results))
;    (define probs (second results))
;    (define context_label (first context))
;    (list (flatten (list 'object 'color_fidelity 'size_fidelity 'type_fidelity 'color_cost 'size_cost 'type_cost 'speaker-opt 'context utts)) (flatten (list  obj color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost speaker-opt context_label (map (lambda (p) (/ p (sum probs))) probs))))
;    ))	


(multiviz
 (hist
  ;;  (power 
  (pragmatic-speaker 
   (first (second context_typical)) color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context_typical)) 
  (stringify (second context_typical))) 
 ;;   speaker-opt) "typical color")
 ; (hist
 ;;  (power 
 ;   (pragmatic-speaker (second (second context_typical)) color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context_typical))) 
 ;;   speaker-opt) "intermediate color typicality")
 ; (hist
 ;;  (power 
 ;   (pragmatic-speaker (third (second context_typical)) color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context_typical))) 
 ;;   speaker-opt) "atypical color")
 ; (hist
 ;;  (power 
 ;   (pragmatic-speaker (fourth (second context_typical)) color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context_typical))) 
 ;;   speaker-opt) "typical color")
 ; (hist
 ;;  (power 
 ;   (pragmatic-speaker (fifth (second context_typical)) color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context_typical))) 
 ;;   speaker-opt) "intermediate color typicality")
 ; (hist
 ;;  (power 
 ;   (pragmatic-speaker (sixth (second context_typical)) color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context_typical))) 
 ;;   speaker-opt) "atypical color")
 (hist
  ;;  (power 
  (pragmatic-speaker 
   (first (second context_intermediate)) color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context_intermediate)) 
  (stringify (second context_intermediate))) 
 ;;   speaker-opt) "intermediate color")
 ; (hist
 ;;  (power 
 ;   (pragmatic-speaker (second (second context_intermediate)) color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context_intermediate))) 
 ;;   speaker-opt) "intermediate color intermediateity")
 ; (hist
 ;;  (power 
 ;   (pragmatic-speaker (third (second context_intermediate)) color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context_intermediate))) 
 ;;   speaker-opt) "aintermediate color")
 ; (hist
 ;;  (power 
 ;   (pragmatic-speaker (fourth (second context_intermediate)) color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context_intermediate))) 
 ;;   speaker-opt) "intermediate color")
 ; (hist
 ;;  (power 
 ;   (pragmatic-speaker (fifth (second context_intermediate)) color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context_intermediate))) 
 ;;   speaker-opt) "intermediate color intermediateity")
 ; (hist
 ;;  (power 
 ;   (pragmatic-speaker (sixth (second context_intermediate)) color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context_intermediate))) 
 ;   speaker-opt) "aintermediate color")
 (hist
  ;;  (power 
  (pragmatic-speaker 
   (first (second context_atypical)) color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context_atypical))
  (stringify (second context_atypical))) 
 ;  (stringify (first (second context_atypical)))) 
 ;;   speaker-opt) "atypical color")
 ; (hist
 ;;  (power 
 ;  (pragmatic-speaker (second (second context_atypical)) color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context_atypical)) 
 ;  (stringify (second (second context_atypical)))) 
 ;;   speaker-opt) "atypical color atypicality")
 ; (hist
 ;;  (power 
 ;  (pragmatic-speaker (third (second context_atypical)) color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context_atypical)) 
 ;  (stringify (third (second context_atypical)))) 
 ;;   speaker-opt) "aatypical color")
 ; (hist
 ;;  (power 
 ;  (pragmatic-speaker (fourth (second context_atypical)) color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context_atypical)) 
 ;  (stringify (fourth (second context_atypical)))) 
 ;;   speaker-opt) "atypical color")
 ; (hist
 ;;  (power 
 ;  (pragmatic-speaker (fifth (second context_atypical)) color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context_atypical)) 
 ;  (stringify (fifth (second context_atypical)))) 
 ;;   speaker-opt) "atypical color atypicality")
 ; (hist
 ;;  (power 
 ;  (pragmatic-speaker (sixth (second context_atypical)) color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context_atypical)) 
 ;  (stringify (sixth (second context_atypical)))) 
 ;;   speaker-opt) "aatypical color")
 ;
 )

~~~


# An implementation of the PRO model as reported in Gatt et al (2013, CogSci Proceedings, Figure 2). 

You can vary:

- x: color selection probability in the color-or-size condition (trades off with size selection probability --  color and size selection probabilities are 1 in the color-only and size-only condition, respectively)

- y: overspecification eagerness 

- context (lists of features)

- what the target and competitors are

This model is specifically to test the PRO predictions and compare with versions of RSA for the lightbulb-style contexts reported by Gatt et al 2011. They use x = .87 and y = -.05

A weird thing that results from the way they set this up is that in their second selection step, they sum up the preference for the feature (a probability) and the eagerness to overspecify (supposedly also a probability) -- but with particular settings of parameters (eg color-preference 0 and negative eagerness to overspecify, given the standard "lightbulb" context, this results in a negative "probability". This is awful.

~~~
; Stuff to vary: free parameters
(define color-preference .87) ; the "x" value from Gatt et al 2013
(define size-preference (- 1 color-preference)) ; they "1-x" value from Gatt et al 2013
(define eager-to-overspecify -.05) ; the "y" value from Gatt et al 2013

; Stuff to vary: context
(define context (list (list 'o1 'big 'red)
                      (list 'o2 'small 'red)
                      (list 'o3 'small 'yellow)))

; Stuff to vary: target and competitors
(define obj-target 'o1)
(define obj-comp1 'o2)
(define obj-comp2 'o3)

; Extracts a list of object IDs for the objects in the context
(define objs
  (map (lambda (obj)
         (first obj))
       context))

; Helper function for utterances: Strips the original context of object IDs
(define pruned-context 
  (map (lambda (cont) 
         (drop cont 1))
       context))		

; Helper function for lexicon: Returns a list of an object's features
(define (check-features obj)
  (list-ref pruned-context (list-index objs obj)))

; Helper function for pro-speaker: returns a list of two numbers that indicate whether the features of the target object are shared with other objects
; (-2 -2) means neither feature is shared (color-or-size condition from GEA 2013)
; (-2 0) means size is not shared, ie it's discriminating (size-only condition from GEA 2013)
; (0 -2) means color is not shared, ie it's discriminating (color-only condition from GEA 2013)
; (0 0) means neither feature is discriminating GEA 2013 don't talk about this)
(define discriminating-features 
  (map (lambda (feature obj-target)
         (sum (list (list-index (check-features obj-comp1) feature) (list-index (check-features obj-comp2) feature))))
       (check-features obj-target)
       ))

; A speaker with color/size preferences and a particular eagerness to over-specify
(define pro-speaker 
  (lambda ()
    ; Helper function for pro-speaker: returns a preference-based choice of color or size, in case both color and size are discriminating
    (define preferred-word
      (multinomial (check-features obj-target) 
                   (list size-preference color-preference)))    
    ; if both color and size discriminate, select one with preference probability, then select the other with sum of preference probability and eagerness to overspecify -- WARNING: THIS CAN RESULT IN NON-PROPER PROBABILITIES!! HOW CAN THEY DO THIS?
    (if (= (sum discriminating-features) -4) ; if both properties are discriminating
        (if (equal? preferred-word (first (check-features obj-target))) 
            (if (flip (+ color-preference eager-to-overspecify)) ; if you selected size, now flip whether you're going to also say color
                (string-append preferred-word '_ (second (check-features obj-target)))
                preferred-word)
            (if (flip (+ size-preference eager-to-overspecify)) ; if you selected color, now flip whether you're going to also say size
                (string-append (first (check-features obj-target)) '_ preferred-word)
                preferred-word))
        (if (= (first discriminating-features) -2) 
            (if (flip (+ color-preference eager-to-overspecify)) ; if only size is discriminating
                (string-append (first (check-features obj-target)) '_ (second (check-features obj-target)))
                (first (check-features obj-target)))
            (if (flip (+ size-preference eager-to-overspecify)) ; if only color is discriminating
                (string-append (first (check-features obj-target)) '_ (second (check-features obj-target)))            
                (second (check-features obj-target)))
            ))
    ))  	

(hist (repeat 10000 pro-speaker))


~~~
