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
          (define utterance (multinomial utterances 
                                         (map (lambda (c) (exp (- c))) costs)))

          utterance

          ;          (and
          (equal? obj
                  (apply multinomial
                         (power (literal-listener utterance) listener-opt)))
          ;           (meaning utterance obj))
          ))))



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
(define type_fidelity .8) ; 1 - noise probability (size -- higher meanss less noise)
(define color_cost .1) ; lower means less costly
(define size_cost .1) ; lower means less costly
(define type_cost .1) ; lower means less costly
(define speaker-opt 15) ; standard speaker optimality parameter
; A context is a labeled list of lists, where sub-lists represent objects with a name (unique ID), a size feature and a color feature (currently only implemented for two features -- TODO: extend!)
(define context_sevensame
  (list 'seven_same
        (list (list 'o1 'big 'red 'fan)
              (list 'o2 'big 'red 'tv)
              (list 'o3 'big 'red 'desk)
              (list 'o4 'big 'red 'couch)
              (list 'o5 'big 'red 'desk)
              (list 'o6 'big 'red 'chair)
              (list 'o7 'big 'red 'couch)                                          
              (list 'o8 'big 'red 'chair)              
              )))

(define context_sevenvaried
  (list 'seven_same
        (list (list 'o1 'small 'blue 'couch)
              (list 'o2 'big 'gray 'chair)
              (list 'o3 'small 'green 'tv)
              (list 'o4 'big 'green 'fan)
              (list 'o5 'big 'red 'fan)
              (list 'o6 'big 'red 'desk)
              (list 'o7 'big 'brown 'tv)                                          
              (list 'o8 'big 'gray 'chair)              
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

         ; Helper function for utterances: concatenates an object's features to create a "_"-separated two-word utterance like "big_red"
         (define (gen-utt obj)
           (append obj (list (string-append (first obj) '_ (second obj) '_ (third obj)))))

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
   speaker-opt) "Seven distractors, low variation")
 (barplot
  (power 
   (pragmatic-speaker 'o1 color_fidelity size_fidelity type_fidelity color_cost size_cost type_cost (second context_sevenvaried)) 
   speaker-opt) "Seven distractors, high variation")  
 )

~~~