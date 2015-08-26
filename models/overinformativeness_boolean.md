---
layout: model
title: Overinformativeness model with Boolean properties
model-status: code
model-language: church
---

# Most basic version: 

- three hard-coded objects with properties +big+red, +big-red, -big+red 

- costs vary: uniform, penalty for extra words, penalty for extra words but color cheaper


~~~
(define (rep n val) (repeat n (lambda () val)))

(define (power dist a) (list (first dist) 
                             (map (lambda (x) (pow x a)) (second dist))))

(define (meaning utterance obj)
  (case utterance      
        (('red) (or (equal? obj 'o1) (equal? obj 'o3)))
        (('big) (or (equal? obj 'o1) (equal? obj 'o2)))
        (('big_red) (equal? obj 'o1))
        ))

(define (utterance-alt utterance-alternatives costs) (multinomial utterance-alternatives costs))

(define literal-listener 
  (mem (lambda (utterance)
         (enumeration-query
          (define obj (uniform-draw objs))

          obj

          (meaning utterance obj)))))

(define pragmatic-speaker 
  (mem (lambda (obj costs)
         (enumeration-query
          (define utterance (utterance-alt utterance-alternatives costs))          

          utterance

          (and
           (equal? obj
                   (apply multinomial
                          (literal-listener utterance)))
           (meaning utterance obj))))))

(define utterance-alternatives '(big red big_red))

(define color_cost 5) ;cost for color (cheaper)
(define extraword_cost .1) ;cost for extra word (more expensive)

(define cost_matrix (list (list 1 1 1) (list 1 1 extraword_cost) (list 1 color_cost extraword_cost)))

(define objs '(o1 o2 o3)) ; only three objects, with properties as defined in the meaning function (o1-o3 are chairs, o4-o6 are tomatoes)

(multiviz (barplot (pragmatic-speaker 'o1 (first cost_matrix)) "+big +red, uniform costs")
          (barplot (pragmatic-speaker 'o1 (second cost_matrix)) "+big +red, extra word penalty")
          (barplot (pragmatic-speaker 'o1 (third cost_matrix)) "+big +red, extra word penalty and cheap color")
          (barplot (pragmatic-speaker 'o2 (first cost_matrix)) "+big -red, uniform costs")
          (barplot (pragmatic-speaker 'o2 (second cost_matrix)) "+big -red, extra word penalty")
          (barplot (pragmatic-speaker 'o2 (third cost_matrix)) "+big -red, extra word penalty and cheap color")
          (barplot (pragmatic-speaker 'o3 (first cost_matrix)) "-big +red, uniform costs")
          (barplot (pragmatic-speaker 'o3 (second cost_matrix)) "-big +red, extra word penalty")
          (barplot (pragmatic-speaker 'o3 (third cost_matrix)) "-big +red, extra word penalty and cheap color"))

	          
~~~
	      
# Next most basic version: 

- four hard-coded objects with properties big red, big yellow, small red, small yellow (and utterances to go with it)

- costs vary: uniform, penalty for extra words, penalty for extra words but color cheaper


~~~
(define (rep n val) (repeat n (lambda () val)))

(define (power dist a) (list (first dist) 
                             (map (lambda (x) (pow x a)) (second dist))))

(define (meaning utterance obj)
  (case utterance      
        (('red) (or (equal? obj 'o1) (equal? obj 'o3)))
        (('yellow) (or (equal? obj 'o2) (equal? obj 'o4)))        
        (('big) (or (equal? obj 'o1) (equal? obj 'o2)))
        (('small) (or (equal? obj 'o3) (equal? obj 'o4)))        
        (('big_red) (equal? obj 'o1))
        (('big_yellow) (equal? obj 'o2))        
        (('small_red) (equal? obj 'o3))                
        (('small_yellow) (equal? obj 'o4))                
        ))

(define (utterance-alt utterance-alternatives costs) (multinomial utterance-alternatives costs))

(define literal-listener 
  (mem (lambda (utterance)
         (enumeration-query
          (define obj (uniform-draw objs))

          obj

          (meaning utterance obj)))))

(define pragmatic-speaker 
  (mem (lambda (obj costs)
         (enumeration-query
          (define utterance (utterance-alt utterance-alternatives costs))          

          utterance

          (and
           (equal? obj
                   (apply multinomial
                          (literal-listener utterance)))
           (meaning utterance obj))))))

(define utterance-alternatives '(big small red yellow big_red big_yellow small_red small_yellow))

(define color_cost 5) ;cost for color (cheaper)
(define extraword_cost .1) ;cost for extra word (more expensive)

(define cost_matrix (list (list 1 1 1 1 1 1 1 1) (list 1 1 1 1 extraword_cost extraword_cost extraword_cost extraword_cost) (list 1 1 color_cost color_cost 1 1 1 1)))

(define objs '(o1 o2 o3 o4)) ; only three four, with properties as defined in the meaning function 

(multiviz (barplot (pragmatic-speaker 'o1 (first cost_matrix)) "big red, uniform costs, context: br, sr, by, sy")
          (barplot (pragmatic-speaker 'o1 (second cost_matrix)) "big red, extra word penalty, context: br, sr, by, sy")
          (barplot (pragmatic-speaker 'o1 (third cost_matrix)) "big red,  cheap color, context: br, sr, by, sy")
;          (barplot (pragmatic-speaker 'o2 (first cost_matrix)) "big -red, uniform costs")
;          (barplot (pragmatic-speaker 'o2 (second cost_matrix)) "+big -red, extra word penalty")
;          (barplot (pragmatic-speaker 'o2 (third cost_matrix)) "+big -red, extra word penalty and cheap color")
;          (barplot (pragmatic-speaker 'o3 (first cost_matrix)) "-big +red, uniform costs")
;          (barplot (pragmatic-speaker 'o3 (second cost_matrix)) "-big +red, extra word penalty")
;          (barplot (pragmatic-speaker 'o3 (third cost_matrix)) "-big +red, extra word penalty and cheap color"))
)
~~~

# Interesting version (size only required for disambiguation): 

- three hard-coded objects with properties big red, small red, small yellow (and utterances to go with it) -- here you need to only say "big" for the big red object, but people say "big red"

- costs vary: uniform, penalty for extra words, penalty for extra words but color cheaper


~~~
(define (rep n val) (repeat n (lambda () val)))

(define (power dist a) (list (first dist) 
                             (map (lambda (x) (pow x a)) (second dist))))

(define (meaning utterance obj)
  (case utterance      
        (('red) (or (equal? obj 'o1) (equal? obj 'o3)))
        (('yellow) (equal? obj 'o3))        
        (('big) (equal? obj 'o1))
        (('small) (or (equal? obj 'o2) (equal? obj 'o3)))        
        (('big_red) (equal? obj 'o1))
        (('big_yellow) false)        
        (('small_red) (equal? obj 'o2))                
        (('small_yellow) (equal? obj 'o3))                
        ))

(define (utterance-alt utterance-alternatives costs) (multinomial utterance-alternatives costs))

(define literal-listener 
  (mem (lambda (utterance)
         (enumeration-query
          (define obj (uniform-draw objs))

          obj

          (meaning utterance obj)))))

(define pragmatic-speaker 
  (mem (lambda (obj costs)
         (enumeration-query
          (define utterance (utterance-alt utterance-alternatives costs))          

          utterance

          (equal? obj
                  (apply multinomial
                         (literal-listener utterance)))
          ))))

(define utterance-alternatives '(big small red yellow big_red big_yellow small_red small_yellow))

(define color_cost 3) ;cost for color (cheaper)
(define extraword_cost .5) ;cost for extra word (more expensive)
(define diff (- color_cost extraword_cost))

(define cost_matrix (list (list 1 1 1 1 1 1 1 1) 
                          (list 1 1 1 1 extraword_cost extraword_cost extraword_cost extraword_cost) 
                          (list 1 1 color_cost color_cost diff diff diff diff)))

(define objs '(o1 o2 o3)) ; only three four, with properties as defined in the meaning function 

(multiviz (barplot (pragmatic-speaker 'o1 (first cost_matrix)) "big red, uniform costs, context: br, sr, sy")
          (barplot (pragmatic-speaker 'o1 (second cost_matrix)) "big red, extra word penalty, context: br, sr, sy")
          (barplot (pragmatic-speaker 'o1 (third cost_matrix)) "big red,  cheap color but extra word penalty, context: br, sr, sy")          
          )

~~~

# Other interesting version (color only required for disambiguation): 

- three hard-coded objects with properties big red, big yellow, small blue (and utterances to go with it) -- here you need to only say "red" for the big red object, and people do say "big red"

- costs vary: uniform, penalty for extra words, penalty for extra words but color cheaper


~~~
(define (rep n val) (repeat n (lambda () val)))

(define (power dist a) (list (first dist) 
                             (map (lambda (x) (pow x a)) (second dist))))

(define (meaning utterance obj)
  (case utterance      
        (('red) (equal? obj 'o1))
        (('yellow) (equal? obj 'o2))
        (('blue) (equal? obj 'o3))                
        (('big) (or (equal? obj 'o1) (equal? obj 'o2)))
        (('small) (equal? obj 'o3))
        (('big_red) (equal? obj 'o1))
        (('big_yellow) (equal? obj 'o2))        
        (('small_blue) (equal? obj 'o3))                
        ))

(define (utterance-alt utterance-alternatives costs) (multinomial utterance-alternatives costs))

(define literal-listener 
  (mem (lambda (utterance)
         (enumeration-query
          (define obj (uniform-draw objs))

          obj

          (meaning utterance obj)))))

(define pragmatic-speaker 
  (mem (lambda (obj costs)
         (enumeration-query
          (define utterance (utterance-alt utterance-alternatives costs))          

          utterance

          (equal? obj
                  (apply multinomial
                         (literal-listener utterance)))
          ))))

(define utterance-alternatives '(big small red yellow blue big_red big_yellow small_blue))

(define color_cost 3) ;cost for color (cheaper)
(define extraword_cost .5) ;cost for extra word (more expensive)
(define diff (- color_cost extraword_cost))

(define cost_matrix (list (list 1 1 1 1 1 1 1 1) 
                          (list 1 1 1 1 1 extraword_cost extraword_cost extraword_cost) 
                          (list 1 1 color_cost color_cost color_cost diff diff diff)))

(define objs '(o1 o2 o3)) ; only three four, with properties as defined in the meaning function 

(multiviz (barplot (pragmatic-speaker 'o1 (first cost_matrix)) "big red, uniform costs, context: br, by, sb")
          (barplot (pragmatic-speaker 'o1 (second cost_matrix)) "big red, extra word penalty, context: br, by, sb")
          (barplot (pragmatic-speaker 'o1 (third cost_matrix)) "big red,  cheap color but extra word penalty, context: br, by, sb")          
          )

~~~

# A literal listener that can do basic composition/decomposition: 

- three hard-coded objects with properties big red, small red, small yellow (and utterances to go with it) -- here you need to only say "big" for the big red object, but people say "big red"


~~~


(define (rep n val) (repeat n (lambda () val)))

(define (power dist a) (list (first dist) 
                             (map (lambda (x) (pow x a)) (second dist))))

(define objs '(o1 o2 o3))
(define utterance-atoms '(big small red yellow))


(define (lexicon utterance obj)
  (case utterance      
        (('red) (or (equal? obj 'o1) (equal? obj 'o3)))
        (('yellow) (equal? obj 'o3))        
        (('big) (equal? obj 'o1))
        (('small) (or (equal? obj 'o2) (equal? obj 'o3)))                      
        ))

(define (lexical-item? utterance)
  (> (list-index utterance-atoms utterance) -1))

(define (meaning utterance obj)
  (if (lexical-item? utterance)
      (lexicon utterance obj)
      (comp-meaning utterance obj)))

(define (comp-meaning utterance obj)
  (define subs (regexp-split utterance '_))
  (and (meaning (first subs) obj) (meaning (second subs) obj)))

(define literal-listener 
  (mem (lambda (utterance)
         (enumeration-query
          (define obj (uniform-draw objs))

          obj

          (meaning utterance obj)))))

(literal-listener 'big_red)

~~~

# A pragmatic speaker that can do basic composition: 

- three hard-coded objects with properties big red, small red, small yellow (and utterances to go with it) -- here you need to only say "big" for the big red object, but people say "big red"


~~~


;;;fold:
(define (rep n val) (repeat n (lambda () val)))

(define (power dist a) (list (first dist) 
                             (map (lambda (x) (pow x a)) (second dist))))

(define objs '(o1 o2 o3))
(define utterance-atoms '(big small red yellow))
(define color-utterances '(red yellow))
(define size-utterances '(big small))
(define basic-costs '(1 1 1 1))
(define tau .6) ;stopping probability of utterance grammar


(define (lexicon utterance obj)
  (case utterance      
        (('red) (or (equal? obj 'o1) (equal? obj 'o2)))
        (('yellow) (equal? obj 'o3))        
        (('big) (equal? obj 'o1))
        (('small) (or (equal? obj 'o2) (equal? obj 'o3)))                      
        ))

(define (lexical-item? utterance)
  (> (list-index utterance-atoms utterance) -1))

(define (meaning utterance obj)
  (if (lexical-item? utterance)
      (lexicon utterance obj)
      (comp-meaning utterance obj)))

(define (comp-meaning utterance obj)
  (define subs (regexp-split utterance '_))
  (and (meaning (first subs) obj) (meaning (second subs) obj)))

(define literal-listener 
  (mem (lambda (utterance)
         (enumeration-query
          (define obj (uniform-draw objs))

          obj

          (meaning utterance obj)))))
;;;

(define (generate-utterance utts costs depth)
  (if (> depth 0)
      (multinomial utts costs)
      (if (flip tau)
          (multinomial utterance-atoms costs)            
          (compose-utterance (multinomial utterance-atoms costs))
          )))

(define (compose-utterance utt)
  (if (> (list-index color-utterances utt) -1)
      (string-append utt '_ (generate-utterance size-utterances '(1 1) 1))
      (string-append utt '_ (generate-utterance color-utterances '(1 1) 1))    
      ))

(define pragmatic-speaker 
  (mem (lambda (obj costs)
         (enumeration-query
          (define utterance (generate-utterance utterance-atoms basic-costs 0))          

          utterance

          (equal? obj
                  (apply multinomial
                         (literal-listener utterance)))
          ))))

(multiviz (barplot (pragmatic-speaker 'o1) "big red, uniform costs  (but extra word dispreferred), context: br, sr, sy")
          (barplot (pragmatic-speaker 'o2) "small red, uniform costs  (but extra word dispreferred), context: br, sr, sy")
          (barplot (pragmatic-speaker 'o3) "small yellow,  uniform costs (but extra word dispreferred), context: br, sr, sy")          
          )

~~~

# A literal listener that can do basic decomposition and takes into account predicate noise: 

- three hard-coded objects with properties big red, small red, small yellow (and utterances to go with it) -- here you need to only say "big" for the big red object, but people say "big red"

- problem (same for the speaker that follows this): if we just assume that the available alternatives are all the atoms (big small red yellow) plus their two-word combinations, then even when one of the features or feature conjunctions is not available in the context, the speaker will produce it if probability of communicative success is sufficiently high (eg in this case "big_yellow" to refer to the small yellow object, because it's the only yellow object, so the listener will still get there). Solution: restrict alternatives to only contain available features/feature conjunctions


~~~


(define (rep n val) (repeat n (lambda () val)))

(define (power dist a) (list (first dist) 
                             (map (lambda (x) (pow x a)) (second dist))))

(define objs '(o1 o2 o3))
(define utterance-atoms '(big small red yellow))
(define color-utterances '(red yellow))
(define size-utterances '(big small))
(define basic-costs '(1 1 1 1))
(define tau .8) ;probability of adding an extra word
(define color-fidelity .99) ;1 - noise probability
(define size-fidelity .85) ;1 - noise probability

(define (lexicon utterance obj)
  (case utterance      
        (('red) (if (flip color-fidelity) 
                    (or (equal? obj 'o1) (equal? obj 'o2))
                    (equal? obj 'o3)))
        ;false))
        ;(equal? obj (uniform-draw objs))))                    
        (('yellow) (if (flip color-fidelity)  
                       (equal? obj 'o3)
                       (or (equal? obj 'o1) (equal? obj 'o2))))
        ;                       false))
        ;(equal? obj (uniform-draw objs))))                       
        (('big) (if (flip size-fidelity)  
                    (equal? obj 'o1)
                    (or (equal? obj 'o2) (equal? obj 'o3))))
        ;                    false))
        ;(equal? obj (uniform-draw objs))))                    
        (('small) (if (flip size-fidelity)  
                      (or (equal? obj 'o2) (equal? obj 'o3))
                      (equal? obj 'o1)))                      
        ;                      false))
        ;(equal? obj (uniform-draw objs))))                      
        ))

(define (lexical-item? utterance)
  (> (list-index utterance-atoms utterance) -1))

(define (meaning utterance obj)
  (if (lexical-item? utterance)
      (lexicon utterance obj)
      (comp-meaning utterance obj)))

(define (comp-meaning utterance obj)
  (define subs (regexp-split utterance '_))
  (and (meaning (first subs) obj) (meaning (second subs) obj)))

(define literal-listener 
  (mem (lambda (utterance)
         (enumeration-query
          (define obj (uniform-draw objs))

          obj

          (meaning utterance obj)))))
~~~

# A pragmatic speaker that can do basic composition and takes into account predicate noise: 

- three hard-coded objects with properties big red, small red, small yellow (and utterances to go with it) -- here you need to only say "big" for the big red object, but people say "big red"


~~~


(define (rep n val) (repeat n (lambda () val)))

(define (power dist a) (list (first dist) 
                             (map (lambda (x) (pow x a)) (second dist))))

(define objs '(o1 o2 o3))
(define utterance-atoms '(big small red yellow))
(define color-utterances '(red yellow))
(define size-utterances '(big small))
(define basic-costs '(1 1 1 1))
(define tau .8) ;probability of adding an extra word
(define color-fidelity .99) ;1 - noise probability
(define size-fidelity .85) ;1 - noise probability

(define (lexicon utterance obj)
  (case utterance      
        (('red) (if (flip color-fidelity) 
                    (or (equal? obj 'o1) (equal? obj 'o2))
                    (equal? obj 'o3)))
        ;false))
        ;(equal? obj (uniform-draw objs))))                    
        (('yellow) (if (flip color-fidelity)  
                       (equal? obj 'o3)
                       (or (equal? obj 'o1) (equal? obj 'o2))))
        ;                       false))
        ;(equal? obj (uniform-draw objs))))                       
        (('big) (if (flip size-fidelity)  
                    (equal? obj 'o1)
                    (or (equal? obj 'o2) (equal? obj 'o3))))
        ;                    false))
        ;(equal? obj (uniform-draw objs))))                    
        (('small) (if (flip size-fidelity)  
                      (or (equal? obj 'o2) (equal? obj 'o3))
                      (equal? obj 'o1)))                      
        ;                      false))
        ;(equal? obj (uniform-draw objs))))                      
        ))

(define (lexical-item? utterance)
  (> (list-index utterance-atoms utterance) -1))

(define (meaning utterance obj)
  (if (lexical-item? utterance)
      (lexicon utterance obj)
      (comp-meaning utterance obj)))

(define (comp-meaning utterance obj)
  (define subs (regexp-split utterance '_))
  (and (meaning (first subs) obj) (meaning (second subs) obj)))

(define literal-listener 
  (mem (lambda (utterance)
         (enumeration-query
          (define obj (uniform-draw objs))

          obj

          (meaning utterance obj)))))

(define (generate-utterance utts costs depth)
  (if (> depth 0)
      (multinomial utts costs)
      (if (flip tau)
          (compose-utterance (multinomial utterance-atoms costs))          
          (multinomial utterance-atoms costs)            
          )))

(define (compose-utterance utt)
  (if (> (list-index color-utterances utt) -1)
      (string-append utt '_ (generate-utterance size-utterances '(1 1) 1))
      (string-append utt '_ (generate-utterance color-utterances '(1 1) 1))    
      ))

(define pragmatic-speaker 
  (mem (lambda (obj costs)
         (enumeration-query
          (define utterance (generate-utterance utterance-atoms basic-costs 0))          

          utterance

          (and
           (equal? obj
                   (apply multinomial
                          (power (literal-listener utterance) 2)))
           (meaning utterance obj))
          ))))

(multiviz (barplot (pragmatic-speaker 'o1) "big red, uniform costs  (but extra word dispreferred), context: br, sr, sy")
          (barplot (pragmatic-speaker 'o2) "small red, uniform costs  (but extra word dispreferred), context: br, sr, sy")
          (barplot (pragmatic-speaker 'o3) "small yellow,  uniform costs (but extra word dispreferred), context: br, sr, sy")          
          )


~~~

# A pragmatic speaker that can do basic composition and takes into account predicate noise (with dynamic contexts)
~~~


(define (power dist a) (list (first dist) 
                             (map (lambda (x) (pow x a)) (second dist))))

; Free parameters:
(define color-fidelity .99) ; 1 - noise probability (color -- higher means less noise)
(define size-fidelity .9) ; 1 - noise probability (size -- higher meanss less noise)
(define extraword_cost 2) ; cost of producing extra word -- higher means cheaper

; A context is a list of lists, where sub-lists represent objects with a name (unique ID), a size feature and a color feature (currently only implemented for two features -- TODO: extend!)
(define context (list (list 'o1 'big 'red)
                      (list 'o2 'small 'red)
                      (list 'o3 'small 'yellow)))
;                      (list 'o4 'small 'red)
;                      (list 'o5 'big 'yellow)))

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
         (if (> (length (regexp-split utt '_)) 1)
             extraword_cost
             1))
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
          (define utterance (multinomial utterances costs))

          utterance

          ;          (and
          (equal? obj
                  (apply multinomial
                         (power (literal-listener utterance) 2)))
          ;           (meaning utterance obj))
          ))))



(multiviz (barplot (pragmatic-speaker 'o1) (stringify (first context)))
          (barplot (pragmatic-speaker 'o2) (stringify (second context)))
          (barplot (pragmatic-speaker 'o3) (stringify (third context))))
;          (barplot (pragmatic-speaker 'o4) (stringify (fourth context)))
;          (barplot (pragmatic-speaker 'o5) (stringify (fifth context))))

~~~



;
;# Adding noise to meaning predicates (size only required for disambiguation): 
;
;- three hard-coded objects with properties big red, small red, small yellow (and utterances to go with it) -- here you need to only say "big" for the big red object, but people say "big red"
;
;- costs vary: uniform, penalty for extra words, penalty for extra words but color cheaper
;
;
;~~~
;(define (rep n val) (repeat n (lambda () val)))
;
;(define (power dist a) (list (first dist) 
;                             (map (lambda (x) (pow x a)) (second dist))))
;
;(define objs '(o1 o2 o3))
;
;(define (lexicon utterance obj)
;  (case utterance      
;        (('red) (or (equal? obj 'o1) (equal? obj 'o3)))
;        (('yellow) (equal? obj 'o3))        
;        (('big) (equal? obj 'o1))
;        (('small) (or (equal? obj 'o2) (equal? obj 'o3)))        
;;        (('big_red) (equal? obj 'o1))
;;        (('big_yellow) false)        
;;        (('small_red) (equal? obj 'o2))                
;;        (('small_yellow) (equal? obj 'o3))                
;        ))
;
;(define (meaning utterance obj)
;	(if (lexical-item? utterance)
;		(lexicon utterance obj)
;		(comp-meaning utterance obj)))
;
;(define (comp-meaning utterance obj)
;	(define subs (regexp-split utterance '_))
;	(and (meaning (first subs) obj) (meaning (second subs) obj))
;)
;
;(define literal-listener 
;  (mem (lambda (utterance)
;         (enumeration-query
;          (define obj (uniform-draw objs))
;
;          obj
;
;          (comp-meaning utterance obj)))))
;

;(define (utterance-alt utterance-alternatives costs) (multinomial utterance-alternatives costs))

;(define pragmatic-speaker 
;  (mem (lambda (obj costs)
;         (enumeration-query
;          (define utterance (utterance-alt utterance-alternatives costs))          
;
;          utterance
;
;          (equal? obj
;                  (apply multinomial
;                         (literal-listener utterance)))
;          ))))
;
;;(define utterance-alternatives '(big small red yellow big_red big_yellow small_red small_yellow))
;(define utterance-atoms '(big small red yellow))
;
;(define color_cost 3) ;cost for color (cheaper)
;(define extraword_cost .5) ;cost for extra word (more expensive)
;(define diff (- color_cost extraword_cost))
;;(define color-noise .01)
;;(define size-noise .4)
;
;(define cost_matrix (list (list 1 1 1 1 1 1 1 1) 
;                          (list 1 1 1 1 extraword_cost extraword_cost extraword_cost extraword_cost) 
;                          (list 1 1 color_cost color_cost diff diff diff diff)))
;
;
;(multiviz (barplot (pragmatic-speaker 'o1 (first cost_matrix)) "big red, uniform costs, context: br, sr, sy")
;          (barplot (pragmatic-speaker 'o1 (second cost_matrix)) "big red, extra word penalty, context: br, sr, sy")
;          (barplot (pragmatic-speaker 'o1 (third cost_matrix)) "big red,  cheap color but extra word penalty, context: br, sr, sy")          
;          )
;
;~~~
;	      	          
;# Next most basic version with object types (chair vs tomato): 
;
;- six hard-coded objects with properties +big+red, +big-red, -big+red (3 chairs, 3 tomatoes)
;
;- costs vary: uniform, penalty for extra words, penalty for extra words but color cheaper
;
;~~~
;
;
;(define (rep n val) (repeat n (lambda () val)))
;
;(define (power dist a) (list (first dist) 
;                             (map (lambda (x) (pow x a)) (second dist))))
;
;(define (meaning utterance obj)
;  (case utterance  
;        (('big) (or (equal? obj 'o1) (equal? obj 'o2) (equal? obj 'o4) (equal? obj 'o5)))
;        (('red) (or (equal? obj 'o1) (equal? obj 'o3) (equal? obj 'o4) (equal? obj 'o6))	        
;         (('big_red) (or (equal? obj 'o1) (equal? obj 'o4)))	  
;         (('chair) (or (equal? obj 'o1) (equal? obj 'o2) (equal? obj 'o3))) ; first three objects are chairs
;         (('tomato) (or (equal? obj 'o4) (equal? obj 'o5) (equal? obj 'o6))) ; second three objects are tomatoes	  		
;         (('big_chair) (or (equal? obj 'o1) (equal? obj 'o2)))	  		
;         (('red_chair) (or (equal? obj 'o1) (equal? obj 'o3)))
;         (('big_red_chair) (equal? obj 'o1))	  			  	
;         (('big_tomato) (or (equal? obj 'o4) (equal? obj 'o5)))	  			 	  			  
;         (('red_tomato) (or (equal? obj 'o4) (equal? obj 'o6)))
;         (('big_red_tomato) (equal? obj 'o4))	  			  		 	  		
;         )))
;
;(define (utterance-alt utterance-alternatives costs) (multinomial utterance-alternatives costs))
;
;(define literal-listener 
;  (mem (lambda (utterance)
;         (enumeration-query
;          (define obj (uniform-draw objs))
;
;          obj
;
;          (meaning utterance obj)))))
;
;(define pragmatic-speaker 
;  (mem (lambda (obj costs)
;         (enumeration-query
;          (define utterance (utterance-alt utterance-alternatives costs))          
;
;          utterance
;
;          (and
;           (equal? obj
;                   (apply multinomial
;                          (literal-listener utterance)))
;           (meaning utterance obj))))))
;
;(define utterance-alternatives '(big red big_red chair tomato big_chair red_chair big_red_chair big_tomato red_tomato big_red_tomato))
;
;(define color_cost 5) ;cost for color (cheaper)
;(define extraword_cost .1) ;cost for extra word (more expensive)
;
;(define cost_matrix (list (list 1 1 1 1 1 1 1 1 1 1 1) 
;                          (list 1 1 extraword_cost 1 1 extraword_cost extraword_cost (+ extraword_cost extraword_cost) extraword_cost extraword_cost (+ extraword_cost extraword_cost))
;                          (list 1 color_cost (+ extraword_cost color_cost) 1 1 extraword_cost (+ extraword_cost color_cost) (+ extraword_cost extraword_cost color_cost) extraword_cost (+ extraword_cost color_cost) (+ extraword_cost extraword_cost color_cost))))
;
;(define objs '(o1 o2 o3 o4 o5 o6)) ; only three objects, with properties as defined in the meaning function
;
;(multiviz (barplot (pragmatic-speaker 'o1 (first cost_matrix)) "+big +red chair, uniform costs")
;          (barplot (pragmatic-speaker 'o1 (second cost_matrix)) "+big +red chair, extra word penalty")
;          (barplot (pragmatic-speaker 'o1 (third cost_matrix)) "+big +red chair, extra word penalty and cheap color")
;          (barplot (pragmatic-speaker 'o2 (first cost_matrix)) "+big -red chair, uniform costs")
;          (barplot (pragmatic-speaker 'o2 (second cost_matrix)) "+big -red chair, extra word penalty")
;          (barplot (pragmatic-speaker 'o2 (third cost_matrix)) "+big -red chair, extra word penalty and cheap color")
;          (barplot (pragmatic-speaker 'o3 (first cost_matrix)) "-big +red chair, uniform costs")
;          (barplot (pragmatic-speaker 'o3 (second cost_matrix)) "-big +red chair, extra word penalty")
;          (barplot (pragmatic-speaker 'o3 (third cost_matrix)) "-big +red chair, extra word penalty and cheap color")
;          (barplot (pragmatic-speaker 'o4 (first cost_matrix)) "+big +red tomato, uniform costs")
;          (barplot (pragmatic-speaker 'o4 (second cost_matrix)) "+big +red tomato, extra word penalty")
;          (barplot (pragmatic-speaker 'o4 (third cost_matrix)) "+big +red tomato, extra word penalty and cheap color")
;          (barplot (pragmatic-speaker 'o5 (first cost_matrix)) "+big -red tomato, uniform costs")
;          (barplot (pragmatic-speaker 'o5 (second cost_matrix)) "+big -red tomato, extra word penalty")
;          (barplot (pragmatic-speaker 'o5 (third cost_matrix)) "+big -red tomato, extra word penalty and cheap color")
;          (barplot (pragmatic-speaker 'o6 (first cost_matrix)) "-big +red tomato, uniform costs")
;          (barplot (pragmatic-speaker 'o6 (second cost_matrix)) "-big +red tomato, extra word penalty")
;          (barplot (pragmatic-speaker 'o6 (third cost_matrix)) "-big +red tomato, extra word penalty and cheap color")	
;
;~~~
;
;# Next most basic version with object types (chair vs tomato): 
;
;- eight hard-coded objects with properties big red, big yellow, small red, small yellow (4 chairs, 4 tomatoes)
;
;- costs vary: uniform, penalty for extra words, penalty for extra words but color cheaper
;
;~~~
;;
;;
;;(define (rep n val) (repeat n (lambda () val)))
;;
;;(define (power dist a) (list (first dist) 
;;                             (map (lambda (x) (pow x a)) (second dist))))
;;
;;(define (meaning utterance obj)
;;  (case utterance  
;;        (('big) (or (equal? obj 'o1) (equal? obj 'o2) (equal? obj 'o4) (equal? obj 'o5)))
;;        (('red) (or (equal? obj 'o1) (equal? obj 'o3) (equal? obj 'o4) (equal? obj 'o6))	        
;;         (('big_red) (or (equal? obj 'o1) (equal? obj 'o4)))	  
;;         (('chair) (or (equal? obj 'o1) (equal? obj 'o2) (equal? obj 'o3))) ; first three objects are chairs
;;         (('tomato) (or (equal? obj 'o4) (equal? obj 'o5) (equal? obj 'o6))) ; second three objects are tomatoes	  		
;;         (('big_chair) (or (equal? obj 'o1) (equal? obj 'o2)))	  		
;;         (('red_chair) (or (equal? obj 'o1) (equal? obj 'o3)))
;;         (('big_red_chair) (equal? obj 'o1))	  			  	
;;         (('big_tomato) (or (equal? obj 'o4) (equal? obj 'o5)))	  			 	  			  
;;         (('red_tomato) (or (equal? obj 'o4) (equal? obj 'o6)))
;;         (('big_red_tomato) (equal? obj 'o4))	  			  		 	  		
;;         )))
;;
;;(define (utterance-alt utterance-alternatives costs) (multinomial utterance-alternatives costs))
;;
;;(define literal-listener 
;;  (mem (lambda (utterance)
;;         (enumeration-query
;;          (define obj (uniform-draw objs))
;;
;;          obj
;;
;;          (meaning utterance obj)))))
;;
;;(define pragmatic-speaker 
;;  (mem (lambda (obj costs)
;;         (enumeration-query
;;          (define utterance (utterance-alt utterance-alternatives costs))          
;;
;;          utterance
;;
;;          (and
;;           (equal? obj
;;                   (apply multinomial
;;                          (literal-listener utterance)))
;;           (meaning utterance obj))))))
;;
;;(define utterance-alternatives '(big red big_red chair tomato big_chair red_chair big_red_chair big_tomato red_tomato big_red_tomato))
;;
;;(define color_cost 5) ;cost for color (cheaper)
;;(define extraword_cost .1) ;cost for extra word (more expensive)
;;
;;(define cost_matrix (list (list 1 1 1 1 1 1 1 1 1 1 1) 
;;                          (list 1 1 extraword_cost 1 1 extraword_cost extraword_cost (+ extraword_cost extraword_cost) extraword_cost extraword_cost (+ extraword_cost extraword_cost))
;;                          (list 1 color_cost (+ extraword_cost color_cost) 1 1 extraword_cost (+ extraword_cost color_cost) (+ extraword_cost extraword_cost color_cost) extraword_cost (+ extraword_cost color_cost) (+ extraword_cost extraword_cost color_cost))))
;;
;;(define objs '(o1 o2 o3 o4 o5 o6)) ; only three objects, with properties as defined in the meaning function
;;
;;(multiviz (barplot (pragmatic-speaker 'o1 (first cost_matrix)) "+big +red chair, uniform costs")
;;          (barplot (pragmatic-speaker 'o1 (second cost_matrix)) "+big +red chair, extra word penalty")
;;          (barplot (pragmatic-speaker 'o1 (third cost_matrix)) "+big +red chair, extra word penalty and cheap color")
;;          (barplot (pragmatic-speaker 'o2 (first cost_matrix)) "+big -red chair, uniform costs")
;;          (barplot (pragmatic-speaker 'o2 (second cost_matrix)) "+big -red chair, extra word penalty")
;;          (barplot (pragmatic-speaker 'o2 (third cost_matrix)) "+big -red chair, extra word penalty and cheap color")
;;          (barplot (pragmatic-speaker 'o3 (first cost_matrix)) "-big +red chair, uniform costs")
;;          (barplot (pragmatic-speaker 'o3 (second cost_matrix)) "-big +red chair, extra word penalty")
;;          (barplot (pragmatic-speaker 'o3 (third cost_matrix)) "-big +red chair, extra word penalty and cheap color")
;;          (barplot (pragmatic-speaker 'o4 (first cost_matrix)) "+big +red tomato, uniform costs")
;;          (barplot (pragmatic-speaker 'o4 (second cost_matrix)) "+big +red tomato, extra word penalty")
;;          (barplot (pragmatic-speaker 'o4 (third cost_matrix)) "+big +red tomato, extra word penalty and cheap color")
;;          (barplot (pragmatic-speaker 'o5 (first cost_matrix)) "+big -red tomato, uniform costs")
;;          (barplot (pragmatic-speaker 'o5 (second cost_matrix)) "+big -red tomato, extra word penalty")
;;          (barplot (pragmatic-speaker 'o5 (third cost_matrix)) "+big -red tomato, extra word penalty and cheap color")
;;          (barplot (pragmatic-speaker 'o6 (first cost_matrix)) "-big +red tomato, uniform costs")
;;          (barplot (pragmatic-speaker 'o6 (second cost_matrix)) "-big +red tomato, extra word penalty")
;;          (barplot (pragmatic-speaker 'o6 (third cost_matrix)) "-big +red tomato, extra word penalty and cheap color")	
;
;~~~
;
;# Next most basic version with dynamic contexts of objects: 
;
;- properties big/small, red/yellow
;
;- object types chair/tomato
;
;- costs vary: uniform, penalty for extra words, penalty for extra words but color cheaper
;
;~~~
;
;
;
;~~~




; A PCFG for utterances (based on Andreas's PCFG on forestdb) -- at this point just one or two adjectives, but should be extended as necessary
; The problem with this: it's not restricted to not produce "big big" or "yellow yellow" or "big small"
;; (define (terminal t) (lambda () t))

;; (define (sample thunk) (thunk))

;; (define A (lambda ()
;;             (map sample
;;                  (multinomial
;;                   (list (list (terminal 'red)) 
;;                         (list (terminal 'yellow)) 
;;                         (list (terminal 'big)) 
;;                         (list (terminal 'small)))
;;                   (list (/ 1 4) (/ 1 4) (/ 1 4) (/ 1 4))))))

;; (define AP (lambda ()
;;              (map sample
;;                   (multinomial
;;                    (list (list A)
;;                          (list A A))
;;                    (list (/ 1 2) (/ 1 2))))))						
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


