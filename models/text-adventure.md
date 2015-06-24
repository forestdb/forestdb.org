---
layout: model
title: TextAdventure
model-language: church
---

<h3>Alice is in the park.<br/>
Bob is in the office.</h3>
<h4>Where is Alice? A:park</h4>

~~~~
;; helper functions
;;;fold:
	(define (seq n)
	  (if (= n 0) '() (append (seq (- n 1)) (list n))))
	(define (remove lst elem)
	  (fold (lambda (nxt acc) (if (equal? nxt elem) acc (append acc (list nxt)))) '() lst))
	(define (draw-n n lst)
	  (if (= n 0) '() ((lambda ()
	                     (define a-draw (uniform-draw lst))
	                     (append (draw-n (- n 1) (remove lst a-draw)) (list a-draw))))))
;;;

(barplot (enumeration-query

;; generative model
;;;fold:
   (define locations '(park office classroom))
   (define people '(Alice Bob))
   (define objects '(football cake apple))
   (define times '(0 1 2 3 4))
   (define prob-move 0.2)
   (define prob-held 0.5)
   (define prob-drop 0.2)
   (define prob-pick-up 0.2)
   (define world-width 1)
   (define world-height 3)

   ;; conceptual lexicon
   (define is-person (mem (lambda (x) (if (member x people) #t #f))))
   (define is-object (mem (lambda (x) (if (member x objects) #t #f))))
   (define is-location (mem (lambda (x) (if (member x locations) #t #f))))
   (define when (lambda (proposition) (filter proposition times)))
   (define (who predicate-on-person)
     (lambda (time)
       (filter (lambda (person) ((predicate-on-person person) time)) people)))
   (define (what predicate-on-object)
     (lambda (time)
       (filter (lambda (object) ((predicate-on-object object) time)) objects)))
   (define (what-location predicate-on-location)
     (lambda (time)
       (filter (lambda (location) ((predicate-on-location location) time)) locations)))
   (define is-in
     (mem (lambda (location)
            (mem (lambda (person)
                   (mem (lambda (time)
                          (equal? location ((where person) time)))))))))
   (define before
     (mem (lambda (proposition)
            (if (and (> (length (when proposition)) 0)
                     (> (apply min (when proposition)) 0))
                (- (apply min (when proposition)) 1)
                0))))
   (define after
     (mem (lambda (proposition)
            (if (and (> (length (when proposition)) 0)
                     (< (apply max (when proposition)) (apply max times)))
                (+ (apply max (when proposition)) 1)
                (apply max times)))))
   (define is-north-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (< (first (coords y)) (first (coords x)))))))
   (define is-south-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (> (first (coords y)) (first (coords x)))))))
   (define is-east-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (< (second (coords y)) (second (coords x)))))))
   (define is-west-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (> (second (coords y)) (second (coords x)))))))
   (define go
     (lambda (location)
       (lambda (person)
         (lambda (time)
           (if (= time 0)
               (equal? ((where person) time) location)
               (and (equal? ((where person) time) location)
                    (not (equal? ((where person) (- time 1)) location))))))))
   (define how-many
     (lambda (set)
       (lambda (predicate)
         (lambda (time)
           (length (filter (lambda (x) ((predicate x) time)) set))))))
   (define both
     (lambda (x y)
       (lambda (predicate)
         (lambda (time)
           (and ((predicate x) time) ((predicate y) time))))))
   (define pick-up
     (lambda (object)
       (lambda (person)
         (lambda (time)
           (equal? ((holder object) time) person)))))

   ;; the generative model of where things are in the world
   (define where
     (mem (lambda (x)
            (define where-person
              (mem (lambda (person)
                     (mem (lambda (time)
                            (define (move person time)
                              (if (flip prob-move)
                                  (uniform-draw locations)
                                  ((where person) (- time 1))))
                            (if (= time 0)
                                ;; initialize
                                (uniform-draw locations)
                                (move person time)))))))
            (define where-object
              (mem (lambda (object)
                     (mem (lambda (time)
                            (if (is-person ((holder object) time))
                                ((where ((holder object) time)) time)
                                (if (is-location ((holder object) time))
                                    ((holder object) time)
                                    (display "WARNING!!! who is currently holding this object?"))))))))
            (mem (lambda (time)
                   (if (is-person x) ((where-person x) time)
                       (if (is-object x) ((where-object x) time)
                           (display "WARNING!!! what are you looking for?"))))))))
   (define coords
     (mem (lambda (location)
            ;; coordinates (south, east) --looks like a 0-based matrix
            (cons (random-integer world-height) (random-integer world-width)))))

   (define holder
     (mem (lambda (object)
            (mem (lambda (time)
                   (if (= time 0)
                       (if (flip prob-held)
                           (uniform-draw people)
                           (uniform-draw locations))
                       ((lambda ()
                          (define previous-holder ((holder object) (- time 1)))
                          (if (is-location previous-holder)
                              ((lambda ()
                                 (define previous-location previous-holder)
                                 (define folks-in-previous-location
                                   ((who (is-in previous-location)) (- time 1)))
                                 (if (and (flip prob-pick-up)
                                          (> (length folks-in-previous-location) 0))
                                     (uniform-draw folks-in-previous-location)
                                     previous-location)))
                              (if (is-person previous-holder)
                                  ((lambda ()
                                     (define previous-location ((where previous-holder) (- time 1)))
                                     (if (flip prob-drop)
                                         previous-location
                                         previous-holder)))
                                  (display "WARNING!!! who was holding this object?")))))))))))
;;;
          ((where 'Alice) 2)

          (condition (((is-in 'park) 'Alice) 0))
          (condition (((is-in 'office) 'Bob) 1))))
~~~~

<h3>Alice is in the park.<br/>
Bob is in the office.<br/>
Alice picked up the football.<br/>
Bob went to the classroom.</h3>
<h4>Where is the football? A:park<br/>
Where was Bob before the classroom? A:office</h4>

~~~~
;; helper functions
;;;fold:
	(define (seq n)
	  (if (= n 0) '() (append (seq (- n 1)) (list n))))
	(define (remove lst elem)
	  (fold (lambda (nxt acc) (if (equal? nxt elem) acc (append acc (list nxt)))) '() lst))
	(define (draw-n n lst)
	  (if (= n 0) '() ((lambda ()
	                     (define a-draw (uniform-draw lst))
	                     (append (draw-n (- n 1) (remove lst a-draw)) (list a-draw))))))

	(define two-questions
;;;
  (mh-query
   1000 1

;; generative model
;;;fold:
   (define locations '(park office classroom))
   (define people '(Alice Bob))
   (define objects '(football cake apple))
   (define times '(0 1 2 3 4))
   (define prob-move 0.2)
   (define prob-held 0.5)
   (define prob-drop 0.2)
   (define prob-pick-up 0.2)
   (define world-width 1)
   (define world-height 3)

   ;; conceptual lexicon
   (define is-person (mem (lambda (x) (if (member x people) #t #f))))
   (define is-object (mem (lambda (x) (if (member x objects) #t #f))))
   (define is-location (mem (lambda (x) (if (member x locations) #t #f))))
   (define when (lambda (proposition) (filter proposition times)))
   (define (who predicate-on-person)
     (lambda (time)
       (filter (lambda (person) ((predicate-on-person person) time)) people)))
   (define (what predicate-on-object)
     (lambda (time)
       (filter (lambda (object) ((predicate-on-object object) time)) objects)))
   (define (what-location predicate-on-location)
     (lambda (time)
       (filter (lambda (location) ((predicate-on-location location) time)) locations)))
   (define is-in
     (mem (lambda (location)
            (mem (lambda (person)
                   (mem (lambda (time)
                          (equal? location ((where person) time)))))))))
   (define before
     (mem (lambda (proposition)
            (if (and (> (length (when proposition)) 0)
                     (> (apply min (when proposition)) 0))
                (- (apply min (when proposition)) 1)
                0))))
   (define after
     (mem (lambda (proposition)
            (if (and (> (length (when proposition)) 0)
                     (< (apply max (when proposition)) (apply max times)))
                (+ (apply max (when proposition)) 1)
                (apply max times)))))
   (define is-north-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (< (first (coords y)) (first (coords x)))))))
   (define is-south-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (> (first (coords y)) (first (coords x)))))))
   (define is-east-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (< (second (coords y)) (second (coords x)))))))
   (define is-west-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (> (second (coords y)) (second (coords x)))))))
   (define go
     (lambda (location)
       (lambda (person)
         (lambda (time)
           (if (= time 0)
               (equal? ((where person) time) location)
               (and (equal? ((where person) time) location)
                    (not (equal? ((where person) (- time 1)) location))))))))
   (define how-many
     (lambda (set)
       (lambda (predicate)
         (lambda (time)
           (length (filter (lambda (x) ((predicate x) time)) set))))))
   (define both
     (lambda (x y)
       (lambda (predicate)
         (lambda (time)
           (and ((predicate x) time) ((predicate y) time))))))
   (define pick-up
     (lambda (object)
       (lambda (person)
         (lambda (time)
           (equal? ((holder object) time) person)))))

   ;; the generative model of where things are in the world
   (define where
     (mem (lambda (x)
            (define where-person
              (mem (lambda (person)
                     (mem (lambda (time)
                            (define (move person time)
                              (if (flip prob-move)
                                  (uniform-draw locations)
                                  ((where person) (- time 1))))
                            (if (= time 0)
                                ;; initialize
                                (uniform-draw locations)
                                (move person time)))))))
            (define where-object
              (mem (lambda (object)
                     (mem (lambda (time)
                            (if (is-person ((holder object) time))
                                ((where ((holder object) time)) time)
                                (if (is-location ((holder object) time))
                                    ((holder object) time)
                                    (display "WARNING!!! who is currently holding this object?"))))))))
            (mem (lambda (time)
                   (if (is-person x) ((where-person x) time)
                       (if (is-object x) ((where-object x) time)
                           (display "WARNING!!! what are you looking for?"))))))))
   (define coords
     (mem (lambda (location)
            ;; coordinates (south, east) --looks like a 0-based matrix
            (cons (random-integer world-height) (random-integer world-width)))))

   (define holder
     (mem (lambda (object)
            (mem (lambda (time)
                   (if (= time 0)
                       (if (flip prob-held)
                           (uniform-draw people)
                           (uniform-draw locations))
                       ((lambda ()
                          (define previous-holder ((holder object) (- time 1)))
                          (if (is-location previous-holder)
                              ((lambda ()
                                 (define previous-location previous-holder)
                                 (define folks-in-previous-location
                                   ((who (is-in previous-location)) (- time 1)))
                                 (if (and (flip prob-pick-up)
                                          (> (length folks-in-previous-location) 0))
                                     (uniform-draw folks-in-previous-location)
                                     previous-location)))
                              (if (is-person previous-holder)
                                  ((lambda ()
                                     (define previous-location ((where previous-holder) (- time 1)))
                                     (if (flip prob-drop)
                                         previous-location
                                         previous-holder)))
                                  (display "WARNING!!! who was holding this object?")))))))))))
;;;

   (list ((where 'football) 4)
         ((where 'Bob) (before ((is-in 'classroom) 'Bob))))

   (condition (((is-in 'park) 'Alice) 0))
   (condition (((is-in 'office) 'Bob) 1))
   (condition (((pick-up 'football) 'Alice) 2))
   (condition (((go 'classroom) 'Bob) 3))))

(hist (map first two-questions) "Where is the football?")
(hist (map second two-questions) "Where was Bob before the classroom?")
~~~~

<h3>Alice picked up the apple.<br/>
Alice went to the office.<br/>
Alice went to the park.<br/>
Alice dropped the apple.</h3>
<h4>Where was the apple before the park? A:office</h4>

Note: This one is wrong. My function for picking up and dropping is not quite right. Also the probabilities of moving, dropping, and picking up are really high, so things keep moving around a lot in between utterances.

~~~~
;; helper functions
;;;fold:
	(define (seq n)
	  (if (= n 0) '() (append (seq (- n 1)) (list n))))
	(define (remove lst elem)
	  (fold (lambda (nxt acc) (if (equal? nxt elem) acc (append acc (list nxt)))) '() lst))
	(define (draw-n n lst)
	  (if (= n 0) '() ((lambda ()
	                     (define a-draw (uniform-draw lst))
	                     (append (draw-n (- n 1) (remove lst a-draw)) (list a-draw))))))
;;;

(hist (mh-query
   1000 1

;; generative model
;;;fold:
   (define locations '(park office classroom))
   (define people '(Alice Bob))
   (define objects '(football cake apple))
   (define times '(0 1 2 3 4))
   (define prob-move 0.2)
   (define prob-held 0.5)
   (define prob-drop 0.2)
   (define prob-pick-up 0.2)
   (define world-width 1)
   (define world-height 3)

   ;; conceptual lexicon
   (define is-person (mem (lambda (x) (if (member x people) #t #f))))
   (define is-object (mem (lambda (x) (if (member x objects) #t #f))))
   (define is-location (mem (lambda (x) (if (member x locations) #t #f))))
   (define when (lambda (proposition) (filter proposition times)))
   (define (who predicate-on-person)
     (lambda (time)
       (filter (lambda (person) ((predicate-on-person person) time)) people)))
   (define (what predicate-on-object)
     (lambda (time)
       (filter (lambda (object) ((predicate-on-object object) time)) objects)))
   (define (what-location predicate-on-location)
     (lambda (time)
       (filter (lambda (location) ((predicate-on-location location) time)) locations)))
   (define is-in
     (mem (lambda (location)
            (mem (lambda (person)
                   (mem (lambda (time)
                          (equal? location ((where person) time)))))))))
   (define before
     (mem (lambda (proposition)
            (if (and (> (length (when proposition)) 0)
                     (> (apply min (when proposition)) 0))
                (- (apply min (when proposition)) 1)
                0))))
   (define after
     (mem (lambda (proposition)
            (if (and (> (length (when proposition)) 0)
                     (< (apply max (when proposition)) (apply max times)))
                (+ (apply max (when proposition)) 1)
                (apply max times)))))
   (define is-north-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (< (first (coords y)) (first (coords x)))))))
   (define is-south-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (> (first (coords y)) (first (coords x)))))))
   (define is-east-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (< (second (coords y)) (second (coords x)))))))
   (define is-west-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (> (second (coords y)) (second (coords x)))))))
   (define go
     (lambda (location)
       (lambda (person)
         (lambda (time)
           (if (= time 0)
               (equal? ((where person) time) location)
               (and (equal? ((where person) time) location)
                    (not (equal? ((where person) (- time 1)) location))))))))
   (define how-many
     (lambda (set)
       (lambda (predicate)
         (lambda (time)
           (length (filter (lambda (x) ((predicate x) time)) set))))))
   (define both
     (lambda (x y)
       (lambda (predicate)
         (lambda (time)
           (and ((predicate x) time) ((predicate y) time))))))
   (define pick-up
     (lambda (object)
       (lambda (person)
         (lambda (time)
           (equal? ((holder object) time) person)))))
   (define drop
     (lambda (object)
       (lambda (person)
         (lambda (time)
           (not (equal? ((holder object) time) person))))))

   ;; the generative model of where things are in the world
   (define where
     (mem (lambda (x)
            (define where-person
              (mem (lambda (person)
                     (mem (lambda (time)
                            (define (move person time)
                              (if (flip prob-move)
                                  (uniform-draw locations)
                                  ((where person) (- time 1))))
                            (if (= time 0)
                                ;; initialize
                                (uniform-draw locations)
                                (move person time)))))))
            (define where-object
              (mem (lambda (object)
                     (mem (lambda (time)
                            (if (is-person ((holder object) time))
                                ((where ((holder object) time)) time)
                                (if (is-location ((holder object) time))
                                    ((holder object) time)
                                    (display "WARNING!!! who is currently holding this object?"))))))))
            (mem (lambda (time)
                   (if (is-person x) ((where-person x) time)
                       (if (is-object x) ((where-object x) time)
                           (display "WARNING!!! what are you looking for?"))))))))
   (define coords
     (mem (lambda (location)
            ;; coordinates (south, east) --looks like a 0-based matrix
            (cons (random-integer world-height) (random-integer world-width)))))

   (define holder
     (mem (lambda (object)
            (mem (lambda (time)
                   (if (= time 0)
                       (if (flip prob-held)
                           (uniform-draw people)
                           (uniform-draw locations))
                       ((lambda ()
                          (define previous-holder ((holder object) (- time 1)))
                          (if (is-location previous-holder)
                              ((lambda ()
                                 (define previous-location previous-holder)
                                 (define folks-in-previous-location
                                   ((who (is-in previous-location)) (- time 1)))
                                 (if (and (flip prob-pick-up)
                                          (> (length folks-in-previous-location) 0))
                                     (uniform-draw folks-in-previous-location)
                                     previous-location)))
                              (if (is-person previous-holder)
                                  ((lambda ()
                                     (define previous-location ((where previous-holder) (- time 1)))
                                     (if (flip prob-drop)
                                         previous-location
                                         previous-holder)))
                                  (display "WARNING!!! who was holding this object?")))))))))))
;;;

   ((where 'apple) (before ((is-in 'kitchen) 'apple)))

   (condition (((pick-up 'apple) 'Alice) 0))
   (condition (((go 'office) 'Alice) 1))
   (condition (((go 'park) 'Alice) 2))
   (condition (((drop 'apple) 'Alice) 3))))
~~~~

<h3>The office is north of the park.<br/>
The park is north of the classroom.</h3>
<h4>What is north of the park? A: office<!-- <br/>
What is the bedroom north of? A: bathroom --></h4>

~~~~
;; helper functions
;;;fold:
	(define (seq n)
	  (if (= n 0) '() (append (seq (- n 1)) (list n))))
	(define (remove lst elem)
	  (fold (lambda (nxt acc) (if (equal? nxt elem) acc (append acc (list nxt)))) '() lst))
	(define (draw-n n lst)
	  (if (= n 0) '() ((lambda ()
	                     (define a-draw (uniform-draw lst))
	                     (append (draw-n (- n 1) (remove lst a-draw)) (list a-draw))))))
;;;

(hist (mh-query
   1000 1

;; generative model
;;;fold:
   (define locations '(park office classroom))
   (define people '(Alice Bob))
   (define objects '(football cake apple))
   (define times '(0 1 2 3 4))
   (define prob-move 0.2)
   (define prob-held 0.5)
   (define prob-drop 0.2)
   (define prob-pick-up 0.2)
   (define world-width 1)
   (define world-height 3)

   ;; conceptual lexicon
   (define is-person (mem (lambda (x) (if (member x people) #t #f))))
   (define is-object (mem (lambda (x) (if (member x objects) #t #f))))
   (define is-location (mem (lambda (x) (if (member x locations) #t #f))))
   (define when (lambda (proposition) (filter proposition times)))
   (define (who predicate-on-person)
     (lambda (time)
       (filter (lambda (person) ((predicate-on-person person) time)) people)))
   (define (what predicate-on-object)
     (lambda (time)
       (filter (lambda (object) ((predicate-on-object object) time)) objects)))
   (define (what-location predicate-on-location)
     (lambda (time)
       (filter (lambda (location) ((predicate-on-location location) time)) locations)))
   (define is-in
     (mem (lambda (location)
            (mem (lambda (person)
                   (mem (lambda (time)
                          (equal? location ((where person) time)))))))))
   (define before
     (mem (lambda (proposition)
            (if (and (> (length (when proposition)) 0)
                     (> (apply min (when proposition)) 0))
                (- (apply min (when proposition)) 1)
                0))))
   (define after
     (mem (lambda (proposition)
            (if (and (> (length (when proposition)) 0)
                     (< (apply max (when proposition)) (apply max times)))
                (+ (apply max (when proposition)) 1)
                (apply max times)))))
   (define is-north-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (< (first (coords y)) (first (coords x)))))))
   (define is-south-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (> (first (coords y)) (first (coords x)))))))
   (define is-east-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (< (second (coords y)) (second (coords x)))))))
   (define is-west-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (> (second (coords y)) (second (coords x)))))))
   (define go
     (lambda (location)
       (lambda (person)
         (lambda (time)
           (if (= time 0)
               (equal? ((where person) time) location)
               (and (equal? ((where person) time) location)
                    (not (equal? ((where person) (- time 1)) location))))))))
   (define how-many
     (lambda (set)
       (lambda (predicate)
         (lambda (time)
           (length (filter (lambda (x) ((predicate x) time)) set))))))
   (define both
     (lambda (x y)
       (lambda (predicate)
         (lambda (time)
           (and ((predicate x) time) ((predicate y) time))))))
   (define pick-up
     (lambda (object)
       (lambda (person)
         (lambda (time)
           (equal? ((holder object) time) person)))))
   (define drop
     (lambda (object)
       (lambda (person)
         (lambda (time)
           (not (equal? ((holder object) time) person))))))

   ;; the generative model of where things are in the world
   (define where
     (mem (lambda (x)
            (define where-person
              (mem (lambda (person)
                     (mem (lambda (time)
                            (define (move person time)
                              (if (flip prob-move)
                                  (uniform-draw locations)
                                  ((where person) (- time 1))))
                            (if (= time 0)
                                ;; initialize
                                (uniform-draw locations)
                                (move person time)))))))
            (define where-object
              (mem (lambda (object)
                     (mem (lambda (time)
                            (if (is-person ((holder object) time))
                                ((where ((holder object) time)) time)
                                (if (is-location ((holder object) time))
                                    ((holder object) time)
                                    (display "WARNING!!! who is currently holding this object?"))))))))
            (mem (lambda (time)
                   (if (is-person x) ((where-person x) time)
                       (if (is-object x) ((where-object x) time)
                           (display "WARNING!!! what are you looking for?"))))))))
   (define coords
     (mem (lambda (location)
            ;; coordinates (south, east) --looks like a 0-based matrix
            (cons (random-integer world-height) (random-integer world-width)))))

   (define holder
     (mem (lambda (object)
            (mem (lambda (time)
                   (if (= time 0)
                       (if (flip prob-held)
                           (uniform-draw people)
                           (uniform-draw locations))
                       ((lambda ()
                          (define previous-holder ((holder object) (- time 1)))
                          (if (is-location previous-holder)
                              ((lambda ()
                                 (define previous-location previous-holder)
                                 (define folks-in-previous-location
                                   ((who (is-in previous-location)) (- time 1)))
                                 (if (and (flip prob-pick-up)
                                          (> (length folks-in-previous-location) 0))
                                     (uniform-draw folks-in-previous-location)
                                     previous-location)))
                              (if (is-person previous-holder)
                                  ((lambda ()
                                     (define previous-location ((where previous-holder) (- time 1)))
                                     (if (flip prob-drop)
                                         previous-location
                                         previous-holder)))
                                  (display "WARNING!!! who was holding this object?")))))))))))
;;;

   ((what-location (is-north-of 'park)) 2)

   (condition (((is-north-of 'park) 'office) 0))
   (condition (((is-north-of 'classroom) 'park) 0))))
~~~~

<h3>Alice is in the park.<br/>
Bob picks up the cake.</h3>
<h4>Is Alice in the office? A:no<br/>
Does Bob have the cake? A:yes</h4>

~~~~
;; helper functions
;;;fold:
	(define (seq n)
	  (if (= n 0) '() (append (seq (- n 1)) (list n))))
	(define (remove lst elem)
	  (fold (lambda (nxt acc) (if (equal? nxt elem) acc (append acc (list nxt)))) '() lst))
	(define (draw-n n lst)
	  (if (= n 0) '() ((lambda ()
	                     (define a-draw (uniform-draw lst))
	                     (append (draw-n (- n 1) (remove lst a-draw)) (list a-draw))))))
    (define (projection fn result)
      (enumeration-query
                (fn (apply multinomial result))
                (condition #t)))

(define two-questions
;;;
(enumeration-query

;; generative model
;;;fold:
   (define locations '(park office classroom))
   (define people '(Alice Bob))
   (define objects '(football cake apple))
   (define times '(0 1 2 3 4))
   (define prob-move 0.2)
   (define prob-held 0.5)
   (define prob-drop 0.2)
   (define prob-pick-up 0.2)
   (define world-width 1)
   (define world-height 3)

   ;; conceptual lexicon
   (define is-person (mem (lambda (x) (if (member x people) #t #f))))
   (define is-object (mem (lambda (x) (if (member x objects) #t #f))))
   (define is-location (mem (lambda (x) (if (member x locations) #t #f))))
   (define when (lambda (proposition) (filter proposition times)))
   (define (who predicate-on-person)
     (lambda (time)
       (filter (lambda (person) ((predicate-on-person person) time)) people)))
   (define (what predicate-on-object)
     (lambda (time)
       (filter (lambda (object) ((predicate-on-object object) time)) objects)))
   (define (what-location predicate-on-location)
     (lambda (time)
       (filter (lambda (location) ((predicate-on-location location) time)) locations)))
   (define is-in
     (mem (lambda (location)
            (mem (lambda (person)
                   (mem (lambda (time)
                          (equal? location ((where person) time)))))))))
   (define before
     (mem (lambda (proposition)
            (if (and (> (length (when proposition)) 0)
                     (> (apply min (when proposition)) 0))
                (- (apply min (when proposition)) 1)
                0))))
   (define after
     (mem (lambda (proposition)
            (if (and (> (length (when proposition)) 0)
                     (< (apply max (when proposition)) (apply max times)))
                (+ (apply max (when proposition)) 1)
                (apply max times)))))
   (define is-north-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (< (first (coords y)) (first (coords x)))))))
   (define is-south-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (> (first (coords y)) (first (coords x)))))))
   (define is-east-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (< (second (coords y)) (second (coords x)))))))
   (define is-west-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (> (second (coords y)) (second (coords x)))))))
   (define go
     (lambda (location)
       (lambda (person)
         (lambda (time)
           (if (= time 0)
               (equal? ((where person) time) location)
               (and (equal? ((where person) time) location)
                    (not (equal? ((where person) (- time 1)) location))))))))
   (define how-many
     (lambda (set)
       (lambda (predicate)
         (lambda (time)
           (length (filter (lambda (x) ((predicate x) time)) set))))))
   (define both
     (lambda (x y)
       (lambda (predicate)
         (lambda (time)
           (and ((predicate x) time) ((predicate y) time))))))
   (define pick-up
     (lambda (object)
       (lambda (person)
         (lambda (time)
           (equal? ((holder object) time) person)))))
   (define drop
     (lambda (object)
       (lambda (person)
         (lambda (time)
           (not (equal? ((holder object) time) person))))))
   (define holds
     (lambda (object)
       (lambda (person)
         (lambda (time)
           (equal? ((holder object) time) person)))))

   ;; the generative model of where things are in the world
   (define where
     (mem (lambda (x)
            (define where-person
              (mem (lambda (person)
                     (mem (lambda (time)
                            (define (move person time)
                              (if (flip prob-move)
                                  (uniform-draw locations)
                                  ((where person) (- time 1))))
                            (if (= time 0)
                                ;; initialize
                                (uniform-draw locations)
                                (move person time)))))))
            (define where-object
              (mem (lambda (object)
                     (mem (lambda (time)
                            (if (is-person ((holder object) time))
                                ((where ((holder object) time)) time)
                                (if (is-location ((holder object) time))
                                    ((holder object) time)
                                    (display "WARNING!!! who is currently holding this object?"))))))))
            (mem (lambda (time)
                   (if (is-person x) ((where-person x) time)
                       (if (is-object x) ((where-object x) time)
                           (display "WARNING!!! what are you looking for?"))))))))
   (define coords
     (mem (lambda (location)
            ;; coordinates (south, east) --looks like a 0-based matrix
            (cons (random-integer world-height) (random-integer world-width)))))

   (define holder
     (mem (lambda (object)
            (mem (lambda (time)
                   (if (= time 0)
                       (if (flip prob-held)
                           (uniform-draw people)
                           (uniform-draw locations))
                       ((lambda ()
                          (define previous-holder ((holder object) (- time 1)))
                          (if (is-location previous-holder)
                              ((lambda ()
                                 (define previous-location previous-holder)
                                 (define folks-in-previous-location
                                   ((who (is-in previous-location)) (- time 1)))
                                 (if (and (flip prob-pick-up)
                                          (> (length folks-in-previous-location) 0))
                                     (uniform-draw folks-in-previous-location)
                                     previous-location)))
                              (if (is-person previous-holder)
                                  ((lambda ()
                                     (define previous-location ((where previous-holder) (- time 1)))
                                     (if (flip prob-drop)
                                         previous-location
                                         previous-holder)))
                                  (display "WARNING!!! who was holding this object?")))))))))))
;;;

   (list (((is-in 'office) 'Alice) 2)
         (((holds 'cake) 'Bob) 2))

   (condition (((is-in 'park) 'Alice) 0))
   (condition (((pick-up 'cake) 'Bob) 1))))

(barplot (projection first two-questions) "Is Alice in the office?")
(barplot (projection second two-questions) "Does Bob have the cake?")
~~~~

<h3>Alice picked up the football.<br/>
Alice dropped the football.<br/>
Alice got the cake.<br/>
Alice took the apple.</h3>
<h4>How many objects is Alice holding? A: two</h4>

~~~~
;; helper functions
;;;fold:
	(define (seq n)
	  (if (= n 0) '() (append (seq (- n 1)) (list n))))
	(define (remove lst elem)
	  (fold (lambda (nxt acc) (if (equal? nxt elem) acc (append acc (list nxt)))) '() lst))
	(define (draw-n n lst)
	  (if (= n 0) '() ((lambda ()
	                     (define a-draw (uniform-draw lst))
	                     (append (draw-n (- n 1) (remove lst a-draw)) (list a-draw))))))
;;;

(hist (mh-query
   1000 1

;; generative model
;;;fold:
   (define locations '(park office classroom))
   (define people '(Alice Bob))
   (define objects '(football cake apple))
   (define times '(0 1 2 3 4))
   (define prob-move 0.2)
   (define prob-held 0.5)
   (define prob-drop 0.2)
   (define prob-pick-up 0.2)
   (define world-width 1)
   (define world-height 3)

   ;; conceptual lexicon
   (define is-person (mem (lambda (x) (if (member x people) #t #f))))
   (define is-object (mem (lambda (x) (if (member x objects) #t #f))))
   (define is-location (mem (lambda (x) (if (member x locations) #t #f))))
   (define when (lambda (proposition) (filter proposition times)))
   (define (who predicate-on-person)
     (lambda (time)
       (filter (lambda (person) ((predicate-on-person person) time)) people)))
   (define (what predicate-on-object)
     (lambda (time)
       (filter (lambda (object) ((predicate-on-object object) time)) objects)))
   (define (what-location predicate-on-location)
     (lambda (time)
       (filter (lambda (location) ((predicate-on-location location) time)) locations)))
   (define is-in
     (mem (lambda (location)
            (mem (lambda (person)
                   (mem (lambda (time)
                          (equal? location ((where person) time)))))))))
   (define before
     (mem (lambda (proposition)
            (if (and (> (length (when proposition)) 0)
                     (> (apply min (when proposition)) 0))
                (- (apply min (when proposition)) 1)
                0))))
   (define after
     (mem (lambda (proposition)
            (if (and (> (length (when proposition)) 0)
                     (< (apply max (when proposition)) (apply max times)))
                (+ (apply max (when proposition)) 1)
                (apply max times)))))
   (define is-north-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (< (first (coords y)) (first (coords x)))))))
   (define is-south-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (> (first (coords y)) (first (coords x)))))))
   (define is-east-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (< (second (coords y)) (second (coords x)))))))
   (define is-west-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (> (second (coords y)) (second (coords x)))))))
   (define go
     (lambda (location)
       (lambda (person)
         (lambda (time)
           (if (= time 0)
               (equal? ((where person) time) location)
               (and (equal? ((where person) time) location)
                    (not (equal? ((where person) (- time 1)) location))))))))
   (define how-many
     (lambda (set)
       (lambda (predicate)
         (lambda (time)
           (length (filter (lambda (x) ((predicate x) time)) set))))))
   (define both
     (lambda (x y)
       (lambda (predicate)
         (lambda (time)
           (and ((predicate x) time) ((predicate y) time))))))
   (define pick-up
     (lambda (object)
       (lambda (person)
         (lambda (time)
           (equal? ((holder object) time) person)))))
   (define drop
     (lambda (object)
       (lambda (person)
         (lambda (time)
           (not (equal? ((holder object) time) person))))))

   ;; the generative model of where things are in the world
   (define where
     (mem (lambda (x)
            (define where-person
              (mem (lambda (person)
                     (mem (lambda (time)
                            (define (move person time)
                              (if (flip prob-move)
                                  (uniform-draw locations)
                                  ((where person) (- time 1))))
                            (if (= time 0)
                                ;; initialize
                                (uniform-draw locations)
                                (move person time)))))))
            (define where-object
              (mem (lambda (object)
                     (mem (lambda (time)
                            (if (is-person ((holder object) time))
                                ((where ((holder object) time)) time)
                                (if (is-location ((holder object) time))
                                    ((holder object) time)
                                    (display "WARNING!!! who is currently holding this object?"))))))))
            (mem (lambda (time)
                   (if (is-person x) ((where-person x) time)
                       (if (is-object x) ((where-object x) time)
                           (display "WARNING!!! what are you looking for?"))))))))
   (define coords
     (mem (lambda (location)
            ;; coordinates (south, east) --looks like a 0-based matrix
            (cons (random-integer world-height) (random-integer world-width)))))

   (define holder
     (mem (lambda (object)
            (mem (lambda (time)
                   (if (= time 0)
                       (if (flip prob-held)
                           (uniform-draw people)
                           (uniform-draw locations))
                       ((lambda ()
                          (define previous-holder ((holder object) (- time 1)))
                          (if (is-location previous-holder)
                              ((lambda ()
                                 (define previous-location previous-holder)
                                 (define folks-in-previous-location
                                   ((who (is-in previous-location)) (- time 1)))
                                 (if (and (flip prob-pick-up)
                                          (> (length folks-in-previous-location) 0))
                                     (uniform-draw folks-in-previous-location)
                                     previous-location)))
                              (if (is-person previous-holder)
                                  ((lambda ()
                                     (define previous-location ((where previous-holder) (- time 1)))
                                     (if (flip prob-drop)
                                         previous-location
                                         previous-holder)))
                                  (display "WARNING!!! who was holding this object?")))))))))))
;;;

   (((how-many objects)
     (lambda (object)
       (lambda (time) (equal? 'Alice ((holder object) time))))) 4)

   (condition (((pick-up 'football) 'Alice) 0))
   (condition (((drop 'football) 'Alice) 1))
   (condition (((pick-up 'cake) 'Alice) 2))
   (condition (((pick-up 'apple) 'Alice) 3))))
~~~~

<h3>Alice picks up the football.<br/>
Alice drops the apple.<br/>
Alice picks up the cake.</h3>
<h4>What is Alice holding? cake, football</h4>

~~~~
;; helper functions
;;;fold:
	(define (seq n)
	  (if (= n 0) '() (append (seq (- n 1)) (list n))))
	(define (remove lst elem)
	  (fold (lambda (nxt acc) (if (equal? nxt elem) acc (append acc (list nxt)))) '() lst))
	(define (draw-n n lst)
	  (if (= n 0) '() ((lambda ()
	                     (define a-draw (uniform-draw lst))
	                     (append (draw-n (- n 1) (remove lst a-draw)) (list a-draw))))))
;;;

(hist (mh-query
   1000 1

;; generative model
;;;fold:
   (define locations '(park office classroom))
   (define people '(Alice Bob))
   (define objects '(football cake apple))
   (define times '(0 1 2 3 4))
   (define prob-move 0.2)
   (define prob-held 0.5)
   (define prob-drop 0.2)
   (define prob-pick-up 0.2)
   (define world-width 1)
   (define world-height 3)

   ;; conceptual lexicon
   (define is-person (mem (lambda (x) (if (member x people) #t #f))))
   (define is-object (mem (lambda (x) (if (member x objects) #t #f))))
   (define is-location (mem (lambda (x) (if (member x locations) #t #f))))
   (define when (lambda (proposition) (filter proposition times)))
   (define (who predicate-on-person)
     (lambda (time)
       (filter (lambda (person) ((predicate-on-person person) time)) people)))
   (define (what predicate-on-object)
     (lambda (time)
       (filter (lambda (object) ((predicate-on-object object) time)) objects)))
   (define (what-location predicate-on-location)
     (lambda (time)
       (filter (lambda (location) ((predicate-on-location location) time)) locations)))
   (define is-in
     (mem (lambda (location)
            (mem (lambda (person)
                   (mem (lambda (time)
                          (equal? location ((where person) time)))))))))
   (define before
     (mem (lambda (proposition)
            (if (and (> (length (when proposition)) 0)
                     (> (apply min (when proposition)) 0))
                (- (apply min (when proposition)) 1)
                0))))
   (define after
     (mem (lambda (proposition)
            (if (and (> (length (when proposition)) 0)
                     (< (apply max (when proposition)) (apply max times)))
                (+ (apply max (when proposition)) 1)
                (apply max times)))))
   (define is-north-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (< (first (coords y)) (first (coords x)))))))
   (define is-south-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (> (first (coords y)) (first (coords x)))))))
   (define is-east-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (< (second (coords y)) (second (coords x)))))))
   (define is-west-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (> (second (coords y)) (second (coords x)))))))
   (define go
     (lambda (location)
       (lambda (person)
         (lambda (time)
           (if (= time 0)
               (equal? ((where person) time) location)
               (and (equal? ((where person) time) location)
                    (not (equal? ((where person) (- time 1)) location))))))))
   (define how-many
     (lambda (set)
       (lambda (predicate)
         (lambda (time)
           (length (filter (lambda (x) ((predicate x) time)) set))))))
   (define both
     (lambda (x y)
       (lambda (predicate)
         (lambda (time)
           (and ((predicate x) time) ((predicate y) time))))))
   (define pick-up
     (lambda (object)
       (lambda (person)
         (lambda (time)
           (equal? ((holder object) time) person)))))
   (define drop
     (lambda (object)
       (lambda (person)
         (lambda (time)
           (not (equal? ((holder object) time) person))))))

   ;; the generative model of where things are in the world
   (define where
     (mem (lambda (x)
            (define where-person
              (mem (lambda (person)
                     (mem (lambda (time)
                            (define (move person time)
                              (if (flip prob-move)
                                  (uniform-draw locations)
                                  ((where person) (- time 1))))
                            (if (= time 0)
                                ;; initialize
                                (uniform-draw locations)
                                (move person time)))))))
            (define where-object
              (mem (lambda (object)
                     (mem (lambda (time)
                            (if (is-person ((holder object) time))
                                ((where ((holder object) time)) time)
                                (if (is-location ((holder object) time))
                                    ((holder object) time)
                                    (display "WARNING!!! who is currently holding this object?"))))))))
            (mem (lambda (time)
                   (if (is-person x) ((where-person x) time)
                       (if (is-object x) ((where-object x) time)
                           (display "WARNING!!! what are you looking for?"))))))))
   (define coords
     (mem (lambda (location)
            ;; coordinates (south, east) --looks like a 0-based matrix
            (cons (random-integer world-height) (random-integer world-width)))))

   (define holder
     (mem (lambda (object)
            (mem (lambda (time)
                   (if (= time 0)
                       (if (flip prob-held)
                           (uniform-draw people)
                           (uniform-draw locations))
                       ((lambda ()
                          (define previous-holder ((holder object) (- time 1)))
                          (if (is-location previous-holder)
                              ((lambda ()
                                 (define previous-location previous-holder)
                                 (define folks-in-previous-location
                                   ((who (is-in previous-location)) (- time 1)))
                                 (if (and (flip prob-pick-up)
                                          (> (length folks-in-previous-location) 0))
                                     (uniform-draw folks-in-previous-location)
                                     previous-location)))
                              (if (is-person previous-holder)
                                  ((lambda ()
                                     (define previous-location ((where previous-holder) (- time 1)))
                                     (if (flip prob-drop)
                                         previous-location
                                         previous-holder)))
                                  (display "WARNING!!! who was holding this object?")))))))))))
;;;

   ((what
     (lambda (object)
       (lambda (time) (equal? 'Alice ((holder object) time))))) 3)

   (condition (((pick-up 'football) 'Alice) 0))
   (condition (((drop 'apple) 'Alice) 1))
   (condition (((pick-up 'cake) 'Alice) 2))))
~~~~

<h3>Alice travelled to the office.<br/>
Bob is no longer in the office.</h3>
<h4>Is Bob in the office? A:no<br/>
Is Alice in the office? A:yes</h4>

~~~~
;; helper functions
;;;fold:
	(define (seq n)
	  (if (= n 0) '() (append (seq (- n 1)) (list n))))
	(define (remove lst elem)
	  (fold (lambda (nxt acc) (if (equal? nxt elem) acc (append acc (list nxt)))) '() lst))
	(define (draw-n n lst)
	  (if (= n 0) '() ((lambda ()
	                     (define a-draw (uniform-draw lst))
	                     (append (draw-n (- n 1) (remove lst a-draw)) (list a-draw))))))
    (define (projection fn result)
      (enumeration-query
                (fn (apply multinomial result))
                (condition #t)))

(define two-questions
;;;
(enumeration-query

;; generative model
;;;fold:
   (define locations '(park office classroom))
   (define people '(Alice Bob))
   (define objects '(football cake apple))
   (define times '(0 1 2 3 4))
   (define prob-move 0.2)
   (define prob-held 0.5)
   (define prob-drop 0.2)
   (define prob-pick-up 0.2)
   (define world-width 1)
   (define world-height 3)

   ;; conceptual lexicon
   (define is-person (mem (lambda (x) (if (member x people) #t #f))))
   (define is-object (mem (lambda (x) (if (member x objects) #t #f))))
   (define is-location (mem (lambda (x) (if (member x locations) #t #f))))
   (define when (lambda (proposition) (filter proposition times)))
   (define (who predicate-on-person)
     (lambda (time)
       (filter (lambda (person) ((predicate-on-person person) time)) people)))
   (define (what predicate-on-object)
     (lambda (time)
       (filter (lambda (object) ((predicate-on-object object) time)) objects)))
   (define (what-location predicate-on-location)
     (lambda (time)
       (filter (lambda (location) ((predicate-on-location location) time)) locations)))
   (define is-in
     (mem (lambda (location)
            (mem (lambda (person)
                   (mem (lambda (time)
                          (equal? location ((where person) time)))))))))
   (define before
     (mem (lambda (proposition)
            (if (and (> (length (when proposition)) 0)
                     (> (apply min (when proposition)) 0))
                (- (apply min (when proposition)) 1)
                0))))
   (define after
     (mem (lambda (proposition)
            (if (and (> (length (when proposition)) 0)
                     (< (apply max (when proposition)) (apply max times)))
                (+ (apply max (when proposition)) 1)
                (apply max times)))))
   (define is-north-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (< (first (coords y)) (first (coords x)))))))
   (define is-south-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (> (first (coords y)) (first (coords x)))))))
   (define is-east-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (< (second (coords y)) (second (coords x)))))))
   (define is-west-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (> (second (coords y)) (second (coords x)))))))
   (define go
     (lambda (location)
       (lambda (person)
         (lambda (time)
           (if (= time 0)
               (equal? ((where person) time) location)
               (and (equal? ((where person) time) location)
                    (not (equal? ((where person) (- time 1)) location))))))))
   (define how-many
     (lambda (set)
       (lambda (predicate)
         (lambda (time)
           (length (filter (lambda (x) ((predicate x) time)) set))))))
   (define both
     (lambda (x y)
       (lambda (predicate)
         (lambda (time)
           (and ((predicate x) time) ((predicate y) time))))))
   (define pick-up
     (lambda (object)
       (lambda (person)
         (lambda (time)
           (equal? ((holder object) time) person)))))
   (define drop
     (lambda (object)
       (lambda (person)
         (lambda (time)
           (not (equal? ((holder object) time) person))))))
   (define holds
     (lambda (object)
       (lambda (person)
         (lambda (time)
           (equal? ((holder object) time) person)))))

   ;; the generative model of where things are in the world
   (define where
     (mem (lambda (x)
            (define where-person
              (mem (lambda (person)
                     (mem (lambda (time)
                            (define (move person time)
                              (if (flip prob-move)
                                  (uniform-draw locations)
                                  ((where person) (- time 1))))
                            (if (= time 0)
                                ;; initialize
                                (uniform-draw locations)
                                (move person time)))))))
            (define where-object
              (mem (lambda (object)
                     (mem (lambda (time)
                            (if (is-person ((holder object) time))
                                ((where ((holder object) time)) time)
                                (if (is-location ((holder object) time))
                                    ((holder object) time)
                                    (display "WARNING!!! who is currently holding this object?"))))))))
            (mem (lambda (time)
                   (if (is-person x) ((where-person x) time)
                       (if (is-object x) ((where-object x) time)
                           (display "WARNING!!! what are you looking for?"))))))))
   (define coords
     (mem (lambda (location)
            ;; coordinates (south, east) --looks like a 0-based matrix
            (cons (random-integer world-height) (random-integer world-width)))))

   (define holder
     (mem (lambda (object)
            (mem (lambda (time)
                   (if (= time 0)
                       (if (flip prob-held)
                           (uniform-draw people)
                           (uniform-draw locations))
                       ((lambda ()
                          (define previous-holder ((holder object) (- time 1)))
                          (if (is-location previous-holder)
                              ((lambda ()
                                 (define previous-location previous-holder)
                                 (define folks-in-previous-location
                                   ((who (is-in previous-location)) (- time 1)))
                                 (if (and (flip prob-pick-up)
                                          (> (length folks-in-previous-location) 0))
                                     (uniform-draw folks-in-previous-location)
                                     previous-location)))
                              (if (is-person previous-holder)
                                  ((lambda ()
                                     (define previous-location ((where previous-holder) (- time 1)))
                                     (if (flip prob-drop)
                                         previous-location
                                         previous-holder)))
                                  (display "WARNING!!! who was holding this object?")))))))))))
;;;

   (list (((is-in 'office) 'Bob) 2)
         (((is-in 'office) 'Alice) 2))

   (condition (((go 'office) 'Alice) 0))
   (condition (not (((is-in 'office) 'Bob) 1)))))

(barplot (projection first two-questions) "Is Bob in the office?")
(barplot (projection second two-questions) "Is Alice in the office?")
~~~~

<h3>Alice is either in the classroom or the park.<br/>
Bob is in the office.</h3>
<h4>Is Alice in the classroom? A:maybe<br/>
Is Alice in the office? A:no</h4>

~~~~
;; helper functions
;;;fold:
	(define (seq n)
	  (if (= n 0) '() (append (seq (- n 1)) (list n))))
	(define (remove lst elem)
	  (fold (lambda (nxt acc) (if (equal? nxt elem) acc (append acc (list nxt)))) '() lst))
	(define (draw-n n lst)
	  (if (= n 0) '() ((lambda ()
	                     (define a-draw (uniform-draw lst))
	                     (append (draw-n (- n 1) (remove lst a-draw)) (list a-draw))))))
    (define (projection fn result)
      (enumeration-query
                (fn (apply multinomial result))
                (condition #t)))

(define two-questions
;;;
(enumeration-query

;; generative model
;;;fold:
   (define locations '(park office classroom))
   (define people '(Alice Bob))
   (define objects '(football cake apple))
   (define times '(0 1 2 3 4))
   (define prob-move 0.2)
   (define prob-held 0.5)
   (define prob-drop 0.2)
   (define prob-pick-up 0.2)
   (define world-width 1)
   (define world-height 3)

   ;; conceptual lexicon
   (define is-person (mem (lambda (x) (if (member x people) #t #f))))
   (define is-object (mem (lambda (x) (if (member x objects) #t #f))))
   (define is-location (mem (lambda (x) (if (member x locations) #t #f))))
   (define when (lambda (proposition) (filter proposition times)))
   (define (who predicate-on-person)
     (lambda (time)
       (filter (lambda (person) ((predicate-on-person person) time)) people)))
   (define (what predicate-on-object)
     (lambda (time)
       (filter (lambda (object) ((predicate-on-object object) time)) objects)))
   (define (what-location predicate-on-location)
     (lambda (time)
       (filter (lambda (location) ((predicate-on-location location) time)) locations)))
   (define is-in
     (mem (lambda (location)
            (mem (lambda (person)
                   (mem (lambda (time)
                          (equal? location ((where person) time)))))))))
   (define before
     (mem (lambda (proposition)
            (if (and (> (length (when proposition)) 0)
                     (> (apply min (when proposition)) 0))
                (- (apply min (when proposition)) 1)
                0))))
   (define after
     (mem (lambda (proposition)
            (if (and (> (length (when proposition)) 0)
                     (< (apply max (when proposition)) (apply max times)))
                (+ (apply max (when proposition)) 1)
                (apply max times)))))
   (define is-north-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (< (first (coords y)) (first (coords x)))))))
   (define is-south-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (> (first (coords y)) (first (coords x)))))))
   (define is-east-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (< (second (coords y)) (second (coords x)))))))
   (define is-west-of
     (lambda (x)
       (lambda (y)
         (lambda (t)
           (> (second (coords y)) (second (coords x)))))))
   (define go
     (lambda (location)
       (lambda (person)
         (lambda (time)
           (if (= time 0)
               (equal? ((where person) time) location)
               (and (equal? ((where person) time) location)
                    (not (equal? ((where person) (- time 1)) location))))))))
   (define how-many
     (lambda (set)
       (lambda (predicate)
         (lambda (time)
           (length (filter (lambda (x) ((predicate x) time)) set))))))
   (define both
     (lambda (x y)
       (lambda (predicate)
         (lambda (time)
           (and ((predicate x) time) ((predicate y) time))))))
   (define pick-up
     (lambda (object)
       (lambda (person)
         (lambda (time)
           (equal? ((holder object) time) person)))))
   (define drop
     (lambda (object)
       (lambda (person)
         (lambda (time)
           (not (equal? ((holder object) time) person))))))
   (define holds
     (lambda (object)
       (lambda (person)
         (lambda (time)
           (equal? ((holder object) time) person)))))

   ;; the generative model of where things are in the world
   (define where
     (mem (lambda (x)
            (define where-person
              (mem (lambda (person)
                     (mem (lambda (time)
                            (define (move person time)
                              (if (flip prob-move)
                                  (uniform-draw locations)
                                  ((where person) (- time 1))))
                            (if (= time 0)
                                ;; initialize
                                (uniform-draw locations)
                                (move person time)))))))
            (define where-object
              (mem (lambda (object)
                     (mem (lambda (time)
                            (if (is-person ((holder object) time))
                                ((where ((holder object) time)) time)
                                (if (is-location ((holder object) time))
                                    ((holder object) time)
                                    (display "WARNING!!! who is currently holding this object?"))))))))
            (mem (lambda (time)
                   (if (is-person x) ((where-person x) time)
                       (if (is-object x) ((where-object x) time)
                           (display "WARNING!!! what are you looking for?"))))))))
   (define coords
     (mem (lambda (location)
            ;; coordinates (south, east) --looks like a 0-based matrix
            (cons (random-integer world-height) (random-integer world-width)))))

   (define holder
     (mem (lambda (object)
            (mem (lambda (time)
                   (if (= time 0)
                       (if (flip prob-held)
                           (uniform-draw people)
                           (uniform-draw locations))
                       ((lambda ()
                          (define previous-holder ((holder object) (- time 1)))
                          (if (is-location previous-holder)
                              ((lambda ()
                                 (define previous-location previous-holder)
                                 (define folks-in-previous-location
                                   ((who (is-in previous-location)) (- time 1)))
                                 (if (and (flip prob-pick-up)
                                          (> (length folks-in-previous-location) 0))
                                     (uniform-draw folks-in-previous-location)
                                     previous-location)))
                              (if (is-person previous-holder)
                                  ((lambda ()
                                     (define previous-location ((where previous-holder) (- time 1)))
                                     (if (flip prob-drop)
                                         previous-location
                                         previous-holder)))
                                  (display "WARNING!!! who was holding this object?")))))))))))
;;;

   (list (((is-in 'classroom) 'Alice) 2)
         (((is-in 'office) 'Alice) 2))

   (condition (or (((is-in 'classroom) 'Alice) 0)
                  (((is-in 'park) 'Alice) 0)))
   (condition (((is-in 'office) 'Bob) 1))))

(barplot (projection first two-questions) "Is Alice in the classroom?")
(barplot (projection second two-questions) "Is Alice in the office?")
~~~~
