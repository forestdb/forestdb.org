---
layout: model
title: Category explanations
---

*All models on this page us an alternative form of the counterfactual model outlined at [forestdb.org/models/exogenous-counterfactuls](http://forestdb.org/models/exogenous-counterfactuls.html). An outline of the original counterfactual model can be found at [forestdb.org/models/because](http://forestdb.org/models/because.html*

### Fish

Below is a model representing a simple taxonomy of fish. Fish come in the northern and southern varieties, and each region has two species: wugs and dels, and haps and niks respectively.

Here I have the listener interpret `(because (big-fins) (eq? (species) 'wug))`. The most interesting effect here is that the model ranks the probability that dels (the other northern species) have big-fins as lower than for either of the southern species. This follows my initial intuition that the categorical counterfactual is interpreted in terms of the super-category: if a wug weren't a wug, it would most likely be a del because only one variable must be resampled. Thus, by saying ¬wug → ¬ big-fins, we are implying that dels don't have big fins more strongly than that the other species don't have big fins. Pragmatics will strengthen this effect because the listener will know that the speaker would say "big-fins because north" if both wugs ands dels had big fins.

~~~~
;; runs in ~12 minutes

;;;fold:
;;first we have a bunch of helper code to do meta-transforms.. converts name to 
;;shadow-name and wraps top-level defines
(define (names model)
  (map (lambda (def)
         (if (is-function-definition? def)
             (first (second def))
             (second def)))
       model))

(define (is-function-definition? def)
  (list? (second def)))

(define (shadow-symbol name)
  (string->symbol (string-append "shadow-" name)))

(define (rename expr from-name to-name)
  (cond [(list? expr) (map (lambda (x) (rename x from-name to-name)) expr)]
        [(eq? expr from-name) to-name]
        [else expr]))

(define (shadow-rename expr name)
  (rename expr name (shadow-symbol name)))

(define (shadow-rename-all expr names)
  (if (null? names)
      expr
      (shadow-rename-all (shadow-rename expr (first names))
                         (rest names))))

(define (make-shadow-defines model)
  (define ns (names model))
  (map (lambda (def)
         (if (is-function-definition? def)
             (shadow-rename-all def ns)
             (let ([name (second def)])
               '(define ,(shadow-symbol name) (if (flip eps) 
                                                  ,(shadow-rename-all (third def) ns) 
                                                  ,name)))))
       model))

;;the meaning function constructs a church expression from an utterance. 
;;for 'because it uses quasiquote mojo to dynamically construct the right expression.
;;(in principle this handles embedded "because", but currently expand-because doesn't do 
;;the right thing since the model is a fixed global.)
(define (meaning utt)
  (define (because? u) (if (list? u) (eq? (first u) 'because) false))
  (if (list? utt)
      (if (because? utt)
          (expand-because (map meaning utt))
          (map meaning utt))
      utt))

;;expand an expr with form '(because a b), ie "a because b", into the (hypothesized) 
;;counterfactual meaning:
(define (expand-because expr) 
  (define a (second expr))
  (define b (third expr))
  '(and ,a ,b
        (apply multinomial
               (enumeration-query
                (define eps 0.01)
                ,@(make-shadow-defines model) ;;the shadow model
                (not ,(shadow-rename-all a (names model)))
                (condition (not ,(shadow-rename-all b (names model))))))))

;;listener is standard RSA literal listener, except we dynamically construct the 
;;query to allow complex meanings that include because:
(define listener 
  (mem (lambda (utt qud)
         (eval
          '(enumeration-query
            ,@model
            ,qud
            (condition ,(meaning utt)))))))

;;the speaker is no different from ordinary RSA
(define (speaker val qud) ;;want to communicate val as value of qud
  (enumeration-query
   (define utt (utt-prior))
   utt
   (condition (equal? val (apply multinomial (listener utt qud))))))

;;;

;; returns value <= p# with probability p#
;;this takes the role of a uniform function if we only care about the value's relation
;; to p1 and p2
(define (get-U-crits p1 p2)
  (multinomial (list p1 p2 1)
               (list p1 (- p2 p1)  (- 1 p2))))

(define model 
  '(
    ;; categories
    (define north:south (flip)) ; exogenous randomness
    (define (region) (if north:south 'north 'south))
    
    (define wug:del (flip))
    (define hap:nik (flip))
    (define (species) (case (region)
                            (('north) (if wug:del 'wug 'del))
                            (('south) (if hap:nik 'hap 'nik))))

    ;; feature-weights
    (define wug→big-fins (if (flip) .2 .8))
    (define del→big-fins (if (flip) .2 .8))
    (define hap→big-fins (if (flip) .2 .8))
    (define nik→big-fins (if (flip) .2 .8))

    ;; features
    (define U-big-fins (get-U-crits .2 .8))
    (define (big-fins)
      (case (species)
            (('wug) (<= U-big-fins wug→big-fins))
            (('del) (<= U-big-fins del→big-fins))
            (('hap) (<= U-big-fins hap→big-fins))
            (('nik) (<= U-big-fins nik→big-fins))
            ))
    ))

(barplot (listener '(because (big-fins) (eq? (species) 'wug))
                   '(list wug→big-fins del→big-fins
                          hap→big-fins nik→big-fins))
         "Big fins because wug")

~~~~

With rejection query instead of enumeration:

~~~~
;;;fold:
;;first we have a bunch of helper code to do meta-transforms.. converts name to 
;;shadow-name and wraps top-level defines
(define (names model)
  (map (lambda (def)
         (if (is-function-definition? def)
             (first (second def))
             (second def)))
       model))

(define (is-function-definition? def)
  (list? (second def)))

(define (shadow-symbol name)
  (string->symbol (string-append "shadow-" name)))

(define (rename expr from-name to-name)
  (cond [(list? expr) (map (lambda (x) (rename x from-name to-name)) expr)]
        [(eq? expr from-name) to-name]
        [else expr]))

(define (shadow-rename expr name)
  (rename expr name (shadow-symbol name)))

(define (shadow-rename-all expr names)
  (if (null? names)
      expr
      (shadow-rename-all (shadow-rename expr (first names))
                         (rest names))))

(define (make-shadow-defines model)
  (define ns (names model))
  (map (lambda (def)
         (if (is-function-definition? def)
             (shadow-rename-all def ns)
             (let ([name (second def)])
               '(define ,(shadow-symbol name) (if (flip eps) 
                                                  ,(shadow-rename-all (third def) ns) 
                                                  ,name)))))
       model))

;;the meaning function constructs a church expression from an utterance. 
;;for 'because it uses quasiquote mojo to dynamically construct the right expression.
;;(in principle this handles embedded "because", but currently expand-because doesn't do 
;;the right thing since the model is a fixed global.)
(define (meaning utt)
  (define (because? u) (if (list? u) (eq? (first u) 'because) false))
  (if (list? utt)
      (if (because? utt)
          (expand-because (map meaning utt))
          (map meaning utt))
      utt))

;;expand an expr with form '(because a b), ie "a because b", into the (hypothesized) 
;;counterfactual meaning:
(define (expand-because expr) 
  (define a (second expr))
  (define b (third expr))
  '(and ,a ,b
        (rejection-query
         (define eps 0.01)
         ,@(make-shadow-defines model) ;;the shadow model
         (not ,(shadow-rename-all a (names model)))
         (condition (not ,(shadow-rename-all b (names model)))))))

;;listener is standard RSA literal listener, except we dynamically construct the 
;;query to allow complex meanings that include because:
(define listener 
  (lambda (utt qud n)
    (eval
     '(repeat ,n
              (lambda () (rejection-query
                          ,@model
                          ,qud
                          (condition ,(meaning utt))))))))

;;the speaker is no different from ordinary RSA
(define (speaker val qud) ;;want to communicate val as value of qud
  (enumeration-query
   (define utt (utt-prior))
   utt
   (condition (equal? val (apply multinomial (listener utt qud))))))

;;;

;; returns value <= p# with probability p#
;;this takes the role of a uniform function if we only care about the value's relation
;; to p1 and p2
(define (get-U-crits p1 p2)
  (multinomial (list p1 p2 1)
               (list p1 (- p2 p1)  (- 1 p2))))

(define model 
  '(
    ;; categories
    (define north:south (flip))
    (define (region) (if north:south 'north 'south))
    
    (define wug:del (flip))
    (define hap:nik (flip))
    (define (species) (case (region)
                            (('north) (if wug:del 'wug 'del))
                            (('south) (if hap:nik 'hap 'nik))))

    ;; feature-weights
    (define wug→big-fins (if (flip) .2 .8))
    (define del→big-fins (if (flip) .2 .8))
    (define hap→big-fins (if (flip) .2 .8))
    (define nik→big-fins (if (flip) .2 .8))

    ;; features
    (define U-big-fins (get-U-crits .2 .8))
    (define (big-fins)
      (case (species)
            (('wug) (<= U-big-fins wug→big-fins))
            (('del) (<= U-big-fins del→big-fins))
            (('hap) (<= U-big-fins hap→big-fins))
            (('nik) (<= U-big-fins nik→big-fins))
            ))

    ))

(hist (listener '(because (big-fins) (eq? (species) 'wug))
                '(list wug→big-fins del→big-fins
                       hap→big-fins nik→big-fins)
                100 ;; number of rejection samples
                )
      "Big fins because wug")
~~~~

#### Modeling the speaker

~~~~
;;;fold:
;;first we have a bunch of helper code to do meta-transforms.. converts name to 
;;shadow-name and wraps top-level defines
(define (names model)
  (map (lambda (def)
         (if (is-function-definition? def)
             (first (second def))
             (second def)))
       model))

(define (is-function-definition? def)
  (list? (second def)))

(define (shadow-symbol name)
  (string->symbol (string-append "shadow-" name)))

(define (rename expr from-name to-name)
  (cond [(list? expr) (map (lambda (x) (rename x from-name to-name)) expr)]
        [(eq? expr from-name) to-name]
        [else expr]))

(define (shadow-rename expr name)
  (rename expr name (shadow-symbol name)))

(define (shadow-rename-all expr names)
  (if (null? names)
      expr
      (shadow-rename-all (shadow-rename expr (first names))
                         (rest names))))

(define (make-shadow-defines model)
  (define ns (names model))
  (map (lambda (def)
         (if (is-function-definition? def)
             (shadow-rename-all def ns)
             (let ([name (second def)])
               '(define ,(shadow-symbol name) (if (flip eps) 
                                                  ,(shadow-rename-all (third def) ns) 
                                                  ,name)))))
       model))

;;the meaning function constructs a church expression from an utterance. 
;;for 'because it uses quasiquote mojo to dynamically construct the right expression.
;;(in principle this handles embedded "because", but currently expand-because doesn't do 
;;the right thing since the model is a fixed global.)
(define (meaning utt)
  (define (because? u) (if (list? u) (eq? (first u) 'because) false))
  (if (list? utt)
      (if (because? utt)
          (expand-because (map meaning utt))
          (map meaning utt))
      utt))

;;expand an expr with form '(because a b), ie "a because b", into the (hypothesized) 
;;counterfactual meaning:
(define (expand-because expr) 
  (define a (second expr))
  (define b (third expr))
  '(and ,a ,b
        (rejection-query
         (define eps 0.01)
         ,@(make-shadow-defines model) ;;the shadow model
         (not ,(shadow-rename-all a (names model)))
         (condition (not ,(shadow-rename-all b (names model)))))))

;;listener is standard RSA literal listener, except we dynamically construct the 
;;query to allow complex meanings that include because:
(define listener 
  (lambda (utt qud)
    (eval
     '(rejection-query
       ,@model
       ,qud
       (condition ,(meaning utt))))))

;;the speaker is no different from ordinary RSA
(define (speaker val qud) ;;want to communicate val as value of qud
  (rejection-query
   (define utt (utt-prior))
   utt
   (condition (equal? val (listener utt qud)))))

;;;

;; returns value <= p# with probability p#
;;this takes the role of a uniform function if we only care about the value's relation
;; to p1 and p2
(define (get-U-crits p1 p2)
  (multinomial (list p1 p2 1)
               (list p1 (- p2 p1)  (- 1 p2))))

(define model 
  '(
    ;; categories
    (define north:south (flip .5))
    (define (region) (if north:south 'north 'south))
    
    (define wug:del (flip .5))
    (define hap:nik (flip .5))
    (define (species) (case (region)
                            (('north) (if wug:del 'wug 'del))
                            (('south) (if hap:nik 'hap 'nik))))

    ;; feature-weights
    (define wug→big-fins (if (flip) .8 .2))
    (define del→big-fins (if (flip) .8 .2))
    (define hap→big-fins (if (flip) .8 .2))
    (define nik→big-fins (if (flip) .8 .2))

    ;; features
    (define U-big-fins (get-U-crits .2 .8))
    (define (big-fins)
      (case (species)
            (('wug) (<= U-big-fins wug→big-fins))
            (('del) (<= U-big-fins del→big-fins))
            (('hap) (<= U-big-fins hap→big-fins))
            (('nik) (<= U-big-fins nik→big-fins))
            ))

    ))

;;utterances can be any chrch expression includning vars from names and 'because. 
;;for now consider all the explanations and 'simpler' expressions:
(define (utt-prior) 
  (uniform-draw    
   '((because (big-fins) (eq? (species) 'wug))
     (and (big-fins) (eq? (species) 'wug))
     )))

(hist
 (repeat 10
         (lambda ()
           (speaker (list .8 .8 .2 .2)
                    '(list wug→big-fins del→big-fins
                           hap→big-fins nik→big-fins)))))
~~~~
