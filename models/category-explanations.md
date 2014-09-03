---
layout: model
title: Category explanations
---

*All models on this page use [the exogenous randomness style](http://forestdb.org/models/exogenous-counterfactuls.html) of counterfactual modeling based on [the original countefactual Church model](http://forestdb.org/models/because.html) and the counterfactuals of Pearl (2000).

## Fish

Below is a model representing a simple taxonomy of fish. Fish come in the northern and southern varieties, and each region has two species: wugs and dels, and haps and niks respectively [(diagram)](imgur.com/TODO). In the model below, the listener interprets `(because (big-fins) (eq? (species) 'wug))`—*"it has big fins because it's a wug"*. 

~~~~
;; runs in ~60 seconds
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
;; this takes the role of a uniform function if we only care about the value's relation
;; to the critical values used as input
(define (get-U p1 p2)
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
    (define U-big-fins (get-U .2 .8))
    (define (big-fins)
      (case (species)
            (('wug) (<= U-big-fins wug→big-fins))
            (('del) (<= U-big-fins del→big-fins))
            (('hap) (<= U-big-fins hap→big-fins))
            (('nik) (<= U-big-fins nik→big-fins))
            ))

    ))


(hist
 (repeat 300
         (lambda ()
           (listener '(because (big-fins) (eq? (species) 'wug))
                     '(list wug→big-fins del→big-fins
                            hap→big-fins nik→big-fins))))
 "\"big fins because wug\"")

;;utterances can be any chrch expression includning vars from names and 'because. 
;;for now consider all the explanations and 'simpler' expressions:
(define (utt-prior) 
  (uniform-draw    
   '((because (big-fins) (eq? (species) 'wug))
     (because (big-fins) (eq? (region) 'north))
     )))

(hist
 (repeat 30
         (lambda ()
           (speaker (list .8 .8 .2 .2)
                    '(list wug→big-fins del→big-fins
                           hap→big-fins nik→big-fins))))
 "wugs and dels have stripes")
(hist
 (repeat 30
         (lambda ()
           (speaker (list .8 .2 .2 .2)
                    '(list wug→big-fins del→big-fins
                           hap→big-fins nik→big-fins))))
 "wugs have stripes")
~~~~

Upon hearing "big-fins because stripes," the model ranks the probability that dels (the other northern species) have big-fins as lower than for either of the southern species. It is equally likely for both southern fish to have big fins as it is for just  dels to have big fins. This follows my initial intuition that the categorical counterfactual is interpreted in terms of the super-category: if a wug weren't a wug, it would most likely be a del. Thus, anything you say about the counter-factual wug that isn't a wug most strongly affects the representation of dels. This effect comes to play with the speaker as well. The speaker perfers to say "because north" when weights are high for both northern species, but "because wug" when weights are high for wugs only.

### Graded predictions

We see the predictions roughly match our intuitions for these edge cases. Now let's look at how the model performs when we vary the probabilities more continuously. Below is a copy of the above model but with four possible weights. Unfortunately, creating these possibilities makes the model very slow. Generating 500 samples for each of the four conditions took several hours. The model below is only for reference. I have not run a full statistical analysis yet, so I am hesitant to provide a graph, but I will provide my results in the following table.

% dels with big fins    |    % "because north"
:-----------: | :-----------:
0.2 | 0.32
0.4 | 0.45
0.6 | 0.53
0.8 | 0.65

~~~~
;; runs in ~25 seconds
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
(define (get-U p1 p2 p3 p4)
  (multinomial (list p1 p2 p3 p4  1)
               (list p1 (- p2 p1) (- p3 p2) (- p4 p3) (- 1 p4))))

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
    (define wug→big-fins (uniform-draw '(.2 .4 .6 .8)))
    (define del→big-fins (uniform-draw '(.2 .4 .6 .8)))
    (define hap→big-fins (uniform-draw '(.2 .4 .6 .8)))
    (define nik→big-fins (uniform-draw '(.2 .4 .6 .8)))

    ;; features
    (define U-big-fins (get-U .2 .4 .6 .8))
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
     (because (big-fins) (eq? (region) 'north))
     )))

(hist
 (repeat 1
         (lambda ()
           (speaker (list .8 .8 .2 .2)
                    '(list wug→big-fins del→big-fins
                           hap→big-fins nik→big-fins))))
 "p(big-fins|wug) = 0.8")

(hist
 (repeat 1
         (lambda ()
           (speaker (list .8 .6 .2 .2)
                    '(list wug→big-fins del→big-fins
                           hap→big-fins nik→big-fins))))
 "p(big-fins|wug) = 0.6")

(hist
 (repeat 1
         (lambda ()
           (speaker (list .8 .4 .2 .2)
                    '(list wug→big-fins del→big-fins
                           hap→big-fins nik→big-fins))))
 "p(big-fins|wug) = 0.4")

(hist
 (repeat 1
         (lambda ()
           (speaker (list .8 .2 .2 .2)
                    '(list wug→big-fins del→big-fins
                           hap→big-fins nik→big-fins))))
 "p(big-fins|wug) = 0.2")
~~~~
