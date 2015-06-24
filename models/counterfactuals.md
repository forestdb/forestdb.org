---
layout: model
title: Counterfactuals
model-language: church
---

### Pragmatic explainer

We model an informative speaker trying to comunicate the world state (all top-level vars in a model) to a literal listener.


#### The literal meaning of "because"

The meaning of "A because B" is both the presuppositions -- "A" and "B" -- and the counterfactual "if B had not happened then A would not have happened". The counterfactual is implemented by creating "shadow" world where each top-level (i.e. defined) variable in the model has its actual-world value with `eps` probability and a fresh value otherwise. In this shadow world we evaluate a codnitional query to see if "shadow-A" changes when "shadow-B" is conditioned to be false.

To construct the shadow-world query we use a bunch of helpers for splicing, wrapping deifnitions, and renaming variables. Here it is, with an example:

~~~~
;;;
;;first we have a bunch of helper code to do meta-transforms.. converts name to shadow-name and wraps top-level defines
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

(define model 
  '((define a (flip .2))
    (define c (flip .5))
    (define b (flip (if (or a c) 0.9 0.1)))))

(display (make-shadow-defines model))
~~~~

Now the meaning of "because" is simply constructing the shadow-world query using these helpers:

~~~~
;;;fold:
;;first we have a bunch of helper code to do meta-transforms.. converts name to shadow-name and wraps top-level defines
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
;;;

;;expand an expr with form '(because a b), ie "a because b", into the (hypothesized) counterfactual meaning:
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

(define model 
  '((define a (flip .2))
    (define c (flip .5))
    (define b (flip (if (or a c) 0.9 0.1)))))

(display (expand-because '(because a b)))
(display (expand-because  '(because a (and b c))))
~~~~

We can now plug this into the standard RSA literal listener that conditions on the lieral meaning of an utterance being true. We dynamically construct the listener query and then eval it, in order to use the previous setup to construct the meaning expression.

~~~~
;;;fold:
;;first we have a bunch of helper code to do meta-transforms.. converts name to shadow-name and wraps top-level defines
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
;;;

;;the meaning function constructs a church expression from an utterance. 
;;for 'because it uses quasiquote mojo to dynamically construct the right expression.
;;(in principle this handles embedded "because", but currently expand-because doesn't do the right thing since the model is a fixed global.)
(define (meaning utt)
  (define (because? u) (if (list? u) (eq? (first u) 'because) false))
  (if (list? utt)
      (if (because? utt)
          (expand-because (map meaning utt))
          (map meaning utt))
      utt))

;;expand an expr with form '(because a b), ie "a because b", into the (hypothesized) counterfactual meaning:
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

;;listener is standard RSA literal listener, except we dynamically construct the query to allow complex meanings that include because:
(define listener 
  (mem (lambda (utt)
         (eval
          '(enumeration-query
            ,@model
           (define state (list ,@(names model))) ;;all the named vars
           state
           (condition ,(meaning utt)))))))

;; put model into global scope:
(define model 
  '((define a (flip .2))
    (define c (flip .5))
    (define b (flip (if (or a c) 0.9 0.1)))))

(barplot (listener '(because b a)) "interpretation of b because a")
(barplot (listener '(because b c)) "interpretation of b because c")
~~~~

#### The pragmatic explainer

Now that we have a literal listener that can understand "because", we can make a standard RSA speaker who is trying to communicate the state of the world and can use "because" explanations:

~~~~
;;;fold:
;;first we have a bunch of helper code to do meta-transforms.. converts name to shadow-name and wraps top-level defines
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
;;(in principle this handles embedded "because", but currently expand-because doesn't do the right thing since the model is a fixed global.)
(define (meaning utt)
  (define (because? u) (if (list? u) (eq? (first u) 'because) false))
  (if (list? utt)
      (if (because? utt)
          (expand-because (map meaning utt))
          (map meaning utt))
      utt))

;;expand an expr with form '(because a b), ie "a because b", into the (hypothesized) counterfactual meaning:
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

;;listener is standard RSA literal listener, except we dynamically construct the query to allow complex meanings that include because:
(define listener 
  (mem (lambda (utt)
         (eval
          '(enumeration-query
            ,@model
           (define state (list ,@(names model))) ;;all the named vars
           state
           (condition ,(meaning utt)))))))

;;;

;;;;;;
;;the speaker is no different from ordinary RSA
(define (speaker state) ;;state is value for each deifned var in model (or whatever QUD says it should be)
  (enumeration-query
   (define utt (utt-prior))
   utt
   (condition (equal? state (apply multinomial (listener utt))))))

;;utterances any expression that can use vars from names and 'because. for now a few alternate becauses:
(define (utt-prior) (uniform-draw '((because b a) (because b c))))

;; put model into global scope:
(define model 
  '((define a (flip .2))
    (define c (flip .5))
    (define b (flip (if (or a c) 0.9 0.1)))))

(barplot (speaker (list true true true)) "utterance if all vars true")
~~~~

Ta-da!

Next to understand if this meaning for "because" actually reflects people's intuitive understanding...
