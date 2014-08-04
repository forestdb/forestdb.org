---
layout: model
title: Counterfactuals
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



### Older versions

This is a version of the above literal listener, who conditions on counterfactual statements, that doesn't need to do quite so much quasi-quote magic:

    (define (begin a b)
      b)
    
    (define (is-function-definition? def)
      (list? (second def)))
    
    (define (shadow name)
      (string->symbol (string-append "shadow-" name)))
    
    (define (rename expr from-name to-name)
      (cond [(list? expr) (map (lambda (x) (rename x from-name to-name)) expr)]
            [(eq? expr from-name) to-name]
            [else expr]))
    
    (define (shadow-rename expr name)
      (rename expr name (shadow name)))
    
    (define (shadow-rename-all expr names)
      (if (null? names)
          expr
          (shadow-rename-all (shadow-rename expr (first names))
                             (rest names))))
    
    (define (get-names defines)
      (map (lambda (def)
             (if (is-function-definition? def)
                 (first (second def))
                 (second def)))
           defines)) 
    
    (define (make-shadow-defines defines names)
      (map (lambda (def)
             (if (is-function-definition? def)
                 (shadow-rename-all def names)
                 (let ([name (second def)])
                   (list 'define 
                         (shadow name) 
                         (list 'if '(flip eps)
                               (shadow-rename-all (third def) names)
                               name
                               )))))
           defines))
    
    (define (make-counterfactual-query defines query-expr antecedent consequent)
      (let* ([names (get-names defines)]
             [shadow-defines (make-shadow-defines defines names)]
             [new-query
              (append (list 'enumeration-query
                            '(define eps .01))
                      defines
                      (list
                       (list 
                        'define 'cf-statement
                        (list 'apply 'multinomial
                              (append 
                               '(enumeration-query)
                               shadow-defines
                               (list (list 'not (shadow consequent)))
                               (list (list 'condition (list 'not (shadow antecedent)))))))
                       query-expr
                       (list 'condition (list 'and antecedent consequent 'cf-statement))))])
        (begin
         (console-log new-query)
         new-query)))
    
    
    ;; Comparing counterfactual to conditioning on antecedent and consequent:
    
    (define (test-counterfactual model query-expr antecedent consequent)
      (barplot
       (eval
        (append '(enumeration-query)
                model
                (list query-expr
                      (list 'and antecedent consequent))))
       "Without counterfactual condition")
    
      (barplot
       (eval
        (make-counterfactual-query model 
                                   query-expr 
                                   antecedent
                                   consequent
                                   ))
       "With counterfactual condition"))
    
    
    ;; -------------------------------------------------------------
    ;; Example 1
    
    (define my-model-1
      '((define a (flip .2))
        (define c (flip .2))
        (define b (flip (if (or a c) 0.9 0.1)))))
    
    (define my-query-expr-1
      '(list a b c))
    
    (define my-antecedent-1 'a)
    
    (define my-consequent-1 'b)
    
    
    ;; -------------------------------------------------------------
    ;; Example 2
    
    (define my-model-2
      '(
    
        (define (strength) (uniform-draw '(0 5 10)))
        (define (lazy) (flip))
        (define (pulling str laz) (if laz (/ str 2) str))
        (define alice-strength (strength))
        (define alice-lazy (lazy))
        (define alice-pulling (pulling alice-strength alice-lazy))
        (define bob-strength (strength))
        (define bob-lazy (lazy))
        (define bob-pulling (pulling bob-strength bob-lazy))
        (define alice-win (>= alice-pulling bob-pulling))
        (define alice-stronger-than-bob (> alice-strength bob-strength))
        
        ))
    
    (define my-query-expr-2
      'bob-lazy)
    
    (define my-antecedent-2 'alice-stronger-than-bob)
    
    (define my-consequent-2 'alice-win)
    
    
    ;; Run example
    
    (test-counterfactual my-model-1
                         my-query-expr-1
                         my-antecedent-1
                         my-consequent-1)

Previous version:

    (define (lookup key list-of-pairs)
      (rest (assoc key list-of-pairs)))
    
    
    (define (run-world world conditioner epsilon)
    
      (define (wrap f)
        (lambda args
          (let* ([name (first args)]
                 [func-args (rest args)])
            (if (or (= epsilon 1.0) (flip epsilon))
                (apply f func-args)
                (lookup name world)))))
              
      (define flip0 (wrap flip))
    
      (enumeration-query
    
       (define A (flip0 'A .2))
       (define B (flip0 'B .8))   
       (define E (or A B))
    
       (make-world A B E)
    
       (conditioner A B E)))

    ;; Helper function
    
    (define (make-world A B E)
      (list (pair 'A A)
            (pair 'B B)
            (pair 'E E)))
    
    
    ;; Prior on worlds
    
    (define (empty-condition A B E) 
      #t)
    
    (barplot (run-world '() empty-condition 1.0)
             "Prior on worlds")
    
    
    ;; Conditioning on the actual world
    
    (define (observation-condition A B E)
      (and (not A) (not B) (not E))) ;; Could use noisy conditioning here
    
    (barplot (run-world '() observation-condition 1.0)
             "Conditioned on actual world (A=0 B=0 E=0)")
    
    
    ;; Counterfactuals
    
    (define actual-world (make-world #f #f #f))
    
    (define (intervention-condition A B E)
      E)
    
    (define epsilon .05)
    
    (barplot (run-world actual-world intervention-condition epsilon)
             "Counterfactual worlds for intervention E=1")
