---
layout: model
title: Simple Causal Explanations
---

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
  (define (and? u) (if (list? u) (eq? (first u) 'and) false))
  (if (list? utt)
      (if (and? utt) '(and (unquote (meaning (second utt))) (unquote (meaning (third utt))))

          (if (because? utt)
              (expand-because (map meaning utt))
              (map meaning utt)))
      utt))

;;expand an expr with form '(because a b), ie "a because b", into the (hypothesized) counterfactual meaning:
(define (expand-because expr)
  (define a (second expr))
  (define b (third expr))
  (define (and? u) (if (list? u) (eq? (first u) 'and) false))
  (if (and? b) 
      (lambda() ((define c (second b)) 
                 (define d (third b))))
      b)
  '(and ,a 
        ,b
        (apply multinomial
               (enumeration-query
                (define eps 0.001)
                ,@(make-shadow-defines model) ;;the shadow model
                (not ,(shadow-rename-all a (names model)))
                (condition (not ,(shadow-rename-all b (names model))))))))
;;;

;;listener is standard RSA literal listener, except we dynamically construct the query to allow complex meanings that include because:
(define listener
  (mem (lambda (utt qud)
         (eval
          '(enumeration-query
            ,@model
            ,qud
           (condition ,(meaning utt)))))))

;;;;;;
;;the speaker is no different from ordinary RSA
(define (speaker val-fns qud) ;;want to communicate val as value of qud
  (enumeration-query
   (define val (map (lambda (x) (x)) val-fns))
   (define utt (utt-prior))
   utt
   (condition (equal? val (apply multinomial (listener utt qud))))))

;;utterances can be any chrch expression includning vars from names and 'because.
;;for now consider all the explanations and 'simpler' expressions:
(define (utt-prior) (uniform-draw '((because c a)
                                    (because c b)
                                    (because c (and a b))
                                  )))


(define pragmatic-listener
  (mem (lambda (utt qud)
         (eval
          '(enumeration-query
            ,@model
            (define val ,qud)
            val
           (equal? utt (apply multinomial (speaker (map (lambda (x) (lambda () x)) val) qud))))))))

;; put model into global scope:
(define model
  '((define a (flip .9))
    (define b (flip .1))
    (define at (flip 0.9))
    (define bt (flip 0.1))
    (define c (or (and a at) (and b bt)))))

(define (return-true) true)
(define (uncertain-.9) (flip .9))
(define (uncertain-.1) (flip .1))

(barplot (speaker (list return-true return-true return-true uncertain-.9 uncertain-.1) 
                  '(list a b c at bt)) "A, B, and C; unknown transmissions")

(barplot (pragmatic-listener '(because c a) '(list a b c at bt))"C because A")
(barplot (pragmatic-listener '(because c b) '(list a b c at bt)) "C because B")
(barplot (pragmatic-listener '(because c (and a b)) '(list a b c at bt)) "C because A and B")

~~~

In the world model above, A has a high prior probability and a high probability of causing 
C. B has a low prior probability and a low probability of causing C.

In this model, instead of knowing for sure the entire state of the world, the speaker instead
makes an inference about the cause of C, and then attempts to communicate that world state.

The speaker model prefers to produce the utterance "C because A and B" over the alternatives
"C because B" and "C because A". How is this behavior produced?

Let's start with the literal meaning of because. Suppose the speaker had chosen to say
"C because A". First, we make sure that A and C are true in the real world. Then, we go to
the counterfactual world and see if C is not true when A is not true, assuming that the other
variables retain their values from the real world. Where does this leave us? If 
