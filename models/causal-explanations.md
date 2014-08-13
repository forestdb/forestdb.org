---
layout: model
title: Simple Causal Explanations
---

###Explanations in a causal world with non-deterministic links

~~~~
;;;;
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
  '(and ;,a 
        ;,b
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
  ;;compute values of variables under discussion
   (define val (map (lambda (x) (x)) val-fns))
   (define utt (utt-prior))
   utt
   (condition (equal? val (apply multinomial (listener utt qud))))))

(define (pragmatic-speaker val-fns qud) ;;want to communicate val as value of qud
  (enumeration-query
  ;;compute values of variables under discussion
   (define val (map (lambda (x) (x)) val-fns))
   (define utt (utt-prior))
   utt
   (condition (equal? val (apply multinomial (pragmatic-listener utt qud))))))

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
    (define b (flip .9))
    ;;are the causal links between a & c and b & c on?
    (define at (flip .1))
    (define bt (flip .1))
    ;;if either variable and its transmission are both on, c happens
    (define c (or (and a at) (and b bt)))))

;;functions to get things the speaker knows
(define (return-true) true)
(define (uncertain-.9) (flip .9))
(define (uncertain-.1) (flip .1))
(define (return-high) .9)
(define (return-low) .1)

(barplot (speaker (list return-true return-true return-true uncertain-.1 uncertain-.1) 
                  '(list a b c at bt)) "A, B, and C")

(barplot (pragmatic-speaker (list return-true return-true return-true uncertain-.1 uncertain-.1) 
                  '(list a b c at bt)) "[pragmatic speaker] A, B, and C")

(barplot (listener '(because c a) '(list a b c at bt)) "C because A")
(barplot (pragmatic-listener '(because c a) '(list a b c at bt)) "[prag] C because A")
(barplot (listener '(because c b) '(list a b c at bt)) "C because B")
(barplot (pragmatic-listener '(because c b) '(list a b c at bt)) "[prag] C because B")
(barplot (listener '(because c (and b a)) '(list a b c at bt)) "C because A&B")
(barplot (pragmatic-listener '(because c (and b a)) '(list a b c at bt)) "[prag] C because A&B")


~~~~

In the world model above, A has a high prior probability and a high probability of causing 
C. B has a low prior probability and a low probability of causing C.

In this model, instead of knowing for sure the entire state of the world, the speaker instead
makes an inference about the cause of C, and then attempts to communicate that world state.

The speaker model prefers to produce the utterance "C because A and B" over the alternatives
"C because B" and "C because A". How is this behavior produced?

Let's start with the literal meaning of because. Suppose the speaker had chosen to say
"C because A". First, we make sure that A and C are true in the real world. Then, we go to
the counterfactual world and see if C is not true when A is not true, assuming that the other
variables retain their values from the real world. Since B has a low prior, B is most likely
to not be true in the worlds we generate. When B is not true, making A not true guarantees
that C will not be true, thus satisfying the counterfactual. However, the speaker knows that
B was true in the real world, so that is why "C because A" is a bad utterance to produce
given that the goal is to make the listener infer the state of the world.

Instead suppose the speaker says "C because B". Why is this a better option? Now when we
sample worlds, we require that both C and B be true. However, A is also likely to be true
since it has a high prior. (Remember that the causal link from A to C needs to be off in order
for the counterfactual not-C given not-B to be true, but this is independent of the value of
A.) This leads listeners to the inference that A, B, and C are all true, which was the speaker's
original communicative intent.

"C because A and B" is deemed the best explanation by the model. It is fairly obvious to 
see why--we require A, B, and C to all be true, and then we go into the counterfactual
world and check that if A and B had not happened (that is, (not (and a b))). Since we a priori
think that the transmission from B to C is off, it is easy to construct worlds where all the 
variables except for B transmission are true, and these will satisfy the counterfactual
condition.
