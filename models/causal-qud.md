---
layout: model
title: Causal QUD
---

Wednesday found that the prior probability of an explanans had no effect on the goodness of an explanation in her experiment. I initially thought that if the QUD focused in on only the causal links, the prior wouldn't matter. But it seems like in the current model, it does matter.

# Example setup

Let's say A is likely ($p_A=0.9$) and B is unlikely ($p_B=0.1$) and they both have high causal power ($c_A=c_B=0.9$).

~~~~

(define model
  '((define a (flip .9))
    (define b (flip .1))
    (define at (flip 0.9))
    (define bt (flip 0.9))
    ;; c is a deterministic function of a, b, at, and bt,
    ;; so make sure it always gets updated
    (define (c) (or (and a at) (and b bt)))))

;;;fold:
(define world-variable-names '(a b c at bt))
(define distribution
  (eval
   '(enumeration-query
     ,@model
     (list a b (c) at bt)
     #t)))

(define get-names
  (lambda (world-state)
    (apply string-append
           (map
            (lambda (on? name) (symbol->string (if on? (string-append " " name) "")))
            world-state
            world-variable-names))))

(barplot
 (list (map get-names (first distribution)) (second distribution)))
;;;


~~~~

Then the most probable state is one where B is false and everything else (causal links included) is true.

# What will the literal listener infer if they hear "C because A" or "C because B"?

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

(define model
  '((define a (flip .9))
    (define b (flip .1))
    (define at (flip 0.9))
    (define bt (flip 0.9))
    ;; c is a deterministic function of a, b, at, and bt,
    ;; so make sure it always gets updated
    (define (c) (or (and a at) (and b bt)))))
;;;

;;listener is standard RSA literal listener, except we dynamically construct the query to allow complex meanings that include because:
(define listener
  (mem (lambda (utt qud)
         (eval
          '(enumeration-query
            ,@model
            ,qud
           (condition ,(meaning utt)))))))

(define because-a (listener '(because (c) a) '(list a b (c) at bt)))

;;;fold:

(define world-variable-names '(a b c at bt))

(define get-names
  (lambda (world-state)
    (apply string-append
           (map
            (lambda (on? name) (symbol->string (if on? (string-append " " name) "")))
            world-state
            world-variable-names))))

(barplot
 (list (map get-names (first because-a)) (second because-a)) "C because A")
;;;


~~~~

When the literal listener hears, "C because A", that's consistent with his prior distribution (If A hadn't happened, C would not have happened -- B was not there to trigger C), except that the a priori unlikely situations where B and bt happened to both be true are now impossible. This strengthens the most likely world state, which is still one in which everything except B is true.

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

(define model
  '((define a (flip .9))
    (define b (flip .1))
    (define at (flip 0.9))
    (define bt (flip 0.9))
    ;; c is a deterministic function of a, b, at, and bt,
    ;; so make sure it always gets updated
    (define (c) (or (and a at) (and b bt)))))

;;listener is standard RSA literal listener, except we dynamically construct the query to allow complex meanings that include because:
(define listener
  (mem (lambda (utt qud)
         (eval
          '(enumeration-query
            ,@model
            ,qud
           (condition ,(meaning utt)))))))
;;;

(define because-b (listener '(because (c) b) '(list a b (c) at bt)))

;;;fold:

(define world-variable-names '(a b c at bt))

(define get-names
  (lambda (world-state)
    (apply string-append
           (map
            (lambda (on? name) (symbol->string (if on? (string-append " " name) "")))
            world-state
            world-variable-names))))

 (barplot
 (list (map get-names (first because-b)) (second because-b)) "C because B")
;;;

~~~~

The a priori most likely world state is not, however, consistent with "C because B". If the listener hears "C because B", he will infer that either A was false or at was false. He won't necessarily infer that bt is true (since maybe bt was false, but in the counterfactual world A and at both got turned on), but he's very likely to. The listener has about a 50% chance of inferring at is true, since at being false is just as likely as A being false and both are possible reasons why B being false would result in C being false.

#Speaker

So if the speaker has two choices of what to say ("C because A" or "C because B") and the speaker wants to communicate that both at and bt are turned on, she should choose the "C because A" more often, because then the listener will more likely infer her intended meaning.

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

(define model
  '((define a (flip .9))
    (define b (flip .1))
    (define at (flip 0.9))
    (define bt (flip 0.9))
    ;; c is a deterministic function of a, b, at, and bt,
    ;; so make sure it always gets updated
    (define (c) (or (and a at) (and b bt)))))

;;listener is standard RSA literal listener, except we dynamically construct the query to allow complex meanings that include because:
(define listener
  (mem (lambda (utt qud)
         (eval
          '(enumeration-query
            ,@model
            ,qud
           (condition ,(meaning utt)))))))
;;;

(define (utt-prior) (uniform-draw '((because (c) a)
                                    (because (c) b))))

(define (speaker val qud) ;;want to communicate val as value of qud
  (enumeration-query
   (define utt (utt-prior))
   utt
   (condition (equal? val (apply multinomial (listener utt qud))))))

(barplot (speaker '(#t #t) '(list at bt)) "Speaker Distribution")

~~~~

So even if we *only* care about communicating the causal links, we can still be affected by the prior probabilities of the explanantia.

# common ground

Even if we set the values of A, B, and C in the actual world to be in common ground, the counterfactual world still samples from the prior distribution in order to make the counterfactual condition true, and so the priors will affect the speaker's choice.

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

(define model
  '((define a (flip .9))
    (define b (flip .1))
    (define at (flip 0.9))
    (define bt (flip 0.9))
    ;; c is a deterministic function of a, b, at, and bt,
    ;; so make sure it always gets updated
    (define (c) (or (and a at) (and b bt)))))
;;;

;;listener is standard RSA literal listener, except we dynamically construct the query to allow complex meanings that include because:
(define listener
  (mem (lambda (utt qud)
         (eval
          '(enumeration-query
            ,@model
            ,qud
           ;; imagine the speaker knows that the listener knows that A, B, and C are true in the actual world.
           (condition (and a b (c)))
           (condition ,(meaning utt)))))))

(define (utt-prior) (uniform-draw '((because (c) a)
                                    (because (c) b))))

(define (speaker val qud) ;;want to communicate val as value of qud
  (enumeration-query
   (define utt (utt-prior))
   utt
   (condition (equal? val (apply multinomial (listener utt qud))))))

(barplot (speaker '(#t #t) '(list at bt)) "Speaker Distribution")

~~~~