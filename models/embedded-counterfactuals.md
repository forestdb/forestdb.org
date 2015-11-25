---
layout: model
title: Counterfactuals
model-language: church
---

Should counterfactual reasoning invlove an embedded query?

For the following background knowlege and counterfactual inference, these can give different counterfactual predicitons.

Here's the functional causal model of the situation:

![story5.png](../assets/img/story5.png)

A, B, C, and D are all true in the actual world. Counterfactually, if C hadn't been true, then would B have been true?

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

(define eps 0.47) ;; our eps is P(new), L&K "stickiness" is P(old)

(define model 
  '((define uA (flip .75))
    (define uB (flip .75))
    (define uBC (flip .9))))

(define a 'uA)
(define b 'uB)

;; if a is true, so is c. if a is false but b is true, c is true 90% of the time.
(define c '(or ,a (and ,b uBC)))

;; our version
(display (eval
          '(enumeration-query
            ,@model ;; sample actual world

            (condition (and ,a ,b ,c)) ;; condition on actual world states

            ;; return result of querying CF "b" given that CF "c" is not true
            (apply multinomial
                   (enumeration-query
                    ,@(make-shadow-defines model) ;; sample shadow world
                    (condition (not ,(shadow-rename-all c (names model))))
                    ,(shadow-rename-all b (names model)))))))

;; L&K version
(display (eval
          '(enumeration-query
            ,@model ;; sample actual world
            ,@(make-shadow-defines model) ;; sample shadow world

            (condition (and
                        ;; condition on actual world states
                        ,a ,b ,c 
                        ;; at the same time, condition on CF "c" is not true
                        (not ,(shadow-rename-all c (names model)))))

            ,(shadow-rename-all b (names model)))))
~~~~

Lucas & Kemp reported 0.43 as the prediction of their model in this case.

## Notes/Questions

* How do we deal with somehting like, "A because (B because C)" if we don't have an embedded inference?
	- Maybe we could have `shadow-vars` *and* `shadow-shadow-vars`.
* In what kinds of situations would there be a greater difference between the two kinds of models? What's driving this difference?
* Which of these is a better fit to the data?