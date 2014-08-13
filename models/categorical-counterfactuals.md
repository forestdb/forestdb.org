---
layout: model
title: Categorical counterfactuals
---

Suppose we have a category structure such that `A` and `B` are categories, and such that there is a single Boolean feature with a value that depends on the category.

### Deterministic dependencies

Let's first consider deterministic dependence:

~~~
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

(define model 
  '(   
    
    (define category
      (if (flip) 'A 'B))
        
    (define feature-on
      (if (eq? category 'A)
          #t
          #f))
    
    ))

;; "feature is on because category is A"
(define a 'feature-on)
(define b '(eq? category 'A))

(barplot
 (eval
  '(enumeration-query
    ,@model
    
    (apply multinomial
           (enumeration-query
            (define eps 0.01)
            ,@(make-shadow-defines model) ;;the shadow model
            (not ,(shadow-rename-all a (names model)))
            (condition (not ,(shadow-rename-all b (names model))))))

    (and ,a ,b))))
~~~

This doesn't look right -- we expect that the counterfactual statement ("if it weren't in category A, then the feature wouldn't be on") is `true` with probability 1.

For deterministic dependencies, we can achieve this by turning the dependent variable into a function:

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

(define model 
  '(   
    
    (define category
      (if (flip) 'A 'B))
        
    (define (feature-on)
      (if (eq? category 'A)
          #t
          #f))
    
    ))

(define a '(feature-on))
(define b '(eq? category 'A))

(barplot
 (eval
  '(enumeration-query
    ,@model
    
    (apply multinomial
           (enumeration-query
            (define eps 0.01)
            ,@(make-shadow-defines model) ;;the shadow model
            (not ,(shadow-rename-all a (names model)))
            (condition (not ,(shadow-rename-all b (names model))))))

    (and ,a ,b))))
~~~~

Functions always get resampled. Now, the counterfactual statements ("if it weren't in category A, then the feature wouldn't be on") is #t with probability 1.

### Stochastic dependencies

What about stochastic dependencies? In this case, we also have the two options shown above -- we could always resample (by expressing `feature-on` as a function), or we could only resample with probability `epsilon`. 

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

;; Version 1

(define model 
  '(   

    (define category
      (if (flip) 'A 'B))

    (define (feature-on)
      (flip
       (case category
             (('A) .6)
             (('B) .4))))

    ))

(define a '(feature-on))
(define b '(eq? category 'A))

(barplot
 (eval
  '(enumeration-query
    ,@model

    (apply multinomial
           (enumeration-query
            (define eps 0.01)
            ,@(make-shadow-defines model) ;;the shadow model
            (not ,(shadow-rename-all a (names model)))
            (condition (not ,(shadow-rename-all b (names model))))))

    (and ,a ,b))))
~~~~

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

;; Version 2

(define model 
  '(   

    (define category
      (if (flip) 'A 'B))

    (define feature-on
      (flip
       (case category
             (('A) .6)
             (('B) .4))))

    ))

(define a 'feature-on)
(define b '(eq? category 'A))

(barplot
 (eval
  '(enumeration-query
    ,@model

    (apply multinomial
           (enumeration-query
            (define eps 0.01)
            ,@(make-shadow-defines model) ;;the shadow model
            (not ,(shadow-rename-all a (names model)))
            (condition (not ,(shadow-rename-all b (names model))))))

    (and ,a ,b))))
~~~~

The variable-based approach (version 2) gives an obviously wrong answer. The function-based approach is not obviously crazy, but still doesn't seem right -- it seems more natural to resample `feature-on` based on the strength of the dependence on `category`: 

- If category and feature are independent,  we should resample `feature-on` with the prior probability `epsilon` even if we update `category`. 
- If `feature-on` deterministically depends on `category`, we should resample it whenever we change `category`. 
- If the dependence is somewhere in between, we should resample with a probability that lies in between the two extremes.

### Towards a solution

Here is one approach based on moving all randomness into independent random variables, and implementing all dependencies using deterministic variables (compare to Pearl's counterfactuals):

~~~~
;;;fold: various
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

(define model 
  '(   

    (define category
      (if (flip) 'A 'B))
    
    (define U (uniform 0 1))

    (define (feature-on)
      (case category
            (('A) (< U .6))
            (('B) (< U .4))))

    ))

(define a '(feature-on))
(define b '(eq? category 'A))

(hist
 (eval
  '(repeat 500
           (lambda () (rejection-query
                       ,@model

                       (rejection-query
                        (define eps 0.01)
                        ,@(make-shadow-defines model) ;;the shadow model
                        (not ,(shadow-rename-all a (names model)))
                        (condition (not ,(shadow-rename-all b (names model)))))

                       (and ,a ,b))))))
~~~~

To make this more efficient (without loss of accuracy), we can discretize the uniform choice:

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

(define model 
  '(   
    
    ;; (define (make-n-correlated-coins ps)
    ;; ...) ;; ps in ascending order
    
    (define category
      (if (flip) 'A 'B))
    
    (define U (multinomial '(u1 u2 u3)    ;; '(.4 (- .6 .4) (- 1.0 .6))
                           '(.4 .2 .4)))  ;; u1: A and B false
    
    (define (feature-on)
      (case category
            (('A) (or (eq? U 'u1)
                      (eq? U 'u2)))
            (('B) (eq? U 'u1))))

    ))

(define a '(feature-on))
(define b '(eq? category 'A))

(barplot
 (eval
  '(enumeration-query
    ,@model

    (apply multinomial
           (enumeration-query
            (define eps 0.01)
            ,@(make-shadow-defines model) ;;the shadow model
            (not ,(shadow-rename-all a (names model)))
            (condition (not ,(shadow-rename-all b (names model))))))

    (and ,a ,b))))
~~~~

### Testing the model

Now we can compare this new Pearl-esque style to the original stlye with randomness embedded in the functions. To see the effect at the listener and speaker levels, we need to look at variables with mediated dependences because the interpretation of `(because b a)` entails both `a` and `b`. The two models below are parallel instantiations of a simple a→b→c causal chain. The values of each node as well as the weights between the nodes are unknown variables that the listener must discover.

#### Old

~~~
;; runs in ~ 2 seconds
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
;;;

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


(define background .1)
;; put model into global scope:
(define model 
  '(
    (define a→b (if (flip) .8 .3))
    (define b→c (if (flip) .8 .3))
    
    (define a (flip))
    (define b (flip (if a
                        a→b
                        background)))
    (define c (flip (if b
                        b→c
                        background)))
    ))
(barplot (listener '(because c a) '(list b '_ a→b b→c))
         "Interpretation of c because a")
(barplot (listener '(and c a) '(list b '_ a→b b→c))
         "Interpretation of c and a")

(define (utt-prior) (uniform-draw '((because c a) (because c b)
                                     (and c a) (and c b))))
(barplot (speaker (list .8 .8 #t #t #t) '(list a→b b→c a b c))
         "Utterance given a→b and b→c are .8 and a, b, c")

~~~

#### Pearl-esque



~~~
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
(define background .1)

;; returns p1 with prob p1, and p2 with prob p1-p2
;; this takes the role of a uniform function if we only care about the value's relation
;; to p1 and p2
(define (get-U p1 p2)
  (multinomial (list p1 p2 1)
               (list p1 (- p2 p1)  (- 1 p2))))

(define model 
  '(
    ;; causal strengths
    (define a→b (if (flip) .8 .3))
    (define b→c (if (flip) .8 .3))

    (define a| (flip)) ;; probability goes outside of function
    (define (a) a|) ;; all nodes are deterministic functions

    (define U (get-U background a→b))
    (define (b) (if (a) (<= U a→b) (<= U background)))

    (define U2 (get-U background b→c))
    (define (c) (if (b) (<= U2 b→c) (<= U2 background)))

    ))
(barplot (listener '(because (c) (a)) '(list (b) a→b b→c))
         "Interpretation of c because a")
(barplot (listener '(and (c) (a)) '(list (b) a→b b→c))
         "Interpretation of c and a")

(define (utt-prior) (uniform-draw '((because (c) (a)) (because (c) (b)) 
                                    (and (c) (a)) (and (c) (b)))))
(barplot (speaker (list .8 .8 #t #t #t) '(list a→b b→c (a)(b)(c)))
         "Utterance given a→b and b→c are .8 and a, b, c")

~~~

This preliminary comparison demonstrates that the Pearl-esque style produces superior predictions to the original style. `(and c a)` have the same interpretation in each model, indicating that the models are similar in their basic causal structure. The interpretation of `(because c a)` however is quite different. Pearl's interpretation is very close to the `and` interpretation except that states in which `b` is false are less probable. This reflects the intuition that `a` can only have caused `c` if `b` was also true. The original style, conversely, makes no general prediction about the value of `b`, giving high weight to the situation in which both causal strengths are low and b is false.

When we examine the speaker, we see the greatest discrepency `(because c a)`, which is the most probable in the Pearl-esque model and least probable in the original model. I argue that Pearl's predictions more closely reflect our intuitive understanding. `(because c a)` is highly informative because it gives information about all variables: (a) and (c) by default and a→b, b→c, and (b) by the counterfactual. To ground this in a physical example, we can interpret `a` to be the knocking over of a vase, `b` to be the vase falling off the table, and `c` to be the vase breaking. In this example, saying "the vase broke because it was knocked over" seems to be a very good explanation. It is unlikely that the QUD in such a situation would be the entire state as it is in the above model, however. We must construct more complex, psychologically plausible,  models in order to better distinguish the two model types.

A final issue is that the Pearl-esque model only weakly prefers `(because c a). This may be specific to this situation, in which simply asserting the truth of any two nodes makes the full state very probable. Creating more complex models will help to confirm or disconfirm this hypothesis.

### Categories and Features

Now that we've handled the issue of indirect causation, we can return to our original goal of modeling categories. Below is a model representing a simple taxonomy of fish. Fish come in the northern and southern varieties, and each region has two species. I have written the model with 4 features; however, all but one is commented out because even with one feature, the model takes over ten minutes to run. Making the model more efficient is critical if we want to model realistic category structures.

In this model I have connected features only to species and not to regions. Allowing multiple category-levels to explicitly impact a feature would dramatically increase run time. Another option, only allowing the super-category to influence a feature, eliminates the task of determining whether a feature is determined by region or species. By assigning features at the lowest level, the model can capture the effect of region indirectly because assigning wug→big-fins and del→big-fins to 0.8 is equivalent to assigning north→big-fins to 0.8. You can see this effect by making the listener interpret `(because (big-fins) (eq? (region) 'north))`.

Here I have the listener interpret `(because (big-fins) (eq? (species) 'wug))`. The most interesting effect here is that the model ranks the probability that dels (the other northern species) have big-fins as lower than for either of the southern species. This follows my initial intuition that the categorical counterfactual is interpreted in terms of the super-category: if a wug weren't a wug, it would most likely be a del because only one variable must be resampled. Thus, by saying ¬wug → ¬ big-fins, we are implying that dels don't have big fins more strongly than that the other species don't have big fins. Pragmatics will strengthen this effect because the listener will know that the speaker would say "big-fins because north" if both wugs ands dels had big fins.

         
~~~
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
    (define north:south (flip .5))
    (define (region) (if north:south 'north 'south))
    
    (define wug:del (flip .5))
    (define hap:mit (flip .5))
    (define (species) (case (region)
                            (('north) (if wug:del 'wug 'del))
                            (('south) (if hap:mit 'hap 'mit))))

    ;; feature-weights
    (define wug→big-fins (if (flip) .8 .2))
    (define del→big-fins (if (flip) .8 .2))
    (define hap→big-fins (if (flip) .8 .2))
    (define mit→big-fins (if (flip) .8 .2))

;;     (define wug→whiskers (if (flip) .8 .2))
;;     (define del→whiskers (if (flip) .8 .2))
;;     (define hap→whiskers (if (flip) .8 .2))
;;     (define mit→whiskers (if (flip) .8 .2))

;;     (define wug→fangs (if (flip) .8 .2))
;;     (define del→fangs (if (flip) .8 .2))
;;     (define hap→fangs (if (flip) .8 .2))
;;     (define mit→fangs (if (flip) .8 .2))

;;     (define wug→stripes (if (flip) .8 .2))
;;     (define del→stripes (if (flip) .8 .2))
;;     (define hap→stripes (if (flip) .8 .2))
;;     (define mit→stripes (if (flip) .8 .2))

    ;; features
    (define U-big-fins (get-U-crits .2 .8))
    (define (big-fins)
      (case (species)
            (('wug) (<= U-big-fins wug→big-fins))
            (('del) (<= U-big-fins del→big-fins))
            (('hap) (<= U-big-fins hap→big-fins))
            (('mit) (<= U-big-fins mit→big-fins))
            ))
;;      (define U-whiskers (get-U-crits .2 .8))
;;      (define (whiskers) (case (species)
;;                               (('wug) (<= U-whiskers wug→whiskers))
;;                               (('del) (<= U-whiskers del→whiskers))
;;                               (('hap) (<= U-whiskers hap→whiskers))
;;                               (('mit) (<= U-whiskers mit→whiskers))
;;                               ))
;;      (define U-fangs (get-U-crits .2 .8))
;;      (define (fangs) (case (species)
;;                               (('wug) (<= U-fangs wug→fangs))
;;                               (('del) (<= U-fangs del→fangs))
;;                               (('hap) (<= U-fangs hap→fangs))
;;                               (('mit) (<= U-fangs mit→fangs))
;;                               ))
;;      (define U-stripes (get-U-crits .2 .8))
;;      (define (stripes) (case (species)
;;                            (('wug) (<= U-stripes wug→stripes))
;;                            (('del) (<= U-stripes del→stripes))
;;                            (('hap) (<= U-stripes hap→stripes))
;;                            (('mit) (<= U-stripes mit→stripes))
;;                            ))
    ))

(barplot (listener '(because (big-fins) (eq? (species) 'wug))
                   '(list wug→big-fins del→big-fins
                          hap→big-fins mit→big-fins))
         "Big fins because wug")

~~~

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
    (define north:south (flip .5))
    (define (region) (if north:south 'north 'south))
    
    (define wug:del (flip .5))
    (define hap:mit (flip .5))
    (define (species) (case (region)
                            (('north) (if wug:del 'wug 'del))
                            (('south) (if hap:mit 'hap 'mit))))

    ;; feature-weights
    (define wug→big-fins (if (flip) .8 .2))
    (define del→big-fins (if (flip) .8 .2))
    (define hap→big-fins (if (flip) .8 .2))
    (define mit→big-fins (if (flip) .8 .2))

    ;; features
    (define U-big-fins (get-U-crits .2 .8))
    (define (big-fins)
      (case (species)
            (('wug) (<= U-big-fins wug→big-fins))
            (('del) (<= U-big-fins del→big-fins))
            (('hap) (<= U-big-fins hap→big-fins))
            (('mit) (<= U-big-fins mit→big-fins))
            ))

    ))

(hist (listener '(because (big-fins) (eq? (species) 'wug))
                '(list wug→big-fins del→big-fins
                       hap→big-fins mit→big-fins)
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
    (define hap:mit (flip .5))
    (define (species) (case (region)
                            (('north) (if wug:del 'wug 'del))
                            (('south) (if hap:mit 'hap 'mit))))

    ;; feature-weights
    (define wug→big-fins (if (flip) .8 .2))
    (define del→big-fins (if (flip) .8 .2))
    (define hap→big-fins (if (flip) .8 .2))
    (define mit→big-fins (if (flip) .8 .2))

    ;; features
    (define U-big-fins (get-U-crits .2 .8))
    (define (big-fins)
      (case (species)
            (('wug) (<= U-big-fins wug→big-fins))
            (('del) (<= U-big-fins del→big-fins))
            (('hap) (<= U-big-fins hap→big-fins))
            (('mit) (<= U-big-fins mit→big-fins))
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
                           hap→big-fins mit→big-fins)))))
~~~~
