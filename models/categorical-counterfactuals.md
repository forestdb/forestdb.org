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

Now we can compare this new Pearl-esque style to the original stlye with randomness embedded in the functions. To see the effect at the listener and speaker levels, we need to look at variables with mediated dependences because the interpretation of `(because b a)` entails both `a` and `b`. In addition, we have implemented randomization
differently such that the truth of `b` given `a` is independent of the truh of `b` given ¬`a`. The two are not independent in the above implementation; if `U` is sampled as `u1`, then feature-on will be true regardless of category. I am not yet sure if this is better, but I have included it here as an example.

*Note: these models are very large, and may take a few minutes to run.*

#### Old

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
    (define a→b (if (flip) .9 background))
    (define b→c (if (flip) .9 background))
    
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
(barplot (speaker (list .9 .9 #t #t #t) '(list a→b b→c a b c))
         "Utterance given a→b and b→c are 0.9 and a, b, c")

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

(define model 
  '(
    ;; causal strengths
    (define a→b (if (flip) .9 background))
    (define b→c (if (flip) .9 background))

    (define a| (flip)) ;; probability goes outside of function
    (define (a) a|) ;; all nodes are deterministic functions

    (define b|a (flip a→b)) ;; if b|a is true and a is true, b is true
    (define b|!a (flip background))
    (define (b) (if (a) b|a  b|!a)) ;; always reference nodes by their function

    (define c|b (flip b→c))
    (define c|!b (flip background))
    (define (c) (if (b) c|b  c|!b))

    ))
(barplot (listener '(because (c) (a)) '(list (b) a→b b→c))
         "Interpretation of c because a")
(barplot (listener '(and (c) (a)) '(list (b) a→b b→c))
         "Interpretation of c and a")

(define (utt-prior) (uniform-draw '((because (c) (a)) (because (c) (b)) 
                                    (and (c) (a)) (and (c) (b)))))
(barplot (speaker (list .9 .9 #t #t #t) '(list a→b b→c (a)(b)(c)))
         "Utterance given a→b and b→c are 0.9 and a, b, c")

~~~
This preliminary comparison demonstrates that the Pearl-esque style produces superior predictions to the original style. `(and c a)` have the same interpretation in each model, indicating that the models are similar in their basic causal structure. The interpretation of `(because c a)` however is quite different. Pearl's interpretation is very close to the `and` interpretation except that states in which `b` is false have been made less probable. This reflects the intuition that `c` can only have caused `a` if `b` was also true. The original style, conversely, makes no general prediction about the value of `b`, giving high weight to the situation in which both causal strengths are low and b is false.

When we examine the speaker model, we see that both models make equal predictions for every statement but `(because c a)`, which is more likely in the Pearl-esque model and less likely in the original model. I argue that Pearl's predictions more closely reflect our intuitive understanding. By saying "c because a" we are telling the listener that there is a causal link between c and a, which by the model's structure must be mediated by `b`. To ground this in a physical example, we can interpret `a` to be the knocking over of a vase, `b` to be the vase falling off the table, and `c` to be the vase breaking. In this example, saying "the vase broke because it was knocked over" seems to be a very good explanation. However, it is unlikely that the QUD in such a situation would be the entire state as it is in the above model. We must construct more complex, psychologically plausible,  models in order to better distinguish the two model types.

A final issue is that the Pearl-esque model only weakly prefers `(because c a). This may be specific to this situation, in which simply asserting the truth of any nodes makes the full state very probable. Creating more complex models will help to confirm or disconfirm this hypothesis.


### Faux-uniform distributions

Using faux-uniform distributions, we can make b\|a dependent on b\|!a, as it was in the first implementation of Pearl-style counterfactuals. This causes the model to make much more dramatic predictions, so I have reduced the causative forces to .3 in order to see the variation. While this version has the benefit of creating more substantial predictions for the speaker, it generates rather odd interpretations for the listener. For example, when given `(and c a)` the model gives equal probability to a state in which `b` is false and causal weights are low as one in which `b` is true and causal weights are high. Intuitively, the second should be much more likely given the utterance.

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
    (define a→b (if (flip) .3 background))
    (define b→c (if (flip) .3 background))

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
(barplot (speaker (list .3 .3 #t #t #t) '(list a→b b→c (a)(b)(c)))
         "Utterance given a→b and b→c are 0.9 and a, b, c")

~~~
