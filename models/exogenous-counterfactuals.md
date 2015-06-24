---
layout: model
title: Exogenous counterfactuals
model-language: church
---

*This forest page discusses issues that arise with the [current counterfactual model](http://forestdb.org/models/because.html) and how these issues can be resolved using exogenous randomness (c.f. Pearl, 2000).*

Suppose we have a category structure such that `A` and `B` are categories, and such that there is a single Boolean feature with a value that depends on the category. Let's look at how the counterfactual statement "If it wasn't in category A, it wouldn't have feature" is handled.

### Deterministic dependencies

Let's first consider deterministic dependence, where the feature is always on for category A and always off for category B.

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
    (and ,a ,b)))
    "If it weren't in category A, it wouldn't have feature")
~~~

This doesn't look right; we expect that the counterfactual statement should be `true` with probability 1. This problem arises because category and feature resample independently. Thus, most of the times that category changes, feature is left with its original value. The causal force of category on feature only comes to play in the .0001 probability event that both resample in the same shadow-cycle. One possible solution to this problem is to make `feature-on` a function, thus forcing it to resample every cycle.

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
    (and ,a ,b)))
    "If it weren't in category A, it wouldn't have feature")
~~~~

Functions always get resampled, so every time the category is changed, the feature changes in response. Now, the counterfactual statement ("if it weren't in category A, then the feature wouldn't be on") is true with probability 1. For deterministic dependencies, this strategy seems to work fairly well.

### Stochastic dependencies

What about stochastic dependencies? In this case, we also have the two options shown above: we can resample with probability `epsilon`, or we can always resample (by expressing `feature-on` as a function). Let's look at the result from each option.

#### Version 1: Variables
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

    (define feature-on
      (flip
       (case category
             (('A) .6)
             (('B) .4))))

    ))

(define a 'feature-on) ;;feature-on is a variable
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
    (and ,a ,b)))
    "If it weren't in category A, it wouldn't have feature")
~~~~

#### Version 2: Functions

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

    (define (feature-on) ;;feature-on is a function
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

    (and ,a ,b)))
    "If it weren't in category A, it wouldn't have feature")
~~~~

The variable-based approach (version 1) gives an obviously wrong answer as it did for deterministic dependency. The function-based approach is not obviously crazy, but still doesn't seem quite right. If the feature only weakly depends on the category, the counterfactual should be true less often. This intuition arises clearly when we consider an extreme case in which the probability given A is .501 and given B is .499. In this case, we intuit that if the feature is on with category A, it would probably be on with category B as well. The function-based approach, however, would predict the counterfactual being true with probability .501. Thus, it seems that we should  resample `feature-on` based on the strength of the dependence on `category`: 

- If category and feature are independent,  we should resample `feature-on` with the prior probability `epsilon` even if we update `category`. 
- If `feature-on` deterministically depends on `category`, we should resample it whenever we change `category`. 
- If the dependence is somewhere in between, we should resample with a probability that lies in between the two extremes.

### Towards a solution

Here is one approach based on moving all randomness into independent random variables, and implementing all dependencies using deterministic functions (compare to Pearl's counterfactuals). We call this exogenous randomness.

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

                       (and ,a ,b)))))
                       "If it weren't in category A, it wouldn't have feature")
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
    (and ,a ,b)))
    "If it weren't in category A, it wouldn't have feature")
~~~~

### Testing the models

Now we can compare this new exogenous style to the original style with randomness embedded in the functions. The two models below are parallel instantiations of a simple a→b→c causal chain. The values of each node as well as the weights between the nodes are unknown variables that the listener must discover. Here I show interpretations of "c because a" and "c and a," as well as the best utterance when all variables are on and weights are high. It may help to comment out certain graphs to more easily compare the two models.

#### Original style

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


;; put model into global scope:
(define model 
  '(
    (define a→b (if (flip) 0.8 0.3))
    (define b→c (if (flip) 0.8 0.3))
    
    (define a (flip))
    (define b (flip (if a
                        a→b
                        0.1)))
    (define c (flip (if b
                        b→c
                        0.1)))
    ))

(barplot (listener '(because c a) 'b)
         "b given \"c because a\"")
(barplot (listener '(and c a) 'b)
         "b given \"c and a\"")

(barplot (listener '(because c a) '(list a→b b→c ))
         "{a→b, b→c} given \"c because a\"")
(barplot (listener '(and c a) '(list a→b b→c ))
         "{a→b, b→c} given \"c and a\"")


(define (utt-prior) (uniform-draw '((because c a) (because c b)
                                     (and c a) (and c b))))
(barplot (speaker (list 0.8 0.8 #t #t #t) '(list a→b b→c a b c))
         "{a→b, b→c} = 0.8 & {a, b, c} = true")
~~~


#### Exogenous style


~~~
;;runs in about ~40 seconds
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
;; to the critical values used as input
(define (get-U p1 p2 p3)
  (multinomial (list p1 p2 p3 1)
               (list p1 (- p2 p1) (- p3 p2) (- 1 p3))))

(define model 
  '(
    ;; causal strengths
    (define a→b (if (flip) 0.8 0.3))
    (define b→c (if (flip) 0.8 0.3))

    (define B→a (flip)) ;; probability goes outside of function
    (define (a) B→a) ;; all nodes are deterministic functions

    (define U (get-U 0.1 0.3 0.8)) ; .1 is background
    (define (b) (if (a) (<= U a→b) (<= U .1)))

    (define U2 (get-U 0.1 0.3 0.8))
    (define (c) (if (b) (<= U2 b→c) (<= U2 .1)))
    ))


(barplot (listener '(because (c) (a) ) '(b) )
         "b given \"c because a\"")


(barplot (listener '(and (c) (a) ) '(b) )
         "b given \"c and a\"")

(barplot (listener '(because (c) (a) ) '(list a→b b→c ))
         "{a→b, b→c} given \"c because a\"")
(barplot (listener '(and (c) (a) ) '(list a→b b→c ))
         "{a→b, b→c} given \"c and a\"")


(define (utt-prior) (uniform-draw '((because (c) (a) ) (because (c) (b) )
                                     (and (c) (a) ) (and (c) (b) ))))
(barplot (speaker (list 0.8 0.8 #t #t #t) '(list a→b b→c  (a) (b) (c) ))
         "{a→b, b→c} = 0.8 & {a, b, c} = true")

~~~

This preliminary comparison demonstrates that the exogenous style produces superior predictions to the original style. `(and c a)` have the same interpretation in each model, indicating that the models are identical in their basic causal structure. The interpretation of `(because c a)` however is quite different. The exogenous "because" interpretation is similar to the "and" interpretation except for two key differences: states in which `b` is false are less probable and causal weights are more likely to be high. This reflects the intuition that `a` can only have caused `c` if `b` was also true, and that a is more likely to be the cause of c if its causal force on c is high. The original style, conversely, predicts that `b` is true with lower probability and that causal weights are lower after hearing "c because a" in comparison to "c and a".

When we examine the speaker, we see the greatest discrepancy for `(because c a)`, which is the most probable in the exogenous model and least probable in the original model. I argue that Pearl's predictions more closely reflect our intuitive understanding. `(because c a)` is highly informative because it gives information about all variables. To ground this in a physical example, we can interpret `a` to be the knocking over of a vase, `b` to be the vase falling off the table, and `c` to be the vase breaking. In this example, saying "the vase broke because it was knocked over" seems to be a very good explanation. However, it is unlikely that the QUD in such a situation would be the entire state as it is in the above model. We must construct more complex, psychologically plausible, models in order to better distinguish the two model types.

There is also the remaining issue that the exogenous style model only weakly prefers `(because c a). This may be specific to this situation, in which simply asserting the truth of any two nodes makes the full state very probable. Creating more complex models will help to confirm or disconfirm this hypothesis.

### Efficiency
In the above models we see that the exogenous style is significantly slower than the original style. Here the difference between two and twelve seconds is not exceedingly inconvenient. It becomes a serious impediment, however, when we being using more complex models. For example, below is a simple fish category model taken from [forestdb.org/models/category-explanation](forestdb.org/models/category-explanation). It takes around twelve minutes to run. We would like to add pragmatics to the model to generate better predictions, but at the moment, it seems infeasible.

Enumeration query is intractable with complex models because the complexity is exponential with respect to the number of variables—every possible combination of variable values must be tested. This actually becomes a problem with complex models in the original style, but because the exogenous style requires more  One way to avoid this problem is to use `rejection-query`. Although it is less accurate, we can still get stable predictions with a large sample size.

~~~~
;;runs in ~15 seconds
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
;; to the critical values used as input
(define (get-U p1 p2 p3)
  (multinomial (list p1 p2 p3 1)
               (list p1 (- p2 p1) (- p3 p2) (- 1 p3))))

(define model 
  '(
    ;; causal strengths
    (define a→b (if (flip) 0.8 0.3))
    (define b→c (if (flip) 0.8 0.3))

    (define B→a (flip)) ;; probability goes outside of function
    (define (a) B→a) ;; all nodes are deterministic functions

    (define U (get-U 0.1 0.3 0.8)) ; 0.1 is background
    (define (b) (if (a) (<= U a→b) (<= U .1)))

    (define U2 (get-U 0.1 0.3 0.8))
    (define (c) (if (b) (<= U2 b→c) (<= U2 .1)))
    ))


(hist (repeat 100 (lambda () (listener '(because (c) (a) ) '(b) )))
         "b given \"c because a\"")
(hist (repeat 100 (lambda () (listener '(and (c) (a) ) '(b) )))
         "b given \"c and a\"")

(hist (repeat 100 (lambda () (listener '(because (c) (a) ) '(list a→b b→c ))))
         "{a→b, b→c} given \"c because a\"")
(hist (repeat 100 (lambda () (listener '(and (c) (a) ) '(list a→b b→c ))))
         "{a→b, b→c} given \"c and a\"")


(define (utt-prior) (uniform-draw '( (because (c) (a)) (because (c) (b))
                                     (and (c) (a)) (and (c) (b))  )))
(hist (repeat 100 (lambda () (speaker (list 0.8 0.8 #t #t #t) '(list a→b b→c  (a) (b) (c) ))))
         "{a→b, b→c} = 0.8 & {a, b, c} = true")


~~~~

Although the difference is small in this case, it becomes much larger as the state-space grows because the runtime of `rejection-query` does not depend directly on the number of variables. `rejection-query` has the additional benefit of having adjustable accuracy. When we are exploring, we can sacrifice accuracy in the interest of time; when we want the best possible results, we can take more samples, sacrificing time in the interest of accuracy. 

Although `rejection-query` avoids the computational explosion of `enumeration-query`, it faces the new problem of extremely unlikely condition statements that arise in the counterfactual queries; there may be thousands of cycles before a condition is met. Adding additional queries for pragmatic speakers and listeners quickly worsens this problem because a difficult-to-satisfy condition may have a query with many of its own difficult-to-satisfy conditions. This makes a full pragmatic model very slow.

The solution likely lies in an intelligent heuristic-based algorithm of the sort described in [this dippl chapter](http://dippl.org/chapters/04-factorseq.html). This is an area of present research. For the time being, however, we can use the `rejection-query` implementation to generate some predictions for small models, for example: [simple category structures](http://forestdb.org/models/category-explanations.html).
