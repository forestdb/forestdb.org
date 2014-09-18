---
layout: model
title: Intervention and counterfactuals
---

*This page argues that a counterfactual model in Church should be based on intervention rather than querying.*

# The problem

In the current counterfactual model, causation is only considered in its effect on the correlation between two variables. This is because dependence between the variables of interest are found with a condition statement, not intervention. An introductory statistics professor will tell you that correlation does not imply causation, and that a study must be experimental (with intervention) to demonstrate causality. I argue that this principle applies to counterfactuals just as it does to science. A model that relies entirely on co-occurrence cannot possibly capture causality.

### Example: colds

Here is an example grounded in the real world of illness and symptoms. We think of illnesses causing symptoms and not the other way around even if p(illness\|symptom) is higher than p(symptom\|illness). The current counterfactual model does not produce these results:

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
    ;; causal links
    (define B→cough (flip .3))
    (define cold→cough (flip .8))
    (define B→runny-nose (flip .1))
    (define cold→runny-nose (flip .4))
    
    ;; independent variables
    (define cold (flip .1))
      
    ;; dependent variables
    (define (cough) (or
                     (and cold cold→cough)
                     B→cough))
    (define (runny-nose) (or
                     (and cold cold→runny-nose)
                     B→runny-nose))
    ))


(define (counterfactual not-a if-not-b title)
  (barplot
   (eval
    '(enumeration-query
      ,@model

      (apply multinomial
             (enumeration-query
              (define eps 0.01)
              ,@(make-shadow-defines model) ;;the shadow model
              (not ,(shadow-rename-all not-a (names model)))
              (condition (not ,(shadow-rename-all if-not-b (names model))))))
      (and ,not-a ,if-not-b)))
   title)
  )

(counterfactual 'cold '(cough) "he wouldn't have cold if he wasn't coughing")
(counterfactual '(cough) 'cold "he wouldn't be coughing if he didn't have a cold")
(counterfactual '(runny-nose) 'cold "he wouldn't have a runny nose if he didn't have a cold")
(counterfactual '(runny-nose) '(cough) "he wouldn't have a runny nose if he wasn't coughing")


~~~

We see here that in a situation where the symptom implies the illness more strongly than the illness implies the symptom, the model prefers to say "illness because symptom." This discrepancy between the model results and the intuitive meaning of causation shows itself in the "because" model as well.

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

(define model 
  '(
    ;; causal links
    (define B→cough (flip .3))
    (define cold→cough (flip .8))
    (define B→runny-nose (flip .1))
    (define cold→runny-nose (flip .4))
    
    ;; independent variables
    (define cold (flip .1))
      
    ;; dependent variables
    (define (cough) (or
                     (and cold cold→cough)
                     B→cough))
    (define (runny-nose) (or
                     (and cold cold→runny-nose)
                     B→runny-nose))
    ))

(define (utt-prior) (uniform-draw '( (because (cough) cold)
                                     (because cold (cough))
                                     (because (runny-nose) (cough))
                                     (and (runny-nose) (cough))
                                     (cough)
;;                                      (because smokes (cough))
;;                                      (and (cough) smokes)
;;                                      (because smokes cold)
;;                                      (because cold smokes)
;;                                      (and smokes cold)
;;                                      smokes cold (cough)
                                     )))

(barplot (speaker (list #t #t #t) '(list cold→cough cold (cough))))
~~~

The speaker has no preference for "he's coughing because he has a cold" over "he has a cold because he's coughing." This is bad. Furthermore, the speaker likes to say "he has a runny nose because he's coughing." This effect occurs because coughing and runny noses are both symptoms of colds. During the shadow cycles, the model may turn off `(cold)` which would turn off coughing and runny noses, verifying the statement "if he wasn't coughing he wouldn't have a runny nose." As we can see by the smaller preference for `(and smokes cold)`, this effect is partially but not entirely due to providing the information that `coughing` and `runny-nose` are on.

# Towards a solution

I propose an intuitive theory of causations that I believe would be easily implementable in Church by someone who knew what they were doing. Depending on the response to this initial description of the problem, I may take on the implementational task myself. For now, I will present a high level description based largely on my intuitive sense of what I do when I consider counterfactuals.

When we consider a counterfactual, we likely do not run through the entire generative model, checking to see if our criteria (e.g. "if he wasn't coughing") is true at the end. Rather, I propose that we intervene in the model at the level where coughing is determined, set it to false, and look for changes lower in the generative model.

We see intuitive evidence for this type of model when we look at counterfactuals as a form of hypothetical (just a negative one). When we imagine hypothetical situations (e.g. "If I was stronger…"), we generally think of the effects of the hypothetical (e.g. "I could lift things more easily," "I'd get picked on less," "I'd get more attention from girls") as opposed to the state of the world that would have caused the hypothetical (e.g. "I would have worked out every day all through high school," "I would have been born with a different body type"). One could argue that this is because the effects are more interesting to us than the causes, especially with this example. As an exercise, the reader can imagine how she would consider a hypothetical in a less emotionally charged, physical causation setting. We could also easily run an mTurk study to look for the effect that I posit here.

### Potential problems

There are some problems that emerge with this proposed style. For example, consider category explanations in which explanation can be bidirectional e.g. "It has black and white stripes because it's a zebra" and "It's a zebra because it has black and white stripes." If we are committed to a generative model that goes in one direction (most likely from category to feature) then any explanation of category because feature is impossible with an intervention-style model.

There are a few possible solutions to this problem. First, it is possible that the category because feature has an elided "I know that." Under this option, we could view the task of determining category as abduction of category based on features. In this case features would have a causal impact on category *knowledge* and thus an intervention-style model could predict category-because-feature explanations

Another (less appealing) possibility is that a generative model based on functions is not fit to model the associations between categories and features. Intuitively, we have a strong sense that colds cause coughing, and thus the notion of modeling coughs as a function of colds is reasonable. However, we lack a strong sense that zebrahood causes stripes (or the other way around). To the extent that causality is related to functions and their associated directional relationship, this may be a problem. What is clear in my mind is that (1) `(define (cough cold)` is clearly superior to `(define (cold cough)` in a way that `(define (feature category))` is not clearly superior to `(define (category feature))` and (2) coughs cause colds more than zebrahood causes stripes. It is an open question as to why my intuitions are parallel in this case, and to what degree they would be parallel in other cases.
