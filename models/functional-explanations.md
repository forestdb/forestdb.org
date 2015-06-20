---
layout: model
model-language: church
---

Playing around with modeling teleological explanation. Teleological explanations seem to have some implicit designer who chooses an action (based on their goal), which results in the explanandum being true.

# basic
~~~~
;; background probabilities
(define coincidental-stripes 0.5)
(define coincidental-survival 0.1)

;; causal strengths
(define stripes->survive 0.9)
(define designer-power 1)

;; designer's model and action
(define (action-prior) (if (flip) 'give-stripes 'not-give-stripes))

(define (stripes? action) (if (equal? action 'give-stripes)
                              (flip designer-power)
                              (flip coincidental-stripes)))

(define (result has-stripes?)
  (if (flip (if has-stripes? stripes->survive coincidental-survival))
      'survive
      'die-off))

(define (designer goal)
  (enumeration-query
   (define action (action-prior))
   (define has-stripes? (stripes? action))
   (define outcome (result has-stripes?))
   (define goal? (equal? outcome goal))
   action
   goal?))

(define (useful has-stripes? goal)
  (enumeration-query
   (define action (designer 'survive))
   (define actual-has-stripes (stripes? action))
   (define outcome (result actual-has-stripes))
   (define goal? (equal? outcome goal))
   goal?
   (equal? has-stripes? actual-has-stripes)))

(barplot (useful #t 'survive) "is having stripes useful for survival?")
(barplot (useful #f 'survive) "is not having stripes useful for survival?")

(barplot (designer 'survive) "what will the designer do?")

(barplot (enumeration-query
          (define action (apply multinomial (designer 'survive)))
          (stripes? action)
          #t) "will tigers have stripes?")
~~~~

# exogenous
~~~~
(barplot (enumeration-query
          ;; make this lower resolution if you want it to run faster
          (define (discrete-uniform) (uniform-draw '(0 0.25 0.75 1)))

          ;; background probabilities
          (define coincidental-stripes 0.5)
          (define coincidental-survival 0.1)

          ;; causal strengths
          (define stripes->survive 0.9)
          (define designer-power 1)

          ;; designer's model and action
          (define (action-prior) (if (flip) 'give-stripes 'not-give-stripes))
          (define (stripes? action-choice) (if (equal? action-choice 'give-stripes)
                                               (flip designer-power)
                                               (flip coincidental-stripes)))
          (define (result has-stripes?) (if (flip (if has-stripes? stripes->survive coincidental-survival))
                                            'survive
                                            'die-off))
          (define (designer goal)
            (enumeration-query
             (define action-choice (action-prior))
             (define has-stripes? (stripes? action-choice))
             (define outcome (result has-stripes?))
             (define goal? (equal? outcome goal))
             action-choice
             goal?))
          (define action-posterior-sample (apply multinomial (designer 'survive)))
          ;; actual action is deterministic function of the action-posterior-sample
          (define (action) action-posterior-sample)

          ;; stripes depends on (action), designer-power, and coincidental-stripes
          (define U-designer-power (discrete-uniform))
          (define U-coincidental-stripes (discrete-uniform))
          (define (stripes?) (if (equal? (action) 'give-stripes)
                                 (>= designer-power U-designer-power)
                                 (>= coincidental-stripes U-coincidental-stripes)))

          (define U-stripes->survive (discrete-uniform))
          (define U-coincidental-survival (discrete-uniform))
          (define (survive?) (if (stripes?)
                                 (>= stripes->survive U-stripes->survive)
                                 (>= coincidental-survival U-coincidental-survival)))
          (stripes?)
          #t) "will tigers have stripes?")

~~~~

# counterfactuals

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

;; make this lower resolution if you want it to run faster
(define (discrete-uniform) (uniform-draw '(0 0.5 1)))

;; background probabilities
(define coincidental-stripes 0.5)
(define coincidental-survival 0.1)

;; causal strengths
(define stripes->survive 0.9)
(define designer-power 1)

;; designer's model and action
(define (action-prior) (if (flip) 'give-stripes 'not-give-stripes))
(define (stripes? action-choice) (if (equal? action-choice 'give-stripes)
                                     (flip designer-power)
                                     (flip coincidental-stripes)))
(define (result has-stripes?) (if (flip (if has-stripes? stripes->survive coincidental-survival))
                                  'survive
                                  'die-off))
(define (designer goal)
  (enumeration-query
   (define action-choice (action-prior))
   (define has-stripes? (stripes? action-choice))
   (define outcome (result has-stripes?))
   (define goal? (equal? outcome goal))
   action-choice
   goal?))

(define design-dist (designer 'survive))
(define (action-posterior) (apply multinomial design-dist))

(define model 
  '(
    (define action-posterior-sample (action-posterior))
    ;; actual action is deterministic function of the action-posterior-sample
    (define (action) action-posterior-sample)
    
    ;; stripes depends on (action), designer-power, and coincidental-stripes
    (define U-designer-power (discrete-uniform))
    (define U-coincidental-stripes (discrete-uniform))
    (define (stripes?) (if (equal? (action) 'give-stripes)
                           (>= designer-power U-designer-power)
                           (>= coincidental-stripes U-coincidental-stripes)))

    (define U-stripes->survive (discrete-uniform))
    (define U-coincidental-survival (discrete-uniform))
    (define (survive?) (if (stripes?)
                           (>= stripes->survive U-stripes->survive)
                           (>= coincidental-survival U-coincidental-survival)))
    ))

;; (barplot (eval '(enumeration-query ,@model (stripes?) #t)) "will tigers have stripes?")

;; Would Y have happened if we observed that X did not happen?
(define (counterfactual? X Y)
  (eval
 '(enumeration-query
 	;;the actual world
   ,@model
   (define eps 0.01)
   ,@(make-shadow-defines model)
   ;;the shadow model
   ,(shadow-rename-all Y (names model))
   ;; participants are told that A and C are true in the real world and asked to counterfacualize on not B
   (not ,(shadow-rename-all X (names model))))))

;; runs slow, so uncomment to graph
;; (barplot (counterfactual? '(stripes?) '(survive?))
;;          "Would tigers survive if they didn't have stripes?")
~~~~

# variable goal

~~~~
;; background probabilities
(define coincidental-stripes 0.5)
(define coincidental-survival 0.1)

;; causal strengths
(define stripes->survive 0.9)
(define designer-power 1)

;; designer's model and action
(define (action-prior) (if (flip) 'give-stripes 'not-give-stripes))

(define (goal-prior) (if (flip) 'survive 'die-off))

(define (stripes? action) (if (equal? action 'give-stripes)
                              (flip designer-power)
                              (flip coincidental-stripes)))

(define (result has-stripes?)
  (if (flip (if has-stripes? stripes->survive coincidental-survival))
      'survive
      'die-off))

(define (designer goal)
  (enumeration-query
   (define action (action-prior))
   (define has-stripes? (stripes? action))
   (define outcome (result has-stripes?))
   (define goal? (equal? outcome goal))
   action
   goal?))

(define (useful has-stripes? goal)
  (enumeration-query
   (define action (designer 'survive))
   (define actual-has-stripes (stripes? action))
   (define outcome (result actual-has-stripes))
   (define goal? (equal? outcome goal))
   goal?
   (equal? has-stripes? actual-has-stripes)))

(barplot (enumeration-query
          (define goal (goal-prior))
          (define action (apply multinomial (designer goal)))
          goal
          (stripes? action))
         "given that tigers have stripes, what was the designer's goal?")
~~~~
