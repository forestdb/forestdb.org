---
layout: model
model-language: church
---

# Functional Explanations

## Designer's beliefs

I think that functional explanations have a (sometimes implicit) designer. The designer has a model of the world which may or may not actually be the way the world works.

~~~~
(define designer-beliefs
  '(
    (define action (action-prior))
    (define action->effect 0.9)
    (define U-effect (discrete-uniform))
    (define (effect) (> U-effect action-effect))
    ))
~~~~

The designer will choose their actions based on their beliefs and their goal.

~~~~
(define (action-prior) (flip))
(define (discrete-uniform) (uniform-draw '(0 0.25 0.5 0.75 1)))

(define designer-beliefs
  '(
    (define action (action-prior))
    ;;designer believes action will probably cause effect and inaction probably won't
    (define action->effect 0.9)
    (define inaction->effect 0.1)
    (define U-effect (discrete-uniform))
    (define (effect) (if action
                         (< U-effect action->effect)
                         (< U-effect inaction->effect)))))
    
;;designer wants effect to be true.
(define designer-goal '(equal? (effect) #t))

;;designer chooses an action conditioning on their goal being achieved
(define designer-action
  (eval '(enumeration-query ,@designer-beliefs action ,designer-goal)))

(barplot designer-action "will the designer act?")
~~~~

The actual world may or may not work the way the designer thinks. We can predict the designers actions based on their beliefs, but we can predict the results of their actions based on how the world actually works.

~~~~
(define (action-prior) (flip))
(define (discrete-uniform) (uniform-draw '(0 0.25 0.5 0.75 1)))

(define designer-beliefs
  '(
    (define action (action-prior))
    ;;designer believes action will probably cause effect and inaction probably won't
    (define action->effect 0.9)
    (define inaction->effect 0.1)
    (define U-effect (discrete-uniform))
    (define (effect) (if action
                         (< U-effect action->effect)
                         (< U-effect inaction->effect)))))
    
;;designer wants effect to be true.
(define designer-goal '(equal? (effect) #t))

;;designer chooses an action conditioning on their goal being achieved
(define action-posterior
  (eval '(enumeration-query ,@designer-beliefs action ,designer-goal)))

;;the actual world may or may not be what the designer thinks
(define actual-world
  '(
    (define action (apply multinomial action-posterior))
    (define action->effect 0.9)
    (define inaction->effect 0.1)
    (define U-effect (discrete-uniform))
    (define (effect) (if action
                         (< U-effect action->effect)
                         (< U-effect inaction->effect)))))

;;we can predict the designers actions and what effects will result
(barplot
 (eval '(enumeration-query ,@actual-world 
                           (list (if action 'action 'inaction)
                                 (if (effect) 'effect 'no-effect))
                           #t)) "what will happen?")
~~~~

We can infer the designer's goal rather than setting it as a known variable.

~~~~
(define (action-prior) (flip))
(define (discrete-uniform) (uniform-draw '(0 0.25 0.5 0.75 1)))

(define designer-beliefs
  '(
    (define action (action-prior))
    ;;designer believes action will probably cause effect and inaction probably won't
    (define action->effect 0.9)
    (define inaction->effect 0.1)
    (define U-effect (discrete-uniform))
    (define (effect) (if action
                         (< U-effect action->effect)
                         (< U-effect inaction->effect)))))

;;we don't know what the designer's goal is
(define goal-prior flip)

;;depending on the agent's goal, they have different posterior distributions over actions
(define action-posterior
  (mem (lambda (goal)
         (eval '(enumeration-query ,@designer-beliefs
                                   action
                                   (equal? (effect) goal))))))

;;the actual world may or may not be what the designer thinks
(define actual-world
  '(
    ;;we can infer the designer's goal
    (define designer-goal (goal-prior))
    (define (action) (apply multinomial (action-posterior designer-goal)))
    (define action->effect 0.9)
    (define inaction->effect 0.1)
    (define U-effect (discrete-uniform))
    (define (effect) (if (action)
                         (< U-effect action->effect)
                         (< U-effect inaction->effect)))))

;;we can infer the designer's goal given their action
(barplot
 (eval '(enumeration-query ,@actual-world
                           designer-goal
                           (equal? (action) #t)))
 "what was designer's goal if they acted?")

;;given their goal, we can predict the designers actions and what effects will result
(barplot
 (eval '(enumeration-query ,@actual-world 
                           (list (if (action) 'action 'inaction)
                                 (if (effect) 'effect 'no-effect))
                           (equal? designer-goal #t)))
 "what will happen if the designer wants the effect?")
~~~~

## Counterfactuals

Let's look at the counterfactual for (what I think is) a functional explanation: "The action happened becasue the designer wanted the effect to happen."

The counterfactual for this would be: "If the designer didn't want the effect to happen, then the action would not have happened."

In our model, we can query whether the action would have happened given that the designer's goal was not for the effect to happen.

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

;; Would Y have happened if we counterfactually observed that X did not happen?
(define (counterfactual X Y model)
  (eval
   '(enumeration-query
     ;;the actual world
     ,@model
     (define eps 0.1)
     ,@(make-shadow-defines model)
     ;;the shadow model
     ,(shadow-rename-all Y (names model))
     (not ,(shadow-rename-all X (names model))))))
;;;

(define (action-prior) (flip))
(define (discrete-uniform) (uniform-draw '(0 0.25 0.5 0.75 1)))

(define designer-beliefs
  '(
    (define action (action-prior))
    ;;designer believes action will probably cause effect and inaction probably won't
    (define action->effect 0.9)
    (define inaction->effect 0.1)
    (define U-effect (discrete-uniform))
    (define (effect) (if action
                         (< U-effect action->effect)
                         (< U-effect inaction->effect)))))

;;we don't know what the designer's goal is
(define goal-prior flip)

;;depending on the agent's goal, they have different posterior distributions over actions
(define action-posterior
  (mem (lambda (goal)
         (eval '(enumeration-query ,@designer-beliefs
                                   action
                                   (equal? (effect) goal))))))

;;the actual world may or may not be what the designer thinks
(define actual-world
  '(
    ;;we can infer the designer's goal
    (define designer-goal (goal-prior))
    (define (action) (apply multinomial (action-posterior designer-goal)))
    (define action->effect 0.9)
    (define inaction->effect 0.1)
    (define U-effect (discrete-uniform))
    (define (effect) (if (action)
                         (< U-effect action->effect)
                         (< U-effect inaction->effect)))))

(barplot (counterfactual '(equal? designer-goal #t) '(equal? (action) #t) actual-world)
         "if the designer's goal wasn't for the effect to happen, would the action have happened?")

~~~~

We can also infer whether the designer thought the action would cause the effect, and counterfactually whether the action would have happened if they didn't think it would cause the effect.

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

;; Would Y have happened if we counterfactually observed that X did not happen?
(define (counterfactual X Y model)
  (eval
   '(enumeration-query
     ;;the actual world
     ,@model
     (define eps 0.1)
     ,@(make-shadow-defines model)
     ;;the shadow model
     ,(shadow-rename-all Y (names model))
     (not ,(shadow-rename-all X (names model))))))
;;;

(define (action-prior) (flip))
(define (discrete-uniform) (uniform-draw '(0 0.25 0.5 0.75 1)))

(define (designer-beliefs designer-thinks-action->effect)
  (if designer-thinks-action->effect
    '(
      (define action (action-prior))
      ;;designer believes action will probably cause effect and inaction probably won't
      (define action->effect 0.9)
      (define inaction->effect 0.5)
      (define U-effect (discrete-uniform))
      (define (effect) (if action
                           (< U-effect action->effect)
                           (< U-effect inaction->effect))))
    '(
      (define action (action-prior))
      ;;designer believes action will probably cause effect and inaction probably won't
      (define action->effect 0.5)
      (define inaction->effect 0.5)
      (define U-effect (discrete-uniform))
      (define (effect) (if action
                           (< U-effect action->effect)
                           (< U-effect inaction->effect))))))

;;we don't know what the designer's goal is
(define goal-prior flip)

;;depending on the agent's goal, they have different posterior distributions over actions
(define action-posterior
  (mem (lambda (goal designer-thinks-action->effect)
         (eval '(enumeration-query ,@(designer-beliefs designer-thinks-action->effect)
                                   action
                                   (equal? (effect) goal))))))

;;the actual world may or may not be what the designer thinks
(define actual-world
  '(
    ;;we can infer the designer's goal
    (define designer-goal (goal-prior))
    (define designer-thinks-action->effect (flip))
    (define (action) (apply multinomial (action-posterior designer-goal designer-thinks-action->effect)))
    (define action->effect 0.9)
    (define inaction->effect 0.1)
    (define U-effect (discrete-uniform))
    (define (effect) (if (action)
                         (< U-effect action->effect)
                         (< U-effect inaction->effect)))))

(barplot (counterfactual '(equal? designer-goal #t) '(equal? (action) #t) actual-world)
         "if the designer's goal wasn't for the effect to happen, would the action have happened?")

(barplot (counterfactual '(equal? designer-thinks-action->effect #t)
                         '(equal? (action) #t) actual-world)
         "if the designer didn't think it would cause the effect, would the action have happened?")
~~~~

## Different kinds of "functional" explanation

Here are some versions of functional explanations for the action:

1. The action happened because the designer wanted the effect to happen.
3. The action happened so that the effect would happen.
4. The action happened because it would cause the effect.
2. The action happened because the designer thought it would cause the effect.
5. The action happened because in general, actions like that cause effects like that.

Here are the counterfactual versions of those explanations:

1. If the designer hadn't wanted the effect to happen, the action wouldn't have happened.
3. *I'm not sure which "because" (if any) the "so that" explanation corresponds to.*
4. If it wouldn't have caused the effect, the action wouldn't have happened.
2. If the designer hadn't thought it would cause the effect, the action wouldn't have happened.
5. If it weren't the case that in general actions like that cause effects like that, the action wouldn't have happened.

Let's look at these explanations in a few different contexts.

### In a world where the action causes the effect and the agent knows that...

Let's say that Sally has a phone that's running low on batteries and a phone charger that works perfectly well. She believes (correctly) that the phone charger will charge her phone. She wants her phone's battery to be charged. So she plugs the phone in to the charger.

Here are some explanations for Sally's behavior:

1. Sally plugged in her phone because she wanted it to charge.
2. Sally plugged in her phone so that it would charge.
3. Sally plugged in her phone because that would cause it to charge.
4. Sally plugged in her phone because she thought that would cause it to charge.
5. Sally plugged in her phone because in general, plugging in phones causes them to charge.

All of these are perfectly good explanations (by my intuition).

Here are the counterfactual versions of those explanations:

1. If Sally hadn't wanted her phone to charge, she wouldn't have plugged it in.
2. --
3. If wouldn't have caused her phone to charge, Sally wouldn't have plugged the phone in.
4. If Sally hadn't thought it would charge her phone, she wouldn't have plugged her phone in.
4. If it weren't the case that in general plugging phones in causes them to charge, Sally would not have plugged her phone in.

These also seem fine.

Here are the model's ratings for the first counterfactual:

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


(define model
  '(
    (define apple (flip))
    (define banana (if apple (flip 0.9) (flip 0.1)))))


;; Would Y have happened if we counterfactually observed that X did not happen?
(define (counterfactual X Y model)
  (eval
   '(enumeration-query
     ;;the actual world
     ,@model
     (define eps 0.1)
     ,@(make-shadow-defines model)
     ;;the shadow model
     (not ,(shadow-rename-all Y (names model)))
     (not ,(shadow-rename-all X (names model))))))

(define (action-prior) (flip))
(define (discrete-uniform) (uniform-draw '(0 0.25 0.5 0.75 1)))

(define designer-beliefs
  '(
    (define action (action-prior))
    ;;designer believes action will probably cause effect and inaction probably won't
    (define action->effect 0.9)
    (define inaction->effect 0.1)
    (define U-effect (discrete-uniform))
    (define (effect) (if action
                         (< U-effect action->effect)
                         (< U-effect inaction->effect)))))

;;we don't know what the designer's goal is
(define goal-prior flip)

;;depending on the agent's goal, they have different posterior distributions over actions
(define action-posterior
  (mem (lambda (goal)
         (eval '(enumeration-query ,@designer-beliefs
                                   action
                                   (equal? (effect) goal))))))

;;the actual world may or may not be what the designer thinks
(define actual-world
  '(
    ;;we can infer the designer's goal
    (define designer-goal (goal-prior))
    (define (action) (apply multinomial (action-posterior designer-goal)))
    (define action->effect 0.9)
    (define inaction->effect 0.1)
    (define U-effect (discrete-uniform))
    (define (effect) (if (action)
                         (< U-effect action->effect)
                         (< U-effect inaction->effect)))))

(barplot (counterfactual '(equal? designer-goal #t) '(equal? (action) #t) actual-world)
         "if sally hadn't wanted her phone to charge, she wouldn't have plugged it in")
;;;
~~~~

### In a world where the action doesn't cause the effect but the agent thinks it does...

Let's say that Sally has a phone that's running low on batteries and a phone charger that is completely broken. She believes that the phone charger will charge her phone, but in actuality it won't. She wants her phone's battery to be charged. So she plugs the phone in to the charger.

Here are some explanations for Sally's behavior:

1. Sally plugged in her phone because she wanted it to charge.
2. Sally plugged in her phone so that it would charge.
3. Sally plugged in her phone because that would cause it to charge.
4. Sally plugged in her phone because she thought that would cause it to charge.
5. Sally plugged in her phone because in general, plugging in phones causes them to charge.

All of these seem to me like good explanations *except* #3. Plugging in her phone would't cause it to charge, so "Sally plugged in her phone because that would cause it to charge" seems like a bad explanation.

Here are the counterfactual versions of those explanations:

1. If Sally hadn't wanted her phone to charge, she wouldn't have plugged it in.
2. --
3. If wouldn't have caused her phone to charge, Sally wouldn't have plugged the phone in.
4. If Sally hadn't thought it would charge her phone, she wouldn't have plugged her phone in.
4. If it weren't the case that in general plugging phones in causes them to charge, Sally would not have plugged her phone in.

Here, counterfactual #3 is blatently false. It doesn't cause her phone to charge, and yet Sally did plug her phone in.

Here are the model's ratings for the first counterfactual:

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


(define model
  '(
    (define apple (flip))
    (define banana (if apple (flip 0.9) (flip 0.1)))))


;; Would Y have happened if we counterfactually observed that X did not happen?
(define (counterfactual X Y model)
  (eval
   '(enumeration-query
     ;;the actual world
     ,@model
     (define eps 0.1)
     ,@(make-shadow-defines model)
     ;;the shadow model
     (not ,(shadow-rename-all Y (names model)))
     (not ,(shadow-rename-all X (names model))))))

(define (action-prior) (flip))
(define (discrete-uniform) (uniform-draw '(0 0.25 0.5 0.75 1)))

(define designer-beliefs
  '(
    (define action (action-prior))
    ;;designer believes action will probably cause effect and inaction probably won't
    (define action->effect 0.9)
    (define inaction->effect 0.1)
    (define U-effect (discrete-uniform))
    (define (effect) (if action
                         (< U-effect action->effect)
                         (< U-effect inaction->effect)))))

;;we don't know what the designer's goal is
(define goal-prior flip)

;;depending on the agent's goal, they have different posterior distributions over actions
(define action-posterior
  (mem (lambda (goal)
         (eval '(enumeration-query ,@designer-beliefs
                                   action
                                   (equal? (effect) goal))))))

;;the actual world may or may not be what the designer thinks
(define actual-world
  '(
    ;;we can infer the designer's goal
    (define designer-goal (goal-prior))
    (define (action) (apply multinomial (action-posterior designer-goal)))
    (define action->effect 0.5)
    (define inaction->effect 0.5)
    (define U-effect (discrete-uniform))
    (define (effect) (if (action)
                         (< U-effect action->effect)
                         (< U-effect inaction->effect)))))

(barplot (counterfactual '(equal? designer-goal #t) '(equal? (action) #t) actual-world)
         "if sally hadn't wanted her phone to charge, she wouldn't have plugged it in")
;;;
~~~~

### In a world where the action causes the effect but the agent doesn't know that...

Let's say Sally knows nothing about technology. Maybe she's really young and she's never seen a phone before, or anything else that plugs in to electricity. But she has played games and puzzles where some objects fit into other objects.

When the phone stops working, she wants it to light up again, but she doesn't know how to make it do that. She does know how to play a different game with the phone, so she starts plugging things in. She has no idea that this will charge her phone, she's just trying to keep playing with this toy.

Here are some explanations for Sally's behavior:

1. Sally plugged in her phone because she wanted it to charge.
2. Sally plugged in her phone so that it would charge.
3. Sally plugged in her phone because that would cause it to charge.
4. Sally plugged in her phone because she thought that would cause it to charge.
5. Sally plugged in her phone because in general, plugging in phones causes them to charge.

These all seem like terrible explanations for Sally's behavior. (*Maybe* you could argue that if plugging in the phone wouldn't charge it, there wouldn't even be a charger to plug in, but that seems kind of beside the point.)

Here are the counterfactual versions of those explanations:

1. If Sally hadn't wanted her phone to charge, she wouldn't have plugged it in.
2. --
3. If wouldn't have caused her phone to charge, Sally wouldn't have plugged the phone in.
4. If Sally hadn't thought it would charge her phone, she wouldn't have plugged her phone in.
4. If it weren't the case that in general plugging phones in causes them to charge, Sally would not have plugged her phone in.

These are all false. Sally's plugging the phone in has nothing to do with it charging (modulo the caveat above).

Here are the model's ratings for the first counterfactual:

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


(define model
  '(
    (define apple (flip))
    (define banana (if apple (flip 0.9) (flip 0.1)))))


;; Would Y have happened if we counterfactually observed that X did not happen?
(define (counterfactual X Y model)
  (eval
   '(enumeration-query
     ;;the actual world
     ,@model
     (define eps 0.1)
     ,@(make-shadow-defines model)
     ;;the shadow model
     (not ,(shadow-rename-all Y (names model)))
     (not ,(shadow-rename-all X (names model))))))

(define (action-prior) (flip))
(define (discrete-uniform) (uniform-draw '(0 0.25 0.5 0.75 1)))

(define designer-beliefs
  '(
    (define action (action-prior))
    ;;designer believes action will probably cause effect and inaction probably won't
    (define action->effect 0.5)
    (define inaction->effect 0.5)
    (define U-effect (discrete-uniform))
    (define (effect) (if action
                         (< U-effect action->effect)
                         (< U-effect inaction->effect)))))

;;we don't know what the designer's goal is
(define goal-prior flip)

;;depending on the agent's goal, they have different posterior distributions over actions
(define action-posterior
  (mem (lambda (goal)
         (eval '(enumeration-query ,@designer-beliefs
                                   action
                                   (equal? (effect) goal))))))

;;the actual world may or may not be what the designer thinks
(define actual-world
  '(
    ;;we can infer the designer's goal
    (define designer-goal (goal-prior))
    (define (action) (apply multinomial (action-posterior designer-goal)))
    (define action->effect 0.9)
    (define inaction->effect 0.1)
    (define U-effect (discrete-uniform))
    (define (effect) (if (action)
                         (< U-effect action->effect)
                         (< U-effect inaction->effect)))))

(barplot (counterfactual '(equal? designer-goal #t) '(equal? (action) #t) actual-world)
         "if sally hadn't wanted her phone to charge, she wouldn't have plugged it in")
;;;
~~~~

### In a world where the action doesn't cause the effect and the agent knows that...

Let's say the same young Sally from before is playing with the phone and charger, but this time the charger is broken. Now even the caveat from before that the charger wouldn't even exist if chargers didn't charge phones can't save the explanations:

1. Sally plugged in her phone because she wanted it to charge.
2. Sally plugged in her phone so that it would charge.
3. Sally plugged in her phone because that would cause it to charge.
4. Sally plugged in her phone because she thought that would cause it to charge.
5. Sally plugged in her phone because in general, plugging in phones causes them to charge.

or the counterfactuals:

1. If Sally hadn't wanted her phone to charge, she wouldn't have plugged it in.
2. --
3. If wouldn't have caused her phone to charge, Sally wouldn't have plugged the phone in.
4. If Sally hadn't thought it would charge her phone, she wouldn't have plugged her phone in.
4. If it weren't the case that in general plugging phones in causes them to charge, Sally would not have plugged her phone in.

Here are the model's ratings for the first counterfactual:

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


(define model
  '(
    (define apple (flip))
    (define banana (if apple (flip 0.9) (flip 0.1)))))


;; Would Y have happened if we counterfactually observed that X did not happen?
(define (counterfactual X Y model)
  (eval
   '(enumeration-query
     ;;the actual world
     ,@model
     (define eps 0.1)
     ,@(make-shadow-defines model)
     ;;the shadow model
     (not ,(shadow-rename-all Y (names model)))
     (not ,(shadow-rename-all X (names model))))))

(define (action-prior) (flip))
(define (discrete-uniform) (uniform-draw '(0 0.25 0.5 0.75 1)))

(define designer-beliefs
  '(
    (define action (action-prior))
    ;;designer believes action will probably cause effect and inaction probably won't
    (define action->effect 0.5)
    (define inaction->effect 0.5)
    (define U-effect (discrete-uniform))
    (define (effect) (if action
                         (< U-effect action->effect)
                         (< U-effect inaction->effect)))))

;;we don't know what the designer's goal is
(define goal-prior flip)

;;depending on the agent's goal, they have different posterior distributions over actions
(define action-posterior
  (mem (lambda (goal)
         (eval '(enumeration-query ,@designer-beliefs
                                   action
                                   (equal? (effect) goal))))))

;;the actual world may or may not be what the designer thinks
(define actual-world
  '(
    ;;we can infer the designer's goal
    (define designer-goal (goal-prior))
    (define (action) (apply multinomial (action-posterior designer-goal)))
    (define action->effect 0.5)
    (define inaction->effect 0.5)
    (define U-effect (discrete-uniform))
    (define (effect) (if (action)
                         (< U-effect action->effect)
                         (< U-effect inaction->effect)))))

(barplot (counterfactual '(equal? designer-goal #t) '(equal? (action) #t) actual-world)
         "if sally hadn't wanted her phone to charge, she wouldn't have plugged it in")
;;;
~~~~
<!-- Here are some functional explanations for the action:

2. The action happened because the designer wanted the effect to happen.
3. The action happened because the designer thought it would cause the effect.
1. The action happened so that the effect would happen.
4. The action happened because it would cause the effect.
5. The action happened because the effect had to happen.

Let's look at explanation 1 ("The action happened because the designer wanted the effect to happen.") in different situations.

## The action happened because the designer wanted the effect to happen. -->

<!--
if the actual world influences the designer's beliefs

(define designer_action->effect 0.9)
(define designer_inaction->effect 0.1)

(define actual_action->effect 0.9)
(define actual_inaction->effect 0.1)

(define (action-prior) (flip))
(define (discrete-uniform) (uniform-draw '(0 0.25 0.5 0.75 1)))

(define designer-beliefs
  '(
    (define action (action-prior))
    ;;designer believes action will probably cause effect and inaction probably won't
    (define U-effect (discrete-uniform))
    (define (effect) (if action
                         (< U-effect designer_action->effect)
                         (< U-effect designer_inaction->effect)))))
    
;;designer wants effect to be true.
(define designer-goal '(equal? (effect) #t))

;;designer chooses an action conditioning on their goal being achieved
(define action-posterior
  (eval '(enumeration-query ,@designer-beliefs action ,designer-goal)))

;;the actual world may or may not be what the designer thinks
(define actual-world
  '(
    (define action (apply multinomial action-posterior))
    (define U-effect (discrete-uniform))
    (define (effect) (if action
                         (< U-effect actual_action->effect)
                         (< U-effect actual_inaction->effect)))))

;;we can predict the designers actions and what effects will result
(barplot
 (eval '(enumeration-query ,@actual-world 
                           (list (if action 'action 'inaction)
                                 (if (effect) 'effect 'no-effect))
                           #t)) "what will happen?")

-->
