---
layout: model
title: Mindsets
author: Justine Kao, Kyle McDonald, Daniel Hawthorne, Gabriel Ben-Dorr, Erin Bennett
---

People vary in their beliefs about the nature of intelligence: whether it is *stable* (entity theory) or *malleable* (incremental theory) (for a review, see Dweck & Leggett, 1988). Further, Dweck and colleagues have shown that holding different theories leads people to adopt different goals and beliefs about the value of effort, which in turn causes differences in behavior. For example, individuals who hold an entity theory of intelligence are more likely to adopt performance goals (e.g., demonstrating intelligence) and exhibit helpless behaviors (e.g., giving up on a task) after experiencing a failure, whereas those with an incremental theory tend to adopt learning goals (e.g., gaining new skills) and exhibit mastery-oriented behavior (e.g., persisting on a task).

In later work, Dweck defines two broad mindsets that separate individuals: fixed vs. growth. Someone with a fixed mindset holds an entity theory of intelligence, is likely to adopt performance goals and exhibits helpless behaviors after experiencing failure. In contrast, someone with a growth mindset holds an incremental theory, is likely to adopt learning goals and exhibits mastery-oriented behavior.

We explore different formalisms of an "entity theory" versus an "incremental theory" and simulate how a rational agent with these theories would respond to different situations. We show that many of the documented behaviors relating to mindsets can fall out as rational responses given theories. However, we also show that many different kinds of theories of intelligence could result in similar behaviors in situations that have so far been tested, making it difficult to fit a particular theory to human data and predict people's responses to situations that have not yet been tested.

### Basic variables in a model of intelligence

Different factors might play a role in someone's theory of intelligence and how it may or may not change over time. We isolate three main variables:

* **ability**: a person may have different degrees of ability in a certain domain. This could be a latent or observed variable (i.e. a person may or may not have knowledge of their own degree of ability).

* **effort**: a person may exert different degrees of effort on a certain task. A person probably knows how much effort they exerted in a task, but an outside observer may or may not know. An agent probably has control over the degree of effort the exert on a task, and so they can choose between different degrees of effort based on their relative utilities.

* **task difficulty**: different tasks have different levels of difficulty. The difficulty of a task might be an inherent property of that task, or it might be relative to a person's current abilities or preferences. In some situations, a person might be able to choose the difficulty of the task they will do, and in other situations they might not have this choice. The level of difficulty might be known or unknown to the person or to an observer.

Other factors that we do not consider in our model, but that might matter, include access to a teacher, quality of learning materials (e.g. Is there a textbook? If so, how accurate and/or accessible is it?), and type of task (e.g. is it a physical task? A performance? Does it involve reasoning and/or memorization?).

We will present a few simple possible models of different theories of intelligence and show what predictions we would make about an agent's behavior given these theories. All of these models will assume a prior distribution over abilities peaked at its center, a uniform distribution over task difficulty, and a cost function on effort such that an agent is *a priori* less likely to put in more effort.

~~~
;;;fold:
(define (scatterify-dist fn)
  (apply map (append (list (lambda (val prob) (list val prob)))
                     (enumeration-query (fn) #t))))
(define high 3)
(define mid 2)
(define low 1)
;;;

; Sample ability from high/medium/low (prior to be measured)
(define ability-prior
  (lambda () (multinomial (list high mid low) '(1 2 1))))

; Sample ability from high/medium/low
(define effort-prior
  (lambda () (multinomial (list high mid low) '(1 2 3))))

; Sample difficulty from high/medium/low
(define difficulty-prior
  (lambda () (multinomial (list high mid low) '(1 1 1))))

;;;fold:
(multilineplot (list (list "ability" (scatterify-dist ability-prior))
                     (list "effort" (scatterify-dist effort-prior))
                     (list "difficulty" (scatterify-dist difficulty-prior)))
               "Priors"
               '("value" "probability"))
;;;
~~~

### Theories of improvement

A person may have different theories about how their abilities can improve in a domain. These include the belief that improvement is impossible; the belief that improvement may occur but is unrelated to effort; and the belief that improvement is a function of effort. It may also be the case that people differ in their idea of what kind of ability can improve as a result of effort in a particular task (e.g. ability to do this exact task in the future might improve, ability to generalize to other similar tasks might improve, or a more domain-general ability/intelligence might improve).

Even considering only theories of improvement where improvement is a function of effort and no other variables, there are many different kinds of such functions. One's ability might increase linearly, with no upper bound; it might increase asymptotically to some "perfect" level; or it might increase exponentially, such that having a higher current ability allows one to improve more quickly.

And within each of these functions, are many different ways of formulating an entity theory. Perhaps different individuals have different rates of improvement, or some ability "cap" after which this individual can no longer improve but others could, or perhaps individuals improve at similar rates but start off at different levels of ability. Any combination of these sources of individual differences could describe a new theory in which intelligence is, in some sense, "fixed".

Out of this huge combinatorial space of possible theories, we model two improvement theories: one where ability stays completely stable, and one where improvement is a function of effort and difficulty. We choose the stable theory as the strongest possible entity theory and the other as a simple but somewhat plausible incremental theory.

~~~
;;;fold:
(define (scatterify-dist fn)
  (apply map (append (list (lambda (val prob) (list val prob)))
                     (enumeration-query (fn) #t))))
(define high 3)
(define mid 2)
(define low 1)

; Sample ability from high/medium/low (prior to be measured)
(define ability-prior
  (lambda () (multinomial (list high mid low) '(1 2 1))))

; Sample ability from high/medium/low
(define effort-prior
  (lambda () (multinomial (list high mid low) '(1 2 3))))

; Sample difficulty from high/medium/low
(define difficulty-prior
  (lambda () (multinomial (list high mid low) '(1 1 1))))
;;;

; Theory of improvement
(define improvement-theory
  (lambda (effort difficulty theory-type) 
    ; Takes in effort, difficulty, and theory-type (independent of ability)
    (if (equal? theory-type 'stable) 
        ; If theory-type is stable, improvement is 0
        0
        ; If theory-type is incremental, improvement is proportional to the
        ; product of effort and task difficulty
        (* 0.111 effort difficulty))))

;;;fold:
(define domain (list low mid high))
(multilineplot
   (append (list (list "stable, all difficulties"
                       (map (lambda (effort)
                              (list effort (improvement-theory effort 1 'stable)))
                            domain)))
           (map (lambda (difficulty)
                  (list (string-append "incremental, difficulty="
                                       (number->string difficulty))
                        (map (lambda (effort)
                               (list effort
                                     (improvement-theory effort difficulty 'incremental)))
                             domain)))
                domain))
   "Theories of Improvement"
   '("improvement" "effort"))
;;;
~~~

### Theories of performance

~~~
;;;fold:
(define (scatterify-dist fn)
  (apply map (append (list (lambda (val prob) (list val prob)))
                     (enumeration-query (fn) #t))))
(define high 3)
(define mid 2)
(define low 1)

; Sample ability from high/medium/low (prior to be measured)
(define ability-prior
  (lambda () (multinomial (list high mid low) '(1 2 1))))

; Sample ability from high/medium/low
(define effort-prior
  (lambda () (multinomial (list high mid low) '(1 2 3))))

; Sample difficulty from high/medium/low
(define difficulty-prior
  (lambda () (multinomial (list high mid low) '(1 1 1))))

; Theory of improvement
(define improvement-theory
  (lambda (effort difficulty theory-type) 
    ; Takes in effort, difficulty, and theory-type (independent of ability)
    (if (equal? theory-type 'stable) 
        ; If theory-type is stable, improvement is 0
        0
        ; If theory-type is incremental, improvement is proportional to the
        ; product of effort and task difficulty
        (* 0.111 effort difficulty))))
;;;

; Theory of performance
(define prob-of-success-theory
  ; Takes in ability, effort, difficulty, and theory-type
  (lambda (ability effort difficulty theory-type) 
    (min 0.95 (if (equal? theory-type 'effort-matters)
               ; If theory-type is "effort matters", then probability of success
               ; is higher with more effort and ability and lower with more difficulty 
               (/ (* ability effort) (* 3.33 difficulty))
               (if (>= ability high) 
                   ; if ability is high, probability of success is very high
                   0.9
                   ; if ability is not high, "effort matters"
                   (prob-of-success-theory ability effort difficulty 'effort-matters))))))

(define (performance-theory ablity effort difficulty theory-type)
  (flip (prob-of-success-theory ablity effort difficulty theory-type)))

;;;fold:
(define domain (list low mid high))
(define (jitter probability)
  (max 0 (min 1 (+ probability (min 0.05 (max -0.05 (gaussian 0 0.01)))))))
(multilineplot
 (append
  (apply append
         (map
          (lambda (ability)
            (map
             (lambda (difficulty)
               (list (string-append "either theory, A="
                                    (number->string ability)
                                    ", D="
                                    (number->string difficulty))
                     (map (lambda (effort)
                            (list effort
                                  (jitter
                                   (prob-of-success-theory ability
                                                           effort
                                                           difficulty
                                                           'ability-trumps-effort))))
                          domain)))
             domain))
          '(1 2)))
  (map (lambda (difficulty)
         (list (string-append "incremental, A=high, D="
                              (number->string difficulty))
               (map (lambda (effort)
                      (list effort (jitter (prob-of-success-theory high
                                                           effort
                                                           difficulty
                                                           'effort-matters))))
                    domain)))
       domain)
  (list (list "stable, A=high"
              (map (lambda (effort)
                     (list effort (jitter (prob-of-success-theory high
                                                          effort
                                                          high
                                                          'ability-trumps-effort))))
                   domain))))
 "Performance Theories"
 '("effort" "probability of success"))
;;;
~~~

### Inferring latent variables from observed ones

### Goals

* to perform well now/future
* to show one’s ability now/future
* to show one’s effort
* to improve one’s ability
* to have high ability now/future

~~~
;;;fold:
(define (plot fn title) (barplot (enumeration-query (fn) #t) title))
(define (scatterify-dist fn)
  (apply map (append (list (lambda (val prob) (list val prob)))
                     (enumeration-query (fn) #t))))
(define high 3)
(define mid 2)
(define low 1)

; Sample ability from high/medium/low (prior to be measured)
(define ability-prior
  (lambda () (multinomial (list high mid low) '(1 2 1))))

; Sample ability from high/medium/low
(define effort-prior
  (lambda () (multinomial (list high mid low) '(1 2 3))))

; Sample difficulty from high/medium/low
(define difficulty-prior
  (lambda () (multinomial (list high mid low) '(1 1 1))))

; Theory of improvement
(define improvement-theory
  (lambda (effort difficulty theory-type) 
    ; Takes in effort, difficulty, and theory-type (independent of ability)
    (if (equal? theory-type 'stable) 
        ; If theory-type is stable, improvement is 0
        0
        ; If theory-type is incremental, improvement is proportional to the
        ; product of effort and task difficulty
        (* 0.111 effort difficulty))))

; Theory of performance
(define prob-of-success-theory
  ; Takes in ability, effort, difficulty, and theory-type
  (lambda (ability effort difficulty theory-type) 
    (min 0.95 (if (equal? theory-type 'effort-matters)
               ; If theory-type is "effort matters", then probability of success
               ; is higher with more effort and ability and lower with more difficulty 
               (/ (* ability effort) (* 3.33 difficulty))
               (if (>= ability high) 
                   ; if ability is high, probability of success is very high
                   0.9
                   ; if ability is not high, "effort matters"
                   (prob-of-success-theory ability effort difficulty 'effort-matters))))))

(define (performance-theory ablity effort difficulty theory-type)
  (flip (prob-of-success-theory ablity effort difficulty theory-type)))
;;;

(define goal-prior
  (lambda (mindset)
    (multinomial (list 'judgement-now 'judgment-later
                       'ability-now 'ability-later
                       'outcome-now 'outcome-later
                       'improvement-now 'improvement-later)
;;                  ; we could set the prior value on different goals
;;                  ; differently for different mindsets, e.g.:
;;                  (if (equal? mindset 'fixed)
;;                      ; fixed: perception of ability (to prove)
;;                      '(2 2 1 1 1 1 1 1)
;;                      ; growth: improvement of ability (to improve)
;;                      '(1 1 1 1 1 1 1 1)))))
         ; or uniform:
         '(1 1 1 1 1 1 1 1))))
         
(plot goal-prior "Prior distribution on goals")
~~~

What goals in the present will a rational agent focus on, given different theories of improvement and performance?

~~~
;;;fold:
(define (plot fn title) (barplot (enumeration-query (fn) #t) title))
(define (scatterify-dist fn)
  (apply map (append (list (lambda (val prob) (list val prob)))
                     (enumeration-query (fn) #t))))
(define high 3)
(define mid 2)
(define low 1)

; Sample ability from high/medium/low (prior to be measured)
(define ability-prior
  (lambda () (multinomial (list high mid low) '(1 2 1))))

; Sample ability from high/medium/low
(define effort-prior
  (lambda () (multinomial (list high mid low) '(1 2 3))))

; Sample difficulty from high/medium/low
(define difficulty-prior
  (lambda () (multinomial (list high mid low) '(1 1 1))))

; Theory of improvement
(define improvement-theory
  (lambda (effort difficulty theory-type) 
    ; Takes in effort, difficulty, and theory-type (independent of ability)
    (if (equal? theory-type 'stable) 
        ; If theory-type is stable, improvement is 0
        0
        ; If theory-type is incremental, improvement is proportional to the
        ; product of effort and task difficulty
        (* 0.111 effort difficulty))))

; Theory of performance
(define prob-of-success-theory
  ; Takes in ability, effort, difficulty, and theory-type
  (lambda (ability effort difficulty theory-type) 
    (min 0.95 (if (equal? theory-type 'effort-matters)
               ; If theory-type is "effort matters", then probability of success
               ; is higher with more effort and ability and lower with more difficulty 
               (/ (* ability effort) (* 3.33 difficulty))
               (if (>= ability high) 
                   ; if ability is high, probability of success is very high
                   0.9
                   ; if ability is not high, "effort matters"
                   (prob-of-success-theory ability effort difficulty 'effort-matters))))))

(define (performance-theory ablity effort difficulty theory-type)
  (flip (prob-of-success-theory ablity effort difficulty theory-type)))

(define goal-prior
  (lambda (mindset)
    (multinomial (list 'judgement
                       'ability
                       'outcome
                       'improvement)
         ; or uniform:
         '(1 1 1 1))))
;;;

(define observer
  (mem (lambda (observation performance-theory-type)
         (enumeration-query
          (define ability (ability-prior))
          (define effort (effort-prior))
          (define difficulty (difficulty-prior))
          (define outcome (performance-theory ability
                                              effort
                                              difficulty
                                              performance-theory-type))
          ability
          (equal? outcome observation)))))

(define (student improvement-theory-type performance-theory-type)
  (enumeration-query
   
   ;; state of the world now
   (define ability (ability-prior))
   (define effort (effort-prior))
   (define difficulty (difficulty-prior))
   (define outcome (performance-theory ability
                                           effort
                                           difficulty
                                           performance-theory-type))
   (define improvement (improvement-theory effort
                                               difficulty
                                               improvement-theory-type))
   (define perceived-ability (apply multinomial (observer outcome
                                                              performance-theory-type)))
   
   ;; goals
   ;;; fold:
   (define goal-name (goal-prior))
   (define (goal goal-name)
     (case goal-name
           (('judgment) (> perceived-ability 1))
           (('ability) (> ability 1))
           (('outcome) (equal? outcome #t))
           (('improvement) (> improvement 0.5))))
   ;;;
   
   ;; query
   goal-name
   
   ;; condition
   (condition (goal goal-name))))

;;;fold:
(barplot (student 'stable 'ability-trumps-effort)
         "Goals given stable ability where ability trumps effort for success")
(barplot (student 'stable 'effort-matters)
         "Goals given stable ability where effort matters to success")
(barplot (student 'malleable 'ability-trumps-effort)
         "Goals given malleable ability where ability trumps effort for success")
(barplot (student 'malleable 'effort-matters)
         "Goals given malleable ability where effort matters")
;;;
~~~

If we extend the agent's model and possible goals to include the future, what goals will the agent endorse?

~~~
;;;fold:
(define (plot fn title) (barplot (enumeration-query (fn) #t) title))
(define (scatterify-dist fn)
  (apply map (append (list (lambda (val prob) (list val prob)))
                     (enumeration-query (fn) #t))))
(define high 3)
(define mid 2)
(define low 1)

; Sample ability from high/medium/low (prior to be measured)
(define ability-prior
  (lambda () (multinomial (list high mid low) '(1 2 1))))

; Sample ability from high/medium/low
(define effort-prior
  (lambda () (multinomial (list high mid low) '(1 2 3))))

; Sample difficulty from high/medium/low
(define difficulty-prior
  (lambda () (multinomial (list high mid low) '(1 1 1))))

; Theory of improvement
(define improvement-theory
  (lambda (effort difficulty theory-type) 
    ; Takes in effort, difficulty, and theory-type (independent of ability)
    (if (equal? theory-type 'stable) 
        ; If theory-type is stable, improvement is 0
        0
        ; If theory-type is incremental, improvement is proportional to the
        ; product of effort and task difficulty
        (* 0.111 effort difficulty))))

; Theory of performance
(define prob-of-success-theory
  ; Takes in ability, effort, difficulty, and theory-type
  (lambda (ability effort difficulty theory-type) 
    (min 0.95 (if (equal? theory-type 'effort-matters)
               ; If theory-type is "effort matters", then probability of success
               ; is higher with more effort and ability and lower with more difficulty 
               (/ (* ability effort) (* 3.33 difficulty))
               (if (>= ability high) 
                   ; if ability is high, probability of success is very high
                   0.9
                   ; if ability is not high, "effort matters"
                   (prob-of-success-theory ability effort difficulty 'effort-matters))))))

(define (performance-theory ablity effort difficulty theory-type)
  (flip (prob-of-success-theory ablity effort difficulty theory-type)))

(define goal-prior
  (lambda (mindset)
    (multinomial (list 'judgement
                       'ability
                       'outcome
                       'improvement
                       'judgement-later
                       'ability-later
                       'outcome-later
                       'improvement-later)
         ; or uniform:
         '(1 1 1 1 1 1 1 1))))

(define observer
  (mem (lambda (observation performance-theory-type)
         (enumeration-query
          (define ability (ability-prior))
          (define effort (effort-prior))
          (define difficulty (difficulty-prior))
          (define outcome (performance-theory ability
                                              effort
                                              difficulty
                                              performance-theory-type))
          ability
          (equal? outcome observation)))))
;;;

(define (student improvement-theory-type performance-theory-type)
  (enumeration-query
   
   ;; state of the world now
   (define ability (ability-prior))
   (define effort (effort-prior))
   (define difficulty (difficulty-prior))
   (define outcome (performance-theory ability
                                           effort
                                           difficulty
                                           performance-theory-type))
   (define improvement (improvement-theory effort
                                               difficulty
                                               improvement-theory-type))
   (define perceived-ability (apply multinomial (observer outcome
                                                              performance-theory-type)))

   ;; state of the world later
   (define ability-later (+ ability improvement))
   (define effort-later (effort-prior))
   (define difficulty-later (difficulty-prior))
   (define outcome-later (performance-theory ability-later
                                           effort-later
                                           difficulty-later
                                           performance-theory-type))
   (define improvement-later (improvement-theory effort-later
                                               difficulty-later
                                               improvement-theory-type))
   (define perceived-ability-later (apply multinomial (observer outcome-later
                                                              performance-theory-type)))
   
   ;; goals
   ;;; fold:
   (define goal-name (goal-prior))
   (define (goal goal-name)
     (case goal-name
           (('judgment) (> perceived-ability 1))
           (('ability) (> ability 1))
           (('outcome) (equal? outcome #t))
           (('improvement) (> improvement 0.5))
           (('judgment-later) (> perceived-ability-later 1))
           (('ability-later) (> ability-later 1))
           (('outcome-later) (equal? outcome-later #t))
           (('improvement-later) (> improvement-later 0.5))))
   ;;;
   
   ;; query
   goal-name
   
   ;; condition
   (condition (goal goal-name))))

;;;fold:
(barplot (student 'stable 'ability-trumps-effort)
         "Goals given stable ability where ability trumps effort for success")
(barplot (student 'stable 'effort-matters)
         "Goals given stable ability where effort matters to success")
(barplot (student 'malleable 'ability-trumps-effort)
         "Goals given malleable ability where ability trumps effort for success")
(barplot (student 'malleable 'effort-matters)
         "Goals given malleable ability where effort matters")
;;;
~~~

### Helpless versus mastery-oriented responses to failure

~~~
;;;fold:
(define (plot fn title) (barplot (enumeration-query (fn) #t) title))
(define (scatterify-fn fn)
  (apply map (append (list (lambda (val prob) (list val prob)))
                     (enumeration-query (fn) #t))))
(define (scatterify-dist dist)
  (apply map (append (list (lambda (val prob) (list val prob)))
                     dist)))
(define high 3)
(define mid 2)
(define low 1)

; Sample ability from high/medium/low (prior to be measured)
(define ability-prior
  (lambda () (multinomial (list high mid low) '(1 2 1))))

; Sample ability from high/medium/low
(define effort-prior
  (lambda () (multinomial (list high mid low) '(1 2 3))))

; Sample difficulty from high/medium/low
(define difficulty-prior
  (lambda () (multinomial (list high mid low) '(1 1 1))))

; Theory of improvement
(define improvement-theory
  (lambda (effort difficulty theory-type) 
    ; Takes in effort, difficulty, and theory-type (independent of ability)
    (if (equal? theory-type 'stable) 
        ; If theory-type is stable, improvement is 0
        0
        ; If theory-type is incremental, improvement is proportional to the
        ; product of effort and task difficulty
        (* 0.111 effort difficulty))))

; Theory of performance
(define prob-of-success-theory
  ; Takes in ability, effort, difficulty, and theory-type
  (lambda (ability effort difficulty theory-type) 
    (min 0.95 (if (equal? theory-type 'effort-matters)
                  ; If theory-type is "effort matters", then probability of success
                  ; is higher with more effort and ability and lower with more difficulty 
                  (/ (* ability effort) (* 3.33 difficulty))
                  (if (>= ability high) 
                      ; if ability is high, probability of success is very high
                      0.9
                      ; if ability is not high, "effort matters"
                      (prob-of-success-theory ability effort difficulty 'effort-matters))))))

(define (performance-theory ablity effort difficulty theory-type)
  (flip (prob-of-success-theory ablity effort difficulty theory-type)))

(define goal-prior
  (lambda (mindset)
    (multinomial (list 'judgement
                       'ability
                       'outcome
                       'improvement
                       'judgement-later
                       'ability-later
                       'outcome-later
                       'improvement-later)
                 ; or uniform:
                 '(1 1 1 1 1 1 1 1))))

(define observer
  (mem (lambda (observation performance-theory-type)
         (enumeration-query
          (define ability (ability-prior))
          (define effort (effort-prior))
          (define difficulty (difficulty-prior))
          (define outcome (performance-theory ability
                                              effort
                                              difficulty
                                              performance-theory-type))
          ability
          (equal? outcome observation)))))
;;;

(define (student past-outcome improvement-theory-type performance-theory-type)
  (enumeration-query

   ;; state of the world now

   ;; use past outcome to infer current ability
   (define ability (apply multinomial (observer past-outcome performance-theory-type)))
   (define effort (effort-prior))
   (define difficulty (difficulty-prior))
   (define outcome (performance-theory ability
                                       effort
                                       difficulty
                                       performance-theory-type))
   (define improvement (improvement-theory effort
                                           difficulty
                                           improvement-theory-type))
   (define perceived-ability (apply multinomial (observer outcome
                                                          performance-theory-type)))

   ;; state of the world later

   (define ability-later (round (+ ability improvement)))
   (define effort-later (effort-prior))
   (define difficulty-later (difficulty-prior))
   (define outcome-later (performance-theory ability-later
                                             effort-later
                                             difficulty-later
                                             performance-theory-type))
   (define improvement-later (improvement-theory effort-later
                                                 difficulty-later
                                                 improvement-theory-type))
   (define perceived-ability-later (apply multinomial (observer outcome-later
                                                                performance-theory-type)))

   ;; goals
   ;;; fold:
   (define goal-name (goal-prior))
   (define (goal goal-name)
     (case goal-name
           (('judgment) (> perceived-ability 1))
           (('ability) (> ability 1))
           (('outcome) (equal? outcome #t))
           (('improvement) (> improvement 0.5))
           (('judgment-later) (> perceived-ability-later 1))
           (('ability-later) (> ability-later 1))
           (('outcome-later) (equal? outcome-later #t))
           (('improvement-later) (> improvement-later 0.5))))
   ;;;
   
   ;; query
   effort

   ;; condition
   (goal goal-name)))
;;;

(define success #t)
(define failure #f)
(multilineplot (list (list "fixed, success"
                           (scatterify-dist (student success 'stable 'ability-trumps-effort)))
                     (list "fixed, failure"
                           (scatterify-dist (student failure 'stable 'ability-trumps-effort)))
                     (list "growth, success"
                           (scatterify-dist (student success 'malleable 'effort-matters)))
                     (list "growth, failure"
                           (scatterify-dist (student failure 'malleable 'effort-matters))))
               "Effort after Sucess and Failure"
               '("effort" "probability"))
~~~

#### Performance theory or improvement theory?

### Endorsement of different goals given different theories

#### Learning goal or discounting?