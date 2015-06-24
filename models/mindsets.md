---
layout: model
author: Justine Kao, Kyle McDonald, Daniel Hawthorne, Gabriel Ben-Dorr, Erin Bennett
model-language: church
---

# Mindsets

* table of contents
{:toc}

People vary in their beliefs about the nature of intelligence: whether it is *stable* (entity theory) or *malleable* (incremental theory) (for a review, see Dweck & Leggett, 1988). Further, Dweck and colleagues have shown that holding different theories leads people to adopt different goals and beliefs about the value of effort, which in turn causes differences in behavior. For example, individuals who hold an entity theory of intelligence are more likely to adopt performance goals (e.g., demonstrating intelligence) and exhibit helpless behaviors (e.g., giving up on a task) after experiencing a failure, whereas those with an incremental theory tend to adopt learning goals (e.g., gaining new skills) and exhibit mastery-oriented behavior (e.g., persisting on a task).

In later work, Dweck defines two broad mindsets that separate individuals: fixed vs. growth. Someone with a fixed mindset holds an entity theory of intelligence, is likely to adopt performance goals and exhibits helpless behaviors after experiencing failure. In contrast, someone with a growth mindset holds an incremental theory, is likely to adopt learning goals and exhibits mastery-oriented behavior.

We explore different formalisms of an "entity theory" versus an "incremental theory" and simulate how a rational agent with these theories would respond to different situations. We show that many of the documented behaviors relating to mindsets can fall out as rational responses given theories. However, we also show that many different kinds of theories of intelligence could result in similar behaviors in situations that have so far been tested, making it difficult to fit a particular theory to human data and predict people's responses to situations that have not yet been tested.

## Theories

### Basic variables in a model of intelligence

Different factors might play a role in someone's theory of intelligence and how it may or may not change over time. We isolate three main variables:

* **ability**: a person may have different degrees of ability in a certain domain. This could be a latent or observed variable (i.e. a person may or may not have knowledge of their own degree of ability).

* **effort**: a person may exert different degrees of effort on a certain task. A person probably knows how much effort they exerted in a task, but an outside observer may or may not know. An agent probably has control over the degree of effort the exert on a task, and so they can choose between different degrees of effort based on their relative utilities.

* **task difficulty**: different tasks have different levels of difficulty. The difficulty of a task might be an inherent property of that task, or it might be relative to a person's current abilities or preferences. In some situations, a person might be able to choose the difficulty of the task they will do, and in other situations they might not have this choice. The level of difficulty might be known or unknown to the person or to an observer.

Other factors that we do not consider in our model, but that might matter, include access to a teacher, quality of learning materials (e.g. Is there a textbook? If so, how accurate and/or accessible is it?), and type of task (e.g. is it a physical task? A performance? Does it involve reasoning and/or memorization?).

We will present a few simple possible models of different theories of intelligence and show what predictions we would make about an agent's behavior given these theories. These models will assume a prior distribution over abilities peaked at its center and uniform distributions over task difficulty and effort<sup><a href="#footnote1" id="ref1">[1]</a></sup>.

~~~
;;;fold:
(define (scatterify-dist fn)
  (apply map (append (list (lambda (val prob) (list val prob)))
                     (enumeration-query (fn) #t))))
(define high 1)
(define mid 0.5)
(define low 0.1)
(define easy 0.2)
(define difficult 0.8)
;;;

; Sample ability from high/medium/low (prior to be measured)
(define ability-prior
  (lambda () (multinomial (list high mid low) '(1 2 1))))

; Sample ability from high/medium/low
(define effort-prior
  (lambda () (multinomial (list high mid low) '(1 1 1))))

; Sample difficulty from high/medium/low
(define difficulty-prior
  (lambda () (multinomial (list easy difficult) '(1 1))))

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

Out of this huge combinatorial space of possible theories, we model two improvement theories: one where ability stays completely stable (`stable`), and one where improvement is a function of effort and difficulty (`incremental`). We choose the stable theory as the strongest possible entity theory and the other as a simple but somewhat plausible incremental theory.

Ability at any future time is then simply the sum of ability at the previous time and whatever improvement took place at the previous time.

~~~
;;;fold:
(define (scatterify-dist fn)
  (apply map (append (list (lambda (val prob) (list val prob)))
                     (enumeration-query (fn) #t))))
(define high 1)
(define mid 0.5)
(define low 0.1)
(define easy 0.2)
(define difficult 0.8)

; Sample ability from high/medium/low (prior to be measured)
(define ability-prior
  (lambda () (multinomial (list high mid low) '(1 2 1))))

; Sample ability from high/medium/low
(define effort-prior
  (lambda () (multinomial (list high mid low) '(1 1 1))))

; Sample difficulty from high/medium/low
(define difficulty-prior
  (lambda () (multinomial (list easy difficult) '(1 1))))
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
        (* effort difficulty))))

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

Another function that we need to define is a student's theory of performance. What variables affect performance? And how?

* *Performance*: a person may have different degrees of performance on a task. For now we assume that performance is binary (success/failure). We assume that performance can be observed by the person and is objective.

<!-- what are some other theories of performance? -->

We create two different performance theories. In one theory, people believe that high ability predicts high performance with high probability, regardless of effort and task difficulty (`ability-trumps-effort`). In this theory, when ability is low, people believe that the probability of success is proportional to ability and effort and inversely proportional to task difficulty. We create another theory in which people believe that probability of success is always proportional to ability and effort and inversely proportional to task difficulty (`effort-matters`).

~~~
;;;fold:
(define (scatterify-dist fn)
  (apply map (append (list (lambda (val prob) (list val prob)))
                     (enumeration-query (fn) #t))))
(define high 1)
(define mid 0.5)
(define low 0.1)
(define easy 0.2)
(define difficult 0.8)

; Sample ability from high/medium/low (prior to be measured)
(define ability-prior
  (lambda () (multinomial (list high mid low) '(1 2 1))))

; Sample ability from high/medium/low
(define effort-prior
  (lambda () (multinomial (list high mid low) '(1 1 1))))

; Sample difficulty from high/medium/low
(define difficulty-prior
  (lambda () (multinomial (list easy difficult) '(1 1))))

; Theory of improvement
(define improvement-theory
  (lambda (effort difficulty theory-type) 
    ; Takes in effort, difficulty, and theory-type (independent of ability)
    (if (equal? theory-type 'stable) 
        ; If theory-type is stable, improvement is 0
        0
        ; If theory-type is incremental, improvement is proportional to the
        ; product of effort and task difficulty
        (* effort difficulty))))
;;;

; Theory of performance
(define prob-of-success-theory
  ; Takes in ability, effort, difficulty, and theory-type
  (lambda (ability effort difficulty theory-type) 
    (min 0.95 (if (equal? theory-type 'effort-matters)
                  ; If theory-type is "effort matters", then probability of success
                  ; is higher with more effort and ability and lower with more difficulty 
                  (/ (* ability effort) difficulty)
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
(define (plot-theory theory label)
  (multilineplot
   (apply append
          (map
           (lambda (ability)
             (map
              (lambda (difficulty)
                (list (string-append "ability="
                                     (number->string ability)
                                     ", difficulty="
                                     (number->string difficulty))
                      (map (lambda (effort)
                             (list effort
                                   (jitter
                                    (prob-of-success-theory ability
                                                            effort
                                                            difficulty
                                                            theory))))
                           domain)))
              (list easy difficult)))
           (list low mid high)))
   label
   '("effort" "probability of success")))

(plot-theory 'ability-trumps-effort
             "Theory of Performance: effort (and difficulty) doesn't matter if you have high ability")
(plot-theory 'effort-matters
             "Theory of Performance: effort (and difficulty) always matters")
;;;
~~~

## Inferences

Simply based on these differences in theories of performance and improvement, we can make interesting predictions about a person’s perceptions of her present and future ability and probability of success. For example, given a failure in the past, people with the `ability-trumps-effort` performance theory believe that they are more likely to have low ability, while people with an `effort-matters` performance theory do not draw this inference as strongly and are perhaps more likely to attribute failure to a lack of effort.

* *Perceived ability*: given a person’s performance on a task, her effort, the task difficulty, and her theory of performance, the person may infer her own ability (assuming that ability is a latent and unobserved variable).

~~~
;;;fold:
(define (jitter-prob probability)
  (max 0 (min 1 (+ probability (min 0.05 (max -0.05 (gaussian 0 0.01)))))))
(define (jitter-num num)
  (+ num (min 0.05 (max -0.05 (gaussian 0 0.01)))))
(define (scatterify-fn fn)
  (apply map (append (list (lambda (val prob) (list val prob)))
                     (enumeration-query (fn) #t))))
(define (scatterify-dist dist)
  (apply map (append (list (lambda (val prob) (list val prob)))
                     dist)))
(define high 1)
(define mid 0.5)
(define low 0.1)
(define easy 0.2)
(define difficult 0.8)

; Sample ability from high/medium/low (prior to be measured)
(define ability-prior
  (lambda () (multinomial (list high mid low) '(1 2 1))))

; Sample ability from high/medium/low
(define effort-prior
  (lambda () (multinomial (list high mid low) '(1 1 1))))

; Sample difficulty from high/medium/low
(define difficulty-prior
  (lambda () (multinomial (list easy difficult) '(1 1))))

; Theory of improvement
(define improvement-theory
  (lambda (effort difficulty theory-type) 
    ; Takes in effort, difficulty, and theory-type (independent of ability)
    (if (equal? theory-type 'stable) 
        ; If theory-type is stable, improvement is 0
        0
        ; If theory-type is incremental, improvement is proportional to the
        ; product of effort and task difficulty
        (* effort difficulty))))

; Theory of performance
(define prob-of-success-theory
  ; Takes in ability, effort, difficulty, and theory-type
  (lambda (ability effort difficulty theory-type) 
    (min 0.95 (if (equal? theory-type 'effort-matters)
                  ; If theory-type is "effort matters", then probability of success
                  ; is higher with more effort and ability and lower with more difficulty 
                  (/ (* ability effort) difficulty)
                  (if (>= ability high) 
                      ; if ability is high, probability of success is very high
                      0.9
                      ; if ability is not high, "effort matters"
                      (prob-of-success-theory ability effort difficulty 'effort-matters))))))

(define (performance-theory ablity effort difficulty theory-type)
  (flip (prob-of-success-theory ablity effort difficulty theory-type)))
;;;

(define observer ;;could be student themself or an outside observer
  (mem (lambda (observed-outcome
                performance-theory-type
                observed-effort ;; effort could be observed, or not.
                observed-difficulty ;; difficulty could be observed, or not.
                )
         (enumeration-query
          ;; state now
          (define ability (ability-prior))
          (define effort (effort-prior))
          (define difficulty (difficulty-prior))
          (define outcome (performance-theory ability
                                              effort
                                              difficulty
                                              performance-theory-type))

          ability
          (and (equal? outcome observed-outcome)
               (if observed-effort
                   (equal? effort observed-effort)
                   #t)
               (if observed-difficulty
                   (equal? difficulty observed-difficulty)
                   #t))))))

;;;fold:
(define success #t)
(define failure #f)

(multilineplot (list (list "ability-trumps-effort, success"
                           (scatterify-dist
                            (observer success
                                      'ability-trumps-effort)))
                     (list "ability-trumps-effort, failure"
                           (scatterify-dist
                            (observer failure
                                      'ability-trumps-effort)))
                     (list "effort-matters, success"
                           (scatterify-dist
                            (observer success
                                      'effort-matters)))
                     (list "effort-matters, failure"
                           (scatterify-dist
                            (observer failure
                                      'effort-matters))))
               "Posterior Distribution on Ability Given Outcome"
               '("ability" "probability"))

(define (expected-ability observed-outcome performance-theory-type)
  (map (lambda (e)
         (define dist
           (observer observed-outcome
                     performance-theory-type
                     e))
         (define vals (first dist))
         (define probs (second dist))
         (define expected (sum (map * vals probs)))
         (list e expected))
       (list low mid high)))
(multilineplot (list (list "ability-trumps-effort, success"
                           (expected-ability  success
                                              'ability-trumps-effort))
                     (list "ability-trumps-effort, failure"
                           (expected-ability  failure
                                              'ability-trumps-effort))
                     (list "effort-matters, success"
                           (expected-ability  success
                                              'effort-matters))
                     (list "effort-matters, failure"
                           (expected-ability  failure
                                              'effort-matters)))
               "Inferred Ability as a Function of Observed Effort Given Outcome"
               '("effort" "inferred ability"))
;;;
~~~

### Prediction of Future Outcome

Given a failure in the past, people with a stable improvement theory are more likely to predict failure in the future, while people with an incremental improvement theory mindset are willing to allow for improvement in ability and increased effort to result in a higher probability of success.

~~~
;;;fold:
(define (jitter-prob probability)
  (max 0 (min 1 (+ probability (min 0.05 (max -0.05 (gaussian 0 0.01)))))))
(define (jitter-num num)
  (+ num (min 0.05 (max -0.05 (gaussian 0 0.01)))))
(define (scatterify-fn fn)
  (apply map (append (list (lambda (val prob) (list val prob)))
                     (enumeration-query (fn) #t))))
(define (scatterify-dist dist)
  (apply map (append (list (lambda (val prob) (list val prob)))
                     dist)))
(define high 1)
(define mid 0.5)
(define low 0.1)
(define easy 0.2)
(define difficult 0.8)

; Sample ability from high/medium/low (prior to be measured)
(define ability-prior
  (lambda () (multinomial (list high mid low) '(1 2 1))))

; Sample ability from high/medium/low
(define effort-prior
  (lambda () (multinomial (list high mid low) '(1 1 1))))

; Sample difficulty from high/medium/low
(define difficulty-prior
  (lambda () (multinomial (list easy difficult) '(1 1))))

; Theory of improvement
(define improvement-theory
  (lambda (effort difficulty theory-type) 
    ; Takes in effort, difficulty, and theory-type (independent of ability)
    (if (equal? theory-type 'stable) 
        ; If theory-type is stable, improvement is 0
        0
        ; If theory-type is incremental, improvement is proportional to the
        ; product of effort and task difficulty
        (* effort difficulty))))

; Theory of performance
(define prob-of-success-theory
  ; Takes in ability, effort, difficulty, and theory-type
  (lambda (ability effort difficulty theory-type) 
    (min 0.95 (if (equal? theory-type 'effort-matters)
                  ; If theory-type is "effort matters", then probability of success
                  ; is higher with more effort and ability and lower with more difficulty 
                  (/ (* ability effort) difficulty)
                  (if (>= ability high) 
                      ; if ability is high, probability of success is very high
                      0.9
                      ; if ability is not high, "effort matters"
                      (prob-of-success-theory ability effort difficulty 'effort-matters))))))

(define (performance-theory ablity effort difficulty theory-type)
  (flip (prob-of-success-theory ablity effort difficulty theory-type)))

(define observer ;;could be student themself or an outside observer
  (mem (lambda (observed-outcome
                performance-theory-type
                improvement-theory-type
                observed-effort ;; effort could be observed, or not.
                observed-difficulty ;; difficulty could be observed, or not.
                )
         (enumeration-query
          ;; state now
          (define ability (ability-prior))
          (define effort (effort-prior))
          (define difficulty (difficulty-prior))
          (define outcome (performance-theory ability
                                              effort
                                              difficulty
                                              performance-theory-type))
;;;
          (define improvement (improvement-theory effort
                                                  difficulty
                                                  improvement-theory-type))
          (define future-ability (+ ability improvement))
          (define future-effort (effort-prior))
          (define future-difficulty (difficulty-prior))
          (define future-outcome (performance-theory future-ability
                                                     future-effort
                                                     future-difficulty
                                                     performance-theory-type))

          future-outcome
;;;fold:
          (and (equal? outcome observed-outcome)
               (if observed-effort
                   (equal? effort observed-effort)
                   #t)
               (if observed-difficulty
                   (equal? difficulty observed-difficulty)
                   #t))))))

(define success #t)
(define failure #f)

(multilineplot (list (list "incremental"
                           (scatterify-dist
                            (observer failure
                                      'ability-trumps-effort
                                      'incremental)))
                     (list "stable"
                           (scatterify-dist
                            (observer failure
                                      'ability-trumps-effort
                                      'stable))))
               "Posterior Distribution on Future Outcomes given Failure when Ability Trumps Effort"
               '("future outcome" "probability"))

(multilineplot (list (list "incremental"
                           (scatterify-dist
                            (observer failure
                                      'effort-matters
                                      'incremental)))
                     (list "stable"
                           (scatterify-dist
                            (observer failure
                                      'effort-matters
                                      'stable))))
               "Posterior Distribution on Future Outcomes given Failure when Effort Always Matters"
               '("future outcome" "probability"))
;;;
~~~

### Choice of Action given Goals

Besides having different theories about performance and improvement, fixed and growth mindsets may also be characterized by different goals. We consider a finite set of goals that a person may have: to create the perception of high ability<sup><a href="#footnote2" id="ref2">[2]</a></sup>; to actually have high ability; to succeed; to improve. These four goals are also crossed with time, i.e. a person may care about achieving these goals in the present or in the future. We can give different mindsets different distributions over goals.

Even with an incremental theory of improvement and a theory of performance that effort and difficulty always matter, an agent's goals influence how much effort they choose to expend.

When the agent is endorsing a "learning goal", such as `improvement` or `future ability` and when the agent believes improvement is possible, the agent is likely to try (more probability mass is on higher values of effort). When the agent is endorsing a `judgment` goal, i.e. to "prove" their ability, they are unlikely to try hard, and in fact they try slightly less hard than even their cost prior when they have recently experienced a failure (helpless behavior). Endorsing a performance goal results in trying somewhat hard in these graphs, where the agent thinks effort matters.

WARNING ABOUT RUNNING THE CODE BELOW! It may take about 1 minute to run. 

~~~
;;;fold:
(define (jitter-prob probability)
  (max 0 (min 1 (+ probability (min 0.05 (max -0.05 (gaussian 0 0.01)))))))
(define (jitter-num num)
  (+ num (min 0.05 (max -0.05 (gaussian 0 0.01)))))
(define (scatterify-fn fn)
  (apply map (append (list (lambda (val prob) (list val prob)))
                     (enumeration-query (fn) #t))))
(define (scatterify-dist dist jitter)
  (if jitter
      (apply map (append (list (lambda (val prob) (list val (jitter-prob prob))))
                         dist))
      (apply map (append (list (lambda (val prob) (list val prob)))
                         dist))))
(define high 1)
(define mid 0.5)
(define low 0.1)
(define easy 0.2)
(define difficult 0.8)

; Sample ability from high/medium/low (prior to be measured)
(define ability-prior
  (lambda () (multinomial (list high mid low) '(1 2 1))))

; Sample ability from high/medium/low
(define effort-prior
  (lambda () (multinomial (list high mid low) '(1 1 1))))

; Sample difficulty from high/medium/low
(define difficulty-prior
  (lambda () (multinomial (list easy difficult) '(1 1))))

; Theory of improvement
(define improvement-theory
  (lambda (effort difficulty theory-type) 
    ; Takes in effort, difficulty, and theory-type (independent of ability)
    (if (equal? theory-type 'stable) 
        ; If theory-type is stable, improvement is 0
        0
        ; If theory-type is incremental, improvement is proportional to the
        ; product of effort and task difficulty
        (* effort difficulty))))

; Theory of performance
(define prob-of-success-theory
  ; Takes in ability, effort, difficulty, and theory-type
  (lambda (ability effort difficulty theory-type) 
    (min 0.95 (if (equal? theory-type 'effort-matters)
                  ; If theory-type is "effort matters", then probability of success
                  ; is higher with more effort and ability and lower with more difficulty 
                  (/ (* ability effort) difficulty)
                  (if (>= ability high) 
                      ; if ability is high, probability of success is very high
                      0.9
                      ; if ability is not high, "effort matters"
                      (prob-of-success-theory ability effort difficulty 'effort-matters))))))

(define (performance-theory ablity effort difficulty theory-type)
  (flip (prob-of-success-theory ablity effort difficulty theory-type)))

(define observer ;;could be student themself or an outside observer
  (mem (lambda (observed-outcome
                performance-theory-type
                observed-effort ;; effort could be observed, or not.
                observed-difficulty ;; difficulty could be observed, or not.
                )
         (enumeration-query
          ;; state now
          (define ability (ability-prior))
          (define effort (effort-prior))
          (define difficulty (difficulty-prior))
          (define outcome (performance-theory ability
                                              effort
                                              difficulty
                                              performance-theory-type))

          ability
          (and (equal? outcome observed-outcome)
               (if observed-effort
                   (equal? effort observed-effort)
                   #t)
               (if observed-difficulty
                   (equal? difficulty observed-difficulty)
                   #t))))))
;;;

(define (effort-cost-prior) (multinomial (list high mid low) '(1 2 3)))

(define student ;;could be student themself or an outside observer
  (mem (lambda (goal-name
                performance-theory-type
                improvement-theory-type
                past-outcome)
         (enumeration-query
          ;; state now
          (define ability
            (if past-outcome
                (apply multinomial
                       (observer (equal? 'success past-outcome) performance-theory-type))
                (ability-prior)))
          (define effort (effort-cost-prior))
          (define difficulty (difficulty-prior))
          (define outcome (performance-theory ability
                                              effort
                                              difficulty
                                              performance-theory-type))
          (define perceived-ability
            (apply multinomial (observer outcome
                                         performance-theory-type
                                         effort ;; could be observed or not
                                         ;difficulty ;; could be observed or not
                                         )))
          (define improvement (improvement-theory effort
                                                  difficulty
                                                  improvement-theory-type))
          (define future-ability (+ ability improvement))
          ;; assume they don't know how hard they'll try in the future
          (define future-effort (effort-prior))
          (define future-difficulty (difficulty-prior))
          (define future-outcome (performance-theory future-ability
                                              future-effort
                                              future-difficulty
                                              performance-theory-type))
          (define future-perceived-ability
            (apply multinomial (observer future-outcome
                                         performance-theory-type
                                         future-effort ;; could be observed or not
                                         ;future-difficulty ;; could be observed or not
                                         )))
          (define future-improvement (improvement-theory future-effort
                                                  future-difficulty
                                                  improvement-theory-type))

          (define (goal goal-name)
            (case goal-name
                  (('judgment) (> perceived-ability 0.5))
                  (('ability) (> ability 0.5))
                  (('outcome) (equal? outcome #t))
                  (('improvement) (> improvement 0.3))
                  (('future-judgment) (> future-perceived-ability 0.5))
                  (('future-ability) (> future-ability 0.5))
                  (('future-outcome) (equal? future-outcome #t))
                  (('future-improvement) (> future-improvement 0.3))))

          effort
          (goal goal-name)))))

;;;fold:
(define (plot-effort g improvement-theory-type past-outcome)
  (list (symbol->string g)
        (scatterify-dist
         (student g
                  'effort-matters
                  improvement-theory-type
                  past-outcome))))

(multilineplot (list (list "prior" (scatterify-fn effort-cost-prior))
                     (plot-effort 'ability 'incremental)
                     (plot-effort 'judgment 'incremental)
                     (plot-effort 'outcome 'incremental)
                     (plot-effort 'improvement 'incremental)
                     (plot-effort 'future-ability 'incremental)
                     (plot-effort 'future-judgment 'incremental)
                     (plot-effort 'future-outcome 'incremental)
                     (plot-effort 'future-improvement 'incremental))
               "Choice of Effort Given Goal (incremental)"
               '("effort" "probability"))
(multilineplot (list (list "prior" (scatterify-fn effort-cost-prior))
                     (plot-effort 'ability 'incremental 'failure)
                     (plot-effort 'judgment 'incremental 'failure)
                     (plot-effort 'outcome 'incremental 'failure)
                     (plot-effort 'improvement 'incremental 'failure)
                     (plot-effort 'future-ability 'incremental 'failure)
                     (plot-effort 'future-judgment 'incremental 'failure)
                     (plot-effort 'future-outcome 'incremental 'failure)
                     (plot-effort 'future-improvement 'incremental 'failure))
               "Choice of Effort Given Goal and Past Failure (incremental)"
               '("effort" "probability"))
(multilineplot (list (list "prior" (scatterify-fn effort-cost-prior))
                     (plot-effort 'ability 'incremental 'success)
                     (plot-effort 'judgment 'incremental 'success)
                     (plot-effort 'outcome 'incremental 'success)
                     (plot-effort 'improvement 'incremental 'success)
                     (plot-effort 'future-ability 'incremental 'success)
                     (plot-effort 'future-judgment 'incremental 'success)
                     (plot-effort 'future-outcome 'incremental 'success)
                     (plot-effort 'future-improvement 'incremental 'success))
               "Choice of Effort Given Goal and Past Success (incremental)"
               '("effort" "probability"))
(multilineplot (list (list "prior" (scatterify-fn effort-cost-prior))
                     (plot-effort 'ability 'stable)
                     (plot-effort 'judgment 'stable)
                     (plot-effort 'outcome 'stable)
                     (plot-effort 'improvement 'stable)
                     (plot-effort 'future-ability 'stable)
                     (plot-effort 'future-judgment 'stable)
                     (plot-effort 'future-outcome 'stable)
                     (plot-effort 'future-improvement 'stable))
               "Choice of Effort Given Goal (stable)"
               '("effort" "probability"))
(multilineplot (list (list "prior" (scatterify-fn effort-cost-prior))
                     (plot-effort 'ability 'stable 'failure)
                     (plot-effort 'judgment 'stable 'failure)
                     (plot-effort 'outcome 'stable 'failure)
                     (plot-effort 'improvement 'stable 'failure)
                     (plot-effort 'future-ability 'stable 'failure)
                     (plot-effort 'future-judgment 'stable 'failure)
                     (plot-effort 'future-outcome 'stable 'failure)
                     (plot-effort 'future-improvement 'stable 'failure))
               "Choice of Effort Given Goal and Past Failure (stable)"
               '("effort" "probability"))
(multilineplot (list (list "prior" (scatterify-fn effort-cost-prior))
                     (plot-effort 'ability 'stable 'success)
                     (plot-effort 'judgment 'stable 'success)
                     (plot-effort 'outcome 'stable 'success)
                     (plot-effort 'improvement 'stable 'success)
                     (plot-effort 'future-ability 'stable 'success)
                     (plot-effort 'future-judgment 'stable 'success)
                     (plot-effort 'future-outcome 'stable 'success)
                     (plot-effort 'future-improvement 'stable 'success))
               "Choice of Effort Given Goal and Past Success (stable)"
               '("effort" "probability"))
;;;
~~~

WARNING ABOUT RUNNING THE CODE ABOVE! It may take about 1 minute to run. 

### Averaging Over Several Possible Goals

We choose a prior distribution over goals for the different goals so that a "proving" distribution over goals puts emphasis on judgment (proving their ability to themselves or others) and less on improvement and so that a "learning" distribution over goals puts more emphasis on improvement and less on judgment.

~~~
(define (goal-prior goal-distribution-type)
  (multinomial (list 'judgment
                     'judgment-later
                     'ability
                     'ability-later
                     'outcome
                     'outcome-later
                     'improvement
                     'improvement-later)
               (if (equal? goal-distribution-type 'proving)
                   '(5 5 3 3 3 3 1 1)
                   '(1 1 3 3 3 3 5 5))))

;;;fold:
(define (plot fn title) (barplot (enumeration-query (fn) #t) title))
(plot (lambda () (goal-prior 'proving)) "Proving Goals")
(plot (lambda () (goal-prior 'learning)) "Learning Goals")
;;;
~~~

Given these goal priors and an incremental theory in which effort and difficulty always matter to performance, we can see what a rational agent will do in different contexts, conditioned on achieving (one of) their goal(s).

WARNING ABOUT RUNNING THE CODE BELOW! It may take about 2 minutes to run. 

~~~

;;;fold:
(define (jitter-prob probability)
  (max 0 (min 1 (+ probability (min 0.05 (max -0.05 (gaussian 0 0.01)))))))
(define (jitter-num num)
  (+ num (min 0.05 (max -0.05 (gaussian 0 0.01)))))
(define (scatterify-fn fn)
  (apply map (append (list (lambda (val prob) (list val prob)))
                     (enumeration-query (fn) #t))))
(define (scatterify-dist dist jitter)
  (if jitter
      (apply map (append (list (lambda (val prob) (list val (jitter-prob prob))))
                         dist))
      (apply map (append (list (lambda (val prob) (list val prob)))
                         dist))))
(define high 1)
(define mid 0.5)
(define low 0.1)
(define easy 0.2)
(define difficult 0.8)

; Sample ability from high/medium/low (prior to be measured)
(define ability-prior
  (lambda () (multinomial (list high mid low) '(1 2 1))))

; Sample ability from high/medium/low
(define effort-prior
  (lambda () (multinomial (list high mid low) '(1 1 1))))

; Sample difficulty from high/medium/low
(define difficulty-prior
  (lambda () (multinomial (list easy difficult) '(1 1))))

; Theory of improvement
(define improvement-theory
  (lambda (effort difficulty theory-type) 
    ; Takes in effort, difficulty, and theory-type (independent of ability)
    (if (equal? theory-type 'stable) 
        ; If theory-type is stable, improvement is 0
        0
        ; If theory-type is incremental, improvement is proportional to the
        ; product of effort and task difficulty
        (* effort difficulty))))

; Theory of performance
(define prob-of-success-theory
  ; Takes in ability, effort, difficulty, and theory-type
  (lambda (ability effort difficulty theory-type) 
    (min 0.95 (if (equal? theory-type 'effort-matters)
                  ; If theory-type is "effort matters", then probability of success
                  ; is higher with more effort and ability and lower with more difficulty 
                  (/ (* ability effort) difficulty)
                  (if (>= ability high) 
                      ; if ability is high, probability of success is very high
                      0.9
                      ; if ability is not high, "effort matters"
                      (prob-of-success-theory ability effort difficulty 'effort-matters))))))

(define (performance-theory ablity effort difficulty theory-type)
  (flip (prob-of-success-theory ablity effort difficulty theory-type)))

(define observer ;;could be student themself or an outside observer
  (mem (lambda (observed-outcome
                performance-theory-type
                observed-effort ;; effort could be observed, or not.
                observed-difficulty ;; difficulty could be observed, or not.
                )
         (enumeration-query
          ;; state now
          (define ability (ability-prior))
          (define effort (effort-prior))
          (define difficulty (difficulty-prior))
          (define outcome (performance-theory ability
                                              effort
                                              difficulty
                                              performance-theory-type))

          ability
          (and (equal? outcome observed-outcome)
               (if observed-effort
                   (equal? effort observed-effort)
                   #t)
               (if observed-difficulty
                   (equal? difficulty observed-difficulty)
                   #t))))))

(define (goal-prior goal-distribution-type)
  (multinomial (list 'judgment
                     'judgment-later
                     'ability
                     'ability-later
                     'outcome
                     'outcome-later
                     'improvement
                     'improvement-later)
               (if (equal? goal-distribution-type 'proving)
                   '(5 5 3 3 3 3 1 1)
                   '(1 1 3 3 3 3 5 5))))

(define (effort-cost-prior) (multinomial (list high mid low) '(1 2 3)))

(define student ;;could be student themself or an outside observer
  (mem (lambda (goal-distribution-type
                performance-theory-type
                improvement-theory-type
                past-outcome)
         (enumeration-query
          ;; state now
          (define ability
            (if past-outcome
                (apply multinomial
                       (observer (equal? 'success past-outcome) performance-theory-type))
                (ability-prior)))
          (define effort (effort-cost-prior))
          (define difficulty (difficulty-prior))
          (define outcome (performance-theory ability
                                              effort
                                              difficulty
                                              performance-theory-type))
          (define perceived-ability
            (apply multinomial (observer outcome
                                         performance-theory-type
                                         effort ;; could be observed or not
                                         ;difficulty ;; could be observed or not
                                         )))
          (define improvement (improvement-theory effort
                                                  difficulty
                                                  improvement-theory-type))
          (define future-ability (+ ability improvement))
          ;; assume they don't know how hard they'll try in the future
          (define future-effort (effort-prior))
          (define future-difficulty (difficulty-prior))
          (define future-outcome (performance-theory future-ability
                                                     future-effort
                                                     future-difficulty
                                                     performance-theory-type))
          (define future-perceived-ability
            (apply multinomial (observer future-outcome
                                         performance-theory-type
                                         future-effort ;; could be observed or not
                                         ;future-difficulty ;; could be observed or not
                                         )))
          (define future-improvement (improvement-theory future-effort
                                                         future-difficulty
                                                         improvement-theory-type))

          (define (goal goal-name)
            (case goal-name
                  (('judgment) (> perceived-ability 0.5))
                  (('ability) (> ability 0.5))
                  (('outcome) (equal? outcome #t))
                  (('improvement) (> improvement 0.3))
                  (('future-judgment) (> future-perceived-ability 0.5))
                  (('future-ability) (> future-ability 0.5))
                  (('future-outcome) (equal? future-outcome #t))
                  (('future-improvement) (> future-improvement 0.3))))
;;;

          (define goal-name (goal-prior goal-distribution-type))

          effort
          (goal goal-name)))))


;;;fold:
(define (plot fn title) (barplot (enumeration-query (fn) #t) title))
(multilineplot (list (list "prior"
                           (scatterify-fn effort-cost-prior))
                     (list "learning goal a priori"
                           (scatterify-dist (student 'learning
                                                     'effort-matters
                                                     'incremental)))
                     (list "proving goal a priori"
                           (scatterify-dist (student 'proving
                                                     'effort-matters
                                                     'incremental)))
                     (list "learning goal given failure"
                           (scatterify-dist (student 'learning
                                                     'effort-matters
                                                     'incremental
                                                     'failure)))
                     (list "proving goal given failure"
                           (scatterify-dist (student 'proving
                                                     'effort-matters
                                                     'incremental
                                                     'failure)))
                     (list "learning goal given success"
                           (scatterify-dist (student 'learning
                                                     'effort-matters
                                                     'incremental
                                                     'success)))
                     (list "proving goal given success"
                           (scatterify-dist (student 'proving
                                                     'effort-matters
                                                     'incremental
                                                     'success))))
               "Effort Given Different Prior Values on Goals (incremental theory)"
               '("effort" "probability"))
(multilineplot (list (list "prior"
                           (scatterify-fn effort-cost-prior))
                     (list "learning goal no history"
                           (scatterify-dist (student 'learning
                                                     'effort-matters
                                                     'stable)))
                     (list "proving goal no history"
                           (scatterify-dist (student 'proving
                                                     'effort-matters
                                                     'stable)))
                     (list "learning goal given failure"
                           (scatterify-dist (student 'learning
                                                     'effort-matters
                                                     'stable
                                                     'failure)))
                     (list "proving goal given failure"
                           (scatterify-dist (student 'proving
                                                     'effort-matters
                                                     'stable
                                                     'failure)))
                     (list "learning goal given success"
                           (scatterify-dist (student 'learning
                                                     'effort-matters
                                                     'stable
                                                     'success)))
                     (list "proving goal given success"
                           (scatterify-dist (student 'proving
                                                     'effort-matters
                                                     'stable
                                                     'success))))
               "Effort Given Different Prior Values on Goals (stable theory)"
               '("effort" "probability"))
;;;
~~~

WARNING ABOUT RUNNING THE CODE ABOVE! It may take about 2 minutes to run. 

<!-- With a uniform prior over 

~~~

;;;fold:
(define (jitter-prob probability)
  (max 0 (min 1 (+ probability (min 0.05 (max -0.05 (gaussian 0 0.01)))))))
(define (jitter-num num)
  (+ num (min 0.05 (max -0.05 (gaussian 0 0.01)))))
(define (scatterify-fn fn)
  (apply map (append (list (lambda (val prob) (list val prob)))
                     (enumeration-query (fn) #t))))
(define (scatterify-dist dist jitter)
  (if jitter
      (apply map (append (list (lambda (val prob) (list val (jitter-prob prob))))
                         dist))
      (apply map (append (list (lambda (val prob) (list val prob)))
                         dist))))
(define high 1)
(define mid 0.5)
(define low 0.1)
(define easy 0.2)
(define difficult 0.8)

; Sample ability from high/medium/low (prior to be measured)
(define ability-prior
  (lambda () (multinomial (list high mid low) '(1 2 1))))

; Sample ability from high/medium/low
(define effort-prior
  (lambda () (multinomial (list high mid low) '(1 1 1))))

; Sample difficulty from high/medium/low
(define difficulty-prior
  (lambda () (multinomial (list easy difficult) '(1 1))))

; Theory of improvement
(define improvement-theory
  (lambda (effort difficulty theory-type) 
    ; Takes in effort, difficulty, and theory-type (independent of ability)
    (if (equal? theory-type 'stable) 
        ; If theory-type is stable, improvement is 0
        0
        ; If theory-type is incremental, improvement is proportional to the
        ; product of effort and task difficulty
        (* effort difficulty))))

; Theory of performance
(define prob-of-success-theory
  ; Takes in ability, effort, difficulty, and theory-type
  (lambda (ability effort difficulty theory-type) 
    (min 0.95 (if (equal? theory-type 'effort-matters)
                  ; If theory-type is "effort matters", then probability of success
                  ; is higher with more effort and ability and lower with more difficulty 
                  (/ (* ability effort) difficulty)
                  (if (>= ability high) 
                      ; if ability is high, probability of success is very high
                      0.9
                      ; if ability is not high, "effort matters"
                      (prob-of-success-theory ability effort difficulty 'effort-matters))))))

(define (performance-theory ablity effort difficulty theory-type)
  (flip (prob-of-success-theory ablity effort difficulty theory-type)))

(define observer ;;could be student themself or an outside observer
  (mem (lambda (observed-outcome
                performance-theory-type
                observed-effort ;; effort could be observed, or not.
                observed-difficulty ;; difficulty could be observed, or not.
                )
         (enumeration-query
          ;; state now
          (define ability (ability-prior))
          (define effort (effort-prior))
          (define difficulty (difficulty-prior))
          (define outcome (performance-theory ability
                                              effort
                                              difficulty
                                              performance-theory-type))

          ability
          (and (equal? outcome observed-outcome)
               (if observed-effort
                   (equal? effort observed-effort)
                   #t)
               (if observed-difficulty
                   (equal? difficulty observed-difficulty)
                   #t))))))

(define (goal-prior goal-distribution-type)
  (multinomial (list 'judgment
                     'judgment-later
                     'ability
                     'ability-later
                     'outcome
                     'outcome-later
                     'improvement
                     'improvement-later)
                   '(1 1 1 1 1 1 1 1)))

(define (effort-cost-prior) (multinomial (list high mid low) '(1 2 3)))

(define student ;;could be student themself or an outside observer
  (mem (lambda (performance-theory-type
                improvement-theory-type
                past-outcome)
         (enumeration-query
          ;; state now
          (define ability
            (if past-outcome
                (apply multinomial
                       (observer (equal? 'success past-outcome) performance-theory-type))
                (ability-prior)))
          (define effort (effort-cost-prior))
          (define difficulty (difficulty-prior))
          (define outcome (performance-theory ability
                                              effort
                                              difficulty
                                              performance-theory-type))
          (define perceived-ability
            (apply multinomial (observer outcome
                                         performance-theory-type
                                         effort ;; could be observed or not
                                         ;difficulty ;; could be observed or not
                                         )))
          (define improvement (improvement-theory effort
                                                  difficulty
                                                  improvement-theory-type))
          (define future-ability (+ ability improvement))
          ;; assume they don't know how hard they'll try in the future
          (define future-effort (effort-prior))
          (define future-difficulty (difficulty-prior))
          (define future-outcome (performance-theory future-ability
                                                     future-effort
                                                     future-difficulty
                                                     performance-theory-type))
          (define future-perceived-ability
            (apply multinomial (observer future-outcome
                                         performance-theory-type
                                         future-effort ;; could be observed or not
                                         ;future-difficulty ;; could be observed or not
                                         )))
          (define future-improvement (improvement-theory future-effort
                                                         future-difficulty
                                                         improvement-theory-type))

          (define (goal goal-name)
            (case goal-name
                  (('judgment) (> perceived-ability 0.5))
                  (('ability) (> ability 0.5))
                  (('outcome) (equal? outcome #t))
                  (('improvement) (> improvement 0.3))
                  (('future-judgment) (> future-perceived-ability 0.5))
                  (('future-ability) (> future-ability 0.5))
                  (('future-outcome) (equal? future-outcome #t))
                  (('future-improvement) (> future-improvement 0.3))))
;;;

          (define goal-name (goal-prior))

          effort
          (goal goal-name)))))


;;;fold:
(define (plot fn title) (barplot (enumeration-query (fn) #t) title))
(multilineplot (list (list "prior"
                           (scatterify-fn effort-cost-prior))
                     (list "incremental"
                           (scatterify-dist (student 'effort-matters
                                                     'incremental)))
                     (list "incremental given failure"
                           (scatterify-dist (student 'effort-matters
                                                     'incremental
                                                     'failure)))
                     (list "stable"
                           (scatterify-dist (student 'effort-matters
                                                     'stable)))
                     (list "stable given failure"
                           (scatterify-dist (student 'effort-matters
                                                     'stable
                                                     'failure))))
               "Effort Given Different Theories"
               '("effort" "probability"))
;;;
~~~ -->

With a uniform prior over goals, we can see which goals an agent will work towards, given their theory of how their effort affects their performance and improvement.

WARNING ABOUT RUNNING THE CODE BELOW! It may take about 2 minues to run.

~~~
;;;fold:
(define (jitter-prob probability)
  (max 0 (min 1 (+ probability (min 0.05 (max -0.05 (gaussian 0 0.01)))))))
(define (jitter-num num)
  (+ num (min 0.05 (max -0.05 (gaussian 0 0.01)))))
(define (scatterify-fn fn)
  (apply map (append (list (lambda (val prob) (list val prob)))
                     (enumeration-query (fn) #t))))
(define (scatterify-dist dist jitter)
  (if jitter
      (apply map (append (list (lambda (val prob) (list val (jitter-prob prob))))
                         dist))
      (apply map (append (list (lambda (val prob) (list val prob)))
                         dist))))
(define high 1)
(define mid 0.5)
(define low 0.1)
(define easy 0.2)
(define difficult 0.8)

; Sample ability from high/medium/low (prior to be measured)
(define ability-prior
  (lambda () (multinomial (list high mid low) '(1 2 1))))

; Sample ability from high/medium/low
(define effort-prior
  (lambda () (multinomial (list high mid low) '(1 1 1))))

; Sample difficulty from high/medium/low
(define difficulty-prior
  (lambda () (multinomial (list easy difficult) '(1 1))))

; Theory of improvement
(define improvement-theory
  (lambda (effort difficulty theory-type) 
    ; Takes in effort, difficulty, and theory-type (independent of ability)
    (if (equal? theory-type 'stable) 
        ; If theory-type is stable, improvement is 0
        0
        ; If theory-type is incremental, improvement is proportional to the
        ; product of effort and task difficulty
        (* effort difficulty))))

; Theory of performance
(define prob-of-success-theory
  ; Takes in ability, effort, difficulty, and theory-type
  (lambda (ability effort difficulty theory-type) 
    (min 0.95 (if (equal? theory-type 'effort-matters)
                  ; If theory-type is "effort matters", then probability of success
                  ; is higher with more effort and ability and lower with more difficulty 
                  (/ (* ability effort) difficulty)
                  (if (>= ability high) 
                      ; if ability is high, probability of success is very high
                      0.9
                      ; if ability is not high, "effort matters"
                      (prob-of-success-theory ability effort difficulty 'effort-matters))))))

(define (performance-theory ablity effort difficulty theory-type)
  (flip (prob-of-success-theory ablity effort difficulty theory-type)))

(define observer ;;could be student themself or an outside observer
  (mem (lambda (observed-outcome
                performance-theory-type
                observed-effort ;; effort could be observed, or not.
                observed-difficulty ;; difficulty could be observed, or not.
                )
         (enumeration-query
          ;; state now
          (define ability (ability-prior))
          (define effort (effort-prior))
          (define difficulty (difficulty-prior))
          (define outcome (performance-theory ability
                                              effort
                                              difficulty
                                              performance-theory-type))

          ability
          (and (equal? outcome observed-outcome)
               (if observed-effort
                   (equal? effort observed-effort)
                   #t)
               (if observed-difficulty
                   (equal? difficulty observed-difficulty)
                   #t))))))

(define (goal-prior)
  (multinomial (list 'judgment
                     'judgment-later
                     'ability
                     'ability-later
                     'outcome
                     'outcome-later
                     'improvement
                     'improvement-later)
                   '(1 1 1 1 1 1 1 1)))

(define (effort-cost-prior) (multinomial (list high mid low) '(1 2 3)))

(define student ;;could be student themself or an outside observer
  (mem (lambda (performance-theory-type
                improvement-theory-type
                past-outcome)
         (enumeration-query
          ;; state now
          (define ability
            (if past-outcome
                (apply multinomial
                       (observer (equal? 'success past-outcome) performance-theory-type))
                (ability-prior)))
          (define effort (effort-cost-prior))
          (define difficulty (difficulty-prior))
          (define outcome (performance-theory ability
                                              effort
                                              difficulty
                                              performance-theory-type))
          (define perceived-ability
            (apply multinomial (observer outcome
                                         performance-theory-type
                                         effort ;; could be observed or not
                                         ;difficulty ;; could be observed or not
                                         )))
          (define improvement (improvement-theory effort
                                                  difficulty
                                                  improvement-theory-type))
          (define future-ability (+ ability improvement))
          ;; assume they don't know how hard they'll try in the future
          (define future-effort (effort-prior))
          (define future-difficulty (difficulty-prior))
          (define future-outcome (performance-theory future-ability
                                                     future-effort
                                                     future-difficulty
                                                     performance-theory-type))
          (define future-perceived-ability
            (apply multinomial (observer future-outcome
                                         performance-theory-type
                                         future-effort ;; could be observed or not
                                         ;future-difficulty ;; could be observed or not
                                         )))
          (define future-improvement (improvement-theory future-effort
                                                         future-difficulty
                                                         improvement-theory-type))

          (define (goal goal-name)
            (case goal-name
                  (('judgment) (> perceived-ability 0.5))
                  (('ability) (> ability 0.5))
                  (('outcome) (equal? outcome #t))
                  (('improvement) (> improvement 0.3))
                  (('future-judgment) (> future-perceived-ability 0.5))
                  (('future-ability) (> future-ability 0.5))
                  (('future-outcome) (equal? future-outcome #t))
                  (('future-improvement) (> future-improvement 0.3))))

          (define goal-name (goal-prior))
;;;

          goal-name
          (goal goal-name)))))


;;;fold:
(barplot (student 'effort-matters 'incremental) "Incremental")
(barplot (student 'effort-matters 'incremental 'failure) "Incremental given failure")
(barplot (student 'effort-matters 'stable) "Stable")
(barplot (student 'effort-matters 'stable 'failure) "Stable given failure")
;;;
~~~

<!-- #### Performance theory or improvement theory?

#### Learning goal or discounting? -->

## Notes
<sup id="footnote1"><a href="#ref1">[1]</a> Though later we will use the prior over effort to reflect a cost function. So that for an agent, their *prior* over efforts favors less effort.

<sup id="footnote2"><a href="#ref2">[2]</a> This perception could be their own, or an outside observer's. Here we simply assume that the observer has the same theory, but it is possible that the student could imagine that this person has a different theory, which might change their perception.</sup>
