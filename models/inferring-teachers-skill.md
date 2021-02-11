---
layout: model
title: Inferring Pedagogical Skill
model-status: code
model-category: Reasoning about Reasoning
model-tags: pedagogical reasoning
model-language: church
---

This is a model of evaluating a teacher's 'pedagogical skill.' We define this pedagogical skill as the degree to which the teacher maximize the learner's utility with her demonstrations. 

The model corresponds to an experiment in which an observer infers a teacher's skill by watching a teacher 'teach' a toy to a learner. The teacher teaches through demonstrating one or more functions of the toy. Some functions were more valuable than others, such that the best teachers weren't necessarily the ones that taught the most functions. The best teachers were sensitive to the differential value and prioritized the most important functions they knew. Therefore the resulting  estimates  of  teacher  skill  are  sensitive  to  the teacher’s  epistemic  state  and  the  value  of  the  functions demonstrated. For example, the quality estimate of a teacher who demonstrates two low-value functions but also knew of a high value function will be lower than someone who just knew  the  two  low-value  functions  he  taught.   Similarly,  ateacher who knows both a high- and a low- value function would get the highest rating for showing both, a lower rating for omitting the low-value function, and an even lower ratingfor omitting the high-value function.

This is a webchurch implementation of the model described in Ref:Bass2015NotBN: 

```scheme
;;; Helper Functions
(define (unzip lst)
  (if (null? lst)
      '()
      (apply map (append (list list) lst))))

(define (contains? lst1 lst2) ;does lst1 contain lst2
  (not (any
        (map (lambda (l1 l2)
               (and (equal? l2 1) (equal? l1 0)))
             lst1
             lst2))))

(define (remove-item item lst)
  (filter (lambda (x) (not (eq? item x))) lst))

(define (get-el-at lst index)
  (if index
      (get-el-at (rest lst) (- index 1))
      (first lst)))

(define (affordance-exists? toy)
  (let ((affordances (filter (lambda (item) (second item)) (apply zip toy))))
    (if (null? affordances)
        '(())
        (map first affordances))))

(define (expectatation-of-utility result)
  (define zipped-result (zip (map second (first result)) (second result)))
  (apply map (pair + (map (lambda (z)
                            (map (lambda (x) (* x (second z))) (first z)))
                          zipped-result))))


(define (marginalize-out-toys toys)
  (map sum (apply map (pair list
                            (map (lambda (x)
                                   (map (lambda (affordance)
                                          (* (second x) affordance))
                                        (first x)))
                                 toys)))))

(define (expectation dist)
  (sum (apply map (pair * dist))))


;;; Cogntive-Model Functions
(define (toy-prior potential-actions)
  (repeat potential-actions (lambda() (if (flip exists-prior) 1 0))))

(define (action-prior known-actions)
  (map (lambda (a) (if a
                       (if (flip (- 1 communication-cost)) 1 0)
                       0))
       known-actions))

;;; Teacher
(define (teacher teacher-knows toy-actions toy-action-utilities teacher-quality communication-cost)
  (enumeration-query

   (define selected-actions
     (action-prior teacher-knows))

   (define learner-action-beliefs
     (marginalize-out-toys (apply zip (learner selected-actions))))

   selected-actions

   (factor (* teacher-quality
              (sum
               (map
                (lambda (utility p_action) (* utility p_action))
                toy-action-utilities learner-action-beliefs))))))


;;; Learner
(define (learner examples)
  (enumeration-query
   (define toy (toy-prior potential-actions))

   toy

   (contains? toy examples))) ; learner knows functions shown to him (and maybe more)

;;; Observer
(define (observer teacher-knows teacher-shows  toy communication-cost)
  (enumeration-query
   (define teacher-quality (uniform-draw '(0 .5 1)))

   teacher-quality

   (equal? teacher-shows (apply multinomial (teacher teacher-knows (first toy) (second toy) teacher-quality communication-cost)))))


;;; PARAMTERS
; free parameters ;
(define utility-difference .6)

;; learner's priors ;;
(define exists-prior .002)

;; teacher's priors ;;
(define communication-cost .75)

;; condition ;;
(define potential-actions 4)

(define h-function (+ .5 (/ utility-difference 2)))
(define l-function (- .5 (/ utility-difference 2)))

; toys are of the form ((function exists) (functions utility))
(define toy (list '(1 1 1 1) (list h-function h-function l-function l-function)))

; conditions are of the form ((items taught) (items known))
(define conditions
  '((L1H1L_T1H   ((1 0 1 0)(1 0 0 0)))
    (L1H1L_T1H1L ((1 0 1 0)(1 0 1 0)))
    (L1H1L_T1L   ((1 0 1 0)(0 0 1 0)))
    (L1H_T1H     ((1 0 0 0)(1 0 0 0)))
    (L1L_T1L     ((0 0 1 0)(0 0 1 0)))
    (L2H_T1H     ((1 1 0 0)(1 0 0 0)))
    (L2H_T2H     ((1 1 0 0)(1 1 0 0)))
    (L2L_T1L     ((0 0 1 1)(0 0 1 0)))
    (L2L_T2L     ((0 0 1 1)(0 0 1 1)))
    (LA_T1H      ((1 1 1 1)(1 0 0 0)))
    (LA_T1H1L    ((1 1 1 1)(1 0 1 0)))
    (LA_T1L      ((1 1 1 1)(0 0 1 0)))
    (LA_T2H      ((1 1 1 1)(1 1 0 0)))
    (LA_T2L      ((1 1 1 1)(0 0 1 1)))
    (LA_TA       ((1 1 1 1)(1 1 1 1)))))


(define results
    (apply zip (list (map first conditions)
                   (map (lambda (teacher-attribs)
                          (expectation
                           (observer (first teacher-attribs) (second teacher-attribs) toy communication-cost)))
                        (map second conditions)))))

results
```
