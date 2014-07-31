---
layout: model
title: Counterfactuals
---

Conditioning on counterfactual statements:

    (define (begin a b)
      b)
    
    (define (is-function-definition? def)
      (list? (second def)))
    
    (define (shadow name)
      (string->symbol (string-append "shadow-" name)))
    
    (define (rename expr from-name to-name)
      (cond [(list? expr) (map (lambda (x) (rename x from-name to-name)) expr)]
            [(eq? expr from-name) to-name]
            [else expr]))
    
    (define (shadow-rename expr name)
      (rename expr name (shadow name)))
    
    (define (shadow-rename-all expr names)
      (if (null? names)
          expr
          (shadow-rename-all (shadow-rename expr (first names))
                             (rest names))))
    
    (define (get-names defines)
      (map (lambda (def)
             (if (is-function-definition? def)
                 (first (second def))
                 (second def)))
           defines)) 
    
    (define (make-shadow-defines defines names)
      (map (lambda (def)
             (if (is-function-definition? def)
                 (shadow-rename-all def names)
                 (let ([name (second def)])
                   (list 'define 
                         (shadow name) 
                         (list 'if '(flip eps)
                               (shadow-rename-all (third def) names)
                               name
                               )))))
           defines))
    
    (define (make-counterfactual-query defines query-expr antecedent consequent)
      (let* ([names (get-names defines)]
             [shadow-defines (make-shadow-defines defines names)]
             [new-query
              (append (list 'enumeration-query
                            '(define eps .01))
                      defines
                      (list
                       (list 
                        'define 'cf-statement
                        (list 'apply 'multinomial
                              (append 
                               '(enumeration-query)
                               shadow-defines
                               (list (list 'not (shadow consequent)))
                               (list (list 'condition (list 'not (shadow antecedent)))))))
                       query-expr
                       (list 'condition (list 'and antecedent consequent 'cf-statement))))])
        (begin
         (console-log new-query)
         new-query)))
    
    
    ;; Comparing counterfactual to conditioning on antecedent and consequent:
    
    (define (test-counterfactual model query-expr antecedent consequent)
      (barplot
       (eval
        (append '(enumeration-query)
                model
                (list query-expr
                      (list 'and antecedent consequent))))
       "Without counterfactual statement")
    
      (barplot
       (eval
        (make-counterfactual-query model 
                                   query-expr 
                                   antecedent
                                   consequent
                                   ))
       "With counterfactual statement"))
    
    
    ;; Example
    
    (define my-model
      '((define a (flip .2))
        (define c (flip .2))
        (define b (flip (if (or a c) 0.9 0.1)))))
    
    (define my-query-expr
      '(list a b c))
    
    (define my-antecedent 'a)
    
    (define my-consequent 'b)
    
    (test-counterfactual my-model
                         my-query-expr
                         my-antecedent
                         my-consequent)

Previous version:

    (define (lookup key list-of-pairs)
      (rest (assoc key list-of-pairs)))
    
    
    (define (run-world world conditioner epsilon)
    
      (define (wrap f)
        (lambda args
          (let* ([name (first args)]
                 [func-args (rest args)])
            (if (or (= epsilon 1.0) (flip epsilon))
                (apply f func-args)
                (lookup name world)))))
              
      (define flip0 (wrap flip))
    
      (enumeration-query
    
       (define A (flip0 'A .2))
       (define B (flip0 'B .8))   
       (define E (or A B))
    
       (make-world A B E)
    
       (conditioner A B E)))

    ;; Helper function
    
    (define (make-world A B E)
      (list (pair 'A A)
            (pair 'B B)
            (pair 'E E)))
    
    
    ;; Prior on worlds
    
    (define (empty-condition A B E) 
      #t)
    
    (barplot (run-world '() empty-condition 1.0)
             "Prior on worlds")
    
    
    ;; Conditioning on the actual world
    
    (define (observation-condition A B E)
      (and (not A) (not B) (not E))) ;; Could use noisy conditioning here
    
    (barplot (run-world '() observation-condition 1.0)
             "Conditioned on actual world (A=0 B=0 E=0)")
    
    
    ;; Counterfactuals
    
    (define actual-world (make-world #f #f #f))
    
    (define (intervention-condition A B E)
      E)
    
    (define epsilon .05)
    
    (barplot (run-world actual-world intervention-condition epsilon)
             "Counterfactual worlds for intervention E=1")
