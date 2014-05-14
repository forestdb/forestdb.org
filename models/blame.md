---
layout: model
title: Blame Attribution
model-status: code
model-category: Miscellaneous
model-tags: counterfactuals, cognitive science
---

    ;; --------------------------------------------------------------------
    ;; Helper functions
    
    (define (bool->num x)
      (if x 1 0))
    
    (define (last xs)
      (first (reverse xs)))
    
    (define (compose f g)
      (lambda (x)
        (f (g x))))
    
    (define (zip xs1 xs2) 
      (if (or (is_null xs1) (is_null xs2)) '() 
        (pair 
          (pair (first xs1) (pair (first xs2) '()))
          (zip (rest xs1) (rest xs2)))))
    
    
    ;; --------------------------------------------------------------------
    ;; Data structures
    
    ;; responsibility measure = [title, func]
    (define measure->title first)
    (define measure->func rest)
    
    ;; world = [agent A, agent B, ..., effect]
    (define world->A first)
    (define world->B second)
    (define world->effect last)
    
    ;; scenario = [actual-world, counterfactual-world, joint prob]
    (define scenario->actual first)
    (define scenario->counterfactual second)
    (define scenario->prob third)
    (define (scenario->direction s)
      (- (world->effect (scenario->counterfactual s))))
    
    
    ;; --------------------------------------------------------------------
    ;; Responsibility measures (by Tobias Gerstenberg)
    
    (define (unnormalized-responsibility scenarios consistent?)
      (let* ([consistent-scenarios (filter consistent? scenarios)])
        (sum (map (lambda (scenario)
                    (* (scenario->direction scenario)
                       (scenario->prob scenario)))
                  consistent-scenarios))))
    
    (define (normalized-responsibility scenarios consistent?)
      (let ([consistent-scenarios (filter consistent? scenarios)])
        (/ (unnormalized-responsibility scenarios consistent?)
           (sum (map scenario->prob consistent-scenarios)))))
    
    (define (unnormalized-inverted-responsibility scenarios consistent?)
      (* -1 (unnormalized-responsibility scenarios
                                         (lambda (s) (not (consistent? s))))))
    
    (define (normalized-inverted-responsibility scenarios consistent?)
      (* -1 (normalized-responsibility scenarios
                                       (lambda (s) (not (consistent? s))))))
    
    (define responsibility-measures
      (list
       (pair "Unnormalized responsibility: " unnormalized-responsibility)
       (pair "Normalized responsibility: " normalized-responsibility)
       (pair "Unnormalized inverted responsibility: " unnormalized-inverted-responsibility)
       (pair "Normalized inverted responsibility: " normalized-inverted-responsibility)))
    
    (define (show-responsibility-measures scenarios scenario-consistent?)
      (for-each
       (lambda (responsibility-measure)
         (display (measure->title responsibility-measure)
                  ((measure->func responsibility-measure) scenarios scenario-consistent?)))
       responsibility-measures))
    
    
    ;; --------------------------------------------------------------------
    ;; Nearby counterfactuals
    
    ;; "perturb-world" samples or perturbs a given random world (mapping names 
    ;; to values) using interventions (mapping names to functions). The 
    ;; pre-intervention distribution on nearby worlds is defined by noisily 
    ;; conditioning on the actual world.
    (define perturb-world 
      (mem 
       (lambda (world interventions noise)
         
         ;; "counterfactual-wrap" makes a function f take as additional 
         ;; (first) argument a name. Before applying the function, we look
         ;; up this name in the given list of interventions. If an
         ;; intervention is given, there must also be a value in the
         ;; world for this name. We apply the intervention function to
         ;; this value and return the result. If no intervention is given, 
         ;; we simply apply f to its arguments.
         (define (counterfactual-wrap f)
           (lambda args
             (let* ([name (first args)]
                    [func-args (rest args)]
                    [intervention-pair (assoc name interventions)])
               (if (eq? intervention-pair #f)
                   (apply f func-args)
                   (let* ([intervention-func (rest intervention-pair)]
                          [world-value (rest (assoc name world))])
                     (intervention-func world-value))))))
         
         (define flip0 (counterfactual-wrap flip))
         (define and0 (counterfactual-wrap and))
         (define or0 (counterfactual-wrap or))  
         
         ;; "noisy-obs" takes a name and a target value as arguments. We look up
         ;; the observed value for the name in the random world. If it is found,
         ;; we apply the usual noisy-=. Otherwise, we always return true.
         (define (noisy-obs name target-value)
           (let ([world-pair (assoc name world)])
             (if (eq? world-pair #f)
                 #t
                 (flip (if (equal? target-value (rest world-pair))
                           1.0
                           noise)))))
         
         ;; Below we define a simple causal model. Using the functions defined
         ;; above, we can noisily condition on the actual world, and apply interventions
         ;; to all variables in the model.
         (enumeration-query
          
          (define A (flip0 'A .1))
          (define B (flip0 'B .9))   
          (define E (and0 'E A B))
          
          (list (pair 'A A)
                (pair 'B B)
                (pair 'E E))
          
          (and (noisy-obs 'A A)
               (noisy-obs 'B B)
               (noisy-obs 'E E))))))
    
    ;; "counterfactual-dist" computes the joint distribution on actual and
    ;; counterfactual worlds, where counterfactuals are defined using the
    ;; "perturb-world" function above (parameterized by user-provided
    ;; interventions and observation noise).
    (define (counterfactual-dist interventions noise)  
      (enumeration-query
       (define prior-world (apply multinomial (perturb-world '() '() noise)))
       (define cf-world (apply multinomial (perturb-world prior-world interventions noise)))
       (list (map (compose bool->num rest) prior-world)
             (map (compose bool->num rest) cf-world))
       #t))
    
    ;; "joint-dist->scenarios" converts the joint distribution into a format
    ;; that can be read by the responsibility measures defined above.
    (define (joint-dist->scenarios joint-dist)
      (map
       (lambda (bin)
         (append (first bin)
                 (rest bin)))
       (apply zip joint-dist)))
    
    
    ;; --------------------------------------------------------------------
    ;; Example
    
    ;; "world" is supposed to represent what happened in the "actual" world.
    (define world
      '((A . #f)
        (B . #f)
        (E . #f)))
    
    ;; "interventions" is a list of variable ("agent") names and functions that 
    ;; intervene on the value of variable and return a counterfactual value.
    (define interventions
      (list (pair 'A (lambda (a) (not a)))))
    
    ;; "scenario-consistent?" returns true if the value of 'A considered
    ;; in the prior world in a scenario is the same as in the actual world.
    ;; This is used in some responsbility measures.
    (define (scenario-consistent? scenario)
      (equal? (world->A (scenario->actual scenario))
              (bool->num (rest (assoc 'A world)))))
    
    ;; Looking at the joint distribution, the most likely scenario is that 
    ;; E is 0 in the actual world because A was 0, but intervening on A makes 
    ;; E be 1.
    (define joint-dist (counterfactual-dist interventions .4))
    (barplot joint-dist "joint probs of actual & counterfactual worlds")
    
    ;; As a result, A carries significant responsibility for the fact that
    ;; E was 0.
    (define scenarios (joint-dist->scenarios joint-dist))
    (show-responsibility-measures scenarios scenario-consistent?)

The above will take some changes to be correct. A first stab looks like this:

    (define run-world 
      (mem 
       (lambda (world interventions observations noise)
         
         (define (counterfactual-wrap f)
           ;; distinguish random from deterministic functions
           (lambda args
             (let* ([name (first args)]
                    [func-args (rest args)]
                    [intervention-pair (assoc name interventions)])
               (if (eq? intervention-pair #f)
                   (apply f func-args)
                   (let* ([intervention-func (rest intervention-pair)]
                          [world-value (rest (assoc name world))])
                     (intervention-func world-value))))))
         
         (define flip0 (counterfactual-wrap flip))
         (define and0 (counterfactual-wrap and))
         (define or0 (counterfactual-wrap or))  
         
         (define (noisy-obs name target-value)
           (let ([world-pair (assoc name observations)])
             (if (eq? world-pair #f)
                 #t
                 (flip (if (equal? target-value (rest world-pair))
                           1.0 ;; could change to (- 1 noise)
                           noise)))))
        
         (enumeration-query
          
          (define A (flip0 'A .1))
          (define B (flip0 'B .9))   
          (define E (and0 'E A B))
          
          (list (pair 'A A)
                (pair 'B B)
                (pair 'E E))
          
          (and (noisy-obs 'A A)
               (noisy-obs 'B B)
               (noisy-obs 'E E))))))
    
    (define observations
      '((A . #f)
        (B . #f)
        (E . #f)))
    
    (define interventions
      (list (pair 'A (lambda (a) (not a)))))
    
    (define noise .05)
    
References:

- Cite:gerstenberg2013making
