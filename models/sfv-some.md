---
layout: model
title: SFV for "some"
model-language: church
---

Looking at the effect of a semantic free variable that can make "some" mean (lexically) some-not-all.

    (define (power dist a) (list (first dist) 
                               (map (lambda (x) (pow x a)) (second dist))))
    
    (define (meaning utterance marbles total-marbles fv)
      (case utterance
            (('null) true)
            (('none) (= marbles 0))
            (('all) (= marbles total-marbles))
            (('some) (and (> marbles 0) (if fv (< marbles total-marbles) true)))))

    (define (QUD-cell QUD marbles total-marbles)
      (case QUD
            (('is-all) (= marbles total-marbles))
            (('is-not-none) (> marbles 0))
            (('how-many) marbles)))
    
    (define (quantifier quantifier-alternatives) (apply multinomial alt-dist))

    (define (marble-state total-marbles allprior) 
        (case total-marbles
              (('3) (multinomial '(0 1 2 3) (list 1 1 1 allprior)))
              (('4) (multinomial '(0 1 2 3 4) (list 1 1 1 1 allprior)))
              (('5) (multinomial '(0 1 2 3 4 5) (list 1 1 1 1 1 allprior)))))

    (define literal-listener 
      (mem (lambda (utterance QUD total-marbles alt-dist fv allprior)
             (enumeration-query
              (define marbles (marble-state total-marbles allprior))
          
              (QUD-cell QUD marbles total-marbles)
          
              (meaning utterance marbles total-marbles fv)))))
    
    (define pragmatic-speaker 
      (mem (lambda (marbles QUD total-marbles quantifier-alternatives fv allprior)
             (enumeration-query
              (define utterance (quantifier alt-dist))
              utterance
              (and
              (equal? (QUD-cell QUD marbles total-marbles)
                       (apply multinomial
                              (power (literal-listener utterance QUD total-marbles alt-dist fv allprior)
                                   speaker-opt)))
              (meaning utterance marbles total-marbles fv))))))

    (define (pragmatic-listener utterance QUD total-marbles alt-dist allprior)
	    (enumeration-query
		    (define marbles (marble-state total-marbles allprior))
            (define fv (flip))
		    marbles
		    (equal? utterance 
                    (apply multinomial 
                           (pragmatic-speaker marbles QUD total-marbles alt-dist fv allprior))))) 
    
    
    (define speaker-opt 3)
    (define alt-dist (list '(null none some all)
                           '(20   1    1    1 )))

    (define (pl pr) (pair pr 
                          (fourth (second 
                                   (pragmatic-listener 'some 'how-many 4 alt-dist pr)))))
    
    (lineplot (map pl '(1 1.1 1.2 1.3 1.4 1.5 1.6 1.7 1.8 1.9 2 2.1 2.2)))
