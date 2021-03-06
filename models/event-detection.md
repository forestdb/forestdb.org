---
layout: model
title: Event Detection
model-status: code
model-category: Miscellaneous
model-tags: poisson
model-language: church
---

Assuming there is a behavior with a certain frequency over time, and given some data about that behavior, do you think something happened to change that behavior at some point? This formulation could apply to changes in missile attacks on Israel, but the same idea applies to changes in personal behavior. For example, suppose you usually send your friend some messages per month, but something happened and now you send more/less. When did something happen? Did something actually happen?

    (define (zip l1 l2) (map list l1 l2))
    
    ;; Poisson distribution
    
    (define (poisson-helper rate k p)
      (let* ((L (exp (- 0 rate)))
             (u (uniform 0 1))
             (newp (* p u)))
        (if (< newp L)
            (- k 1)
            (poisson-helper rate (+ 1 k) newp))))
    
    (define (poisson rate) (poisson-helper rate 1 1))
    
    ;; Forward model of behavioral change (e.g. of number of texts per month)
    
    (define months (iota 24))
    (define something-happened? (flip 0.5))
    (define month-when-something-happened (random-integer (length months)))
    (define rate1 (uniform 10 50))
    (define rate2 (+ rate1 (if (flip) 
                               (uniform 5 20)
                               (uniform -20 -5))))
    
    (define (num-texts month)
      (poisson
       (if something-happened?
           (if (< month month-when-something-happened)
               rate1
               rate2)
           rate1)))
    
    (scatter (zip (iota 24) (map num-texts months)))
    
    (display something-happened? month-when-something-happened rate1 rate2)

Conditioning using single-site MCMC doesn't work very well in this model. Here is a starting point for a simplified version of the model:

    (define data '(5 5 5 5 20 20 20 20))
    
    (define months (iota (length data)))
    
    (define samples
      (mh-query 
       1000 100
       (define something-happened? #t)
       (define month-when-something-happened (random-integer (length months)))
       (define rate1 5)
       (define rate2 20)
       (define (num-texts month target-value)
         (poisson
          (if (< month month-when-something-happened)
              rate1
              rate2)
          target-value))
    
       ;; condition
       (map (lambda (datum month) (num-texts month datum))
            data
            months)
    
       ;; query
       month-when-something-happened
    
       #t
       ))
    
    (hist samples)

References:

- [Switchpoint detection in PyMC](http://nbviewer.ipython.org/github/CamDavidsonPilon/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers/blob/master/Chapter1_Introduction/Chapter1_Introduction.ipynb)
- Model by [Tomer Ullman](http://www.mit.edu/~tomeru/)
