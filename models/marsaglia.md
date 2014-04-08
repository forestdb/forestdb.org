---
layout: model
title: Marsaglia
model-status: code
model-category: Miscellaneous
model-tags: random-variate 
---

This model implements the rejection form of the Box-Muller for generating Gaussian random numbers given a source of uniformly distributed random numbers.

    (define (marsaglia-normal mean variance) 
       (define x (uniform -1.0 1.0)) 
       (define y (uniform -1.0 1.0)) 
       (define s (+ (* x x) (* y y)))
       (if (< s 1)
           (+ mean (* (sqrt variance)
                      (* x (sqrt (* -2 (/ (log s) s))))))
           (marsaglia-normal mean variance)))
    
    (density (repeat 1000 (lambda () (marsaglia-normal 5 1))))

References:

- Cite:marsaglia1964convenient
- Cite:wood2014new
