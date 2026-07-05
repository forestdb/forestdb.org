---
layout: model
title: Grade Inflation
model-status: code
model-category: Probability and Bayesian Data Analysis
model-tags: discrete, continuous
model-language: church
---

A university location, either the United States or India, gets picked first, and then a GPA is drawn from a location-specific mixture of a point mass and a beta distribution. Conditioning on an observed GPA of 4.0, inference recovers the posterior probability that the student attends each system.

    (define (american_gpa) 
      (if (flip .9) 
          (* 4 (beta 5 5)) 
          (* 4 (if (flip .8) 1 0))))
    
    (define (indian_gpa)
      (if (flip .85) 
          (* 10 (beta 5 5)) 
          (* 10 (if (flip .7) 1 0))))
    
    (define observed-gpa 4.0)
    
    (define samples
      (mh-query 
    
       1000 10
    
       (define university_location 
         (if (flip .5) 'USA 'India))
    
       (define student_gpa 
         (if (eq? university_location 'USA) 
             (american_gpa) 
             (indian_gpa)))
    
       university_location
    
       (= (gaussian student_gpa 0.01) 
          observed-gpa)))
    
    (hist samples)
    
References:

- [American/Indian GPA example in Anglican](https://probprog.github.io/anglican/examples/viewer/?worksheet=indian-gpa)
