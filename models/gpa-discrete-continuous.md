---
layout: model
title: Inferring Nationality from GPA
model-status: code
model-category: Miscellaneous
model-tags: discrete, continuous
---

This is an example of a mixed discrete-continuous model.

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
    
       (= (gaussian student_gpa 0.01 observed-gpa) 
          observed-gpa)))
    
    (hist samples)
    
References:

- [American/Indian GPA example in Anglican](http://www.robots.ox.ac.uk/~fwood/anglican/examples/american_indian_gpa/)
