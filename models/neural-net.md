---
layout: model
title: Bayesian Neural Network
model-status: code
model-category: Machine Learning
model-tags: neural net, continuous
---

This models is a neural network that learns the XOR function. The model is based on the [Anglican implementation of a neural net](http://www.robots.ox.ac.uk/~fwood/anglican/examples/neural_net/).

    ;; Define expected inputs and outputs as lists
    (define inputs  (list (list -1 -1) (list -1  1) (list  1 -1) (list  1  1)))
    (define outputs (list           0            1            1            0))
    
    ;; Helper functions for accessing expected inputs / outputs
    (define (I i l j) (list-ref (list-ref inputs i) j))
    (define (O i)     (list-ref outputs i))
    
    ;; Define number of layers (l-max + 1)
    (define l-max 3)
    
    (define samples
      (mh-query 
       1000
       10
       ;; Define number of nodes in each layer (n-max + 1)
       (define n-max
         (mem
          (lambda (l)
            (- (cond ((= l 0) 2)
                     ((= l 1) 4)
                     ((= l 2) 2)
                     ((= l 3) 1)) 1))))
    
       ;; Define weights dynamically as required
       ;; l = layer, n = node id; j = input node id in layer l-1
       (define w
         (mem
          (lambda (l n j) (gaussian 0 0.5))))
    
       ;; Define activation function
       (define (activate v)
         (if (<= v 0.5) -1 1))
    
       ;; Evaluate a single weighted input
       ;; f = function to evaluate to get input value, i = input training pattern id
       (define (get-input f i l n j)
         (* (f i l j) (w l n j)))
    
       ;; Sum all weighted inputs to a node
       (define (sum-inputs f i l n j)
         (if (= j 0)
             (get-input f i l n j)
             (+ (sum-inputs f i l n (- j 1)) (get-input f i l n j))))
    
       ;; Evaluate node activation
       (define N
         (mem
          (lambda (i l n)
            (activate
             (if (= l 1)
                 (sum-inputs I i (- l 1) n (n-max 0))
                 (sum-inputs N i (- l 1) n (n-max (- l 1))))))))
    
       ;; Compute output value based on layer 2 nodes
       (define p-O
         (mem
          (lambda (i)
            (if (>= (N i l-max (n-max l-max)) 0) 1 0))))
    
       ;; Predict response to the four input values
       (map p-O (list 0 1 2 3))
    
       ;; Train the network using our training data
       (and (= (gaussian (p-O 0) 0.1 (O 0)) (O 0))
            (= (gaussian (p-O 1) 0.1 (O 1)) (O 1))
            (= (gaussian (p-O 2) 0.1 (O 2)) (O 2))
            (= (gaussian (p-O 3) 0.1 (O 3)) (O 3)))))
    
    (hist (map first samples) "Input -1, -1")
    (hist (map second samples) "Input -1, 1")
    (hist (map third samples) "Input 1, -1")
    (hist (map fourth samples) "Input 1, 1")

References:

- [Bayesian neural net in Anglican](http://www.robots.ox.ac.uk/~fwood/anglican/examples/neural_net/)
