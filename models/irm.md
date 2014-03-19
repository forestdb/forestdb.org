---
layout: model
title: Infinite Relational Model
model-status: code
model-category: Nonparametric Models
model-tags: clustering, cognitive science, nonparametric statistics
---

The Infinite Relational Model (IRM) is a nonparametric model that,
given data involving different kinds of entities, discovers which
kinds there are and which relations hold between kinds. From the
paper by Kemp et al, 2006:

> "Suppose we are given one or more relations involving one or more
> types. The goal of the IRM is to partition each type into clusters,
> where a good set of partitions allows relationships between
> entities to be predicted by their cluster assignments. For example,
> we may have a single type *people* and a single relation
> *likes(i,j)* which indicates whether person *i* likes person
> *j*. Our goal is to organize the entities into clusters that relate
> to each other in predictable ways (Figure 1a)."

    (define samples
      (mh-query
       300 100
    
       (define class-distribution (DPmem 1.0 gensym))
    
       (define object->class
         (mem (lambda (object) (class-distribution))))
    
       (define classes->parameters
         (mem (lambda (class1 class2) (first (beta 0.5 0.5)))))
    
       (define (talks object1 object2)
         (flip (classes->parameters (object->class object1)
                                    (object->class object2))))
    
       (list (equal? (object->class 'tom) (object->class 'fred))
             (equal? (object->class 'tom) (object->class 'mary)))
    
       (and (talks 'tom 'fred)
            (talks 'tom 'jim)
            (talks 'jim 'fred)
            (not (talks 'mary 'fred))
            (not (talks 'mary 'jim))
            (not (talks 'sue 'fred))
            (not (talks 'sue 'tom))
            (not (talks 'ann 'jim))
            (not (talks 'ann 'tom))
            (talks 'mary 'sue)
            (talks 'mary 'ann)
            (talks 'ann 'sue)
            )))
    
    (hist (map first samples) "tom and fred in same group?")
    (hist (map second samples) "tom and mary in same group?")

Sources: 

- Kemp, C., Tenenbaum, J. B., Griffiths, T. L., Yamada, T., & Ueda, N. (2006). Learning Systems of Concepts with an Inﬁnite Relational Model, 1–8.
- [probmods.org - nonparametric models](https://probmods.org/non-parametric-models.html)    
