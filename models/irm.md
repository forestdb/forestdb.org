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
paper by Ref:Kemp2006uv:

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
         (mem (lambda (class1 class2) (beta 0.5 0.5))))
    
       (define (talks object1 object2 conditioning-value)
         (flip (classes->parameters (object->class object1)
                                    (object->class object2))
               conditioning-value))
    
       (list (equal? (object->class 'tom) (object->class 'fred))
             (equal? (object->class 'tom) (object->class 'mary)))
    
       (and (talks 'tom 'fred true)
            (talks 'tom 'jim true)
            (talks 'jim 'fred true)
            (not (talks 'mary 'fred false))
            (not (talks 'mary 'jim false))
            (not (talks 'sue 'fred false))
            (not (talks 'sue 'tom false))
            (not (talks 'ann 'jim false))
            (not (talks 'ann 'tom false))
            (talks 'mary 'sue true)
            (talks 'mary 'ann true)
            (talks 'ann 'sue true)
            )))
    
    (hist (map first samples) "tom and fred in same group?")
    (hist (map second samples) "tom and mary in same group?")

References:

- Cite:Kemp2006uv
- Cite:ProbMods
