---
layout: model
title: Infinite Relational Model
model-status: code
model-category: Miscellaneous
model-tags: clustering, cognitive science, nonparametric statistics
---

The Infinite Relational Model (IRM) is a nonparametric model that,
given data involving different kinds of entities, discovers which
kinds there are and which relations hold between kinds. From the
paper by @kemp:2006uv:

> "Suppose we are given one or more relations involving one or more
> types. The goal of the IRM is to partition each type into clusters,
> where a good set of partitions allows relationships between
> entities to be predicted by their cluster assignments. For example,
> we may have a single type *people* and a single relation
> *likes(i,j)* which indicates whether person *i* likes person
> *j*. Our goal is to organize the entities into clusters that relate
> to each other in predictable ways (Figure 1a)."

Model by @churchwiki.

    (define objects (list 'bob 'jane 'mary 'steve))
    
    (query
    
     (define get-cat (DPmem 1.0 gensym))
     (define type (mem (lambda (obj) (get-cat))))
     (define reln-obs-fn (mem (lambda (cat1 cat2) (make-beta-binomial 1.0 1.0))))
     (define observe (mem (lambda (obj1 obj2)
                            (sample (reln-obs-fn (type obj1)
                                                 (type obj2))))))
    
     (map type objects)
    
     (and (observe 'bob 'jane)
          (not (observe 'jane 'steve))
          (observe 'mary 'steve)))
