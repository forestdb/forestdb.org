---
layout: model
title: Simple Bayesian Networks
model-status: code
model-category: Miscellaneous
model-tags: bayesnet, simple
---

A **chain** of three discrete nodes:

    (define a (flip))
    (define b (flip (if a .3 .5)))
    (define c (flip (if b .2 .4)))
    c

A **chain** of three continuous nodes:

    (define a (uniform 0 1))
    (define b (uniform 0 a))
    (define c (uniform 0 b))
    c

A **tree** with five nodes, two forks:

    (define a (flip))
    (define b1 (flip (if a .3 .5)))
    (define b2 (flip (if a .2 .4)))
    (define c1 (flip (if b2 .1 .2)))
    (define c2 (flip (if b2 .5 .6)))
    c2

An **inverse tree** with five nodes, two joins.

    (define a1 (flip))
    (define a2 (flip))
    (define b1 (flip (if (or a1 a2) .3 .5)))
    (define b2 (flip))
    (define c (flip (if (or b1 b2) .4 .6)))
    c

A **diamond** shape with four nodes:

    (define a (flip))
    (define b1 (flip (if a .3 .4)))
    (define b2 (flip (if a .4 .5)))
    (define c (flip (if (or b1 b2) .2 .3)))
    c
