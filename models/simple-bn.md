---
layout: model
title: Simple Bayesian Networks
model-status: code
model-category: Graphical Models and Causality
model-tags: bayesnet, simple
model-language: church
---

This page collects five minimal Bayes net topologies in WebChurch: a chain and an alternate continuous chain, a two-fork tree, an inverse tree with two joins, and a diamond, each just a few flip or uniform calls deep. Each snippet returns the final downstream node so its induced marginal distribution can be inspected directly.

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

An **inverse tree** with five nodes, two joins:

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
