---
layout: model
title: Little Trees
model-status: hidden
model-category: Program Induction and Concept Learning
model-tags: concepts, generative model
model-language: church
---

Little Trees is a model of concept learning where concepts are noisy tree grammars: probabilistic programs that generate labeled trees. Given example trees produced from a grammar with noise on node labels and subtree structure, the model infers the underlying tree-generating expression by Bayesian inference, scoring hypotheses via importance sampling.
