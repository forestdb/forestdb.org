---
layout: model
title: Network Analysis
model-status: link
model-category: Time Series and Stochastic Processes
model-tags: graphs
---

Small Problem 6 of DARPA PPAML Challenge Problem 4, the "Network Analysis Expressiveness Challenge": write a generative model of an undirected graph in which nodes arrive sequentially and attach their edges via a mixture of uniform and preferential ("rich-get-richer") attachment, then condition on global graph properties such as the clustering coefficient or the degree distribution to infer the posterior over the attachment mixture weight and the new-edges prior. The original Galois repository (GaloisInc/ppaml-cp4) is no longer available, but the problem spec survives:

- [Problem 6 specification (PDF)](https://github.com/mhoward2718/ppaml-cp4/blob/master/problems/problem6/problem-6-details.pdf) in a fork of the official ppaml-cp4 repository
- [PPAML CP4 Small Problems Collection](https://web.archive.org/web/20141018115000/http://ppaml.galois.com/wiki/wiki/CP4SmallProblemsCollection) (archived wiki page)
