---
layout: model
title: Genetic Linkage Analysis
model-status: link
model-category: Graphical Models and Causality
model-tags: benchmark, discrete
model-language: church
---

Genetic linkage analysis infers recombination parameters between genetic loci from the observed genotypes and phenotypes of related individuals. The pedigree forms a discrete Bayesian network over founder alleles, inheritance, and recombination indicators, with inference computing the data likelihood as a function of the recombination fraction.

The canonical exact-inference implementation is SUPERLINK by Fishelson and Geiger, which represents general pedigrees as Bayesian networks and combines variable elimination with conditioning.

- [SUPERLINK (Rockefeller genetic analysis software list)](https://gaow.github.io/genetic-analysis-software/s/superlink/)
- [Superlink-Online: faster multipoint linkage analysis (Silberstein et al., 2006)](https://pmc.ncbi.nlm.nih.gov/articles/PMC1474109/)
- [Fishelson & Geiger, "Exact genetic linkage computations for general pedigrees" (Bioinformatics 2002)](https://pubmed.ncbi.nlm.nih.gov/12169547/)
