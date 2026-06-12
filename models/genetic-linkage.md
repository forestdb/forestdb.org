---
layout: model
title: Genetic Linkage Analysis
model-status: link
model-category: PPAML Challenge Problems
model-tags: benchmark, discrete
model-language: church
---

Genetic linkage analysis infers the recombination (linkage) parameters between genetic loci from the observed genotypes and phenotypes of related individuals in a pedigree. The pedigree is encoded as a large discrete Bayesian network over founder alleles, Mendelian inheritance, and recombination indicators, and the inference task is to compute the likelihood of the data as a function of the recombination fraction. It is a classic exact-inference benchmark for probabilistic programs; the canonical implementation is SUPERLINK by Fishelson and Geiger, which represents general pedigrees as Bayesian networks and combines variable elimination with conditioning.

- [SUPERLINK (Rockefeller genetic analysis software list)](https://gaow.github.io/genetic-analysis-software/s/superlink/)
- [Superlink-Online: faster multipoint linkage analysis (Silberstein et al., 2006)](https://pmc.ncbi.nlm.nih.gov/articles/PMC1474109/)
- [Fishelson & Geiger, "Exact genetic linkage computations for general pedigrees" (Bioinformatics 2002)](https://pubmed.ncbi.nlm.nih.gov/12169547/)
