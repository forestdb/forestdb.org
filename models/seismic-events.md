---
layout: model
title: Seismic Event Detection
model-status: link
model-category: PPAML Challenge Problems
model-tags: blog, benchmark
---

NET-VISA (Network Processing Vertically Integrated Seismic Analysis) is a generative model of global seismology: seismic events occur at random times, locations, depths, and magnitudes; their signals propagate through the earth and are detected—or missed—at the stations of the International Monitoring System, alongside noise detections. Inference recovers the set of events with highest posterior probability given the station detections, reducing missed events by about 60% relative to the deployed nuclear-test-ban monitoring pipeline ([Arora, Russell & Sudderth 2013](https://people.eecs.berkeley.edu/~russell/papers/bssa-netvisa.pdf)). The model is a classic open-universe probabilistic program in the style of BLOG, and appears as the signal interpretation problem in the [PPAML small problems collection](https://web.archive.org/web/20170731061143/http://ppaml.galois.com/wiki/wiki/CP4SmallProblemsCollection): given signals measured at an array of seismometers, determine the location and magnitude of each seismic event. A self-contained 2-D version of the problem—with training data, test episodes, evaluation code, and a baseline solution—is available at [nimar/seismic-2d](https://github.com/nimar/seismic-2d).
