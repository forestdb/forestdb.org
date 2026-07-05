---
layout: model
title: Seismic Event Detection
model-status: link
model-category: Scientific and Physical Models
model-tags: blog, benchmark
---

NET-VISA (Network Processing Vertically Integrated Seismic Analysis) generates seismic events, signal propagation, station detections, missed signals, and noise detections. Inference recovers the most probable events from station data, reducing missed events by about 60% relative to the deployed monitoring pipeline ([Arora, Russell & Sudderth 2013](https://people.eecs.berkeley.edu/~russell/papers/bssa-netvisa.pdf)).

The model is an open-universe probabilistic program in the style of BLOG and appears in the [PPAML small problems collection](https://web.archive.org/web/20170731061143/http://ppaml.galois.com/wiki/wiki/CP4SmallProblemsCollection). A self-contained 2-D version with data, evaluation code, and a baseline solution is available at [nimar/seismic-2d](https://github.com/nimar/seismic-2d).
