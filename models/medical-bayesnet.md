---
layout: model
title: Bayesnet for Medical Diagnosis
model-status: code
model-category: Miscellaneous
model-tags: bayesnet, medicine
---

    (define samples
      (mh-query 1000 10
                (define lung-cancer (flip 0.01))
                (define TB (flip 0.005))
                (define cold (flip 0.2))
                (define stomach-flu (flip 0.1))
                (define other (flip 0.1))
                
                (define cough (or (and cold (flip 0.5)) (and lung-cancer (flip 0.3)) (and TB (flip 0.7)) (and other (flip 0.01))))
                (define fever (or (and cold (flip 0.3)) (and stomach-flu (flip 0.5)) (and TB (flip 0.2)) (and other (flip 0.01))))
                (define chest-pain (or (and lung-cancer (flip 0.4)) (and TB (flip 0.5)) (and other( flip 0.01))))
                (define shortness-of-breath (or (and lung-cancer (flip 0.4)) (and TB (flip 0.5)) (and other (flip 0.01))))
                
                (list lung-cancer TB)
                
                (and cough fever chest-pain shortness-of-breath)
                
                ))
    
    (hist samples)

Source: [probmods.org - conditioning](https://probmods.org/conditioning.html)    
