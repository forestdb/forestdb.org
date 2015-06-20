---
layout: model
title: Brightness Constancy
model-status: code
model-category: Miscellaneous
model-tags: cognitive science, vision
model-language: church
---

How we perceive the reflectance of a visual object depends on the assumptions our visual system makes about the *illumination* of the object (as indicated e.g. by shadows falling onto the object) and on the actual *luminance* of the object (the amount of light emitted by the object).

    (define observed-luminance 3.0)
    
    (define samples
       (mh-query
        1000 10
    
        (define reflectance (gaussian 1 1))
        (define illumination (gaussian 3 0.5))
        (define luminance (* reflectance illumination))
    
        reflectance
    
        (= luminance (gaussian observed-luminance 0.1))))
    
    (multiviz "Mean reflectance: " (mean samples)
              (hist samples "Reflectance"))

References:

- Cite:ProbMods
