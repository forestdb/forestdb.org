---
layout: model
title: Brightness Constancy
model-status: code-fail
model-status-verbose: The MH chain cannot be initialized.
model-category: Miscellaneous
model-tags: cognitive science, vision
---

How we perceive the reflectance of a visual object depends on the
assumptions our visual system makes about the *illumination* of the
object (as indicated e.g. by shadows falling onto the object) and
on the actual *luminance* of the object (the amount of light
emitted by the object). Model by @churchwiki.

    (define (noisy= target value variance)
      (= 0 (gaussian (- target value) variance)))
    
    (mh-query
     10 10
    
     (define reflectance (gaussian 1 1))
     (define illumination (gaussian 3 0.5))
     (define luminance (* reflectance illumination))
    
     reflectance
    
     (and (noisy= 3.0 luminance 0.1)
          (noisy= 0.5 illumination 0.1)))

References:

- Cite:ProbMods
