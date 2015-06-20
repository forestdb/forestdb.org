---
layout: model
model-language: church
---

# The 49ers are *going* to win

###(...because I want them to)

Imagine people believe what they want to believe, or at least are more *likely* to believe things they want to be true.

~~~~
(define bias .4)
(define (biased-beliefs desire)
  (define (prob event) (if (equal? desire event) (+ .5 bias) (- .5 bias)))
  (list '(rain shine) (list (prob 'rain) (prob 'shine))))

(barplot (biased-beliefs 'rain) "my prior beliefs if i want rain")
(barplot (biased-beliefs 'shine) "my prior beliefs if i want shine")
(barplot (biased-beliefs 'whatevs) "my prior beliefs if i don't care")
~~~~

Even with this preference, they can still infer that an undesired outcome has occured if they have enough evidence.

~~~~
;;;fold:
(define bias .4)
(define (biased-beliefs desire)
  (define (prob event) (if (equal? desire event) (+ .5 bias) (- .5 bias)))
  (list '(rain shine) (list (prob 'rain) (prob 'shine))))
;;;


(define (inference desire evidence)
  (enumeration-query
   (define rain? (equal? 'rain (apply multinomial (biased-beliefs desire))))
   (define umbrella? (flip (if rain? 0.9 0.1)))
   (define water-falling-from-the-sky? (flip (if rain? 0.99999 0.00001)))
   (if rain? 'rain 'shine)
   (condition (eval evidence))))

(barplot (inference
          'shine
          '(equal? water-falling-from-the-sky? #t))
         "if water is falling from the sky, it's raining even if i don't want it to be.")
~~~~

But this will color people's inferences.

~~~~
;;;fold:
(define bias .4)
(define (biased-beliefs desire)
  (define (prob event) (if (equal? desire event) (+ .5 bias) (- .5 bias)))
  (list '(rain shine) (list (prob 'rain) (prob 'shine))))

(define (inference desire evidence)
  (enumeration-query
   (define rain? (equal? 'rain (apply multinomial (biased-beliefs desire))))
   (define umbrella? (flip (if rain? 0.9 0.1)))
   (define water-falling-from-the-sky? (flip (if rain? 0.99999 0.00001)))
   (if rain? 'rain 'shine)
   (condition (eval evidence))))
;;;

(barplot (inference
          'rain
          '(equal? umbrella? #t))
         "seeing an umbrella means rain if i want rain...")

(barplot (inference
          'shine
          '(equal? umbrella? #t))
         "...but not if i don't.")
~~~~

A literal listener can infer what the someone wants and what they believe based on what they assert and what evidence they have access to. That is, if the speaker asserts something they don't have good evidence for, it's probably because they want it to be true and therefore consider it *a priori* more likely.

~~~~
;;;fold:
(define bias .4)
(define (biased-beliefs desire)
  (define (prob event) (if (equal? desire event) (+ .5 bias) (- .5 bias)))
  (list '(rain shine) (list (prob 'rain) (prob 'shine))))

(define (inference desire evidence)
  (enumeration-query
   (define rain? (equal? 'rain (apply multinomial (biased-beliefs desire))))
   (define umbrella? (flip (if rain? 0.9 0.1)))
   (define water-falling-from-the-sky? (flip (if rain? 0.99999 0.00001)))
   (if rain? 'rain 'shine)
   (condition (eval evidence))))
;;;

(define (desire-prior) (uniform-draw '(rain shine whatevs)))

(define (see obs)
  (case obs
        (('shine) '(equal? water-falling-from-the-sky? #f))
        (('no-umbrella) '(equal? umbrella? #f))
        (('no-evidence) #t)
        (('umbrella) '(equal? umbrella? #t))
        (('rain) '(equal? water-falling-from-the-sky? #t))))

(define (meaning utterance)
  (case utterance
        (('SILENCE) #t)
        (('rain) '(equal? belief 'rain))
        (('shine) '(equal? belief 'shine))
        (('want-rain) '(equal? desire 'rain))
        (('want-shine) '(equal? desire 'shine))
        (('dont-care) '(equal? desire 'whatevs))))

(define literal-listener
  (mem (lambda (utterance evidence)
         (enumeration-query
          (define desire (desire-prior))
          (define belief (apply multinomial (inference desire evidence)))
          (list belief desire)
          (eval (meaning utterance))))))

(barplot (literal-listener 'shine (see 'rain))
         "if a biased speaker tells a literal listener 'it's not raining' when it clearly is, what does the literal listener think they (believe, desire)?")
~~~~

A speaker can communicate their desire this way, even if they don't actually believe what they say is actually true.

~~~~
;;;fold:
(define bias .4)
(define (biased-beliefs desire)
  (define (prob event) (if (equal? desire event) (+ .5 bias) (- .5 bias)))
  (list '(rain shine) (list (prob 'rain) (prob 'shine))))

(define (inference desire evidence)
  (enumeration-query
   (define rain? (equal? 'rain (apply multinomial (biased-beliefs desire))))
   (define umbrella? (flip (if rain? 0.9 0.1)))
   (define water-falling-from-the-sky? (flip (if rain? 0.99999 0.00001)))
   (if rain? 'rain 'shine)
   (condition (eval evidence))))

(define (desire-prior) (uniform-draw '(rain shine whatevs)))

(define (see obs)
  (case obs
        (('shine) '(equal? water-falling-from-the-sky? #f))
        (('no-umbrella) '(equal? umbrella? #f))
        (('no-evidence) #t)
        (('umbrella) '(equal? umbrella? #t))
        (('rain) '(equal? water-falling-from-the-sky? #t))))

(define (meaning utterance)
  (case utterance
        (('SILENCE) #t)
        (('rain) '(equal? belief 'rain))
        (('shine) '(equal? belief 'shine))
        (('want-rain) '(equal? desire 'rain))
        (('want-shine) '(equal? desire 'shine))
        (('dont-care) '(equal? desire 'whatevs))))
;;;

(define literal-listener
  (mem (lambda (utterance evidence QUD)
         (enumeration-query
          (define desire (desire-prior))
          (define belief (apply multinomial (inference desire evidence)))
          (eval QUD)
          (eval (meaning utterance))))))

(define (utterance-prior) (multinomial '(SILENCE rain shine want-rain want-shine dont-care)
                                       (map (lambda (x) (exp (- x))) '(2 1 1 1 1 1))))

(define speaker
  (mem (lambda (belief desire evidence QUD)
         (enumeration-query
          (define utterance (utterance-prior))
          utterance
          (condition (equal? (apply multinomial (literal-listener utterance evidence QUD))
                             (eval QUD)))))))

(barplot (speaker 'rain 'shine (see 'umbrella) 'desire)
         "we both see an umbrella. i think it's raining, but i want it not to be. to communicate my desire what should i say?")
(barplot (speaker 'rain 'shine (see 'umbrella) 'belief)
         "we both see an umbrella. i think it's raining, but i want it not to be. to communicate my belief what should i say?")
~~~~

A pragmatic listener who believes the speaker is actually biased can infer what the speaker wants, believes, and wants to communicate about.

~~~~
;;;fold:
(define bias .4)
(define (biased-beliefs desire)
  (define (prob event) (if (equal? desire event) (+ .5 bias) (- .5 bias)))
  (list '(rain shine) (list (prob 'rain) (prob 'shine))))

(define (inference desire evidence)
  (enumeration-query
   (define rain? (equal? 'rain (apply multinomial (biased-beliefs desire))))
   (define umbrella? (flip (if rain? 0.9 0.1)))
   (define water-falling-from-the-sky? (flip (if rain? 0.99999 0.00001)))
   (if rain? 'rain 'shine)
   (condition (eval evidence))))

;; utilities & parameters
(define alpha 5)
(define (power lst alpha) (map (lambda (x) (expt x alpha)) lst))
(define (seq start distance end)
  (if (> (+ start distance) end) (list start)
      (append (list start) (seq (+ start distance) distance end))))
(define (theta-prior) (uniform-draw (seq 0 0.2 0.8)))
(define (approx? a b) (< (abs (- a b)) 0.2))

(define (desire-prior) (uniform-draw '(rain shine whatevs)))

(define (see obs)
  (case obs
        (('shine) '(equal? water-falling-from-the-sky? #f))
        (('no-umbrella) '(equal? umbrella? #f))
        (('no-evidence) #t)
        (('umbrella) '(equal? umbrella? #t))
        (('rain) '(equal? water-falling-from-the-sky? #t))))

(define (meaning utterance)
  (case utterance
        (('SILENCE) #t)
        (('rain) '(equal? belief 'rain))
        (('shine) '(equal? belief 'shine))
        (('want-rain) '(equal? desire 'rain))
        (('want-shine) '(equal? desire 'shine))
        (('dont-care) '(equal? desire 'whatevs))))
(define (utterance-prior) (multinomial '(SILENCE rain shine want-rain want-shine dont-care)
                                       (map (lambda (x) (exp (- x))) '(2 1 1 1 1 1))))

(define literal-listener
  (mem (lambda (utterance evidence QUD)
         (enumeration-query
          (define desire (desire-prior))
          (define belief (apply multinomial (inference desire evidence)))
          (eval QUD)
          (eval (meaning utterance))))))

(define speaker
  (mem (lambda (belief desire evidence QUD)
         (enumeration-query
          (define utterance (utterance-prior))
          utterance
          (condition (equal? (apply multinomial (literal-listener utterance evidence QUD))
                             (eval QUD)))))))
;;;

(define pragmatic-listener
  (mem (lambda (utterance evidence whichvar?)
         (enumeration-query
          (define desire (desire-prior))
          (define belief (apply multinomial (inference desire evidence)))
          (define QUD (uniform-draw '(desire belief)))
          (eval whichvar?)
          (condition (equal? utterance
                             (apply multinomial (speaker belief desire evidence QUD))))))))

(barplot (pragmatic-listener 'shine (see 'umbrella) '(list QUD belief desire))
         "*sees umbrella* 'it's not raining' (QUD, belief, desire)")

(barplot (pragmatic-listener 'shine (see 'umbrella) 'QUD)
         "what is the speaker trying to communicate about?")
(barplot (pragmatic-listener 'shine (see 'umbrella) 'desire)
         "what does the speaker want?")
(barplot (pragmatic-listener 'shine (see 'umbrella) 'belief)
         "what does the speaker believe?")
~~~~

But maybe the pragmatic listener doesn't think the speaker is *actually* biased.

~~~~
;;;fold:
(define bias .4)
(define (biased-beliefs desire)
  (define (prob event) (if (equal? desire event) (+ .5 bias) (- .5 bias)))
  (list '(rain shine) (list (prob 'rain) (prob 'shine))))

(define (inference desire evidence)
  (enumeration-query
   (define rain? (equal? 'rain (apply multinomial (biased-beliefs desire))))
   (define umbrella? (flip (if rain? 0.9 0.1)))
   (define water-falling-from-the-sky? (flip (if rain? 0.99999 0.00001)))
   (if rain? 'rain 'shine)
   (condition (eval evidence))))

(define (desire-prior) (uniform-draw '(rain shine whatevs)))

(define (see obs)
  (case obs
        (('shine) '(equal? water-falling-from-the-sky? #f))
        (('no-umbrella) '(equal? umbrella? #f))
        (('no-evidence) #t)
        (('umbrella) '(equal? umbrella? #t))
        (('rain) '(equal? water-falling-from-the-sky? #t))))

(define (meaning utterance)
  (case utterance
        (('SILENCE) #t)
        (('rain) '(equal? belief 'rain))
        (('shine) '(equal? belief 'shine))
        (('want-rain) '(equal? desire 'rain))
        (('want-shine) '(equal? desire 'shine))
        (('dont-care) '(equal? desire 'whatevs))))
(define (utterance-prior) (multinomial '(SILENCE rain shine want-rain want-shine dont-care)
                                       (map (lambda (x) (exp (- x))) '(2 1 1 1 1 1))))

(define literal-listener
  (mem (lambda (utterance evidence QUD)
         (enumeration-query
          (define desire (desire-prior))
          (define belief (apply multinomial (inference desire evidence)))
          (eval QUD)
          (eval (meaning utterance))))))

(define speaker
  (mem (lambda (belief desire evidence QUD)
         (enumeration-query
          (define utterance (utterance-prior))
          utterance
          (condition (equal? (apply multinomial (literal-listener utterance evidence QUD))
                             (eval QUD)))))))
;;;

(define pragmatic-listener
  (mem (lambda (utterance evidence whichvar?)
         (enumeration-query
          (define desire (desire-prior))
          (define belief (apply multinomial (inference 'whatevs evidence)))
          (define QUD (uniform-draw '(desire belief)))
          (eval whichvar?)
          (condition (equal? utterance
                             (apply multinomial (speaker belief desire evidence QUD))))))))

(barplot (pragmatic-listener 'shine (see 'umbrella) '(list QUD belief desire))
         "*sees umbrella* 'it's not raining' (QUD, belief, desire)")

(barplot (pragmatic-listener 'shine (see 'umbrella) 'QUD)
         "what is the speaker trying to communicate about?")
(barplot (pragmatic-listener 'shine (see 'umbrella) 'desire)
         "what does the speaker want?")
(barplot (pragmatic-listener 'shine (see 'umbrella) 'belief)
         "what does the speaker believe?")

~~~~

Even if the bias is not very strong, we can still use it to communicate desire.

~~~~
(define bias .1)

;;;fold:
(define (biased-beliefs desire)
  (define (prob event) (if (equal? desire event) (+ .5 bias) (- .5 bias)))
  (list '(rain shine) (list (prob 'rain) (prob 'shine))))

(define (inference desire evidence)
  (enumeration-query
   (define rain? (equal? 'rain (apply multinomial (biased-beliefs desire))))
   (define umbrella? (flip (if rain? 0.9 0.1)))
   (define water-falling-from-the-sky? (flip (if rain? 0.99999 0.00001)))
   (if rain? 'rain 'shine)
   (condition (eval evidence))))

(define (desire-prior) (uniform-draw '(rain shine whatevs)))

(define (see obs)
  (case obs
        (('shine) '(equal? water-falling-from-the-sky? #f))
        (('no-umbrella) '(equal? umbrella? #f))
        (('no-evidence) #t)
        (('umbrella) '(equal? umbrella? #t))
        (('rain) '(equal? water-falling-from-the-sky? #t))))

(define (meaning utterance)
  (case utterance
        (('SILENCE) #t)
        (('rain) '(equal? belief 'rain))
        (('shine) '(equal? belief 'shine))
        (('want-rain) '(equal? desire 'rain))
        (('want-shine) '(equal? desire 'shine))
        (('dont-care) '(equal? desire 'whatevs))))
(define (utterance-prior) (multinomial '(SILENCE rain shine want-rain want-shine dont-care)
                                       (map (lambda (x) (exp (- x))) '(2 1 1 1 1 1))))

(define literal-listener
  (mem (lambda (utterance evidence QUD)
         (enumeration-query
          (define desire (desire-prior))
          (define belief (apply multinomial (inference desire evidence)))
          (eval QUD)
          (eval (meaning utterance))))))

(define speaker
  (mem (lambda (belief desire evidence QUD)
         (enumeration-query
          (define utterance (utterance-prior))
          utterance
          (condition (equal? (apply multinomial (literal-listener utterance evidence QUD))
                             (eval QUD)))))))

(define pragmatic-listener
  (mem (lambda (utterance evidence whichvar?)
         (enumeration-query
          (define desire (desire-prior))
          (define belief (apply multinomial (inference 'whatevs evidence)))
          (define QUD (uniform-draw '(desire belief)))
          (eval whichvar?)
          (condition (equal? utterance
                             (apply multinomial (speaker belief desire evidence QUD))))))))
;;;

(barplot (biased-beliefs 'rain) "my biased prior if i want rain")
(barplot (biased-beliefs 'shine) "my biased prior if i want shine")
(barplot (biased-beliefs 'whatevs) "my biased prior if i don't care")

(barplot (pragmatic-listener 'shine (see 'umbrella) '(list QUD belief desire))
         "*sees umbrella* 'it's not raining' (QUD, belief, desire)")

(barplot (pragmatic-listener 'shine (see 'umbrella) 'QUD)
         "what is the speaker trying to communicate about?")
(barplot (pragmatic-listener 'shine (see 'umbrella) 'desire)
         "what does the speaker want?")
(barplot (pragmatic-listener 'shine (see 'umbrella) 'belief)
         "what does the speaker believe?")
~~~~

The less evidence there is for your claim, the more likely you are trying to express the desire for that claim to be true.

~~~~
(define bias .4)

;;;fold:
(define (biased-beliefs desire)
  (define (prob event) (if (equal? desire event) (+ .5 bias) (- .5 bias)))
  (list '(rain shine) (list (prob 'rain) (prob 'shine))))

(define (inference desire evidence)
  (enumeration-query
   (define rain? (equal? 'rain (apply multinomial (biased-beliefs desire))))
   (define umbrella? (flip (if rain? 0.9 0.1)))
   (define water-falling-from-the-sky? (flip (if rain? 0.99999 0.00001)))
   (if rain? 'rain 'shine)
   (condition (eval evidence))))

(define (desire-prior) (uniform-draw '(rain shine whatevs)))

(define (see obs)
  (case obs
        (('shine) '(equal? water-falling-from-the-sky? #f))
        (('no-umbrella) '(equal? umbrella? #f))
        (('no-evidence) #t)
        (('umbrella) '(equal? umbrella? #t))
        (('rain) '(equal? water-falling-from-the-sky? #t))))

(define (meaning utterance)
  (case utterance
        (('SILENCE) #t)
        (('rain) '(equal? belief 'rain))
        (('shine) '(equal? belief 'shine))
        (('want-rain) '(equal? desire 'rain))
        (('want-shine) '(equal? desire 'shine))
        (('dont-care) '(equal? desire 'whatevs))))
(define (utterance-prior) (multinomial '(SILENCE rain shine want-rain want-shine dont-care)
                                       (map (lambda (x) (exp (- x))) '(2 1 1 1 1 1))))

(define literal-listener
  (mem (lambda (utterance evidence QUD)
         (enumeration-query
          (define desire (desire-prior))
          (define belief (apply multinomial (inference desire evidence)))
          (eval QUD)
          (eval (meaning utterance))))))

(define speaker
  (mem (lambda (belief desire evidence QUD)
         (enumeration-query
          (define utterance (utterance-prior))
          utterance
          (condition (equal? (apply multinomial (literal-listener utterance evidence QUD))
                             (eval QUD)))))))

(define pragmatic-listener
  (mem (lambda (utterance evidence whichvar?)
         (enumeration-query
          (define desire (desire-prior))
          (define belief (apply multinomial (inference 'whatevs evidence)))
          (define QUD (uniform-draw '(desire belief)))
          (eval whichvar?)
          (condition (equal? utterance
                             (apply multinomial (speaker belief desire evidence QUD))))))))
;;;

(first (pragmatic-listener 'shine (see 'umbrella) '(list QUD desire)))

(define (getdist obs) (second (pragmatic-listener 'shine (see obs) '(list QUD desire))))

(define observations '(shine no-umbrella no-evidence umbrella rain))

(barplot (list observations (map third (map getdist observations)))
         "when is 'it's raining' most likely to mean 'i want it to be raining?")
~~~~
