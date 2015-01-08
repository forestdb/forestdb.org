---
layout: model
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

;; utilities & parameters
(define alpha 5)
(define (power lst alpha) (map (lambda (x) (expt x alpha)) lst))
(define (seq start distance end)
  (if (> (+ start distance) end) (list start)
      (append (list start) (seq (+ start distance) distance end))))
(define (theta-prior) (uniform-draw (seq 0 0.2 0.8)))
(define (approx? a b) (< (abs (- a b)) 0.2))

(define (utterance-prior) (multinomial '(SILENCE rain shine)
                                       (list (exp -2) (exp -1) (exp -1))))
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
        (('want-shine) '(equal? desire 'shine))))

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
        (('want-shine) '(equal? desire 'shine))))
;;;

(define literal-listener
  (mem (lambda (utterance evidence QUD)
         (enumeration-query
          (define desire (desire-prior))
          (define belief (apply multinomial (inference desire evidence)))
          (eval QUD)
          (eval (meaning utterance))))))

(define (utterance-prior) (multinomial '(SILENCE rain shine want-rain want-shine)
                                       (list (exp -2) (exp -1) (exp -1) (exp -1) (exp -1))))

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

A pragmatic listener who believes the speaker is actually biased can infer what the speaker wants, believes, and want to communicate about.

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
        (('want-shine) '(equal? desire 'shine))))

(define literal-listener
  (mem (lambda (utterance evidence QUD)
         (enumeration-query
          (define desire (desire-prior))
          (define belief (apply multinomial (inference desire evidence)))
          (eval QUD)
          (eval (meaning utterance))))))

(define (utterance-prior) (multinomial '(SILENCE rain shine want-rain want-shine)
                                       (list (exp -2) (exp -1) (exp -1) (exp -1) (exp -1))))

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

But maybe the pragmatic listener doesn't think the speaker is *actually* biased. This makes the inference about desire even stronger.

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
        (('want-shine) '(equal? desire 'shine))))

(define literal-listener
  (mem (lambda (utterance evidence QUD)
         (enumeration-query
          (define desire (desire-prior))
          (define belief (apply multinomial (inference desire evidence)))
          (eval QUD)
          (eval (meaning utterance))))))

(define (utterance-prior) (multinomial '(SILENCE rain shine want-rain want-shine)
                                       (list (exp -2) (exp -1) (exp -1) (exp -1) (exp -1))))

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

But maybe the pragmatic listener doesn't think the speaker is *actually* biased. This makes the inference about desire even stronger.

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
        (('want-shine) '(equal? desire 'shine))))

(define literal-listener
  (mem (lambda (utterance evidence QUD)
         (enumeration-query
          (define desire (desire-prior))
          (define belief (apply multinomial (inference desire evidence)))
          (eval QUD)
          (eval (meaning utterance))))))

(define (utterance-prior) (multinomial '(SILENCE rain shine want-rain want-shine)
                                       (list (exp -2) (exp -1) (exp -1) (exp -1) (exp -1))))

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
        (('want-shine) '(equal? desire 'shine))))

(define literal-listener
  (mem (lambda (utterance evidence QUD)
         (enumeration-query
          (define desire (desire-prior))
          (define belief (apply multinomial (inference desire evidence)))
          (eval QUD)
          (eval (meaning utterance))))))

(define (utterance-prior) (multinomial '(SILENCE rain shine want-rain want-shine)
                                       (list (exp -2) (exp -1) (exp -1) (exp -1) (exp -1))))

(define speaker
  (mem (lambda (belief desire evidence QUD)
         (enumeration-query
          (define utterance (utterance-prior))
          utterance
          (condition (equal? (apply multinomial (literal-listener utterance evidence QUD))
                             (eval QUD)))))))
;;;

(define pragmatic-listener
(mem (lambda (utterance evidence))))

(barplot (speaker 'rain 'shine (see 'umbrella) 'desire)
         "we both see an umbrella. i think it's raining, but i want it not to be. to communicate my desire what should i say?")
(barplot (speaker 'rain 'shine (see 'umbrella) 'belief)
         "we both see an umbrella. i think it's raining, but i want it not to be. to communicate my belief what should i say?")
~~~~

~~~~

(define speaker
  (mem (lambda (belief)
         (enumeration-query
          (define utterance (utterance-prior))
          utterance
          (condition (equal? (apply multinomial (literal-listener utterance))
                             belief))))))
;;;

(define (see obs)
  (case obs
        (('shine) '(equal? water-falling-from-the-sky? #f))
        (('no-umbrella) '(equal? umbrella? #f))
        (('no-evidence) #t)
        (('umbrella) '(equal? umbrella? #t))
        (('rain) '(equal? water-falling-from-the-sky? #t))))

(define pragmatic-listener
  (mem (lambda (utterance evidence)
         (enumeration-query
          (define desire (uniform-draw '(rain shine whatevs)))
          ;;the pragmatic listener thinks the speaker is probably biased by their desires
          (define belief (apply multinomial (inference desire evidence)))
          (list belief desire)
          (condition (equal? utterance
                             (apply multinomial (speaker belief evidence))))))))

(define (see obs)
  (case obs
        (('shine) '(equal? water-falling-from-the-sky? #f))
        (('no-umbrella) '(equal? umbrella? #f))
        (('no-evidence) #t)
        (('umbrella) '(equal? umbrella? #t))
        (('rain) '(equal? water-falling-from-the-sky? #t))))

(barplot (pragmatic-listener 'shine (see 'umbrella))
         "if you and a biased speaker see an umbrella and they tell you, 'it's not raining', what do they probably (believe, desire)?")

(barplot (pragmatic-listener 'shine (see 'shine)) "what if you can both see that it's clearly sunny?")
(barplot (pragmatic-listener 'shine (see 'rain)) "what if you can both see that it's clearly raining?")
(barplot (pragmatic-listener 'shine (see 'no-evidence)) "what if you have no evidence at all?")
~~~~

If a pragmatic speaker knows it's probably raining, they can say it's not if the question under discussion is their desires rather than their beliefs.

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

(define (utterance-prior) (multinomial '(SILENCE rain shine)
                                       (list (exp -2) (exp -1) (exp -1))))

(define (meaning utterance)
  (case utterance
        (('SILENCE) #t)
        (('rain) '(equal? belief 'rain))
        (('shine) '(equal? belief 'shine))))

(define literal-listener
  (mem (lambda (utterance)
         (enumeration-query
          ;;let's say the listener is unbiased and has no evidence
          (define belief (apply multinomial (inference 'whatevs #t)))
          belief
          (eval (meaning utterance))))))

(define speaker
  (mem (lambda (belief)
         (enumeration-query
          (define utterance (utterance-prior))
          utterance
          (condition (equal? (apply multinomial (literal-listener utterance))
                             belief))))))

(define (see obs)
  (case obs
        (('shine) '(equal? water-falling-from-the-sky? #f))
        (('no-umbrella) '(equal? umbrella? #f))
        (('no-evidence) #t)
        (('umbrella) '(equal? umbrella? #t))
        (('rain) '(equal? water-falling-from-the-sky? #t))))
;;;

(define pragmatic-listener
  (mem (lambda (utterance evidence QUD)
         (enumeration-query
          (define desire (uniform-draw '(rain shine whatevs)))
          ;;the pragmatic listener thinks the speaker is probably biased by their desires
          (define belief (apply multinomial (inference desire evidence)))
          (eval QUD)
          (condition (equal? utterance
                             (apply multinomial (speaker belief evidence))))))))

(define pragmatic-speaker
  (mem (lambda (belief desire evidence QUD)
         (enumeration-query
          (define utterance (utterance-prior))
          utterance
          (condition (equal? (apply multinomial (pragmatic-listener utterance evidence QUD))
                             (eval QUD)))))))

(barplot (pragmatic-speaker 'rain 'shine (see 'umbrella) 'desire)
         "we both see an umbrella. i think it's raining. to communicate my desire what should i say?")

(barplot (pragmatic-speaker 'rain 'shine (see 'umbrella) 'belief)
         "we both see an umbrella. i think it's raining. to communicate my belief what should i say?")

~~~~

Moving up the chain, a pragmatic L2 listener can infer the QUD, whether or not it's raining, and what the speaker desires. Let's assume first that the l2 listener thinks the pragmatic speaker is a wishful thinker.

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

(define (utterance-prior) (multinomial '(SILENCE rain shine)
                                       (list (exp -2) (exp -1) (exp -1))))

(define (meaning utterance)
  (case utterance
        (('SILENCE) #t)
        (('rain) '(equal? belief 'rain))
        (('shine) '(equal? belief 'shine))))

(define literal-listener
  (mem (lambda (utterance)
         (enumeration-query
          ;;let's say the listener is unbiased and has no evidence
          (define belief (apply multinomial (inference 'whatevs #t)))
          belief
          (eval (meaning utterance))))))

(define speaker
  (mem (lambda (belief)
         (enumeration-query
          (define utterance (utterance-prior))
          utterance
          (condition (equal? (apply multinomial (literal-listener utterance))
                             belief))))))

(define (see obs)
  (case obs
        (('shine) '(equal? water-falling-from-the-sky? #f))
        (('no-umbrella) '(equal? umbrella? #f))
        (('no-evidence) #t)
        (('umbrella) '(equal? umbrella? #t))
        (('rain) '(equal? water-falling-from-the-sky? #t))))

(define pragmatic-listener
  (mem (lambda (utterance evidence QUD)
         (enumeration-query
          (define desire (uniform-draw '(rain shine whatevs)))
          (define belief (apply multinomial (inference desire evidence)))
          (eval QUD)
          (condition (equal? utterance
                             (apply multinomial (speaker belief evidence))))))))

(define pragmatic-speaker
  (mem (lambda (belief desire evidence QUD)
         (enumeration-query
          (define utterance (utterance-prior))
          utterance
          (condition (equal? (apply multinomial (pragmatic-listener utterance evidence QUD))
                             (eval QUD)))))))
;;;

(define l2-listener
  (mem (lambda (utterance evidence whichvar?)
         (enumeration-query
          ;;the l2 listener might think the pragmatic speaker is biased, but they might not.
          (define desire (uniform-draw '(rain shine whatevs)))
          (define belief (apply multinomial (inference desire evidence)))
          (define QUD (uniform-draw '(belief desire)))
          (eval whichvar?)
          (condition (equal? (apply multinomial (pragmatic-speaker belief desire evidence QUD))
                             utterance))))))

(barplot (l2-listener 'shine (see 'no-evidence) 'QUD)
          "out of the blue i tell you it's not raining. what do i want to communicate about?")
(barplot (l2-listener 'shine (see 'umbrella) 'QUD)
          "we both see an umbrella and i tell you it's not raining. what do i want to communicate about?")
(barplot (l2-listener 'shine (see 'rain) 'QUD)
          "we both see that it's clearly raining and i tell you it's not raining. what do i want to communicate about?")

(barplot (l2-listener 'shine (see 'rain) '(list belief desire QUD))
          "we both see that it's clearly raining and i tell you it's not raining. what do i (believe, desire, want to communicate about?)")

(barplot (l2-listener 'shine (see 'no-evidence) '(list belief desire QUD))
          "we see nothing and i tell you it's not raining. what do i (believe, desire, want to communicate about?)")
~~~~

Even if the l2 listener doesn't think the pragmatic speaker is a wishful thinker, because they have a convention of a hypothetical wishful speaker, the l2 listener and pragmatic speaker can communicate about desire in this way.

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

(define (utterance-prior) (multinomial '(SILENCE rain shine)
                                       (list (exp -2) (exp -1) (exp -1))))

(define (meaning utterance)
  (case utterance
        (('SILENCE) #t)
        (('rain) '(equal? belief 'rain))
        (('shine) '(equal? belief 'shine))))

(define literal-listener
  (mem (lambda (utterance)
         (enumeration-query
          ;;let's say the listener is unbiased and has no evidence
          (define belief (apply multinomial (inference 'whatevs #t)))
          belief
          (eval (meaning utterance))))))

(define speaker
  (mem (lambda (belief)
         (enumeration-query
          (define utterance (utterance-prior))
          utterance
          (condition (equal? (apply multinomial (literal-listener utterance))
                             belief))))))

(define (see obs)
  (case obs
        (('shine) '(equal? water-falling-from-the-sky? #f))
        (('no-umbrella) '(equal? umbrella? #f))
        (('no-evidence) #t)
        (('umbrella) '(equal? umbrella? #t))
        (('rain) '(equal? water-falling-from-the-sky? #t))))

(define pragmatic-listener
  (mem (lambda (utterance evidence QUD)
         (enumeration-query
          (define desire (uniform-draw '(rain shine whatevs)))
          (define belief (apply multinomial (inference desire evidence)))
          (eval QUD)
          (condition (equal? utterance
                             (apply multinomial (speaker belief evidence))))))))

(define pragmatic-speaker
  (mem (lambda (belief desire evidence QUD)
         (enumeration-query
          (define utterance (utterance-prior))
          utterance
          (condition (equal? (apply multinomial (pragmatic-listener utterance evidence QUD))
                             (eval QUD)))))))
;;;

(define l2-listener
  (mem (lambda (utterance evidence whichvar?)
         (enumeration-query
          ;;the l2 listener might think the pragmatic speaker is biased, but they might not.
          (define desire (uniform-draw '(rain shine whatevs)))
          (define belief (apply multinomial (inference desire evidence)))
          (define QUD (uniform-draw '(belief desire)))
          (eval whichvar?)
          (condition (equal? (apply multinomial (pragmatic-speaker belief desire evidence QUD))
                             utterance))))))

(barplot (l2-listener 'shine (see 'no-evidence) '(list belief desire QUD))
          "no evidence")

(barplot (l2-listener 'shine (see 'umbrella) '(list belief desire QUD))
          "see umbrella")

(barplot (l2-listener 'shine (see 'rain) '(list belief desire QUD))
          "see rain")
~~~~

What if the bias due to wishful thinking is more subtle?

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

;; utilities & parameters
(define alpha 5)
(define (power lst alpha) (map (lambda (x) (expt x alpha)) lst))
(define (seq start distance end)
  (if (> (+ start distance) end) (list start)
      (append (list start) (seq (+ start distance) distance end))))
(define (theta-prior) (uniform-draw (seq 0 0.2 0.8)))
(define (approx? a b) (< (abs (- a b)) 0.2))

(define (utterance-prior) (multinomial '(SILENCE rain shine)
                                       (list (exp -2) (exp -1) (exp -1))))

(define (meaning utterance)
  (case utterance
        (('SILENCE) #t)
        (('rain) '(equal? belief 'rain))
        (('shine) '(equal? belief 'shine))))

(define literal-listener
  (mem (lambda (utterance)
         (enumeration-query
          ;;let's say the listener is unbiased and has no evidence
          (define belief (apply multinomial (inference 'whatevs #t)))
          belief
          (eval (meaning utterance))))))

(define speaker
  (mem (lambda (belief)
         (enumeration-query
          (define utterance (utterance-prior))
          utterance
          (condition (equal? (apply multinomial (literal-listener utterance))
                             belief))))))

(define (see obs)
  (case obs
        (('shine) '(equal? water-falling-from-the-sky? #f))
        (('no-umbrella) '(equal? umbrella? #f))
        (('no-evidence) #t)
        (('umbrella) '(equal? umbrella? #t))
        (('rain) '(equal? water-falling-from-the-sky? #t))))

(define pragmatic-listener
  (mem (lambda (utterance evidence QUD)
         (enumeration-query
          (define desire (uniform-draw '(rain shine whatevs)))
          (define belief (apply multinomial (inference desire evidence)))
          (eval QUD)
          (condition (equal? utterance
                             (apply multinomial (speaker belief evidence))))))))

(define pragmatic-speaker
  (mem (lambda (belief desire evidence QUD)
         (enumeration-query
          (define utterance (utterance-prior))
          utterance
          (condition (equal? (apply multinomial (pragmatic-listener utterance evidence QUD))
                             (eval QUD)))))))

(define l2-listener
  (mem (lambda (utterance evidence whichvar?)
         (enumeration-query
          ;;the l2 listener might think the pragmatic speaker is biased, but they might not.
          (define desire (uniform-draw '(rain shine whatevs)))
          (define belief (apply multinomial (inference desire evidence)))
          (define QUD (uniform-draw '(belief desire)))
          (eval whichvar?)
          (condition (equal? (apply multinomial (pragmatic-speaker belief desire evidence QUD))
                             utterance))))))
;;;

(barplot (biased-beliefs 'rain) "my prior beliefs if i want rain")
(barplot (biased-beliefs 'shine) "my prior beliefs if i want shine")
(barplot (biased-beliefs 'whatevs) "my prior beliefs if i don't care")

(barplot (l2-listener 'shine (see 'rain) '(list belief desire QUD))
          "we both see that it's clearly raining and i tell you it's not raining. what do i (believe, desire, want to communicate about?)")
~~~~

Of course, it's also possible for the speaker to just say what they want.

~~~~
(define bias .9)
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

;; utilities & parameters
(define alpha 5)
(define (power lst alpha) (map (lambda (x) (expt x alpha)) lst))
(define (seq start distance end)
  (if (> (+ start distance) end) (list start)
      (append (list start) (seq (+ start distance) distance end))))
(define (theta-prior) (uniform-draw (seq 0 0.2 0.8)))
(define (approx? a b) (< (abs (- a b)) 0.2))

(define (desire-prior) (uniform-draw '(rain shine whatevs)))

(define (utterance-prior) (multinomial '(SILENCE rain shine want-rain want-shine)
                                       (list (exp -2) (exp -1) (exp -1) (exp -1) (exp -1))))

(define (meaning utterance)
  (case utterance
        (('SILENCE) #t)
        (('want-rain) '(equal? desire 'rain))
        (('want-shine) '(equal? desire 'shine))
        (('rain) '(equal? belief 'rain))
        (('shine) '(equal? belief 'shine))))

(define literal-listener
  (mem (lambda (utterance)
         (enumeration-query
          ;;let's say the listener is unbiased and has no evidence
          (define belief (apply multinomial (inference 'whatevs #t)))
          (define desire (desire-prior))
          belief
          (eval (meaning utterance))))))

(define speaker
  (mem (lambda (belief desire)
         (enumeration-query
          (define utterance (utterance-prior))
          utterance
          (condition (equal? (apply multinomial (literal-listener utterance))
                             belief))))))

(define (see obs)
  (case obs
        (('shine) '(equal? water-falling-from-the-sky? #f))
        (('no-umbrella) '(equal? umbrella? #f))
        (('no-evidence) #t)
        (('umbrella) '(equal? umbrella? #t))
        (('rain) '(equal? water-falling-from-the-sky? #t))))

(define pragmatic-listener
  (mem (lambda (utterance evidence QUD)
         (enumeration-query
          (define desire (uniform-draw '(rain shine whatevs)))
          (define belief (apply multinomial (inference desire evidence)))
          (eval QUD)
          (condition (equal? utterance
                             (apply multinomial (speaker belief evidence))))))))

(define pragmatic-speaker
  (mem (lambda (belief desire evidence QUD)
         (enumeration-query
          (define utterance (utterance-prior))
          utterance
          (condition (equal? (apply multinomial (pragmatic-listener utterance evidence QUD))
                             (eval QUD)))))))

(define l2-listener
  (mem (lambda (utterance evidence whichvar?)
         (enumeration-query
          ;;the l2 listener might think the pragmatic speaker is biased, but they might not.
          (define desire (uniform-draw '(rain shine whatevs)))
          (define belief (apply multinomial (inference desire evidence)))
          (define QUD (uniform-draw '(belief desire)))
          (eval whichvar?)
          (condition (equal? (apply multinomial (pragmatic-speaker belief desire evidence QUD))
                             utterance))))))
;;;

(barplot (pragmatic-listener 'shine (see 'rain))
         "L1 *sees rain* "it's not raining" (belief, desire)")

(barplot (l2-listener 'shine (see 'rain) '(list belief desire QUD))
          "L2 *sees rain* "it's not raining" (belief, desire, QUD)")
~~~~
