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
   (define umbrella? (flip (if rain? 0.7 0.3)))
   (define rain-boots? (flip (if rain? 0.9 0.1)))
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
   (define umbrella? (flip (if rain? 0.7 0.3)))
   (define rain-boots? (flip (if rain? 0.9 0.1)))
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

The pragmatic listener can infer what the speaker wants and what they believe based on what they assert and what evidence they have access to. That is, if the speaker asserts something they don't have good evidence for, it's probably because they want it to be true and therefore consider it *a priori* more likely.

~~~~
;;;fold:
(define bias .4)
(define (biased-beliefs desire)
  (define (prob event) (if (equal? desire event) (+ .5 bias) (- .5 bias)))
  (list '(rain shine) (list (prob 'rain) (prob 'shine))))

(define (inference desire evidence)
  (enumeration-query
   (define rain? (equal? 'rain (apply multinomial (biased-beliefs desire))))
   (define umbrella? (flip (if rain? 0.7 0.3)))
   (define rain-boots? (flip (if rain? 0.9 0.1)))
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
;;;

(define pragmatic-listener
  (mem (lambda (utterance evidence)
         (enumeration-query
          (define desire (uniform-draw '(rain shine whatevs)))
          (define belief (apply multinomial (inference desire evidence)))
          (list desire belief)
          (condition (equal? utterance
                             (apply multinomial (speaker belief evidence))))))))

(define (evidence obs)
  (case obs
        (('sees-shine) '(equal? water-falling-from-the-sky? #f))
        (('sees-no-umbrella) '(equal? umbrella? #f))
        (('no-evidence) #t)
        (('sees-umbrella) '(equal? umbrella? #t))
        (('sees-rain) '(equal? water-falling-from-the-sky? #t))))

(barplot (pragmatic-listener 'shine (evidence 'sees-umbrella))
         "if you and a biased speaker see an umbrella and they tell you, 'it's not raining', what do they probably (desire, believe)")
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
   (define umbrella? (flip (if rain? 0.7 0.3)))
   (define rain-boots? (flip (if rain? 0.9 0.1)))
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
          (define belief (apply multinomial (inference desire evidence)))
          (eval QUD)
          (condition (equal? utterance
                             (apply multinomial (speaker belief evidence))))))))

(define pragmatic-speaker
  (mem (lambda (desire evidence QUD)
         (enumeration-query
          (define belief (apply multinomial (inference desire evidence)))
          (define utterance (utterance-prior))
          utterance
          (condition (equal? (apply multinomial (pragmatic-listener utterance evidence QUD))
                             (eval QUD)))))))

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

Moving up the chain, a pragmatic L2 listener can infer the QUD, whether or not it's raining, and what the speaker desires.

~~~~
;;;fold:
(define bias .4)
(define (biased-beliefs desire)
  (define (prob event) (if (equal? desire event) (+ .5 bias) (- .5 bias)))
  (list '(rain shine) (list (prob 'rain) (prob 'shine))))

(define (inference desire evidence)
  (enumeration-query
   (define rain? (equal? 'rain (apply multinomial (biased-beliefs desire))))
   (define umbrella? (flip (if rain? 0.7 0.3)))
   (define rain-boots? (flip (if rain? 0.9 0.1)))
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
          (define belief (apply multinomial (inference 'whatevs evidence)))
          (define desire (uniform-draw '(rain shine whatevs)))
          (define QUD (uniform-draw '(belief desire)))
          ;(list belief desire QUD)
          (eval whichvar?)
          (condition (equal? (apply multinomial (pragmatic-speaker belief desire evidence QUD))
                             utterance))))))

(barplot (l2-listener 'shine (see 'umbrella) 'QUD)
          "we both see an umbrella and i tell you it's not raining. what do i want to communicate about?")
(barplot (l2-listener 'shine (see 'umbrella) 'desire)
          "we both see an umbrella and i tell you it's not raining. what do i want?")
(barplot (l2-listener 'shine (see 'umbrella) 'belief)
          "we both see an umbrella and i tell you it's not raining. what do i think the weather is like?")

(barplot (l2-listener 'shine (see 'no-evidence) 'QUD)
          "out of the blue i tell you it's not raining. what do i want to communicate about?")
(barplot (l2-listener 'shine (see 'no-evidence) 'desire)
          "out of the blue i tell you it's not raining. what do i want?")
(barplot (l2-listener 'shine (see 'no-evidence) 'belief)
          "out of the blue i tell you it's not raining. what do i think the weather is like?")

~~~~

<!-- (define l2-listener
  (mem (lambda (utterance evidence)
         (enumeration-query
          (define belief (apply multinomial (inference 'whatevs evidence)))
          (define desire (uniform-draw '(rain shine whatevs)))
          (define QUD (uniform-draw '(belief desire)))
          (list belief desire QUD)
          (condition (equal? (apply multinomial (pragmatic-speaker belief desire evidence QUD))
                             utterance))))))

(barplot (l2-listener 'shine (see 'umbrella))
          "we both see an umbrella and i tell you it's not raining. what do i (believe, desire, want to communicate about?)") -->


<!-- When am I most likely to effectively communicate my desire in this way?

~~~~
;;;fold:
(define bias .4)
(define (biased-beliefs desire)
  (define (prob event) (if (equal? desire event) (+ .5 bias) (- .5 bias)))
  (list '(rain shine) (list (prob 'rain) (prob 'shine))))

(define (inference desire evidence)
  (enumeration-query
   (define rain? (equal? 'rain (apply multinomial (biased-beliefs desire))))
   (define umbrella? (flip (if rain? 0.7 0.3)))
   (define rain-boots? (flip (if rain? 0.9 0.1)))
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
  (mem (lambda (desire evidence QUD)
         (enumeration-query
          (define belief (apply multinomial (inference desire evidence)))
          (define utterance (utterance-prior))
          utterance
          (condition (equal? (apply multinomial (pragmatic-listener utterance evidence QUD))
                             (eval QUD)))))))

(define l2-listener
  (mem (lambda (utterance evidence which-variable?)
         (enumeration-query
          (define belief (apply multinomial (inference 'whatevs evidence)))
          (define desire (uniform-draw '(rain shine whatevs)))
          (define QUD (uniform-draw '(belief desire)))
          (eval which-variable?)
          (condition (equal? (apply multinomial (pragmatic-speaker desire evidence QUD))
                             utterance))))))
;;;

(define (prob-wants-shine obs) (second (second (l2-listener 'shine (see obs) 'desire))))

(define contexts '(shine no-umbrella no-evidence umbrella rain))

(barplot (list contexts (map prob-wants-shine contexts))
         "when will the listener correctly infer the speaker's desire for shine?")
~~~~ -->



<!-- 
~~~~
;;;fold:
(define bias .4)
(define (biased-beliefs desire)
  (define (prob event) (if (equal? desire event) (+ .5 bias) (- .5 bias)))
  (list '(rain shine) (list (prob 'rain) (prob 'shine))))

(define (inference desire evidence)
  (enumeration-query
   (define rain? (equal? 'rain (apply multinomial (biased-beliefs desire))))
   (define umbrella? (flip (if rain? 0.7 0.3)))
   (define rain-boots? (flip (if rain? 0.9 0.1)))
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
  (mem (lambda (desire evidence)
         (enumeration-query
          (define belief (apply multinomial (inference desire evidence)))
          (define utterance (utterance-prior))
          utterance
          (condition (equal? (apply multinomial (literal-listener utterance))
                             belief))))))

(define (pragmatic-listener utterance evidence)
  (enumeration-query
   (define desire (if (flip) 'rain 'shine))
   desire
   (condition (equal? utterance
                      (apply multinomial (speaker desire evidence))))))

(barplot (pragmatic-listener 'shine '(equal? umbrella? #t))
         "if i say it's raining when i see an umbrella, what do i probably want?")
~~~~

If the listener knows the QUD is the speaker's desire, the more evidence against the speaker's claim, the more likely it is they desire the claim to be true.

~~~~
;;;fold:
(define bias .4)
(define (biased-beliefs desire)
  (define (prob event) (if (equal? desire event) (+ .5 bias) (- .5 bias)))
  (list '(rain shine) (list (prob 'rain) (prob 'shine))))

(define (inference desire evidence)
  (enumeration-query
   (define rain? (equal? 'rain (apply multinomial (biased-beliefs desire))))
   (define umbrella? (flip (if rain? 0.7 0.3)))
   (define rain-boots? (flip (if rain? 0.9 0.1)))
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
  (mem (lambda (desire evidence)
         (enumeration-query
          (define belief (apply multinomial (inference desire evidence)))
          (define utterance (utterance-prior))
          utterance
          (condition (equal? (apply multinomial (literal-listener utterance))
                             belief))))))

(define pragmatic-listener
(mem (lambda (utterance evidence)
  (enumeration-query
   (define desire (if (flip) 'rain 'shine))
   desire
   (condition (equal? utterance
                      (apply multinomial (speaker desire evidence))))))))

(define pragmatic-speaker
(mem (lambda (desire evidence)
(enumeration-query
(define utterance (utterance-prior))
utterance
(condition (equal? (apply multinomial (pragmatic-listener utterance evidence))
                   desire))))))
;;;

(define (evidence obs)
  (case obs
        (('sees-shine) '(equal? water-falling-from-the-sky? #f))
        (('sees-no-umbrella) '(equal? umbrella? #f))
        (('no-evidence) #t)
        (('sees-umbrella) '(equal? umbrella? #t))
        (('sees-rain) '(equal? water-falling-from-the-sky? #t))))

(define (prob-wants-shine obs) (first (second (pragmatic-listener 'shine (evidence obs)))))

(define contexts '(sees-shine sees-no-umbrella no-evidence sees-umbrella sees-rain))

(barplot (list contexts (map prob-wants-shine contexts))
         "when will the listener infer the speaker's desire for shine?")
~~~~

The listener can also infer the QUD.

~~~~
;;;fold:
(define bias .4)
(define (biased-beliefs desire)
  (define (prob event) (if (equal? desire event) (+ .5 bias) (- .5 bias)))
  (list '(rain shine) (list (prob 'rain) (prob 'shine))))

(define (inference desire evidence)
  (enumeration-query
   (define rain? (equal? 'rain (apply multinomial (biased-beliefs desire))))
   (define umbrella? (flip (if rain? 0.7 0.3)))
   (define rain-boots? (flip (if rain? 0.9 0.1)))
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
;;;

(define pragmatic-listener
(mem (lambda (utterance evidence)
  (enumeration-query
   (define desire (if (flip) 'rain 'shine))
   (define belief (apply multinomial (inference desire evidence)))
   (list desire belief)
   (condition (equal? utterance
                      (apply multinomial (speaker desire evidence))))))))

(define (evidence obs)
  (case obs
        (('sees-shine) '(equal? water-falling-from-the-sky? #f))
        (('sees-no-umbrella) '(equal? umbrella? #f))
        (('no-evidence) #t)
        (('sees-umbrella) '(equal? umbrella? #t))
        (('sees-rain) '(equal? water-falling-from-the-sky? #t))))

(barplot (pragmatic-listener shine (evidence 'sees-umbrella)))
~~~~ -->

<!-- A pragmatic speaker can choose to communicate their desire in this way.

~~~~
;;;fold:
(define bias .4)
(define (biased-beliefs desire)
  (define (prob event) (if (equal? desire event) (+ .5 bias) (- .5 bias)))
  (list '(rain shine) (list (prob 'rain) (prob 'shine))))

(define (inference desire evidence)
  (enumeration-query
   (define rain? (equal? 'rain (apply multinomial (biased-beliefs desire))))
   (define umbrella? (flip (if rain? 0.7 0.3)))
   (define rain-boots? (flip (if rain? 0.9 0.1)))
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
  (mem (lambda (desire evidence)
         (enumeration-query
          (define belief (apply multinomial (inference desire evidence)))
          (define utterance (utterance-prior))
          utterance
          (condition (equal? (apply multinomial (literal-listener utterance))
                             belief))))))

(define pragmatic-listener
(mem (lambda (utterance evidence)
  (enumeration-query
   (define desire (if (flip) 'rain 'shine))
   desire
   (condition (equal? utterance
                      (apply multinomial (speaker desire evidence))))))))
;;;

(define pragmatic-speaker
(mem (lambda (desire evidence)
(enumeration-query
(define utterance (utterance-prior))
utterance
(condition (equal? (apply multinomial (pragmatic-listener utterance evidence))
                   desire))))))

(barplot (pragmatic-speaker 'shine '(equal? umbrella? #t)))
~~~~

Communicating desire in this way is most likely to happen when the evidence doesn't clearly contradict the belief, but also doesn't definitively support it.

~~~~
;;;fold:
(define bias .4)
(define (biased-beliefs desire)
  (define (prob event) (if (equal? desire event) (+ .5 bias) (- .5 bias)))
  (list '(rain shine) (list (prob 'rain) (prob 'shine))))

(define (inference desire evidence)
  (enumeration-query
   (define rain? (equal? 'rain (apply multinomial (biased-beliefs desire))))
   (define umbrella? (flip (if rain? 0.7 0.3)))
   (define rain-boots? (flip (if rain? 0.9 0.1)))
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
  (mem (lambda (desire evidence)
         (enumeration-query
          (define belief (apply multinomial (inference desire evidence)))
          (define utterance (utterance-prior))
          utterance
          (condition (equal? (apply multinomial (literal-listener utterance))
                             belief))))))

(define pragmatic-listener
(mem (lambda (utterance evidence)
  (enumeration-query
   (define desire (if (flip) 'rain 'shine))
   desire
   (condition (equal? utterance
                      (apply multinomial (speaker desire evidence))))))))

(define pragmatic-speaker
(mem (lambda (desire evidence)
(enumeration-query
(define utterance (utterance-prior))
utterance
(condition (equal? (apply multinomial (pragmatic-listener utterance evidence))
                   desire))))))
;;;

(define (evidence obs)
  (case obs
        (('sees-shine) '(equal? water-falling-from-the-sky? #f))
        (('sees-no-umbrella) '(equal? umbrella? #f))
        (('no-evidence) #t)
        (('sees-umbrella) '(equal? umbrella? #t))
        (('sees-rain) '(equal? water-falling-from-the-sky? #t))))

(define (prob-says-shine obs) (third (second (pragmatic-speaker 'shine (evidence obs)))))

(define contexts '(sees-shine sees-no-umbrella no-evidence sees-umbrella sees-rain))

(barplot (list contexts (map prob-says-shine contexts))
         "when will the speaker communicate desire this way?")
~~~~ -->

<!-- Here's some m-implicature for fun.

~~~~
;;;fold:
(define (biased-beliefs desire)
  (define (prob event) (if (equal? desire event) 0.9 .1))
  (list '(rain shine) (list (prob 'rain) (prob 'shine))))

(define (inference desire evidence)
  (enumeration-query
   (define rain? (equal? 'rain (apply multinomial (biased-beliefs desire))))
   (define umbrella? (flip (if rain? 0.9 0.1)))
   (define rain-boots? (flip (if rain? 0.9 0.1)))
   (define water-falling-from-the-sky? (flip (if rain? 1 0)))
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
;;;

;;let's assume the listener thinks the speaker probably knows one way or the other
(define (certainty-prior) (multinomial '(0 0.2 0.4 0.6 0.8 1) '(1 2 4 7 11 16)))

(define (utterance-prior) (multinomial '(SILENCE rain shine must-rain must-shine) '(3 2 2 1 1)))

(define (meaning utterance)
  (case utterance
        (('SILENCE) #t)
        (('rain) '(and (equal? belief 'rain) (> certainty theta)))
        (('shine) '(and (equal? belief 'shine) (> certainty theta)))
        (('must-rain) '(and (equal? belief 'rain) (> certainty theta-must)))
        (('must-shine) '(and (equal? belief 'shine) (> certainty theta-must)))))

(define literal-listener
  (mem (lambda (utterance theta theta-must)
         (enumeration-query
          (define certainty (certainty-prior))

          ;;let's say the listener is unbiased and has no evidence
          (define belief (apply multinomial (inference 'whatevs #t)))

          (list belief certainty)

          (eval (meaning utterance))))))

(define speaker
  (mem (lambda (belief certainty theta theta-must)
         (enumeration-query
          (define utterance (utterance-prior))
          (define interpretation (apply multinomial (literal-listener utterance theta theta-must)))
          utterance
          (condition (equal? (first interpretation) belief))
          (condition (approx? (second interpretation) certainty))))))

(define (listener utterance)
  (enumeration-query
   (define theta (theta-prior))
   (define theta-must (theta-prior))
   (define belief (if (flip) 'rain 'shine))
   (define certainty (certainty-prior))
   (list belief certainty)
   (condition (equal? utterance
                      (apply multinomial (speaker belief certainty theta theta-must))))))

(barplot (listener 'rain) "it's raining")
(barplot (listener 'must-rain) "it must be raining")
~~~~ -->