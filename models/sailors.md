---
layout: model
title: SAILORS teaching example
model-language: church
---
### Outline
- Probability and why it's useful in a world where there is often uncertainty
- Generative models and why they're useful for describing situations that involve uncertainty
- Bayes' rule and how it helps you infer things that are not directly observable in a generative model
- Brainstorm: What are some things that Bayes' rule can help you figure out? Draw a generative model.
- Concrete example with student performance
  * Brainstorm variables that affect performance on a given task (current ability, effort, task difficulty, etc)
  * Model 1 (effort -> performance)
    * Effort is the only variable that determines performance
    * Trying is more likely to generate good performance; not trying less likely
    * Given uniform prior for effort, what is the probability of doing well?
    * Given a different prior, what is the probability of doing well?
    * Given that a student did well, how likely is it that she tried?
    * Conditioning as utility/choice rule: given that a student wants to do well, how much should she try?
  * Model 2 (effort, current ability -> performance)
    * Explaining away
    * Given high effort and good performance, what is the probability that the student has high abiltiy?
    * Given no effort and good performance, what is the probability that the student has high ability?
    * What might be some problems in this model?
  * Introduce growth mindset: effort can lead to higher future ability!
  * Model 3 (effort, current ability -> performance, making others think you have high abiltiy, future ability)
    * Introduce "improvement": effort right now leads to very high ability in the future; no effort makes ability stay the same
    * Introduce "evaluation": other person's inference of the student's ability given outcome and effort
    * Given this model, what should you do given the following goals? (goals: succeeding now, making others think you have high ability, having high ability in the future)
    * How does this change given different beliefs (priors) about your ability? (high/low/neutral)
    * How does this change given different beliefs about improvement?
- General discussion of mindsets
- General discussion about why it might be useful to formalize some of these intuitions about mindsets in a model
  * Interventions
  * Predictions
- Bayes' rule is used A LOT in both AI and cognitive science
  * Medical diagnosis
  * Spam detection
  * etc

### Motivation

Humans are very quite good at reasoning about other people. In particular, humans are very good at reasoning about other people's goals and preferences, even when these goals and preferences are not directly observable. How are people so good at this? If we understand how *people* do it, maybe we would be able to build smarter machines that can learn about people's goals and preferences and respond to them in smarter ways.

### Generative Model of a Vending Machine

Imagine a weird vending machine that has a bunch of cookies and bagels in it. There is a red button on the vending machine.

Press "run" to find out what the vending machine gives you when you press the red button! (This is called a *simulation*.) Press "run" several times. How many times did you get bagels? How many times did you get cookies? Are you more likely to get a bagel or a cookie?

~~~
(define (red_button) (if (flip 0.7) 'bagel 'cookie))

(red_button)
~~~

What happens if you press the button 100 times?

How many bagels will you get?

How many cookies will you get?

Try it out!

~~~
(define (button) (if (flip 0.7) 'bagel 'cookie))

(hist (repeat 100 button))
~~~

You explore the vending machine a bit more, and you find out that there are actually two buttons. You press both buttons 100 times, and you end up with these *distributions* of bagels and cookies. If you really wanted a cookie, which button would you press?

~~~
(define (red_button) (if (flip 0.7) 'bagel 'cookie))
(define (blue_button) (if (flip 0.7) 'cookie 'bagel))

(hist (repeat 100 red_button) "red button!")
(hist (repeat 100 blue_button) "blue button!")
~~~

Imagine your four-year-old cousin Amy plays with the vending machine. She doesn't know how the machine works, so she presses the red and blue buttons *randomly*. That is to say, she is equally likely to press the red button as she is to press the blue button.

What is the probability that Amy gets a cookie?

~~~
(define (red_button) (if (flip 0.7) 'bagel 'cookie))
(define (blue_button) (if (flip 0.7) 'cookie 'bagel))

(barplot (enumeration-query
          (define (amy_food) (if (flip 0.5) (red_button) (blue_button)))
          (amy_food)
          (condition #t)))
~~~

### Bayes Rule! Conditioning on observations.

Let's say Amy pressed a button and got a cookie. We want to guess what button she pressed.

How would *you* guess? What information would you have to consider?

Here's how our program can guess:

~~~
(define (red_button) (if (flip 0.7) 'bagel 'cookie))
(define (blue_button) (if (flip 0.7) 'cookie 'bagel))

(barplot (enumeration-query
	(define amy_color (if (flip 0.5) 'red 'blue))
	(define amy_food (if (equal? amy_color 'red) (red_button) (blue_button)))
	amy_color
	(condition #t)) "Probability of Amy pressing the red/blue button, without knowing what food she got (prior)")

(barplot (enumeration-query
	(define amy_color (if (flip 0.5) 'red 'blue))
	(define amy_food (if (equal? amy_color 'red) (red_button) (blue_button)))
	amy_color
	(condition (equal? amy_food 'cookie))) "Probability of Amy pressing the red/blue button given she got a cookie (posterior)")
~~~

Suppose you know that Amy likes the color red more than blue, which makes her more likely to press the red button. Let's say she presses the red button 80% of the time.

Now how likely is she to get a cookie?

~~~
(define (red_button) (if (flip 0.7) 'bagel 'cookie))
(define (blue_button) (if (flip 0.7) 'cookie 'bagel))

(barplot (enumeration-query
          (define (amy_food) (if (flip 0.8) (red_button) (blue_button)))
          (amy_food)
          (condition #t)))
~~~

Now, given that she gets a cookie, what button do we think she pressed? Is this answer different from when we *didn't* know Amy's color preference? Why?

~~~
(define (red_button) (if (flip 0.7) 'bagel 'cookie))
(define (blue_button) (if (flip 0.7) 'cookie 'bagel))

(barplot (enumeration-query
	(define amy_color (if (flip 0.8) 'red 'blue))
	(define amy_food (if (equal? amy_color 'red) (red_button) (blue_button)))
	amy_color
	(condition #t)) "Probability of Amy pressing the red/blue button, without knowing what food she got (prior)")

(barplot (enumeration-query
	(define amy_color (if (flip 0.8) 'red 'blue))
	(define amy_food (if (equal? amy_color 'red) (red_button) (blue_button)))
	amy_color
	(condition (equal? amy_food 'cookie))) "Probability of Amy pressing the red/blue button given she got a cookie (posterior)")
~~~

<!-- ### Inferring people's goals

~~~
(define (red_button) (if (flip 0.7) 'bagel 'cookie))
(define (blue_button) (if (flip 0.7) 'cookie 'bagel))

(barplot (enumeration-query
	(define amy_color (if (flip 0.8) 'red 'blue))
	(define amy_food (if (equal? amy_color 'red) (red_button) (blue_button)))
	amy_color
	(condition #t)) "Prior")

(barplot (enumeration-query
	(define amy_color (if (flip 0.8) 'red 'blue))
	(define amy_food (if (equal? amy_color 'red) (red_button) (blue_button)))
	amy_color
	(condition (equal? amy_food 'cookie))) "Posterior")
~~~ -->
