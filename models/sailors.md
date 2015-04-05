---
layout: model
title: SAILORS teaching example
---
### Outline
- Probability and why it's useful in a world where there is often uncertainty
- Generative models and why they're useful for describing situations that often involve uncertainty
- Bayes' rule and how it helps you infer things that are not directly observable in a generative model
- Brainstorm: What are some things that Bayes' rule can help you figure out? Draw a generative model.
- Concrete example with vending machine
..* Actions (button press) generate outcomes (cookies/bagels)
..* Given an outcome, what was the action?
..* What if you had different prior expectations about the action?
- Exercise: Specify your generative model in more detail (what actions/causes lead to what effects with what probability?) Make some predictions/inferences.
- Bayes' rule is used A LOT in both AI and cognitive science
..* Medical diagnosis
..* Spam detection
..* etc

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
