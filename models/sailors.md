---
layout: model
title: SAILORS teaching example
---

### Motivation

Reasoning under uncertainty. And reasoning about other people who reason under uncertainty. And reasoning about other people who reason ...

### Generative Model of a Vending Machine

Imagine we have a broken vending machine. It has one button, and when you press it, the vending machine usually gives you a bagel, but not always.

It only gives you a bagel 60% of the time. The rest of the time, it gives you a cookie.

Press the button!

~~~
(define (button) (if (flip 0.7) 'bagel 'cookie))

(button)
~~~

What happens if you press the button 100 times?

How many bagels will you get?

How many cookies will you get?

Try it out!

~~~
(define (button) (if (flip 0.7) 'bagel 'cookie))

(hist (repeat 100 button))
~~~

Imagine a slightly more complicated vending machine that still behaves randomly. Now there are two buttons. The red button gives you a bagel 60% of the time, and the blue button gives you a cookie 60% of the time.

~~~
(define (red_button) (if (flip 0.7) 'bagel 'cookie))
(define (blue_button) (if (flip 0.7) 'cookie 'bagel))

(hist (repeat 100 red_button) "red button!")
(hist (repeat 100 blue_button) "blue button!")
~~~

Imagine your four-year-old cousin Amy plays with the vending machine. She doesn't know how the machine works, so she presses the red and blue buttons equally often (50% of the time each).

What's the probability that Amy gets a cookie?

~~~
(define (red_button) (if (flip 0.7) 'bagel 'cookie))
(define (blue_button) (if (flip 0.7) 'cookie 'bagel))

(barplot (enumeration-query
          (define (amy_food) (if (flip 0.5) (red_button) (blue_button)))
          (amy_food)
          (condition #t)))
~~~

### Bayes Rule!! Conditioning on observations.

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
	(condition #t)) "Prior")

(barplot (enumeration-query
	(define amy_color (if (flip 0.5) 'red 'blue))
	(define amy_food (if (equal? amy_color 'red) (red_button) (blue_button)))
	amy_color
	(condition (equal? amy_food 'cookie))) "Posterior")
~~~

What if we know that Amy likes the color red the most (so she presses the red button 80% of the time).

Now how likely is she to get a cookie?

~~~
(define (red_button) (if (flip 0.7) 'bagel 'cookie))
(define (blue_button) (if (flip 0.7) 'cookie 'bagel))

(barplot (enumeration-query
          (define (amy_food) (if (flip 0.8) (red_button) (blue_button)))
          (amy_food)
          (condition #t)))
~~~

Now, given that she gets a cookie, what button do we think she pressed?

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