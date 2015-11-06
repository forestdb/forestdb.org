---
layout: model
title: 	Inferences with Binomials
model-category: Bayesian Data Analysis
model-status: code
model-language: webppl
---

<script src="http://web.stanford.edu/~erindb/webppl-viz/webppl.min.js"></script>  
<link rel="stylesheet" href="http://web.stanford.edu/~erindb/webppl-viz/viz.css">

## Lee & Wagenmakers 3.1: Inferring a rate

Flip a coin 10 times. It comes up heads 5 of those 10 times.

~~~~
var data =  5
var n = 10

var model = function(){
	var theta = beta(1,1)

	var score = binomialERP.score([theta,n], data);

	factor(score)

	return {"theta":theta}
}

var results = MH(model, 5000)

vizPrint(results, "theta")
print("expected value of theta is ")
print(expectation(results, function(v){return v["theta"]}))
~~~~

## Lee & Wagenmakers 3.2: Difference between two rates

Flip two different coins, 20 times each. One comes up heads 3 times.
The other comes up heads 11 times. What is the difference in weights?

~~~~
var k1 =  3
var k2 = 11
var n1 = 20
var n2 = 20

var model = function(){
	var theta1 = beta(1,1)
	var theta2 = beta(1,1)

	var score = binomialERP.score([theta1,n1], k1) + 
				binomialERP.score([theta2,n2], k2);

	factor(score)

	return {"difference":theta1-theta2}
}

var results = MH(model, 5000)

vizPrint(results)
print("expected value of the difference in thetas is ")
print(expectation(results, function(v){return v["difference"]}))
~~~~

## Lee & Wagenmakers 3.3: Inferring a common rate

Flip two identical coins, 20 times each. One comes up heads 14 times.
The other comes up heads 16 times. What is their (common) weight?

~~~~
var k1 = 14
var k2 = 16
var n1 = 20
var n2 = 20

var model = function(){
	var theta = beta(1,1)

	var score = binomialERP.score([theta,n1], k1) + 
				binomialERP.score([theta,n2], k2);

	factor(score)

	return {"theta":theta}
	// return {"predictive": binomial(theta, 20)}
}

var results = MH(model, 5000)

vizPrint(results)
print("expected value of the common theta is ")
print(expectation(results, function(v){return v["theta"]}))

// print("expected value of the posterior predictive is ")
// print(expectation(results, function(v){return v["predictive"]}))
~~~~

Now try changing the data (`k1`, `k2`) so that they are wildly different:
1 and 19. What is the most likely coin weight to generate these two?

Now try uncommented the alternative return statement (and commenting the original one). 
Also switch the print statements at the bottom.
This shows you the distribution on heads (i.e., the results of flipping the coin) given
what you've learned about the coin's weight. We call this distribuion the *posterior predictive distribution*; it shows what data the model actually predicts.
Look at the posterior distribution. Are the original data points likely under this model?
(i.e., does the posterior predictive assign `k1` and `k2` high probability?)
Why or why not?




