---
layout: model
title: 	Inferences with binomials
model-category: Bayesian Data Analysis
model-status: code
model-language: webppl
---

## Lee & Wagenmakers 3.1: Inferring a rate

~~~~
var data =  5
var n = 10

var model = function(){
	var theta = beta(1,1)

	var score = binomialERP.score([theta,n], data);

	factor(score)

	return theta
}

var results = MH(model,1000)

print("expected value of theta is ")
print(expectation(results))
~~~~

## Lee & Wagenmakers 3.2: Difference between two rates

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

	return theta1-theta2
}

var results = MH(model,1000)

print("expected value of the difference in thetas is ")
print(expectation(results))
~~~~

## Lee & Wagenmakers 3.3: Inferring a common rate

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

	return theta
}

var results = MH(model,1000)

print("expected value of the common theta is ")
print(expectation(results))
~~~~






