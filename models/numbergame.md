---
layout: model
title: SAILORS Teaching Example
model-language: webppl
---

* toc
{:toc}

## Introduction

AI researchers have become extremely good at designing algorithms to process and make sense of the huge amount of messy, unstructured data we have on the Internet. It is commonly believed that "Big Data" will revolutionalize AI and enable machines to automatically acquire rich knowledge from data in order to behave intelligently. While the sheer amount of data that the ditigal world has accumulated is undoubtedly powerful, it is often useful to take a step back and think about how humans--the most intelligent machines we have around--reason about and learn from data. Humans don't always require thousands of data points to learn concepts and draw conclusions. Often, we see a small amount of data and draw highly reasonable conclusions about it. This is sometimes called an "inductive leap:" observing small pieces of evidence, making generalizations about them, and using these generalizations to draw larger conclusions and make predictions.

Let's make this idea of inductive reasoning a bit more concrete with a simple example.

Suppose your friend shows you this sequence of numbers and tells you that they are "positive examples" of a category of numbers: {16, 8, 2, 64}.

(Note: "positive examples" are examples of that category; "negative examples" are examples that are *not* of that category.)

* Do you think 4 belongs to this category?
* What about 7?
* Or 10?

Now, suppose someone shows you this sequence of numbers and tells you that they are "positive examples" of a different category of numbers: {60, 80, 10, 30}

* Do you think 4 belongs to this category?
* What about 7?
* Or 10?

How did you come up with these answers? What do you think these number categories are?

## Hypotheses

Let's zoom in on the first sequence: {2, 8, 16, 64}. What are some hypotheses of which number category these numbers came from? Another way to think of this is, what is the process that *generated* these numbers?

### Generative Models

It is often intuitive and helpful to think of data samples (such as these numbers) as being "generated" from a category. Since there are several different number categories that we consider, there are also several different ways in which these numbers were generated from those categories.

Here is one way the numbers {2, 8, 16, 64} could have been generated:

1. Your friend was thinking of the concept (or number category): "powers of 2".
2. Your friend randomly samples some numbers that are powers of 2.

This is what that looks like in code:

~~~~
///fold:
// sample N items from an array, without replacement
var drawN = function(array, N) {
  if (N == 0) {
    return [];
  } else {
    var sampled = uniformDraw(array);
    var remaining = remove(sampled, array);
    return [sampled].concat(drawN(remaining, N-1));
  }
}
// sample 4 items from an array, without replacement
var draw4 = function(array) {
  return drawN(array, 4);
}
///

// first we make an array of all the powers of 2 less than 50:
var powers_of_2 = [1, 2, 4, 8, 16, 32];

// then we sample 4 of those powers of 2:
var get_examples = function() {
  return draw4(powers_of_2)
};

get_examples();
~~~~

You can press the "run" button above multiple times to get different samples from this category.

You can also see graphs of this simulation. (Let's put our 4 numbers in order, so the graph looks nicer)

~~~~
///fold:
// sample N items from an array, without replacement
var drawN = function(array, N) {
  if (N == 0) {
    return [];
  } else {
    var sampled = uniformDraw(array);
    var remaining = remove(sampled, array);
    // NOTE: now we're sorting the result.
    return sort([sampled].concat(drawN(remaining, N-1)));
  }
}
// sample 4 items from an array, without replacement
var draw4 = function(array) {
  return drawN(array, 4);
}

// first we make an array of all the powers of 2 less than 50:
var powers_of_2 = [1, 2, 4, 8, 16, 32];

// then we sample 4 of those powers of 2:
var get_examples = function() {return draw4(powers_of_2)};

get_examples();
///
print(Enumerate(get_examples));
~~~~  

## Powers of 2 or Multiples of 2?

Of course, there are other ways the numbers {2, 8, 16, 64} could have been generated:

1. Your friend was thinking of the concept (or number category): "multiples of 2".
2. Your friend randomly samples some numbers that are multiples of 2.

Or even this generative process:

1. Your friend was thinking of the concept (or number category): "integers less than 50".
2. Your friend randomly samples some numbers that are integers smaller than 50.


### Generative Model

Let's say your friend could be thinking of either "multiples of 2" or "powers of 2", and that she'll only give you numbers less than or equal to 10.

First, she randomly samples one of those concepts:

~~~~
var pick_concept = function() {
  var concept = uniformDraw(["powers_of_2", "multiples_of_2"]);
  return concept;
}

print(Enumerate(pick_concept));
~~~~  

Then, whatever concept she chooses, she gives you 2 examples from that concept:

~~~~
///fold:
// sample N items from an array, without replacement
var drawN = function(array, N) {
  if (N == 0) {
    return [];
  } else {
    var sampled = uniformDraw(array);
    var remaining = remove(sampled, array);
    // NOTE: now we're sorting the result.
    return sort([sampled].concat(drawN(remaining, N-1)));
  }
}
// sample 2 items from an array, without replacement
var draw3 = function(array) {
  return drawN(array, 3);
}

var pick_concept = function() {
  var concept = uniformDraw(["powers_of_2", "multiples_of_2"]);
  return concept;
}
///

// make arrays of all the powers and multiples of 2 up to 10:
var concept_examples = {
  "powers_of_2": [1, 2, 4, 8],
  "multiples_of_2": [0, 2, 4, 6, 8, 10]
}

// in the generative model, we chose a concept, and then sample from it
var generate_examples = function() {
  var concept = pick_concept();
  var examples = draw3(concept_examples[concept]);
  return examples;
}

print(Enumerate(generate_examples));
~~~~  

### Conditioning

What's the conditional probability of [2, 4, 8] given that your friend is thinking of "multiples of 2"? what if she was thinking of "powers of 2"?

~~~~
///fold:
// sample N items from an array, without replacement
var drawN = function(array, N) {
  if (N == 0) {
    return [];
  } else {
    var sampled = uniformDraw(array);
    var remaining = remove(sampled, array);
    // NOTE: now we're sorting the result.
    return sort([sampled].concat(drawN(remaining, N-1)));
  }
}
// sample 2 items from an array, without replacement
var draw3 = function(array) {
  return drawN(array, 3);
}

var pick_concept = function() {
  var concept = uniformDraw(["powers_of_2", "multiples_of_2"]);
  return concept;
}
///

// make arrays of all the powers and multiples of 2 up to 10:
var concept_examples = {
  "powers_of_2": [1, 2, 4, 8],
  "multiples_of_2": [0, 2, 4, 6, 8, 10]
}

// in the generative model, we chose a concept, and then sample from it
var generate_examples_with_condition = function() {
  var concept = pick_concept();
  var examples = draw3(concept_examples[concept]);
  condition( concept == "multiples_of_2" );
  return examples;
}

print(Enumerate(generate_examples_with_condition));
~~~~ 

### Infer category given number sequence

Now that we have different hypotheses about how these numbers were generated, how do we determine which hypothesis is the most likely one given the data?

We can do this using Bayes' Rule.

~~~~
///fold:
// a function that checks whether the elements (and order) of two arrays
// are the same, even though technically, the *arrays themselves* are
// stored in separate places and are not connected to each other, so they
// not "the same thing"
var array_equals = function(arrayA, arrayB) {
  return JSON.stringify(arrayA) == JSON.stringify(arrayB);
}

// sample N items from an array, without replacement
var drawN = function(array, N) {
  if (N == 0) {
    return [];
  } else {
    var sampled = uniformDraw(array);
    var remaining = remove(sampled, array);
    // NOTE: now we're sorting the result.
    return sort([sampled].concat(drawN(remaining, N-1)));
  }
}
// sample 2 items from an array, without replacement
var draw3 = function(array) {
  return drawN(array, 3);
}

var pick_concept = function() {
  var concept = uniformDraw(["powers_of_2", "multiples_of_2"]);
  return concept;
}
///

// make arrays of all the powers and multiples of 2 up to 10:
var concept_examples = {
  "powers_of_2": [1, 2, 4, 8],
  "multiples_of_2": [0, 2, 4, 6, 8, 10]
}

/* the inference process includes
*     * the generative model
*     * a condition on the observations
*/
var infer_concept = function(observations) {
  return function() {
    var concept = pick_concept();
    var examples = draw3(concept_examples[concept]);
    condition( array_equals( examples, observations ) );
    return concept;
  }
}

print(Enumerate(infer_concept([1, 4, 8])));
~~~~

#### Size principle

Suppose you saw the following sequences:

* [1, 4, 8]
* [0, 6, 10]
* [2, 4, 8]

What concept do you think these are examples of?

Does our program make the same inferences? Try plugging these observations in to the model above.

Can you explain the model's inference for the observed sequence [2, 8]?
*Hint:* How many numbers are powers of 2 and how many numbers are multiples of 2?
What is the conditional probability of [2, 4, 8] given each of the concepts?

This is called the "size principle": your friend could have been thinking of "multiples of 2" or "powers of 2", since both concepts are consistent with [2, 8]. But if your friend had been thinking of "multiples of 2", there are a lot more sequences she *could have chosen* than if she was thinking of "powers of 2". <!--The probability that she stumbles upon a sequence that happens to be full of powers of 3 when she was thinking of "multiples of 3" is a *lot* lower than the probability that she picks a "powers of 3" sequence that happens to be full of multiples of 3. -->

## Priors

What if we change the prior probabilities of different hypotheses? 

Suppose the friend who generated the number sequence {16, 8, 2, 64} is a precocious 4th-grader. You know that she has not yet learned the concept of "powers." You believe that it is quite unlikely that she would be thinking about a number category as complex as "powers of 2." How does this change your belief about what number category she is thinking of, given that she generated the number sequence {16, 8, 2, 64}?

## Sampling method

Notice that there are many different ways your friend could have generated these examples. She could have sampled the numbers randomly, as we assumed in the code boxes before. Or, she could have sampled them in an intentional manner, to purposefully help you understand the number category and not confuse it with a different number category. Or, she could have sampled them in a different intentional manner--to intentionally deceive you into thinking that it is a different number category.

* positive examples
* positive and negative examples
* labelling things in the world

OED? If you have 2 competing hypotheses, what number should you test next?


## Why is this important?


## Backup examples?

* word/concept learning (rational rules)
* 

## Resources/References

* dippl.org
* probmods.org
* http://web.mit.edu/cocosci/Papers/nips99preprint.pdf

