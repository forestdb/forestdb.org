---
layout: model
title: SAILORS Teaching Example
model-language: webppl
---

* toc
{:toc}

## Introduction

AI has become increasingly effective at processing and making sense of the huge amounts of messy, unstructured data that we have on the Internet. It is commonly believed that "**Big Data**"---datasets ranging from terabytes to petabytes in size---will revolutionalize AI, enabling machines to automatically acquire rich knowledge from data, and, ultimately, behave intelligently. 
While the sheer amount of data that the ditigal world has accumulated is undoubtedly powerful, it is often useful to take a step back and think about how humans---the most intelligent machines we have around---reason about and learn from data. Humans don't always require thousands of data points to learn concepts and draw conclusions. Often, we see a small amount of data and draw accurate conclusions about it. This is sometimes called an "**inductive leap**:" observing small pieces of evidence, making generalizations about them, and using these generalizations to draw larger conclusions and make meaningful predictions.

Let's make this idea of inductive reasoning a bit more concrete with a simple example.

### Number game

Suppose your friend shows you this sequence of numbers and tells you that they are "positive examples" of a category of numbers: `[8, 16, 64]`.

(Note: **positive examples** are examples of that category; **negative examples** are examples that are *not* of that category.)

* Do you think 4 belongs to this category?
* What about 7?
* Or 10?

Now, suppose someone shows you this sequence of numbers and tells you that they are "positive examples" of a different category of numbers: `[10, 30, 40]`

* Do you think 4 belongs to this category?
* What about 7?
* Or 10?

How did you come up with these answers? What are your hypotheses about which number category your friend was thinking of?

Let's zoom in on the first sequence: `[8, 16, 64]`. In order to predict what other numbers might belong to this category, it can be useful to think about the process that *generated* these three numbers. Since there are several different number categories that we consider, there are also several different ways in which these numbers were generated from those categories.

Here is one way the numbers `[8, 16, 64]` could have been generated:

1. Your friend was thinking of the concept (or number category): "*powers of 2*".
2. Your friend randomly samples some numbers that are powers of 2.

This is what this **generative process** looks like in code:

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
// sample 3 items from an array, without replacement
var draw3 = function(array) {
  return drawN(array, 3);
}
///

// first we make an array of all the powers of 2
// since there are infinitely many powers of 2, 
// for simplicty we restrict it to less than 50
var powers_of_2 = [1, 2, 4, 8, 16, 32];

// then we sample 3 of those powers of 2:
var get_examples = function() {
  return draw3(powers_of_2)
};

get_examples();
~~~~

You can press the "run" button above multiple times to get different samples from this category. This is called a **simulation**---simulating the way your friend chooses different numbers.

You can also see graphs of this simulation. (Let's put our 3 numbers in order, so the graph looks nicer)

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
// sample 3 items from an array, without replacement
var draw3 = function(array) {
  return drawN(array, 3);
}

// first we make an array of all the powers of 2 less than 50:
var powers_of_2 = [1, 2, 4, 8, 16, 32];

// then we sample 3 of those powers of 2:
var get_examples = function() {return draw3(powers_of_2)};

get_examples();
///
print(Enumerate(get_examples));
~~~~  

Of course, there are other ways the numbers `[8, 16, 64]` could have been generated:

1. Your friend was thinking of the concept (or number category): "*multiples of 2*".
2. Your friend randomly samples some numbers that are multiples of 2.

What is the more likely category given the numbers `[8, 16, 64]`---*powers of 2*, or *multiples of 2*?

## Probabilistic programs

It can be quite tedious to calculate the probabilites by hand. Instead, we can use "**Probabilistic Programs**" (programs that have some randomness and uncertainty in them) to let the computer figure out the answer for us. That is part of the power of programming!

### Generative Model

To write a program that does this work for us, we first need to give it some information we have about your friend and how she chooses numbers. We can do this in the form of a "**generative model**": in this case, a description of how your friend generates numbers from concepts.

Let's say your friend could be thinking of either "*multiples of 2*" or "*powers of 2*". For simplicity, let's also assume that she'll only give you numbers less than or equal to 10 (otherwise there would be too many number combinations to consider!).

First, she randomly samples one of those concepts:

~~~~
var pick_concept = function() {
// uniformDraw will choose "powers_of_2" and "multiples_of_2" with equal probability.
  var concept = uniformDraw(["powers_of_2", "multiples_of_2"]);
  return concept;
}
// this graph confirms that the two concepts are each 50% likely to to be chosen.
print(Enumerate(pick_concept));
~~~~  

Then, whatever concept your friend chooses, she gives you 3 example numbers from that concept:

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
// sample 3 items from an array, without replacement
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
  var concept = pick_concept(); // this line picks a concept randomly
  var examples = draw3(concept_examples[concept]); // given the randomly chosen concept, choose 3 examples from it
  return examples;
}

print(Enumerate(generate_examples));
~~~~  

#### Questions
* What is the probability that `[0, 4, 8]` is chosen? (hint: put your cursor over the bar for `[0, 4, 8]`)
* What is the probability that `[1, 4, 8]` is chosen?
* What is the probability that `[2, 4, 8]` is chosen?
* Which number sequence is most likely to be chosen? (bonus: Why?)

### Conditioning

In the last section, the program generated number sequences without knowing which concept was chosen. Given that we know your friend chose the concept "*multiples_of_two*", what is the probability of generating the sequence of `[2, 4, 8]`? (This is called the **conditional probability** of `[2, 4, 8]` given the concept "*multiples_of_2*."

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
// sample 3 items from an array, without replacement
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
  condition( concept == "multiples_of_2" ); // multiples: conditoned on the concept being "multiples_of_2"
  //condition( concept == "powers_of_2" ); // powers: conditoned on the concept being "powers_of_2"
  return examples;
}

print(Enumerate(generate_examples_with_condition));
~~~~ 

#### Questions
* What is the probability that `[2, 4, 8]` is chosen, given the concept "*multiples_of_2*"? 
* What is the probability that `[0, 4, 8]` is chosen, given the concept "*multiples_of_2*?"
* What is the probability that `[2, 4, 8]` is chosen, given the concept "*powers_of_2*"? (hint: comment out the line for "multiples" and uncomment the line for "powers," then rerun the code and see what happens.
* What is the probability that `[0, 4, 8]` is chosen, given the concept "*powers_of_2*?"

### Infer category given number sequence

Now that we are bit more familiar with how your friend generates these numbers, we can tackle the actual question---given the numbers you observe (e.g. `[2, 4, 8]`), which concept is your friend most likely to be thinking of? 

Note that we are again asking for a **conditional probability**, except now it goes the other direction. Instead of asking for the probability that `[2, 4, 8]` is generated given that your friend chose the concept "*multiples_of_2*", we now ask for the probability that your friend chose the concept "*multiples_of_2*" given that `[2, 4, 8]` was generated.

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
// sample 3 items from an array, without replacement
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
*     * conditioning on the observations
*/
var infer_concept = function(observations) {
  return function() {
    var concept = pick_concept();
    var examples = draw3(concept_examples[concept]);
    condition( array_equals( examples, observations ) ); // conditioning on the observations
    return concept;
  }
}

print(Enumerate(infer_concept([2, 4, 8])));
~~~~

#### Questions
* Which concept is most likely given the number sequence `[2, 4, 8]`? Does this match your intuitions?
* Which concept is most likely given the number sequnece `[1, 4, 8]`? (hint: replace `[2, 4, 8]` in the last line with `[1, 4, 8]`)
* Can you explain the model's inference for the observed sequence `[2, 4, 8]`? (hint: How many numbers are powers of 2 and how many numbers are multiples of 2?)
* What is the conditional probability of `[2, 4, 8]` given each of the concepts?

An important insight that emerges is something called the **size principle**: your friend could have been thinking of "*multiples of 2*" or "*powers of 2*", since both concepts are consistent with `[2, 4, 8]`. But if your friend had been thinking of "*multiples of 2*", there are a lot more sequences she *could have chosen* than if she was thinking of "*powers of 2*". In other words, the probability that she stumbles upon a sequence that happens to be full of powers of 2 when she was thinking of "*multiples of 2*" is a *lot* lower than the probability that she picks a "*powers of 2*" sequence that happens to be full of multiples of 2.

## Prior probabilities of hypotheses

In the previous examples, we assumed that the concepts "*multiples_of_2*" and "*powers_of_2*" are equally likely to be chosen. Is that always a valid assumption?

Suppose the friend who generated the number sequence `[2, 4, 8]` is a precocious 3rd-grader. You know that she has not yet learned the concept of "powers." You believe that it is quite unlikely that she would be thinking about a number category as complex as "*powers of 2*." How does this change your belief about what number category she is thinking of, given that she generated the number sequence `[2, 4, 8]`?

Instead of the picking the two concepts with equal probability, this function is now much less likely to pick "*powers_of_2*":

~~~~
var pick_3rd_grade_concept = function() {
  var prob_powers = 0.1; // prior probability that your friend knows about "powers" and is thinking of the concept
  var concept = flip(prob_powers)? "powers_of_2": "multiples_of_2"; // picks "powers_of_2" with probability prob_powers
  return concept;
}
// this graph confirms that the "powers_of_2" is now only 10% likely to to be chosen.
print(Enumerate(pick_3rd_grade_concept));
~~~~  

Given these new **prior probabilities** over how likely your friend is to pick the two concepts, this program now makes different inferences about which concept generated the number sequence `[2, 4, 8]`.

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
// sample 3 items from an array, without replacement
var draw3 = function(array) {
  return drawN(array, 3);
}
///

var pick_3rd_grade_concept = function() {
  var prob_powers = 0.1; // prior probability that your friend knows about "powers" and is thinking of the concept
  var concept = flip(prob_powers)? "powers_of_2": "multiples_of_2"; // picks "powers_of_2" with probability prob_powers
  return concept;
}


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
    var concept = pick_3rd_grade_concept();
    var examples = draw3(concept_examples[concept]);
    condition( array_equals( examples, observations ) );
    return concept;
  }
}

print(Enumerate(infer_concept([2, 4, 8])));
~~~~

#### Questions
* What happens to the results when you change "prob_powers" to different values?
* What does this tell you about how the probability of choosing a concept and the probability of generating certain numbers from the concept interact to produce the *inferred* probability of a concept given the observed number sequence?
  * **Prior probability**: The probability of your friend choosing a concept
  * **Likelihood**: The probability of generating certain numbers given the chosen concept
  * **Posterior probability**: The probability that your friend chose a certain cocnept, given that she generated the number sequence you observed.

We can give the model certain prior knowledge and beliefs about the word (e.g., the belief that your friend is unlikey to know about the concept "*powers of 2.*"). However, given strong contradictory evidence, the model can still end up believing that your friend was thinking about the concept "*powers of 2.*" 

## Sampling methods

Notice that there are many different ways your friend could have generated these examples. She could have sampled the numbers randomly from the number category, as we assumed in the programs we've looked at so far. Or, she could have sampled them in an intentional manner, to purposefully help you understand the number category and not confuse it with a different number category. Or, she could have sampled them in a different intentional manner---to intentionally deceive you into thinking that it is a different number category.

In addition to different sampling strategies, your friend could also give you diferent types of examples. We have been assuming that your friend only shows you *positive examples* of the category, for example the numbers `[2, 4, 8]` for the category "*powers of 2*." What if your friend can also show you *negative examples*, such as stating that `[0, 6, 10]` are *not* in the category? Or a mix of positive and negative examples, such as stating that `[2, 4]` are in the category, but not `6`? Which of these examples will make you more likely to believe that the category is "*powers of 2*"?

Moreover, your friend could also label a random set of numbers between 0 and 10. For example, if the chosen set is `[1, 5, 8]` and the category is "*powers of 2*," the labels would be `[yes, no, yes]`. Is this method more or less likely to help you guess the right category?

## Why is this important?

An important aspect of intelligence is the ability to combine prior knowledge and new evidence to acquire a better understanding of the world. This better understanding allows us to make more accurate predictions about future events as well as more informed decisions in the face of uncertainty. Even with the simple number game example, we see that human intuitive reasoning is able to combine many different types of assumptions---the process of generating numbers, the space of hypotheses (which concepts your friend could be thinking about), the sampling strategies, etc---to figure out something  **unobserved** (e.g. the number concept that your friend is thinking about), given something **obvserved** (e.g. the number sequence she showed you). Importantly, the probabilistic programs we wrote down are able to mimic this type of human reasoning and produce results that match our intuitions. This suggests that this type of modeling approach---**(1) come up with generative model that links hypotheses to data (2) infer which hypothesis is more likely given observed data**---may be extremely useful for helping machines think and behave in more human-like and intelligent ways.

## Other examples of probabilistic reasoning

* Coin flips
  * You flip coin A six times and get this sequence of outcomes: `HTHTTH`. Is this a fair coin?
  * You flip coin B six times and get this sequence of outcomes: `HHHHTH`. Is this a fair coin?
  * You flip coin C six times and get this sequence of outcomes: `HHHHHH`. Is this a fair coin?
  
* Medical diagnoses
  * Patient A experiences the following symptoms: a cough, a fever, no chest pain. Does he have a cold, or lung cancer?
  * Patient B experiences the following symptoms: a cough, a fever, and chest pain. Does he have a cold, or lung cancer?
  * Patient C experiences the following symptoms: a cough, no fever, and chest pain. Does he have a cold, or lung cancer?
  
* Beliefs and preferences
  * There is a Chipotle 5 minutes from Stanford and an In-N-Out Burger 15 minutes away. Both restaurants are open until 11pm.
    * It is 10pm. Alice knows both restaurants are open and chose to go to In-N-Out. Which restaurant does she like more?
    * It is 10pm. Bob knows both restaurants are open and chose to go to Chipotle. Which restaurant does he like more?
    * It is 10pm. Cathy likes Chipotle better. She chose to go to Chipotle. Does she think In-N-Out is still open?
    * It is 10pm. Dan likes In-N-Out better. He chose to go to Chipotle. Does he think In-N-Out is still open?
    * It is 10pm. Emily likes Chipotle better. She chose to go to In-N-Out. Does she think Chiptole is still open?

## Resources and References

* dippl.org
* probmods.org
* http://web.mit.edu/cocosci/Papers/nips99preprint.pdf

