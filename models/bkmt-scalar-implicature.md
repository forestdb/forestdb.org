---
layout: model
title: Buenagua, Kephart, Malone, Tran Scalar implicature
model-language: webppl
---

Markdown- puzzle and modeling approach

The most basic idea these models are trying to communicate is, out of 3 apples how 
many are red by using an utterance and the prior world knowledge that 80% of all 
apples are red. To start us off in the joint inference model it is important to 
note that the speaker is not omniscient, which was part of the knowledge model 
that we previously covered, but that leads into the puzzle we are going to be 
discussing for our project. Now the listener's knowledge of speaker competency is 
taken away, so only the speaker knows the given access. This presents the new 
problem with uncertainty on both sides, because the listener does not know what 
kind of access the speaker is dealing with. Obviously, this means the listener is 
quite blind in this situation, making it even more difficult to pin down the true 
world state.
 
Fortunately the framework of this approach is not too difficult to understand, due 
to the fact that it’s not too far from the Vanilla RSA model, as a refresher that 
goes something like- a pragmatic listener infers a speaker's utterance by 
reasoning about a speaker attempting to inform a literal listener about the state 
of the world. The listener reasons about the true world state, given the utterance 
produced by a speaker who is reasoning how the listener would most likely 
interpret the utterance given some expected prior knowledge. That deals with the 
layers and recursive reasoning, and then we are just switching from squares and 
circles to apples with a base rate like previously mentioned, and the utterances 
just change to none, some or all. We can then move to the next stepping stone and 
recall the knowledge model that we previously went over in class, which just made 
it so the speaker was no longer omniscient and had a specified access, or how many 
apples they were able to see. What we are presenting is just one step past that 
where the listener does not possess the knowledge of what access the speaker is 
dealing with. Otherwise from an empirical view all the utterances, priors and 
important stuff is the same. One difference to note is the addition of the null 
utterance, which is pretty intuitive and essential because there are situations 
such as no access where any utterance could effectively be an accidental lie. If 
the speaker has no information to work with it's better to say nothing rather than 
possibly leading the listener astray. Other than that, the framework is just a few 
steps out from the vanilla RSA model and looks a lot more confusing due to how the 
math is implemented even though the effects are the same. So, the big idea to 
solve this puzzle with all these unknown variables is the listeners' joint 
inference over those unknowns.


Markdown - similarities and differences

As with all RSA models, the same basic framework makes up the body: a statePrior, 
an utterancePrior, a literalMeanings, some layers of speaker and listener 
functions (literalListener, speaker, and pragmaticListener). The pragmaticListener 
uses Bayesian inference to consider their prior beliefs of the state of the world 
and the probability of the speaker choosing a particular utterance (to communicate 
that world state). This Bayesian inference is used to calculate whether the next 
apple in the draw will be red or not. 

The joint inference model is an expansion of the knowledge model for scalar 
implicature, so there are a lot of knowledge model components the joint inference 
model builds upon. Like in the knowledge model, the joint inference’s statePrior 
factors in the base rate of redness and the total number of apples to establish 
the model’s prior over states. The joint inference model sets its speaker 
optimality feature to alpha = 1, simulating an optimal speaker. The utterance 
meanings (for “all,” “some,” and “none”) remain the same with the exception of the 
“null” utterance. The same base rate of redness is used in the joint inference 
model: 0.8. And a utility function (minimizing the cost and maximizing the 
informativity) and a belief function (in which the speaker makes an inference 
based off of the access) are implemented in the joint inference model as they were 
in the knowledge model.

However, there are fundamental differences between the two models. As opposed to 
the knowledge model, which only makes a single inference on the world states, the 
joint inference model makes a three-part inference: on the world states, on the 
access (what the speaker can see), and on the observed (how many red apples there 
are from what the speaker can see). Because the pragmatic listener in the joint 
inference model isn’t sure whether the speaker is fully competent or not, the 
listener has to put in more work to infer what they believe the speaker sees. 
These extra inferences the pragmatic listener makes are introduced via the 
observePrior and the accessPrior –– two functions that were lacking in the 
knowledge model. On top of implementing new access and observed inferences, the 
joint inference model also differs from the knowledge model in that it utilizes a 
hypergeometric distribution, which factors in the possibility that the speaker may 
be speaking on partial access rather than full access and communicates the 
speaker’s beliefs based on that access.


Markdown - model description

~~~~
// red apple base rate
var total_apples = 3
var base_rate_red = 0.8

// state = how many apples of 'total_apples' are red?
var statePrior = function() {
  binomial({p: base_rate_red, n: total_apples})
}
~~~~

First, we need to set up the current state of the world. In this world, there are 
three apples (given by total_apples). 

Apples in this world have an 80% chance of being red (given by base_rate_red). 

The statePrior() returns a distribution over possible states, meaning a 
distribution over any number of apples being red. 

~~~~
///fold:
// red apple base rate
var total_apples = 3
var base_rate_red = 0.8

// state = how many apples of 'total_apples' are red?
var statePrior = function() {
  binomial({p: base_rate_red, n: total_apples})
}
///

// binomial-hypergeometric belief model

var factorial = function(x) {
  if (x < 0) {return "input to factorial function must be non-negative"}
  return x == 0 ? 1 : x * factorial(x-1)
}

var binom = function(a, b) {
  var numerator = factorial(a)
  var denominator = factorial(a-b) *  factorial(b)
  return numerator / denominator
}

// urn contains N balls of which K are black
// and N-K are white; we draw n balls at random
// without replacement; what's the probability
// of obtaining k black balls?
var hypergeometricPMF = function(k,N,K,n) {
  k > Math.min(n, K) ? 0 :
  k < n+K-N ? 0 :
  binom(K,k) * binom(N-K, n-k) / binom(N,n)
}
~~~~

Factorial() and binom() are helper functions for the hypergeometricPMF(). 

Factorial will return the factorial product of its input (ie. the factorial of 3 = 
3*2*1 = 6). 

Binom() sets up a fraction framework for the factorials of two inputs. 

The hypergeometricPMF function utilizes the binom() function to implement Bayes’s 
rule for some type of numerical input. 

~~~~
///fold:
// red apple base rate
var total_apples = 3
var base_rate_red = 0.8

// state = how many apples of 'total_apples' are red?
var statePrior = function() {
  binomial({p: base_rate_red, n: total_apples})
}

// binomial-hypergeometric belief model

var factorial = function(x) {
  if (x < 0) {return "input to factorial function must be non-negative"}
  return x == 0 ? 1 : x * factorial(x-1)
}

var binom = function(a, b) {
  var numerator = factorial(a)
  var denominator = factorial(a-b) *  factorial(b)
  return numerator / denominator
}

// urn contains N balls of which K are black
// and N-K are white; we draw n balls at random
// without replacement; what's the probability
// of obtaining k black balls?
var hypergeometricPMF = function(k,N,K,n) {
  k > Math.min(n, K) ? 0 :
  k < n+K-N ? 0 :
  binom(K,k) * binom(N-K, n-k) / binom(N,n)
}
///

var hypergeometricSample = function(N,K,n) {
  var support = _.range(N+1) // possible values 0, ..., N
  var PMF = map(function(k) {hypergeometricPMF(k,N,K,n)}, support)
  categorical({vs: support, ps: PMF })
}
~~~~

The hypergeometricSample will calculate Bayes’s rule for information from our 
state of the world. In our case, the hypergeometricSample will consider the 
following inputs: N = total number of apples (total_apples), K = amount of the 
total apples that are red, n = amount of apples we have access to that are red, 
and k = obtaining another red apple when we gain access to another apple. Imagine 
in our world there are three apples (N) but two apples are covered by a cloth. Out 
of the total three apples, two are red (K). We can see one apple, and that one 
apple is red (k). If we take off the cloth one apple at a time, will the next 
apple we gain access to be red (n)? The hypergeometricSample generates this 
probability for gaining access to each apple in the set. 

~~~~
///fold:
// red apple base rate
var total_apples = 3
var base_rate_red = 0.8

// state = how many apples of 'total_apples' are red?
var statePrior = function() {
  binomial({p: base_rate_red, n: total_apples})
}

// binomial-hypergeometric belief model

var factorial = function(x) {
  if (x < 0) {return "input to factorial function must be non-negative"}
  return x == 0 ? 1 : x * factorial(x-1)
}

var binom = function(a, b) {
  var numerator = factorial(a)
  var denominator = factorial(a-b) *  factorial(b)
  return numerator / denominator
}

// urn contains N balls of which K are black
// and N-K are white; we draw n balls at random
// without replacement; what's the probability
// of obtaining k black balls?
var hypergeometricPMF = function(k,N,K,n) {
  k > Math.min(n, K) ? 0 :
  k < n+K-N ? 0 :
  binom(K,k) * binom(N-K, n-k) / binom(N,n)
}

var hypergeometricSample = function(N,K,n) {
  var support = _.range(N+1) // possible values 0, ..., N
  var PMF = map(function(k) {hypergeometricPMF(k,N,K,n)}, support)
  categorical({vs: support, ps: PMF })
}
///

var belief = cache(function(access, observed){
  Infer({model: function() {
    var state = statePrior()
    var hyperg_sample = hypergeometricSample(total_apples,
                         state,
                         access)
    condition(hyperg_sample == observed)
    return state
  }})
})
~~~~

The speaker belief() function generates how many of the total_apples the speaker 
believes to be red. The function takes two inputs: the amount of apples the 
speaker can see (access), and how many of the apples that the speaker can see are 
red (observed). The function will select a state from the statePrior() and compare 
the state to the probability generated from the hypergeometricSample, about how 
probable another apple in the set is red. If the generated probability matches the 
current state, then the state will be passed. Infer() is run over this process to 
generate a probability distribution over possible states.  

~~~~
///fold:
// red apple base rate
var total_apples = 3
var base_rate_red = 0.8

// state = how many apples of 'total_apples' are red?
var statePrior = function() {
  binomial({p: base_rate_red, n: total_apples})
}

// binomial-hypergeometric belief model

var factorial = function(x) {
  if (x < 0) {return "input to factorial function must be non-negative"}
  return x == 0 ? 1 : x * factorial(x-1)
}

var binom = function(a, b) {
  var numerator = factorial(a)
  var denominator = factorial(a-b) *  factorial(b)
  return numerator / denominator
}

// urn contains N balls of which K are black
// and N-K are white; we draw n balls at random
// without replacement; what's the probability
// of obtaining k black balls?
var hypergeometricPMF = function(k,N,K,n) {
  k > Math.min(n, K) ? 0 :
  k < n+K-N ? 0 :
  binom(K,k) * binom(N-K, n-k) / binom(N,n)
}

var hypergeometricSample = function(N,K,n) {
  var support = _.range(N+1) // possible values 0, ..., N
  var PMF = map(function(k) {hypergeometricPMF(k,N,K,n)}, support)
  categorical({vs: support, ps: PMF })
}

var belief = cache(function(access, observed){
  Infer({model: function() {
    var state = statePrior()
    var hyperg_sample = hypergeometricSample(total_apples,
                         state,
                         access)
    condition(hyperg_sample == observed)
    return state
  }})
})
///

// utterance prior
var utterancePrior = function() {
  categorical({vs: ['all','some','none','null'],
               ps: [1,1,1,0.0000001]})
}

// meaning function to interpret utterances
var literalMeanings = {
  all:  function(state) { state == total_apples },
  some: function(state) { state > 0 },
  none: function(state) { state == 0 },
  null: function(state) { state >= 0 }
}
~~~~

To continue building our state of the world, we establish more prior knowledge. 
The utterancePrior() establishes the possible utterances (all, some, none, and 
“null”) and runs categorical with how likely the speaker is to use that utterance. 
In our world, the speaker is equally likely to use none, some, or all, but is 
extremely unlikely to not say an utterance at all (null). 

The literalMeanings() function will define the states in which the utterance is 
true. “All'' is only true when three apples are red, “some” is only true when at 
least one apple is red, “none” is only true when no apples are red, and “null” can 
be applied to any state. This is because at any time the speaker can choose to not 
say an utterance, which cannot be incorrect, though it is highly uninformative. 

~~~~
///fold:
// red apple base rate
var total_apples = 3
var base_rate_red = 0.8

// state = how many apples of 'total_apples' are red?
var statePrior = function() {
  binomial({p: base_rate_red, n: total_apples})
}

// binomial-hypergeometric belief model

var factorial = function(x) {
  if (x < 0) {return "input to factorial function must be non-negative"}
  return x == 0 ? 1 : x * factorial(x-1)
}

var binom = function(a, b) {
  var numerator = factorial(a)
  var denominator = factorial(a-b) *  factorial(b)
  return numerator / denominator
}

// urn contains N balls of which K are black
// and N-K are white; we draw n balls at random
// without replacement; what's the probability
// of obtaining k black balls?
var hypergeometricPMF = function(k,N,K,n) {
  k > Math.min(n, K) ? 0 :
  k < n+K-N ? 0 :
  binom(K,k) * binom(N-K, n-k) / binom(N,n)
}

var hypergeometricSample = function(N,K,n) {
  var support = _.range(N+1) // possible values 0, ..., N
  var PMF = map(function(k) {hypergeometricPMF(k,N,K,n)}, support)
  categorical({vs: support, ps: PMF })
}

var belief = cache(function(access, observed){
  Infer({model: function() {
    var state = statePrior()
    var hyperg_sample = hypergeometricSample(total_apples,
                         state,
                         access)
    condition(hyperg_sample == observed)
    return state
  }})
})

// utterance prior
var utterancePrior = function() {
  categorical({vs: ['all','some','none','null'],
               ps: [1,1,1,0.0000001]})
}

// meaning function to interpret utterances
var literalMeanings = {
  all:  function(state) { state == total_apples },
  some: function(state) { state > 0 },
  none: function(state) { state == 0 },
  null: function(state) { state >= 0 }
}
///

// literal listener
var literalListener = cache(function(utt) {
  return Infer({model: function(){
    var state = statePrior()
    var meaning = literalMeanings[utt]
    condition(meaning(state))
    return state
  }})
})
~~~~

The literalListener() function will take in the utterance and evaluate the meaning 
of this utterance. They will also draw a state from the statePrior() and evaluate 
if this state is true given the meaning of the utterance. If the state is true, 
then that state will be returned. Infer() is run over this function to return a 
distribution over possible states given the meaning of the utterance. 

~~~~
///fold:
// red apple base rate
var total_apples = 3
var base_rate_red = 0.8

// state = how many apples of 'total_apples' are red?
var statePrior = function() {
  binomial({p: base_rate_red, n: total_apples})
}

// binomial-hypergeometric belief model

var factorial = function(x) {
  if (x < 0) {return "input to factorial function must be non-negative"}
  return x == 0 ? 1 : x * factorial(x-1)
}

var binom = function(a, b) {
  var numerator = factorial(a)
  var denominator = factorial(a-b) *  factorial(b)
  return numerator / denominator
}

// urn contains N balls of which K are black
// and N-K are white; we draw n balls at random
// without replacement; what's the probability
// of obtaining k black balls?
var hypergeometricPMF = function(k,N,K,n) {
  k > Math.min(n, K) ? 0 :
  k < n+K-N ? 0 :
  binom(K,k) * binom(N-K, n-k) / binom(N,n)
}

var hypergeometricSample = function(N,K,n) {
  var support = _.range(N+1) // possible values 0, ..., N
  var PMF = map(function(k) {hypergeometricPMF(k,N,K,n)}, support)
  categorical({vs: support, ps: PMF })
}

var belief = cache(function(access, observed){
  Infer({model: function() {
    var state = statePrior()
    var hyperg_sample = hypergeometricSample(total_apples,
                         state,
                         access)
    condition(hyperg_sample == observed)
    return state
  }})
})

// utterance prior
var utterancePrior = function() {
  categorical({vs: ['all','some','none','null'],
               ps: [1,1,1,0.0000001]})
}

// meaning function to interpret utterances
var literalMeanings = {
  all:  function(state) { state == total_apples },
  some: function(state) { state > 0 },
  none: function(state) { state == 0 },
  null: function(state) { state >= 0 }
}

// literal listener
var literalListener = cache(function(utt) {
  return Infer({model: function(){
    var state = statePrior()
    var meaning = literalMeanings[utt]
    condition(meaning(state))
    return state
  }})
})
///

// set speaker optimality
var alpha = 1

// expected utilities
var get_EUs = function(access, observed, utterance){
  var EUs = sum(map(function(s) {
      var eu_at_state = Math.exp(belief(access, observed).score(s)) *
          literalListener(utterance).score(s)
      _.isNaN(eu_at_state) ? 0 : eu_at_state // convention here: 0*-Inf=0
    }, _.range(total_apples + 1)))
  return EUs
}

// pragmatic speaker
var speaker = cache(function(access, observed) {
  return Infer({model: function(){
    var utterance = utterancePrior()
    var EUs = get_EUs(access, observed, utterance)
    factor(alpha * EUs)
    return utterance
  }})
})
~~~~

Then we begin to build the pieces for the speaker function. First, we determine 
how optimal the speaker is (alpha = 1, the speaker is optimal). 

Then, we have to evaluate the utility of each possible utterance. The function 
get_EUs() takes the speaker’s access to the amount of apples, and observed amount 
of apples that are red, as well as the utterance. A helper function eu_at_state 
determines how useful a state is given the speaker’s belief of how many apples are 
red in conjunction with the literalListener()’s interpretation of the utterance at 
hand. From this calculation, we are returned the utility of each utterance. 

Finally, we reach the speaker function, which will determine which utterance the 
speaker will use to convey to a pragmatic speaker about how many apples are red. 
The speaker will use the information known from how many apples they can see 
(access) and how many of those apples are red (observed). An utterance will be 
generated from the utterancePrior(), the utility of this utterance will be 
calculated (get_Eus), then the resulting utility will be factored by the 
optimality of the speaker (alpha). Infer() will run over this process to return 
the utterance that generates the highest utility, which is ultimately the 
utterance that the speaker will use.  

~~~~
///fold:
// red apple base rate
var total_apples = 3
var base_rate_red = 0.8

// state = how many apples of 'total_apples' are red?
var statePrior = function() {
  binomial({p: base_rate_red, n: total_apples})
}

// binomial-hypergeometric belief model

var factorial = function(x) {
  if (x < 0) {return "input to factorial function must be non-negative"}
  return x == 0 ? 1 : x * factorial(x-1)
}

var binom = function(a, b) {
  var numerator = factorial(a)
  var denominator = factorial(a-b) *  factorial(b)
  return numerator / denominator
}

// urn contains N balls of which K are black
// and N-K are white; we draw n balls at random
// without replacement; what's the probability
// of obtaining k black balls?
var hypergeometricPMF = function(k,N,K,n) {
  k > Math.min(n, K) ? 0 :
  k < n+K-N ? 0 :
  binom(K,k) * binom(N-K, n-k) / binom(N,n)
}

var hypergeometricSample = function(N,K,n) {
  var support = _.range(N+1) // possible values 0, ..., N
  var PMF = map(function(k) {hypergeometricPMF(k,N,K,n)}, support)
  categorical({vs: support, ps: PMF })
}

var belief = cache(function(access, observed){
  Infer({model: function() {
    var state = statePrior()
    var hyperg_sample = hypergeometricSample(total_apples,
                         state,
                         access)
    condition(hyperg_sample == observed)
    return state
  }})
})

// utterance prior
var utterancePrior = function() {
  categorical({vs: ['all','some','none','null'],
               ps: [1,1,1,0.0000001]})
}

// meaning function to interpret utterances
var literalMeanings = {
  all:  function(state) { state == total_apples },
  some: function(state) { state > 0 },
  none: function(state) { state == 0 },
  null: function(state) { state >= 0 }
}

// literal listener
var literalListener = cache(function(utt) {
  return Infer({model: function(){
    var state = statePrior()
    var meaning = literalMeanings[utt]
    condition(meaning(state))
    return state
  }})
})

// set speaker optimality
var alpha = 1

// expected utilities
var get_EUs = function(access, observed, utterance){
  var EUs = sum(map(function(s) {
      var eu_at_state = Math.exp(belief(access, observed).score(s)) *
          literalListener(utterance).score(s)
      _.isNaN(eu_at_state) ? 0 : eu_at_state // convention here: 0*-Inf=0
    }, _.range(total_apples + 1)))
  return EUs
}

// pragmatic speaker
var speaker = cache(function(access, observed) {
  return Infer({model: function(){
    var utterance = utterancePrior()
    var EUs = get_EUs(access, observed, utterance)
    factor(alpha * EUs)
    return utterance
  }})
})
///

var observePrior = function(){
  uniformDraw(_.range(total_apples + 1))
}

var accessPrior = function(){
  uniformDraw(_.range(total_apples + 1))
}
~~~~

In our world, the pragmatic listener doesn’t know how many apples the speaker can 
see (access), nor how many apples the speaker can see that are red (observed). 

In order to generate a guess of how many red apples the speaker can see, 
observePrior() will run a uniformDraw() over the total amount of apples. 

In order to generate a guess of how many apples the speaker can see at all, the 
accessPrior() will run a uniformDraw() over the total amount of apples. 

~~~~
///fold:
// red apple base rate
var total_apples = 3
var base_rate_red = 0.8

// state = how many apples of 'total_apples' are red?
var statePrior = function() {
  binomial({p: base_rate_red, n: total_apples})
}

// binomial-hypergeometric belief model

var factorial = function(x) {
  if (x < 0) {return "input to factorial function must be non-negative"}
  return x == 0 ? 1 : x * factorial(x-1)
}

var binom = function(a, b) {
  var numerator = factorial(a)
  var denominator = factorial(a-b) *  factorial(b)
  return numerator / denominator
}

// urn contains N balls of which K are black
// and N-K are white; we draw n balls at random
// without replacement; what's the probability
// of obtaining k black balls?
var hypergeometricPMF = function(k,N,K,n) {
  k > Math.min(n, K) ? 0 :
  k < n+K-N ? 0 :
  binom(K,k) * binom(N-K, n-k) / binom(N,n)
}

var hypergeometricSample = function(N,K,n) {
  var support = _.range(N+1) // possible values 0, ..., N
  var PMF = map(function(k) {hypergeometricPMF(k,N,K,n)}, support)
  categorical({vs: support, ps: PMF })
}

var belief = cache(function(access, observed){
  Infer({model: function() {
    var state = statePrior()
    var hyperg_sample = hypergeometricSample(total_apples,
                         state,
                         access)
    condition(hyperg_sample == observed)
    return state
  }})
})

// utterance prior
var utterancePrior = function() {
  categorical({vs: ['all','some','none','null'],
               ps: [1,1,1,0.0000001]})
}

// meaning function to interpret utterances
var literalMeanings = {
  all:  function(state) { state == total_apples },
  some: function(state) { state > 0 },
  none: function(state) { state == 0 },
  null: function(state) { state >= 0 }
}

// literal listener
var literalListener = cache(function(utt) {
  return Infer({model: function(){
    var state = statePrior()
    var meaning = literalMeanings[utt]
    condition(meaning(state))
    return state
  }})
})

// set speaker optimality
var alpha = 1

// expected utilities
var get_EUs = function(access, observed, utterance){
  var EUs = sum(map(function(s) {
      var eu_at_state = Math.exp(belief(access, observed).score(s)) *
          literalListener(utterance).score(s)
      _.isNaN(eu_at_state) ? 0 : eu_at_state // convention here: 0*-Inf=0
    }, _.range(total_apples + 1)))
  return EUs
}

// pragmatic speaker
var speaker = cache(function(access, observed) {
  return Infer({model: function(){
    var utterance = utterancePrior()
    var EUs = get_EUs(access, observed, utterance)
    factor(alpha * EUs)
    return utterance
  }})
})

var observePrior = function(){
  uniformDraw(_.range(total_apples + 1))
}

var accessPrior = function(){
  uniformDraw(_.range(total_apples + 1))
}
///

var pragmaticListener = cache(function(utt) {
  return Infer({method: "enumerate",
//                 strategy: "breadthFirst",
                model: function(){
    var state = statePrior()
    var access = accessPrior()
    var observed = observePrior()
    factor(Math.log(hypergeometricPMF(observed, total_apples,
                                      state, access)))
    observe(speaker(access, observed), utt)
    return {state, access, observed}
  }})
});
~~~~

The pragmatic listener will hear the utterance and return a joint inference about 
the amount of apples they think are red, the amount of apples they think the 
speaker has access to and how many apples the listener thinks the speaker believes 
to be red. The pragmaticListener() function does this by drawing a state from the 
statePrior(), access level from the accessPrior() and observation level from the 
observePrior(). Factor considers the output of the hypergeometricPMF with the 
pragmatic listener’s generated priors and knowledge of the state of the world. 
Then, the output to the speaker function is compared to the utterance used under 
observe. From this final filter we are left with the pragmatic listener’s joint 
inference. 

~~~~
///fold:
// red apple base rate
var total_apples = 3
var base_rate_red = 0.8

// state = how many apples of 'total_apples' are red?
var statePrior = function() {
  binomial({p: base_rate_red, n: total_apples})
}

// binomial-hypergeometric belief model

var factorial = function(x) {
  if (x < 0) {return "input to factorial function must be non-negative"}
  return x == 0 ? 1 : x * factorial(x-1)
}

var binom = function(a, b) {
  var numerator = factorial(a)
  var denominator = factorial(a-b) *  factorial(b)
  return numerator / denominator
}

// urn contains N balls of which K are black
// and N-K are white; we draw n balls at random
// without replacement; what's the probability
// of obtaining k black balls?
var hypergeometricPMF = function(k,N,K,n) {
  k > Math.min(n, K) ? 0 :
  k < n+K-N ? 0 :
  binom(K,k) * binom(N-K, n-k) / binom(N,n)
}

var hypergeometricSample = function(N,K,n) {
  var support = _.range(N+1) // possible values 0, ..., N
  var PMF = map(function(k) {hypergeometricPMF(k,N,K,n)}, support)
  categorical({vs: support, ps: PMF })
}

var belief = cache(function(access, observed){
  Infer({model: function() {
    var state = statePrior()
    var hyperg_sample = hypergeometricSample(total_apples,
                         state,
                         access)
    condition(hyperg_sample == observed)
    return state
  }})
})

// utterance prior
var utterancePrior = function() {
  categorical({vs: ['all','some','none','null'],
               ps: [1,1,1,0.0000001]})
}

// meaning function to interpret utterances
var literalMeanings = {
  all:  function(state) { state == total_apples },
  some: function(state) { state > 0 },
  none: function(state) { state == 0 },
  null: function(state) { state >= 0 }
}

// literal listener
var literalListener = cache(function(utt) {
  return Infer({model: function(){
    var state = statePrior()
    var meaning = literalMeanings[utt]
    condition(meaning(state))
    return state
  }})
})

// set speaker optimality
var alpha = 1

// expected utilities
var get_EUs = function(access, observed, utterance){
  var EUs = sum(map(function(s) {
      var eu_at_state = Math.exp(belief(access, observed).score(s)) *
          literalListener(utterance).score(s)
      _.isNaN(eu_at_state) ? 0 : eu_at_state // convention here: 0*-Inf=0
    }, _.range(total_apples + 1)))
  return EUs
}

// pragmatic speaker
var speaker = cache(function(access, observed) {
  return Infer({model: function(){
    var utterance = utterancePrior()
    var EUs = get_EUs(access, observed, utterance)
    factor(alpha * EUs)
    return utterance
  }})
})

var observePrior = function(){
  uniformDraw(_.range(total_apples + 1))
}

var accessPrior = function(){
  uniformDraw(_.range(total_apples + 1))
}

var pragmaticListener = cache(function(utt) {
  return Infer({method: "enumerate",
//                 strategy: "breadthFirst",
                model: function(){
    var state = statePrior()
    var access = accessPrior()
    var observed = observePrior()
    factor(Math.log(hypergeometricPMF(observed, total_apples,
                                      state, access)))
    observe(speaker(access, observed), utt)
    return {state, access, observed}
  }})
});
///

var pl = pragmaticListener("all")
display("Marginal beliefs about the true world state:")
viz(marginalize(pl, 'state'))
display("Marginal beliefs about how many apples the speaker observed in total:")
viz(marginalize(pl, 'access'))
display("Marginal beliefs about how many apples the speaker observed to be red:")
viz(marginalize(pl, 'observed'))

var pl = pragmaticListener("null")
display("Marginal beliefs about the true world state:")
viz(marginalize(pl, 'state'))
display("Marginal beliefs about how many apples the speaker observed in total:")
viz(marginalize(pl, 'access'))
display("Marginal beliefs about how many apples the speaker observed to be red:")
viz(marginalize(pl, 'observed'))

var pl = pragmaticListener("some")
display("Marginal beliefs about the true world state:")
viz(marginalize(pl, 'state'))
display("Marginal beliefs about how many apples the speaker observed in total:")
viz(marginalize(pl, 'access'))
display("Marginal beliefs about how many apples the speaker observed to be red:")
viz(marginalize(pl, 'observed'))


var pl = pragmaticListener("none")
display("Marginal beliefs about the true world state:")
viz(marginalize(pl, 'state'))
display("Marginal beliefs about how many apples the speaker observed in total:")
viz(marginalize(pl, 'access'))
display("Marginal beliefs about how many apples the speaker observed to be red:")
viz(marginalize(pl, 'observed'))
~~~~

Problang.org provided a framework for us to visualize the marginal distributions 
over the pragmatic listener's inference on the state of the world, speaker's 
access, and speaker's observations, based on the utterance heard. 

In the case of the “all” utterance, the pragmatic listener only believes that the 
true state of the world is all three apples are red, that the speaker had access 
to all 3 apples, and the speaker observed all three apples to be red. This is due 
to a number of reasons. First, the definition of the “all” utterance in the 
literalMeanings function is that all three apples must be red. Since this model 
does not permit the speaker to lie, the speaker must have seen three red apples in 
order to make the all utterance. Even if the speaker saw two red apples and did 
not know about the third apple, the speaker could have chosen the utterance “some” 
because this does not eliminate the possibility of the third apple being red, and 
it is less costly than saying “all” in that situation. That is due to the fact 
that in a partial access situation saying “all” puts them in a position where they 
could potentially lie. 

This result is similar to what we see for the “all” utterance in the knowledge 
model. For a full access speaker in the knowledge model, the pragmatic listener 
only believes that there are three red apples in this world. However, this is 
slightly different when we have the utterance “all” for a partial access speaker 
in the knowledge model. Here, the pragmatic listener is willing to assume the 
speaker believes the third apple is red, given the base rate of redness of apples 
in this world, so they are likely to believe the third apple is red. However, 
there is a small probability given to the state where two apples are red when the 
speaker said “all”, as if to mean “all the apples I can see are red”. In the 
knowledge model, this is acceptable because the listener knows how many apples the 
speaker has access to. This cannot occur in the joint inference model because the 
pragmatic listener has no way of knowing how many apples the speaker can see, or 
how many they observed to be red, so the pragmatic listener is unwilling to 
believe that “all” could mean anything other than a full access speaker who saw 
three red apples.

In a situation where a pragmatic listener hears the “null” utterance many 
assumptions have to be made based on the prior world knowledge in the model. It is 
easiest to explain this bottom-up because each step helps to explain the next, so 
we are starting with the explanation of visualizing the observed red apples. It is 
also helpful to remember the reason why the “null” utterance was introduced in the 
first place; since the joint inference model restricts knowledge the speaker needs 
a way to circumvent the issue of accidentally providing false information. That 
being said, what situation presents this issue? A situation in which the speaker 
has observed 0 red apples of course. We can see this is exactly what the pragmatic 
listener believes when we visualize observed with the “null” utterance.
 
When we visualize access (or total observed) the pragmatic listener already knows 
the speaker could not have seen all 3 apples due to the fact that any other 
utterance would have been used to more accurately describe the state. A small 
amount of probability is placed on 2, which makes sense because while it is 
possible that the speaker saw two of the apples, it is highly unlikely that 
neither of them were red due to the base rate. Some more probability is placed on 
the 1 state, but this is still a low amount, again due to such a high base rate. 
Finally, the 0 state is by far the most likely reflecting the base rate at .80 
probability.
 
Looking at the beliefs of the true world state can seem a bit confusing at first 
glance, as one might expect to see more probability put on the lower states like 
the other graphs, but after going through the aforementioned analysis it becomes 
much more intuitive. All this graph really does is reflect the prior knowledge of 
the world in this model. Due to the base rate of red apples being 80% it is very 
unlikely the true world state to have 0 or 1 red apples. The pragmatic listener 
knows this and assumes it is very likely that 2 or 3 of the apples will be red, 
and again the addition of these two states nearly reflects the base rate, coming 
in at just over 80%.
 
Comparing this to the knowledge model we can see why there is no “null” utterance 
included. Unlike in the joint inference model there is no utility for such an 
utterance in the knowledge model. Due to the fact that the pragmatic listener 
knows how competent the speaker is this addition would just reiterate what the 
listener already knows. Once the access is set to 0 the pragmatic listener 
immediately understands nothing the speaker says makes any difference because they 
have no knowledge of the true world state. In this situation the access 
effectively acts as a “null” utterance, and the listener makes an inference solely 
on her prior beliefs about the world just as she would if she had heard the “null” 
utterance in the joint inference model.

Turning now to the "some" utterance, we can see that there is no probability given 
for 0 apples in any of the respective marginal distribution graphs for a pragmatic 
listener's beliefs about the true world state, the number of apples that the 
speaker is able to access, and the number of apples that the speaker has observed 
to be red. This is due to the fact that the state of there being no apples that 
are red is not covered by how the "some" utterance is defined in the 
literalMeaning function. With that out of the way, a discussion on what happens 
when the pragmatic listener hears the "some" utterance in this model, can now 
proceed in earnest.

Looking at the graph of the marginal distribution of the pragmatic listener's 
belief about the true world state, we see that although the probability that all 3 
apples are red is slightly higher than the probability that 2 of the 3 apples are 
red, these probabilities are each a lot higher than the probability that only 1 of 
the 3 apples are red. These results have something to do with the base rate in 
this model being set at 0.8. Also, notice how the respective probabilities of all 
3 apples being red and of 2 of the 3 apples being red both add up to more than the 
aforementioned value for the base rate. That is because in this model, the 
listener (regardless of whether they are a literal listener or a pragmatic 
listener) does not actually know definitively how many of the apples are red.

Looking now at the graph of the marginal distribution of the pragmatic listener's 
belief about the number of apples that the speaker is able to access, we see that 
the highest probability is given for the speaker having access to only 2 apples 
and also that the respective probabilities of the speaker having access to only 1 
apple and of the speaker having access to only 2 apples are each higher than the 
probability of the speaker having access to all 3 apples. There are three reasons 
for these results. The first reason is that as mentioned earlier, the "all" 
utterance is defined in the literalMeaning function as the state in which all 3 
apples are red and that the pragmatic listener would have inferred that the 
speaker has access to all 3 apples if the latter have said the "all" utterance, 
hence the pragmatic listener's inferring that the speaker has access to all 3 
apples if the latter says the "some" utterance having the lowest probability. The 
second reason is that as mentioned in the previous paragraph, the base rate is set 
at 0.8, which explains the relatively high probability of the pragmatic listener 
inferring that the speaker has access to all 3 apples. The third and final reason 
is that, again, the listener in this model has no definitive idea about the number 
of apples to which the speaker has access, allowing them to default to the base 
rate when making an inference about the speaker's access.

Looking now at the graph of the marginal distribution of the pragmatic listener's 
belief about the number of apples that the speaker has observed to be red, we see 
that the respective probabilities of the speaker observing that only 1 of 3 apples 
is red and of the speaker observing that 2 of the 3 apples are red are each a lot 
higher than the probability of the speaker observing that all 3 apples are red, 
that the lattermost probability is a non-zero number with a value that lies about 
halfway between 0.05 and 0.10, and that the probability that the speaker observes 
only 1 apple to be red is slightly higher than the probability that the speaker 
observes 2 out of the 3 apples to be red. These results are such for the following 
four reasons. Firstly, even though the state of the speaker having observed all 3 
apples to be red is covered by the "all" utterance, such state is also covered by 
the "some" utterance. Secondly, the speaker does not actually have access to all 3 
apples, let alone observe all 3 apples to be red. Thirdly, the base rate is such 
that there is a non-zero probability that the pragmatic listener would still infer 
that the speaker has observed all 3 apples to be red despite not knowing whether 
this is indeed the case. Lastly, the base rate is also such that there is a 
possibility that only 1 of the apples to which the speaker has access is red, 
which results in the probability that the pragmatic listener infers that the 
speaker has observed only 1 apple to be red being the highest among all 
probabilities visualized in the aforementioned graph.

Let us now compare the results of the p1 function (which utilizes the 
pragmaticListener function) in the joint inference model with the results of the 
pragmaticListener function in the knowledge model. Note once again that in 
contrast with the joint inference model, the listener in the knowledge model knows 
the number of apples to which the speaker has access, allowing us to visualize 
both situations in which the speaker has full access and situations in which the 
speaker has only partial access.

Taking a look at the graph of the results of the pragmaticListener function for a 
full-access speaker, we can see a number of findings. The first finding is that 
when the "some" utterance is heard, the probability of the pragmatic listener 
assuming that the speaker saw that 2 of the 3 apples are red is higher than either 
the probability of the pragmatic listener assuming that the speaker saw that only 
1 of the 3 apples is red or the probability of the pragmatic listener assuming 
that the speaker saw that all 3 apples are red. The second finding is that the 
probability of the pragmatic listener assuming that the speaker saw that only 1 of 
the 3 apples is red is slightly higher than the probability of the pragmatic 
listener assuming that the speaker saw that all 3 apples are red. The third and 
last finding is that there is no probability given in the graph, for the pragmatic 
listener assuming that the speaker saw that none of the 3 apples are red. There 
are two reasons for these findings. The first reason is that the "some" utterance 
covers scenarios in which the pragmatic listener assumes that the speaker saw that 
at least 1 apple is red. The second reason is that there is already an utterance 
for scenarios in which the pragmatic listener assumes that the speaker saw that 
all 3 apples are red, the "all" utterance, which provides for greater information 
flow than if the "some" utterance is used. To put it another way, if the speaker 
saw that all 3 apples are in fact red, the pragmatic listener assumes that the 
speaker would have said the "all" utterance instead of the "some" utterance.

Taking a look now at the graph of the results of the pragmaticListener function 
for a partial-access speaker, we can notice a number of things. The first thing we 
notice is that there is a non-zero probability given for the pragmatic listener 
assuming that the speaker saw that none of the apples are red when the former 
hears the latter use the "some" utterance. The second thing we notice is that the 
probability of the pragmatic listener assuming that the speaker saw that 2 of the 
3 apples are red is the highest among the probabilities given in this graph. The 
third and final thing we notice is that the respective probabilities of the 
pragmatic listener assuming that the speaker saw that all 3 apples are red and of 
the pragmatic listener assuming that the speaker saw that only 1 of the 3 apples 
are red are more or less the same. There are several reasons for these findings. 
The first reason is that there is a chance, however small, that the third apple 
(the apple that the speaker does not have access to) is not actually red. The 
second reason is that the "some" utterance may cover scenarios in which the 
speaker is assumed by the pragmatic listener to have seen all 3 apples being red. 
The third reason is that as mentioned in the previous paragraph, the "all" 
utterance covers the scenarios laid out in the previous sentence, which explains 
why the probability of the pragmatic listener assuming that the speaker saw all 3 
apples being red is not higher than it is in this graph. The fourth and final 
reason is that the base rate in the knowledge model, just like in the joint 
inference model, is set at 0.8, which means that there is a good chance (though 
not that high) that the third apple is, in fact, red even if the speaker uses the 
"some" utterance to indicate that they saw that all 3 apples are red.

With the “none” utterance, the pragmatic listener infers that the speaker believes 
there to be no red apples. This could be due to the fact that the speaker has 
partial access and observes that there are no red apples among the ones they can 
see. In this case, the speaker would have access to either one or two apples and 
observe that they are, for instance, green instead of red. Considering that the 
base rate of redness is 0.8 (indicating a significant likelihood for red apples), 
however, it would be costly to gamble the possibility that there are no red apples 
given that the speaker does not have full access. Therefore, it is practically 
impossible that the speaker would utter “none” if they only have partial access. 

The more likely case is that the speaker has access to all apples and observes 
that there are zero red apples. The speaker would then use the utterance “none” 
since they are sure that there are no red apples in this scenario. This is why 
when we visualize the “none” utterance there is a 100% probability that there are 
0 red apples given world states, there is a 100% probability that there are 0 red 
apples the speaker observes, and there is a 100% probability that the speaker has 
access to all 3 apples.

The knowledge model shows that a full-access speaker uttering the utterance “none” 
would have seen all 3 apples, witnessed there to be 0 red apples, and therefore, 
utter that there are 0 red apples in the world. Since there is no probability that 
the speaker would have partial access given the “none” utterance, the results 
displayed in the joint inference model are virtually the same as the ones we would 
see in the knowledge model.


Markdown - relevant results

To start off our discussion of the results of the joint inference model and 
comparing them with the results of the knowledge model, let us now turn to the 
statePrior function. Provided below is the relevant code pertaining to the 
statePrior function in the joint inference model.

~~~~
// total number of apples (known by speaker and listener)
var total_apples = 3

// red apple base rate
var base_rate_red = 0.8

// state = how many apples of 'total_apples' are red?
var statePrior = function() {
  binomial({p: base_rate_red, n: total_apples})
}

Infer(statePrior)
~~~~

Taking a look at the graph of the results of running Infer over the statePrior 
function, we see that the probability of each state happening increases as the 
number of red apples in that state increases, going from a near-zero probability 
of having none of the 3 apples be red to a probability of slightly over 0.50 that 
all 3 apples are red; that the highest probabilities are given for both the state 
of having 2 of the 3 apples be red and the state of having all 3 apples be red, 
with the probability that all 3 apples are red being significantly higher than the 
probability that 2 of the 3 apples are red); and that the respective probabilities 
for those states are each substantially higher than the combined probability of 
having either none or only 1 of the 3 apples be red. These results reflect the 
value of the base rate shown in the above code, which is also the reason why the 
respective probabilities of only 1 of the 3 apples being red and of none of the 3 
apples being red are both low.

Let us go now to the results of the statePrior function in the knowledge model. 
Provided below is the relevant code from the knowledge model. Note that we added a 
bit (i.e., only one line) of our own code, but that is just for the purpose of 
helping us visualize the results of the statePrior function in this model.

~~~~
///fold:
// tally up the state
var numTrue = function(state) {
  var fun = function(x) {
    x ? 1 : 0
  }
  return sum(map(fun,state))
}
///

// red apple base rate
var baserate = 0.8

// state builder
var statePrior = function() {
  var s1 = flip(baserate)
  var s2 = flip(baserate)
  var s3 = flip(baserate)
  return [s1,s2,s3]
}

viz.hist(Infer(statePrior))
~~~~

Looking at the graph of the results of running viz.hist over Infer(statePrior), we 
see several things. The first is that the highest probability in the graph is 
given to the outcome in which all 3 apples are red, indicated by [true,true,true] 
in the graph, at slightly over 0.50. The second is that the second highest 
probability in the graph is given to the outcomes in which 2 of the 3 apples are 
red (indicated by [true,true,false]; [true,false,true]; and [false,true,true] in 
the graph) at about slightly over halfway between 0.10 and 0.15 each; note that 
these 3 outcomes have, more or less, the same probability. The third is that the 
probability of any of the outcomes in which only 1 of the 3 apples is red 
(indicated by [true,false,false]; [false,true,false]; and [false,false,true] in 
the graph) happening is low at approximately 0.03 each; like with the previous 
sentence, the 3 outcomes mentioned in this sentence have the same value for their 
respective probabilities. The fourth is that the probability of the outcome in 
which none of the 3 apples are red, indicated by [false,false,false] in the graph, 
is very low (the lowest probability given in the graph, in fact) at close to 0. 
The last, but certainly not the least, is that as shown in the graph, the 
probability of the outcome in which all 3 apples are indeed red is a lot higher 
than the individual probabilities of the outcomes in which less than 3 apples are 
red. However, if we combine the probabilities of the outcomes in which only 1 of 
the 3 apples are red together into an overall probability of only 1 of the 3 
apples being red and also combine the probabilities of the outcomes in which 2 of 
the 3 apples are red together into an overall probability of 2 of the 3 apples 
being red, we get a graph that looks very similar to the graph of the results of 
running Infer over the statePrior function in the joint inference model. There is 
one overarching reason for all of these findings: the base rate in the knowledge 
model, just like in the joint inference model, is set at 0.8.

Another set of results that we would like to discuss is what happens when we run 
viz over the belief function with the speaker having access to only 2 apples and 
observing that 1 of those 2 apples is red, in the joint inference model. The 
reason for running such an operation is to help us visualize the respective 
probabilities of the next apple (i.e., the apple that the speaker does not have 
access to) being red and of that same apple being a color other than red. To help 
put this discussion into context, below is the code pertaining to the belief 
function in the joint inference model.

~~~~
// code for hypergeometric 
///fold:
var factorial = function(x) {
  if (x < 0) {return "input to factorial function must be non-negative"}
  return x == 0 ? 1 : x * factorial(x-1)
}

var binom = function(a, b) {
  var numerator = factorial(a)
  var denominator = factorial(a-b) *  factorial(b)
  return numerator / denominator
}

// urn contains N balls of which K are black
// and N-K are white; we draw n balls at random
// without replacement; what's the probability
// of obtaining k black balls?
var hypergeometricPMF = function(k,N,K,n) {
  k > Math.min(n, K) ? 0 :
  k < n+K-N ? 0 :       
  binom(K,k) * binom(N-K, n-k) / binom(N,n)
}

var hypergeometricSample = function(N,K,n) {
  var support = _.range(N+1) // possible values 0, ..., N
  var PMF = map(function(k) {hypergeometricPMF(k,N,K,n)}, support)
  categorical({vs: support, ps: PMF })    
}
///

// total number of apples (known by speaker and listener)
var total_apples = 3

// red apple base rate
var base_rate_red = 0.8

// state = how many apples of 'total_apples' are red?
var statePrior = function() {
  binomial({p: base_rate_red, n: total_apples})
}

var belief = cache(function(access, observed){
  Infer({model: function() {
    var state = statePrior()
    var hyperg_sample = hypergeometricSample(total_apples,
                         state,
                         access)
    condition(hyperg_sample == observed)
    return state
  }}) 
})

viz(belief(2,1))
~~~~

In the graph of the results of running viz over the aforementioned belief 
function, an interesting finding that we see is that the probability that the next 
apple is red has the same value as the base rate in the joint inference model. The 
reason for this finding is that as the base rate in this model is set at 0.8 and 
the speaker (along with the listener, though this is not mentioned in the code 
provided just above) does not actually know whether this as-of-yet inaccessible 
apple is red or not in this model, the speaker ends up defaulting to their prior 
beliefs about the true world state of red apples as a subset of the total 
population of apples and the speaker therefore infers that the probability that 
the aforementioned apple is red (and thereafter, that 2 of the 3 apples in total 
are red) is 0.8 and, consequently, that the probability that this same apple is 
not red (and thereafter, that only 1 of the 3 apples in total is red) is 0.2.

Let us turn now to the belief function in the knowledge model. Shown below is the 
code in this model pertaining to the belief function.

~~~~
///fold:
// tally up the state
var numTrue = function(state) {
  var fun = function(x) {
    x ? 1 : 0
  }
  return sum(map(fun,state))
}
///

// red apple base rate
var baserate = 0.8

// state builder
var statePrior = function() {
  var s1 = flip(baserate)
  var s2 = flip(baserate)
  var s3 = flip(baserate)
  return [s1,s2,s3]
}

// speaker belief function
var belief = function(actualState, access) {
  var fun = function(access_val,state_val) {
    return access_val ? state_val : uniformDraw(statePrior())
  }
  return map2(fun, access, actualState);
}

print("1000 runs of the speaker's belief function:")

viz.auto(repeat(1000,function() {
  numTrue(belief([true,true,true],[true,true,false]))
}))
~~~~

Running viz.auto over the belief function (with repeat and numTrue in between, of 
course, among other things) gets us a graph that looks very similar to the graph 
that we get from running viz over the belief function in the joint inference 
model. The reason for this being the case is that the base rate in the knowledge 
model is also set at 0.8. In fact, the results that we have discussed so far in 
this writeup are such due to the base rate in both the joint inference model and 
the knowledge model being at 0.8. Just like with the graph of the results of 
running viz over the belief function in the joint inference model, the speaker 
defaults to their prior beliefs about the true world state of red apples as a 
subset of the total population of apples and hence believes that the probability 
of the as-of-yet inaccessible apple being red (and thereafter, of all 3 apples 
being red) is approximately 0.8 and, consequently, that the probability of that 
same apple being some other color than red (and thereafter, of only 2 of the 3 
apples in total being red) is approximately 0.2. 

On a final note, please see the section titled "Markdown - model description" for 
the discussion of the results of the pragmaticListener function in both the joint 
inference model and the knowledge model.