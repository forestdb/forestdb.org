---
layout: model
title: Metaphor with Varying Priors and Goals
model-language: webppl
model-category: Language and Pragmatics
model-status: code
---

*By Josh, Kayla, Lauren*

The puzzle we are addressing is: How do listeners make sense of metaphorical or figurative
utterances, such as “John is a whale,” when those utterances are not literally true?

For example, when someone says “John is a whale,” we intuitively understand they are likely
commenting on John’s size, not that he’s actually a marine mammal. This model simulates how
pragmatic reasoning allows listeners to arrive at that kind of inference by modeling the beliefs,
goals, and choices of both the speaker and the listener.

In the very first part of the code, we define the categoriesPrior. John can either be a whale or a
person, but there’s only a 1% probability that he’s actually a whale. This reflects our
common-sense belief that people are typically people.


~~~~
// John could either be a whale or a person.
var categories = ["whale", "person"]

// It is extremely unlikely that John is actually a whale.
var categoriesPrior = function() {
  categorical([0.01, 0.99], categories)
}
~~~~

The utterancePrior function treats both “whale” and “person” as equally likely choices. This means
the model assumes they are equally easy to say. Therefore, there’s no cost difference driving the
choice.


~~~~
// The speaker could either say "John is a whale" or "John is a person."
var utterances = ["whale", "person"]
// The utterances are equally costly.
var utterancePrior = function() {
categorical([1,1], utterances)
}
~~~~

Now we move to featureSets, which lay out eight possible combinations of features John might
have. These features are: large, graceful, and majestic. Each one of these features is
represented in binary numbers.

The featureSetPrior function then assigns probabilities to each combination based on whether
John is a person or a whale. This reflects empirical world knowledge. For example, the first
feature set has the highest probability when John is a whale. That is because whales are more
often all three of those things than people are.

So this function shows how the model uses real-world associations between category and traits to
help inform interpretation.


~~~~
var featureSets = [
{large : 1, graceful : 1, majestic : 1},
{large : 1, graceful : 1, majestic : 0},
{large : 1, graceful : 0, majestic : 1},
{large : 1, graceful : 0, majestic : 0},
{large : 0, graceful : 1, majestic : 1},
{large : 0, graceful : 1, majestic : 0},
{large : 0, graceful : 0, majestic : 1},
{large : 0, graceful : 0, majestic : 0}
]
// information about feature priors (probabilistic world knowledge)
// obtained by an experimental study (see paper)
var featureSetPrior = function(category) {
category === "whale" ? categorical([0.30592786494628, 0.138078454222818,
 0.179114768847673, 0.13098781834847,
 0.0947267162507846, 0.0531420411185539,
 0.0601520520596695, 0.0378702842057509],
 featureSets) :
category === "person" ? categorical([0.11687632453038, 0.105787535267869,
 0.11568145784997, 0.130847056136141,
 0.15288225956497, 0.128098151176801,
 0.114694702836614, 0.135132512637255],
 featureSets) :
true
}
~~~~

Now we model what the speaker might be trying to communicate, which is the role of the
goalPrior. Here, we assume the speaker could be trying to highlight any one of the three traits:
large, graceful, or majestic. Additionally, each of these traits has equal probability. So the model
starts out with no bias about which trait the speaker is focused on.

We then define a goalState function, which checks if a given set of features satisfies a goal. For
instance, if the speaker wants to communicate that John is majestic, and John is in fact majestic,
then goalState returns 1—this goal is met.

To keep things consistent, the model uses literalInterpretation to make sure the utterance aligns
with the literal category. So if the speaker says “whale,” it assumes John must be a whale.

One important parameter here is alpha, set to 3. This controls how strongly the speaker prefers
informative utterances. A higher alpha means the speaker is more pragmatic, making it more
likely to choose an utterance that effectively communicates the intended trait.

While the speaker selects an utterance to highlight a single trait, the listener does not know which
trait that is. Instead, the listener infers the likelihood of all traits jointly, given the utterance.

~~~~
// Speaker's possible goals are to communicate feature 1, 2, or 3
var goals = ["large", "graceful", "majestic"]
// Prior probability of speaker's goal is set to uniform but can
// change with context/QUD.
var goalPrior = function() {
categorical([1,1,1], goals)
}
// Speaker optimality parameter
var alpha = 3
// Check if interpreted category is identical to utterance
var literalInterpretation = function(utterance, category) {
utterance === category
}
// Check if goal is satisfied
var goalState = function(goal, featureSet) {
goal === "large" ? featureSet.large :
goal === "graceful" ? featureSet.graceful :
goal === "majestic" ? featureSet.majestic :
true
}
~~~~

In the case of the literal listener, it hears an utterance like “whale,” starts by considering both
categories equally, and samples a feature set based on the category.

Then, it begins to filter responses: it only keeps interpretations where the category matches the
utterance. Hence, “whale” means John must be a whale.

Finally, it checks whether the sampled features satisfy the communicative goal using goalState.

~~~~
// Define a literal listener
var literalListener = function(utterance, goal) {
Infer({model: function() {
 var category = uniformDraw(categories)
 var featureSet = featureSetPrior(category)
 condition(literalInterpretation(utterance, category))
 return goalState(goal, featureSet)
}})
}
~~~~

The speaker, on the other hand, knows both John’s actual features and the specific trait they want
to communicate.

The speaker then evaluates both possible utterances—“person” and “whale”—by simulating how
well each one would help the literal listener infer the intended trait.

This is where factor comes in. It redistributes the speaker’s choices: the better an utterance leads
the listener to the correct inference, the more likely the speaker is to use it. This is the model’s
way of favoring informative, goal-relevant speech.


~~~~
// Speaker model
var speaker = function(large, graceful, majestic, goal) {
Infer({model: function() {
 var utterance = utterancePrior()
 factor(alpha *
 literalListener(utterance,goal).score(goalState(goal, {large : large, graceful
: graceful, majestic : majestic})))
 return utterance
}})
}
~~~~

The pragmatic listener, implemented as pragmaticListener(utterance), models how listeners infer
intended meaning behind figurative language. It starts by sampling a possible category for John
using categoriesPrior() and then draws a set of traits from featureSetPrior(category).

It also samples a communicative goal using goalPrior(), representing which trait the speaker
might be trying to highlight, such as largeness, gracefulness, or majesty.

The crucial step is the observe function, which filters for cases where a speaker with those traits
and that goal would have actually chosen the utterance that was heard. The Infer function uses
this filtering to generate updated beliefs about John's likely category and traits.

Although the statement “John is a whale” is literally false, the model shows how a listener can
infer that John likely has a whale-like quality, such as being very large. This reflects how people
use pragmatic reasoning to go beyond literal meaning.

~~~~
// Define a pragmatic listener
var pragmaticListener = function(utterance) {
Infer({model: function() {
 var category = categoriesPrior()
 var featureSet = featureSetPrior(category)
 var large = featureSet.large
 var graceful = featureSet.graceful
 var majestic = featureSet.majestic
 var goal = goalPrior()
 observe(speaker(large, graceful, majestic, goal), utterance)
 return {category, large, graceful, majestic}
}})
}
~~~~

Both the hyperbole and metaphor models involve the use of a speaker, pragmatic listener, factor,
and alpha. The speaker in both models chooses what to say that would get the listener to get to the
point. The pragmatic listener hears the utterances and infers what is being said, along with the
reasoning behind it. In terms of factor and observe, the speaker utilizes factor to score each
utterance, with more informative ones being scored higher. Observe is used by the listener as it
runs the speaker model, depending on which utterance was actually said.

~~~~
// Speaker model
var speaker = function(large, graceful, majestic, goal) {
  Infer({model: function() {
    var utterance = utterancePrior()
    factor(alpha *
           literalListener(utterance,goal).score(goalState(goal, {large : large, graceful : graceful, majestic : majestic})))
    return utterance
  }})
}

// Define a pragmatic listener
var pragmaticListener = function(utterance) {
  Infer({model: function() {
    var category = categoriesPrior()
    var featureSet = featureSetPrior(category)
    var large = featureSet.large
    var graceful = featureSet.graceful
    var majestic = featureSet.majestic
    var goal = goalPrior()
    observe(speaker(large, graceful, majestic, goal), utterance)
    return {category, large, graceful, majestic}
  }})
}
~~~~

QUD (question under discussion) and goal both communicate the importance in the model and
attempt to address it by shaping what the speaker chooses to say.


~~~~
// Speaker's possible goals are to communicate feature 1, 2, or 3
var goals = ["large", "graceful", "majestic"]

// Prior probability of speaker's goal is set to uniform but can
// change with context/QUD.
var goalPrior = function() {
  categorical([1,1,1], goals)
}
~~~~

The differences between the two models are the utterances and how they work with the model,
the process of understanding. For the hyperbole model, it utilizes approximation of values since
precise values are more costly. On the other hand, the metaphor model uses binary entities to
mark which trait is more likely tied to the utterance, John or whale. As the literal listener only
accepts literal matches, the literal meaning is not probable due to prior probabilities. The
pragmatic listener then utilizes the speaker model and the prior probabilities to infer that the
utterances are not to be taken literally, but intertwined in intention. Simply put, the pragmatic
listener processes the intended meaning rather than what is actually being stated.

~~~~
// John could either be a whale or a person.
var categories = ["whale", "person"]

// It is extremely unlikely that John is actually a whale.
var categoriesPrior = function() {
  categorical([0.01, 0.99], categories)
}

// The speaker could either say "John is a whale" or "John is a person."
var utterances = ["whale", "person"]

// The utterances are equally costly.
var utterancePrior = function() {
  categorical([1,1], utterances)
}

// The features of John being considered are "large", "graceful",
// "majestic." Features are binary.
var featureSets = [
  {large : 1, graceful : 1, majestic : 1},
  {large : 1, graceful : 1, majestic : 0},
  {large : 1, graceful : 0, majestic : 1},
  {large : 1, graceful : 0, majestic : 0},
  {large : 0, graceful : 1, majestic : 1},
  {large : 0, graceful : 1, majestic : 0},
  {large : 0, graceful : 0, majestic : 1},
  {large : 0, graceful : 0, majestic : 0}
]
~~~~

Below is the result produced when putting in “viz.hist(pragmaticListener("whale"))” to visualize
the probability of what utterance is more likely to be referred to when the pragmatic listener
hears whale.

~~~~
// John could either be a whale or a person.
var categories = ["whale", "person"]

// It is extremely unlikely that John is actually a whale.
var categoriesPrior = function() {
  categorical([0.01, 0.99], categories)
}

// The speaker could either say "John is a whale" or "John is a person."
var utterances = ["whale", "person"]

// The utterances are equally costly.
var utterancePrior = function() {
  categorical([1,1], utterances)
}

// The features of John being considered are "large", "graceful",
// "majestic." Features are binary.
var featureSets = [
  {large : 1, graceful : 1, majestic : 1},
  {large : 1, graceful : 1, majestic : 0},
  {large : 1, graceful : 0, majestic : 1},
  {large : 1, graceful : 0, majestic : 0},
  {large : 0, graceful : 1, majestic : 1},
  {large : 0, graceful : 1, majestic : 0},
  {large : 0, graceful : 0, majestic : 1},
  {large : 0, graceful : 0, majestic : 0}
]

// information about feature priors (probabilistic world knowledge)
// obtained by an experimental study (see paper)
var featureSetPrior = function(category) {
  category === "whale" ? categorical([0.30592786494628, 0.138078454222818,
                                      0.179114768847673, 0.13098781834847,
                                      0.0947267162507846, 0.0531420411185539,
                                      0.0601520520596695, 0.0378702842057509],
                                     featureSets) :
  category === "person" ? categorical([0.11687632453038, 0.105787535267869,
                                       0.11568145784997, 0.130847056136141,
                                       0.15288225956497, 0.128098151176801,
                                       0.114694702836614, 0.135132512637255],
                                      featureSets) :
  true
}

// Speaker's possible goals are to communicate feature 1, 2, or 3
var goals = ["large", "graceful", "majestic"]

// Prior probability of speaker's goal is set to uniform but can
// change with context/QUD.
var goalPrior = function() {
  categorical([1,1,1], goals)
}

// Speaker optimality parameter
var alpha = 3

// Check if interpreted category is identical to utterance
var literalInterpretation = function(utterance, category) {
  utterance === category
}

// Check if goal is satisfied
var goalState = function(goal, featureSet) {
  goal === "large" ? featureSet.large :
  goal === "graceful" ? featureSet.graceful :
  goal === "majestic" ? featureSet.majestic :
  true
}

//  Define a literal listener
var literalListener = function(utterance, goal) {
  Infer({model: function() {
    var category = uniformDraw(categories)
    var featureSet = featureSetPrior(category)
    condition(literalInterpretation(utterance, category))
    return goalState(goal, featureSet)
  }})
}         

// Speaker model
var speaker = function(large, graceful, majestic, goal) {
  Infer({model: function() {
    var utterance = utterancePrior()
    factor(alpha *
           literalListener(utterance,goal).score(goalState(goal, {large : large, graceful : graceful, majestic : majestic})))
    return utterance
  }})
}

// Define a pragmatic listener
var pragmaticListener = function(utterance) {
  Infer({model: function() {
    var category = categoriesPrior()
    var featureSet = featureSetPrior(category)
    var large = featureSet.large
    var graceful = featureSet.graceful
    var majestic = featureSet.majestic
    var goal = goalPrior()
    observe(speaker(large, graceful, majestic, goal), utterance)
    return {category, large, graceful, majestic}
  }})
}

viz.hist(pragmaticListener("whale"))
~~~~

~~~~
// John could either be a whale or a person.
var categories = ["whale", "person"]

// It is extremely unlikely that John is actually a whale.
var categoriesPrior = function() {
  categorical([0.01, 0.99], categories)
}

// The speaker could either say "John is a whale" or "John is a person."
var utterances = ["whale", "person"]

// The utterances are equally costly.
var utterancePrior = function() {
  categorical([1,1], utterances)
}

// The features of John being considered are "large", "graceful",
// "majestic." Features are binary.
var featureSets = [
  {large : 1, graceful : 1, majestic : 1},
  {large : 1, graceful : 1, majestic : 0},
  {large : 1, graceful : 0, majestic : 1},
  {large : 1, graceful : 0, majestic : 0},
  {large : 0, graceful : 1, majestic : 1},
  {large : 0, graceful : 1, majestic : 0},
  {large : 0, graceful : 0, majestic : 1},
  {large : 0, graceful : 0, majestic : 0}
]

// information about feature priors (probabilistic world knowledge)
// obtained by an experimental study (see paper)
var featureSetPrior = function(category) {
  category === "whale" ? categorical([0.30592786494628, 0.138078454222818,
                                      0.179114768847673, 0.13098781834847,
                                      0.0947267162507846, 0.0531420411185539,
                                      0.0601520520596695, 0.0378702842057509],
                                     featureSets) :
  category === "person" ? categorical([0.11687632453038, 0.105787535267869,
                                       0.11568145784997, 0.130847056136141,
                                       0.15288225956497, 0.128098151176801,
                                       0.114694702836614, 0.135132512637255],
                                      featureSets) :
  true
}

// Speaker's possible goals are to communicate feature 1, 2, or 3
var goals = ["large", "graceful", "majestic"]

// Prior probability of speaker's goal is set to uniform but can
// change with context/QUD.
var goalPrior = function() {
  categorical([1,1,1], goals)
}

// Speaker optimality parameter
var alpha = 3

// Check if interpreted category is identical to utterance
var literalInterpretation = function(utterance, category) {
  utterance === category
}

// Check if goal is satisfied
var goalState = function(goal, featureSet) {
  goal === "large" ? featureSet.large :
  goal === "graceful" ? featureSet.graceful :
  goal === "majestic" ? featureSet.majestic :
  true
}

//  Define a literal listener
var literalListener = function(utterance, goal) {
  Infer({model: function() {
    var category = uniformDraw(categories)
    var featureSet = featureSetPrior(category)
    condition(literalInterpretation(utterance, category))
    return goalState(goal, featureSet)
  }})
}         

// Speaker model
var speaker = function(large, graceful, majestic, goal) {
  Infer({model: function() {
    var utterance = utterancePrior()
    factor(alpha *
           literalListener(utterance,goal).score(goalState(goal, {large : large, graceful : graceful, majestic : majestic})))
    return utterance
  }})
}

// Define a pragmatic listener
var pragmaticListener = function(utterance) {
  Infer({model: function() {
    var category = categoriesPrior()
    var featureSet = featureSetPrior(category)
    var large = featureSet.large
    var graceful = featureSet.graceful
    var majestic = featureSet.majestic
    var goal = goalPrior()
    observe(speaker(large, graceful, majestic, goal), utterance)
    return {category, large, graceful, majestic}
  }})
}

viz.table(pragmaticListener("whale"))
viz.marginals(pragmaticListener("whale"))
~~~~

Above is the default prediction of the metaphor model
(viz.table(pragmaticListener("whale"))) and visualizations of its marginal
distributions (viz.marginals(pragmaticListener("whale"))). All displays show what
the pragmatic listener believes upon hearing the utterance “John is a whale,” specifically what
they believe about the species of the subject as well as which features the subject has (large,
graceful and/or majestic).

As is apparent in both the table and marginal distribution, the person category completely
dominates the whale category in terms of probability. This division of categories is a result of
the extreme imbalance in the categoriesPrior, which assigns a weight of 0.99 to person
and 0.01 to whale via a categorical draw. In reality, a speaker is much more likely to be
talking about a person than a whale at any given point, which is reflected in this
categoriesPrior.

Looking at the table, we also see patterns among the feature distributions within each category.
It is always most likely that the pragmatic listener believes the subject will have all three
features, then ⅔ features, then ⅓ features, then none of the features. The more features a
person or whale has, the more informative the utterance “John is a whale” becomes, and it is
assumed that the speaker is always trying to be as informative as possible. This assumption is
also supported by the marginal distributions of the features. For all features, the pragmatic
listener believes that it is more likely the feature is true of the subject than not true. It would not
make sense for “not true” to be the more likely belief, because that would make the utterance
more uninformative.

~~~~
// John could either be a whale or a person.
var categories = ["whale", "person"]

// It is equally likely that John is a whale or a person.
var categoriesPrior = function() {
  uniformDraw(categories)
}

// The speaker could either say "John is a whale" or "John is a person."
var utterances = ["whale", "person"]

// The utterances are equally costly.
var utterancePrior = function() {
  categorical([1,1], utterances)
}

// The features of John being considered are "large", "graceful",
// "majestic." Features are binary.
var featureSets = [
  {large : 1, graceful : 1, majestic : 1},
  {large : 1, graceful : 1, majestic : 0},
  {large : 1, graceful : 0, majestic : 1},
  {large : 1, graceful : 0, majestic : 0},
  {large : 0, graceful : 1, majestic : 1},
  {large : 0, graceful : 1, majestic : 0},
  {large : 0, graceful : 0, majestic : 1},
  {large : 0, graceful : 0, majestic : 0}
]

// information about feature priors (probabilistic world knowledge)
// obtained by an experimental study (see paper)
var featureSetPrior = function(category) {
  category === "whale" ? categorical([0.30592786494628, 0.138078454222818,
                                      0.179114768847673, 0.13098781834847,
                                      0.0947267162507846, 0.0531420411185539,
                                      0.0601520520596695, 0.0378702842057509],
                                     featureSets) :
  category === "person" ? categorical([0.11687632453038, 0.105787535267869,
                                       0.11568145784997, 0.130847056136141,
                                       0.15288225956497, 0.128098151176801,
                                       0.114694702836614, 0.135132512637255],
                                      featureSets) :
  true
}

// Speaker's possible goals are to communicate feature 1, 2, or 3
var goals = ["large", "graceful", "majestic"]

// Prior probability of speaker's goal is set to uniform but can
// change with context/QUD.
var goalPrior = function() {
  categorical([1,1,1], goals)
}

// Speaker optimality parameter
var alpha = 3

// Check if interpreted category is identical to utterance
var literalInterpretation = function(utterance, category) {
  utterance === category
}

// Check if goal is satisfied
var goalState = function(goal, featureSet) {
  goal === "large" ? featureSet.large :
  goal === "graceful" ? featureSet.graceful :
  goal === "majestic" ? featureSet.majestic :
  true
}

//  Define a literal listener
var literalListener = function(utterance, goal) {
  Infer({model: function() {
    var category = uniformDraw(categories)
    var featureSet = featureSetPrior(category)
    condition(literalInterpretation(utterance, category))
    return goalState(goal, featureSet)
  }})
}         

// Speaker model
var speaker = function(large, graceful, majestic, goal) {
  Infer({model: function() {
    var utterance = utterancePrior()
    factor(alpha *
           literalListener(utterance,goal).score(goalState(goal, {large : large, graceful : graceful, majestic : majestic})))
    return utterance
  }})
}

// Define a pragmatic listener
var pragmaticListener = function(utterance) {
  Infer({model: function() {
    var category = categoriesPrior()
    var featureSet = featureSetPrior(category)
    var large = featureSet.large
    var graceful = featureSet.graceful
    var majestic = featureSet.majestic
    var goal = goalPrior()
    observe(speaker(large, graceful, majestic, goal), utterance)
    return {category, large, graceful, majestic}
  }})
}

viz.table(pragmaticListener("whale"))
viz.marginals(pragmaticListener("whale"))
~~~~

Above is the prediction that the model produces when the categoriesPrior is changed from
a categorical([0.01, 0.99], categories) draw to uniformDraw (still running
viz.table(pragmaticListener("whale")) and
viz.marginals(pragmaticListener("whale"))). It is no longer the case that person
completely dominates whale; now, the pragmatic listener believes that whale is actually more
likely to be the subject of the utterance than person, as seen in the marginal distribution. The
interpretation of the utterance “John is a whale” is notably more literal, which may be the case in
situations where one is as likely to encounter a whale as a person (say, while whale-watching).
We also observe a slight increase in the probability of all features being true of the subject in
their respective marginal distributions.

Because the categoriesPrior is now neutralized, the results of this version can be
explained by other parameters of the model. In the original model, the optimality parameter
alpha = 3, which is considerably higher than the default alpha value of 1 from other models
we looked at in class. Neutralizing the alpha parameter by setting it to 1, or making it more
extreme by setting it to 10, yields different predictions for the pragmatic listener (see below).
With respect to alpha = 3, reducing the value of alpha distributes probability more evenly
across the items in the table and increasing alpha pulls probability towards the already
high-probability items.

~~~~ norun
// John could either be a whale or a person.
var categories = ["whale", "person"]

// It is equally likely that John is a whale or a person.
var categoriesPrior = function() {
  uniformDraw(categories)
}

// The speaker could either say "John is a whale" or "John is a person."
var utterances = ["whale", "person"]

// The utterances are equally costly.
var utterancePrior = function() {
  categorical([1,1], utterances)
}

// The features of John being considered are "large", "graceful",
// "majestic." Features are binary.
var featureSets = [
  {large : 1, graceful : 1, majestic : 1},
  {large : 1, graceful : 1, majestic : 0},
  {large : 1, graceful : 0, majestic : 1},
  {large : 1, graceful : 0, majestic : 0},
  {large : 0, graceful : 1, majestic : 1},
  {large : 0, graceful : 1, majestic : 0},
  {large : 0, graceful : 0, majestic : 1},
  {large : 0, graceful : 0, majestic : 0}
]

// information about feature priors (probabilistic world knowledge)
// obtained by an experimental study (see paper)
var featureSetPrior = function(category) {
  category === "whale" ? categorical([0.30592786494628, 0.138078454222818,
                                      0.179114768847673, 0.13098781834847,
                                      0.0947267162507846, 0.0531420411185539,
                                      0.0601520520596695, 0.0378702842057509],
                                     featureSets) :
  category === "person" ? categorical([0.11687632453038, 0.105787535267869,
                                       0.11568145784997, 0.130847056136141,
                                       0.15288225956497, 0.128098151176801,
                                       0.114694702836614, 0.135132512637255],
                                      featureSets) :
  true
}

// Speaker's possible goals are to communicate feature 1, 2, or 3
var goals = ["large", "graceful", "majestic"]

// Prior probability of speaker's goal is set to uniform but can
// change with context/QUD.
var goalPrior = function() {
  categorical([1,1,1], goals)
}

// Speaker optimality parameter (un-comment each string to see predictions)
// var alpha = 1
// var alpha = 10

// Check if interpreted category is identical to utterance
var literalInterpretation = function(utterance, category) {
  utterance === category
}

// Check if goal is satisfied
var goalState = function(goal, featureSet) {
  goal === "large" ? featureSet.large :
  goal === "graceful" ? featureSet.graceful :
  goal === "majestic" ? featureSet.majestic :
  true
}

//  Define a literal listener
var literalListener = function(utterance, goal) {
  Infer({model: function() {
    var category = uniformDraw(categories)
    var featureSet = featureSetPrior(category)
    condition(literalInterpretation(utterance, category))
    return goalState(goal, featureSet)
  }})
}         

// Speaker model
var speaker = function(large, graceful, majestic, goal) {
  Infer({model: function() {
    var utterance = utterancePrior()
    factor(alpha *
           literalListener(utterance,goal).score(goalState(goal, {large : large, graceful : graceful, majestic : majestic})))
    return utterance
  }})
}

// Define a pragmatic listener
var pragmaticListener = function(utterance) {
  Infer({model: function() {
    var category = categoriesPrior()
    var featureSet = featureSetPrior(category)
    var large = featureSet.large
    var graceful = featureSet.graceful
    var majestic = featureSet.majestic
    var goal = goalPrior()
    observe(speaker(large, graceful, majestic, goal), utterance)
    return {category, large, graceful, majestic}
  }})
}

viz.table(pragmaticListener("whale"))
viz.marginals(pragmaticListener("whale"))
~~~~

Another parameter which influences the probability order of items is the featureSetPrior,
which has assigned probabilities to category and feature combinations according to data from
Kao et al. (2014).

~~~~
// John could either be a whale or a person.
var categories = ["whale", "person"]

// It is extremely unlikely that John is actually a whale.
var categoriesPrior = function() {
  categorical([0.01, 0.99], categories)
}

// The speaker could either say "John is a whale" or "John is a person."
var utterances = ["whale", "person"]

// The utterances are equally costly.
var utterancePrior = function() {
  categorical([1,1], utterances)
}

// The features of John being considered are "large", "graceful",
// "majestic." Features are binary.
var featureSets = [
  {large : 1, graceful : 1, majestic : 1},
  {large : 1, graceful : 1, majestic : 0},
  {large : 1, graceful : 0, majestic : 1},
  {large : 1, graceful : 0, majestic : 0},
  {large : 0, graceful : 1, majestic : 1},
  {large : 0, graceful : 1, majestic : 0},
  {large : 0, graceful : 0, majestic : 1},
  {large : 0, graceful : 0, majestic : 0}
]

// information about feature priors (probabilistic world knowledge)
// obtained by an experimental study (see paper)
var featureSetPrior = function(category) {
  category === "whale" ? categorical([0.30592786494628, 0.138078454222818,
                                      0.179114768847673, 0.13098781834847,
                                      0.0947267162507846, 0.0531420411185539,
                                      0.0601520520596695, 0.0378702842057509],
                                     featureSets) :
  category === "person" ? categorical([0.11687632453038, 0.105787535267869,
                                       0.11568145784997, 0.130847056136141,
                                       0.15288225956497, 0.128098151176801,
                                       0.114694702836614, 0.135132512637255],
                                      featureSets) :
  true
}

// Speaker's possible goals are to communicate feature 1, 2, or 3
var goals = ["large", "graceful", "majestic"]

// Probability of speaker's goal being "large" is double that of other goals.
var goalPrior = function() {
  categorical([2,1,1], goals)
}

// Speaker optimality parameter
var alpha = 3

// Check if interpreted category is identical to utterance
var literalInterpretation = function(utterance, category) {
  utterance === category
}

// Check if goal is satisfied
var goalState = function(goal, featureSet) {
  goal === "large" ? featureSet.large :
  goal === "graceful" ? featureSet.graceful :
  goal === "majestic" ? featureSet.majestic :
  true
}

//  Define a literal listener
var literalListener = function(utterance, goal) {
  Infer({model: function() {
    var category = uniformDraw(categories)
    var featureSet = featureSetPrior(category)
    condition(literalInterpretation(utterance, category))
    return goalState(goal, featureSet)
  }})
}         

// Speaker model
var speaker = function(large, graceful, majestic, goal) {
  Infer({model: function() {
    var utterance = utterancePrior()
    factor(alpha *
           literalListener(utterance,goal).score(goalState(goal, {large : large, graceful : graceful, majestic : majestic})))
    return utterance
  }})
}

// Define a pragmatic listener
var pragmaticListener = function(utterance) {
  Infer({model: function() {
    var category = categoriesPrior()
    var featureSet = featureSetPrior(category)
    var large = featureSet.large
    var graceful = featureSet.graceful
    var majestic = featureSet.majestic
    var goal = goalPrior()
    observe(speaker(large, graceful, majestic, goal), utterance)
    return {category, large, graceful, majestic}
  }})
}

viz.table(pragmaticListener("whale"))
viz.marginals(pragmaticListener("whale"))
~~~~

Above is the prediction produced by the model when the probability of the speaker’s goal being
to communicate that the subject is large is doubled in the goalPrior (still running
viz.table(pragmaticListener("whale")) and
viz.marginals(pragmaticListener("whale"))). Previously, all three of the features
were equally as likely to be the speaker’s goal, but this change might more accurately reflect the
intentions of a real-world speaker saying, “John is a whale,” based on common uses of the
metaphor. While the categoriesPrior has been reset to its default categorical draw,
within each category, all items that include the large feature are more likely than those which
exclude it. In the marginal distribution, we also see that the probability of the pragmatic listener
believing the subject is large is double that of their belief that the subject is not large. The
goals of the pragmatic speaker affect the pragmatic listener’s predictions because the speaker
exists within the pragmatic listener, and the speaker is assumed to be truthful. If the pragmatic
listener knows that the speaker’s primary goal is to communicate that the subject is large, and
that the speaker is truthful, it follows that the pragmatic listener believes the subject is indeed
large.

However, when privileging the other goals, we do not observe the same feature dominance of
graceful and majestic as we do for large in the pragmatic listener predictions (see
below). This disparity can be attributed to the featureSetPrior, which itself privileges the
four combinations which include the large feature over the sets of four which include the
graceful and majestic features, especially in the whale category.

~~~~ norun
// John could either be a whale or a person.
var categories = ["whale", "person"]

// It is extremely unlikely that John is actually a whale.
var categoriesPrior = function() {
  categorical([0.01, 0.99], categories)
}

// The speaker could either say "John is a whale" or "John is a person."
var utterances = ["whale", "person"]

// The utterances are equally costly.
var utterancePrior = function() {
  categorical([1,1], utterances)
}

// The features of John being considered are "large", "graceful",
// "majestic." Features are binary.
var featureSets = [
  {large : 1, graceful : 1, majestic : 1},
  {large : 1, graceful : 1, majestic : 0},
  {large : 1, graceful : 0, majestic : 1},
  {large : 1, graceful : 0, majestic : 0},
  {large : 0, graceful : 1, majestic : 1},
  {large : 0, graceful : 1, majestic : 0},
  {large : 0, graceful : 0, majestic : 1},
  {large : 0, graceful : 0, majestic : 0}
]

// information about feature priors (probabilistic world knowledge)
// obtained by an experimental study (see paper)
var featureSetPrior = function(category) {
  category === "whale" ? categorical([0.30592786494628, 0.138078454222818,
                                      0.179114768847673, 0.13098781834847,
                                      0.0947267162507846, 0.0531420411185539,
                                      0.0601520520596695, 0.0378702842057509],
                                     featureSets) :
  category === "person" ? categorical([0.11687632453038, 0.105787535267869,
                                       0.11568145784997, 0.130847056136141,
                                       0.15288225956497, 0.128098151176801,
                                       0.114694702836614, 0.135132512637255],
                                      featureSets) :
  true
}

// Speaker's possible goals are to communicate feature 1, 2, or 3
var goals = ["large", "graceful", "majestic"]

// One goal has double the probability of the other two (un-comment each string to see predictions)
// var goalPrior = function() {
//   categorical([1,2,1], goals)
// }
// var goalPrior = function() {
//   categorical([1,1,2], goals)
// }

// Speaker optimality parameter
var alpha = 3

// Check if interpreted category is identical to utterance
var literalInterpretation = function(utterance, category) {
  utterance === category
}

// Check if goal is satisfied
var goalState = function(goal, featureSet) {
  goal === "large" ? featureSet.large :
  goal === "graceful" ? featureSet.graceful :
  goal === "majestic" ? featureSet.majestic :
  true
}

//  Define a literal listener
var literalListener = function(utterance, goal) {
  Infer({model: function() {
    var category = uniformDraw(categories)
    var featureSet = featureSetPrior(category)
    condition(literalInterpretation(utterance, category))
    return goalState(goal, featureSet)
  }})
}         

// Speaker model
var speaker = function(large, graceful, majestic, goal) {
  Infer({model: function() {
    var utterance = utterancePrior()
    factor(alpha *
           literalListener(utterance,goal).score(goalState(goal, {large : large, graceful : graceful, majestic : majestic})))
    return utterance
  }})
}

// Define a pragmatic listener
var pragmaticListener = function(utterance) {
  Infer({model: function() {
    var category = categoriesPrior()
    var featureSet = featureSetPrior(category)
    var large = featureSet.large
    var graceful = featureSet.graceful
    var majestic = featureSet.majestic
    var goal = goalPrior()
    observe(speaker(large, graceful, majestic, goal), utterance)
    return {category, large, graceful, majestic}
  }})
}

viz.table(pragmaticListener("whale"))
viz.marginals(pragmaticListener("whale"))
~~~~

Thus far, all of the alternate predictions I have explored are those that show what the pragmatic
listener thinks upon hearing the utterance, “John is a whale.” The remainder of this write-up will
analyze what the model predicts when the pragmatic listener hears the utterance, “John is a
person” (viz.table(pragmaticListener("person"))).

~~~~
// John could either be a whale or a person.
var categories = ["whale", "person"]

// It is extremely unlikely that John is actually a whale.
var categoriesPrior = function() {
  categorical([0.01, 0.99], categories)
}

// The speaker could either say "John is a whale" or "John is a person."
var utterances = ["whale", "person"]

// The utterances are equally costly.
var utterancePrior = function() {
  categorical([1,1], utterances)
}

// The features of John being considered are "large", "graceful",
// "majestic." Features are binary.
var featureSets = [
  {large : 1, graceful : 1, majestic : 1},
  {large : 1, graceful : 1, majestic : 0},
  {large : 1, graceful : 0, majestic : 1},
  {large : 1, graceful : 0, majestic : 0},
  {large : 0, graceful : 1, majestic : 1},
  {large : 0, graceful : 1, majestic : 0},
  {large : 0, graceful : 0, majestic : 1},
  {large : 0, graceful : 0, majestic : 0}
]

// information about feature priors (probabilistic world knowledge)
// obtained by an experimental study (see paper)
var featureSetPrior = function(category) {
  category === "whale" ? categorical([0.30592786494628, 0.138078454222818,
                                      0.179114768847673, 0.13098781834847,
                                      0.0947267162507846, 0.0531420411185539,
                                      0.0601520520596695, 0.0378702842057509],
                                     featureSets) :
  category === "person" ? categorical([0.11687632453038, 0.105787535267869,
                                       0.11568145784997, 0.130847056136141,
                                       0.15288225956497, 0.128098151176801,
                                       0.114694702836614, 0.135132512637255],
                                      featureSets) :
  true
}

// Speaker's possible goals are to communicate feature 1, 2, or 3
var goals = ["large", "graceful", "majestic"]

// Prior probability of speaker's goal is set to uniform but can
// change with context/QUD.
var goalPrior = function() {
  categorical([1,1,1], goals)
}

// Speaker optimality parameter
var alpha = 3

// Check if interpreted category is identical to utterance
var literalInterpretation = function(utterance, category) {
  utterance === category
}

// Check if goal is satisfied
var goalState = function(goal, featureSet) {
  goal === "large" ? featureSet.large :
  goal === "graceful" ? featureSet.graceful :
  goal === "majestic" ? featureSet.majestic :
  true
}

//  Define a literal listener
var literalListener = function(utterance, goal) {
  Infer({model: function() {
    var category = uniformDraw(categories)
    var featureSet = featureSetPrior(category)
    condition(literalInterpretation(utterance, category))
    return goalState(goal, featureSet)
  }})
}         

// Speaker model
var speaker = function(large, graceful, majestic, goal) {
  Infer({model: function() {
    var utterance = utterancePrior()
    factor(alpha *
           literalListener(utterance,goal).score(goalState(goal, {large : large, graceful : graceful, majestic : majestic})))
    return utterance
  }})
}

// Define a pragmatic listener
var pragmaticListener = function(utterance) {
  Infer({model: function() {
    var category = categoriesPrior()
    var featureSet = featureSetPrior(category)
    var large = featureSet.large
    var graceful = featureSet.graceful
    var majestic = featureSet.majestic
    var goal = goalPrior()
    observe(speaker(large, graceful, majestic, goal), utterance)
    return {category, large, graceful, majestic}
  }})
}

viz.table(pragmaticListener("person"))
viz.marginals(pragmaticListener("person"))
~~~~

Inputting this utterance negates the function of the metaphor model entirely. While the whale
utterance can be used in this case to refer to either a person or a whale, the person utterance
can only ever describe a person. Upon hearing the utterance, “John is a person,” there is only a
miniscule chance that the pragmatic listener thinks John is a whale.

If it is true that John is a person, then it is most likely that John has none of the features of a
whale, and least likely that John has all of the features of a whale. This pattern can be observed
in both the table and the marginal distribution graphs. Using the utterance “John is a person”
when John is a person communicates nothing about John’s features, but the pragmatic listener
gleans some information from the fact that the speaker did not choose to say “John is a whale.”
Because the whale utterance is also an option, the pragmatic listener infers that the speaker
would have used that utterance if John had any features of a whale.

On the very off chance that John is a whale (a category which receives probability only
because whales exist), the relative likelihood of feature set combinations is determined mainly
by the featureSetPrior.