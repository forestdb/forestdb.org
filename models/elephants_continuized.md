---
layout: model
title: elephants
model-language: webppl
model-language-version: v0.9.13
---

The continuized _elephants_ model has several changes with respect to the original model:
- The world knowledge is modified to include a *both* predicate, which represents the probability of an elephant living in both Africa and Asia. This represents a problem for inference because the size of the world state increases.
- The semantic interpretation function does not perform type-checking in order to determine the direction of functional application. Rather, more of the responsibility of functional composition is delegated to the grammar. Concretely, the syntax tree is annotated at each branching node with the type of the constituent (e.g. S or VP). This contains the information needed to perform functional application at each node. It also enables the same phrase to be continuized in multiple ways. For example, the S node can either be annotated with _S1_ or _S2_, where _S1_ reflects the continuization where the NP is interpreted as the continuation of the VP (and > gen), and _S2_ reflects the continuization where the VP is interpreted as the continuation of the NP (gen > and).
- Previously, the meaning of a noun was an index into the world state (e.g. the noun _elephants_) representing a _kind_. Additionally, the meaning of a VP (e.g. the VP _live in Africa_) was an index into the world state representing a _feature_ of that _kind_. In the new model, the meaning of a noun and the meaning of a VP is the meaning of a predicate in the formal semantics sense of denoting a set of individuals (e.g. the meaning of _elephants_ is the set of individuals that are elephants). This modification was made in order to be more faithful to the semantics of continuations as formalized in Barker (2001). In order for the meaning of the quantifier (e.g. _gen_) to compose with the meaning of the predicate (e.g. _elephants_), the function that denotes the predicate can be thought of as a _kind checker_ which checks the index into the world state and makes sure that it matches the predicate. Similarly, the function that denotes _live in Africa_ can be thought of as a _feature checker_; it checks the index into the world state representing the feature of the kind and makes sure that it matches _live in Africa_. Only the predicate _both_ will match both _live in Africa_ and _live in Asia_ as composed by the continuized conjunction _and_, thus leading to the (gen > and) reading when using the _S2_ continuization (from above).

In the following box we define the semantics of the quantifiers, of conjunction, and of predicates (_live in Africa_ and _live in Asia_ but not _elephants_).

~~~~
// denotation of quantifier:
// lambda noun: lambda c: noun(lambda P: quantifier(P, c))
// quantifier function is responsible for finding the kind in the world state
// consistent with P (e.g. elephants) and the features relativized to that kind
// consistent with c (e.g. live in Africa), and evaluating the obtained prevalence
// with respect to the threshold (either fixed or inferred)
var gen = function(kindChecker, featureChecker, worldState, threshold) {
  var kinds = Object.keys(worldState)
  var applicableKinds = filter(function(k) {return kindChecker(k)}, kinds)
  if (applicableKinds.length != 1) {
    error('cannot find unique kind')
  }
  var kind = applicableKinds[0]
  var features = Object.keys(worldState[kind])
  var applicableFeatures = filter(function(f) {return featureChecker(f)}, features)
  // if more than one feature is retained, 'both' should be excluded since it always
  // passes feature checking
  var feature = applicableFeatures.length > 1 ? filter(function(f) {return f != 'both'}, applicableFeatures)[0] : applicableFeatures[0]
  return worldState[kind][feature] > threshold
}
var genFunc = function(noun, worldState, threshold) {
  return function(continuation) {
    return noun(function(predicate) {
      gen(predicate, continuation, worldState, threshold)
    })
  }
}
var most = function(kindChecker, featureChecker, worldState) {
  var kinds = Object.keys(worldState)
  var applicableKinds = filter(function(k) {return kindChecker(k)}, kinds)
  if (applicableKinds.length != 1) {
    error('cannot find unique kind')
  }
  var kind = applicableKinds[0]
  var features = Object.keys(worldState[kind])
  var applicableFeatures = filter(function(f) {return featureChecker(f)}, features)
  // if more than one feature is retained, 'both' should be excluded since it always
  // passes feature checking
  var feature = applicableFeatures.length > 1 ? filter(function(f) {return f != 'both'}, applicableFeatures)[0] : applicableFeatures[0]
  return worldState[kind][feature] > 0.5
}
var mostFunc = function(noun, worldState, threshold) {
  return function(continuation) {
    return noun(function(predicate) {
      most(predicate, continuation, worldState)
    })
  }
}
var some = function(kindChecker, featureChecker, worldState) {
  var kinds = Object.keys(worldState)
  var applicableKinds = filter(function(k) {return kindChecker(k)}, kinds)
  if (applicableKinds.length != 1) {
    error('cannot find unique kind')
  }
  var kind = applicableKinds[0]
  var features = Object.keys(worldState[kind])
  var applicableFeatures = filter(function(f) {return featureChecker(f)}, features)
  // if more than one feature is retained, 'both' should be excluded since it always
  // passes feature checking
  var feature = applicableFeatures.length > 1 ? filter(function(f) {return f != 'both'}, applicableFeatures)[0] : applicableFeatures[0]
  return worldState[kind][feature] > 0
}
var someFunc = function(noun, worldState, threshold) {
  return function(continuation) {
    return noun(function(predicate) {
      some(predicate, continuation, worldState)
    })
  }
}

var conjFunc = function(leftConjunct, rightConjunct) {
  // denotation of conjunction:
  // lambda c: c(left(c), right(c))
  return function(continuation) {
    return leftConjunct(continuation) && rightConjunct(continuation)
  }
}

var makePredicateSemantics = function(predicate) {
  // denotation of predicate (feature)
  // lambda c: c(lambda P: P is consistent with predicate)
  return function(continuation) {
    return continuation(function(testPredicate) {
      return testPredicate == predicate || testPredicate == 'both'
    })
  }
}

editor.put('genFunc', genFunc)
editor.put('mostFunc', mostFunc)
editor.put('someFunc', someFunc)
editor.put('conjFunc', conjFunc)
editor.put('makePredicateSemantics', makePredicateSemantics)
~~~~

In the following box we define the lexicon. Each lexical entry contains both a pronunciation and a semantics, where the semantics is simply a function that will be composed by one of the phrasal composition rules.

~~~~
var genFunc = editor.get('genFunc')
var mostFunc = editor.get('mostFunc')
var someFunc = editor.get('someFunc')
var conjFunc = editor.get('conjFunc')
var makePredicateSemantics = editor.get('makePredicateSemantics')

var africaFunc = makePredicateSemantics("africa")
var asiaFunc = makePredicateSemantics("asia")
var elephantsFunc = function(continuation) {
  // denotation of noun (kind):
  // lambda c: c(lambda P: P is consistent with predicate)
  // no 'both' index for kinds based on the definition of world state
  return continuation(function(testPredicate) {
    return testPredicate == "elephants"
  })
}

var lexicalEntries = {
  "gen": {
    pronunciation: "",
    semantics: genFunc,
  },
  "most": {
    pronunciation: "most",
    semantics: mostFunc,
  },
  "some": {
    pronunciation: "some",
    semantics: someFunc,
  },
  "conj": {
    pronunciation: "and",
    semantics: conjFunc,
  },
  "asia": {
    pronunciation: "live in Asia",
    semantics: asiaFunc,
  },
  "elephants": {
    pronunciation: "elephants",
    semantics: elephantsFunc,
  },
  "africa": {
    pronunciation: "live in Africa",
    semantics: africaFunc,
  },
}

editor.put('lexicalEntries', lexicalEntries)
~~~~

Next we define the composition rules for the grammar. Each rule is a function that takes as input the phrasal _constituents_, the _world state_, and the sampled generic _threshold_. Not all composition rules require information about the _world state_ and _threshold_, but they take in that information for simplicity. Each composition rule is responsible for functionally composing its constituents.

The S rules are unique in that they represent two different ways of continuizing the grammar. The _S1_ rule represents the continuization where the NP is the continuation of the VP (and > gen). The _S2_ rule represents the continuization where the VP is the continuation of the NP (gen > and). As seen below, this simply represents a swap in the order of functional composition.

~~~~
var lexicalEntries = editor.get('lexicalEntries')

var s1Func = function(constituents, worldState, threshold) {
  // S -> QP VP
  // denotation of S (assuming trivial continuation)
  // VP(lambda P: QP(lambda x: P(x)))
  var qp = constituents[0]
  var vp = constituents [1]
  return vp(function(predicate) {
    return qp(function(x) {
      return predicate(x)
    })
  })
}

var s2Func = function(constituents, worldState, threshold) {
  // S -> QP VP
  // denotation of S (assuming trivial continuation)
  // QP(lambda x: VP(lambda P: P(x)))
  var qp = constituents[0]
  var vp = constituents[1]
  return qp(function(x) {
    return vp(function(predicate) {
      return predicate(x)
    })
  })
}

var qpFunc = function(constituents, worldState, threshold) {
  // QP -> Q N
  // denotation of QP:
  // Q(N)
  var q = constituents[0]
  var n = constituents[1]
  return q(n, worldState, threshold)
}

var andPFunc = function(constituents, worldState, threshold) {
  // X -> X1 and X2
  // denotation of andP:
  // and(X1, X2)
  var leftConjunct = constituents[0]
  if (!_.isEqual(constituents[1], lexicalEntries.conj.semantics)) {
    error('andP does not have correct form')
  }
  var and = constituents[1]
  var rightConjunct = constituents[2]
  return and(leftConjunct, rightConjunct)
}

var phrases = {
  's1': s1Func,
  's2': s2Func,
  'qp': qpFunc,
  'andP': andPFunc
}

editor.put('phrases', phrases)
~~~~

The world knowledge is the same as the original _elephants_ model, except for the addition of a third feature _both_ which represents the prevalence of elephants living on both continents. The degree of mutual exclusivity (`k`) can be modulated.

~~~~
var k = 50; // degree of mutual exclusivity

var probability = function(Dist, x) {
  return Math.exp(Dist.score(x));
}
// for prevalence
var bins = [
  0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,
  0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99
];
var theta_bins = [
  0.01, 0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,
  0.5, 0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95
]
var makeComponent = function(params){
  return Categorical({
    vs: bins,
    ps: map(function(b) {
      probability(Beta(params), b) + Number.EPSILON // what is this?
    }, bins )
  })
}
var priorParameters = {
  "a": 1,
  "b": 1,
}
// makes 3-way joint on living in africa, living in asia, and living in both
var makePrevalencePrior = function(me) {
  return Infer({model: function(){
    // elephants living in Asia (or both)
    var asiaProb = sample(
      // binned beta distribution on prior prevalence of living in Asia
      makeComponent(priorParameters)
    )
    // elephants living in Africa (or both)
    var africaProb = sample(
      // binned beta distribution on prior prevalence of living in Africa
      makeComponent(priorParameters)
    )
    var totalPrevalence = asiaProb + africaProb
    var p =  {
      asia: asiaProb,
      africa: africaProb,
      both: totalPrevalence > 1 ? totalPrevalence - 1 : 0 // elephants live in both
      // prevalence cannot add up to greater than 1, so leftover prevalence must be
      // elephants living in both Africa and Asia
    } 
    // if correlated, force mutual exclusivity by decreasing probability of there
    // being elephants that live in both places
    factor(me ? totalPrevalence > 1 ? -k*Math.log(totalPrevalence) : 0 : 0)
    return p
  }})
}

print("prevalence prior")
viz(marginalize(makePrevalencePrior(true), function(x) {
  return {
    africa: x.africa,
    asia: x.asia,
  }
}))
viz(marginalize(makePrevalencePrior(true), function(x) {
  return {
    africa: x.africa,
    both: x.both,
  }
}))
viz(marginalize(makePrevalencePrior(true), function(x) {
  return {
    both: x.both
  }
}))

// prior on the generic threshold value
// same threshold value for all three predicates to help with inference
var makeThetaPrior = function() {
  return Infer({model: function(){
    var sampleTheta = function(){ uniformDraw(theta_bins) }
    return sampleTheta()
  }})
};

editor.put('makePrevalencePrior', makePrevalencePrior)
editor.put('makeThetaPrior', makeThetaPrior)
~~~~

Here we define the pronunciation function for linearizing a syntax tree. (As the incremental model is not implemented, `pronounce` is simply used for making syntax trees human-readable.)

The semantic interpretation function is very simple because the responsibility of functional application is now contained in the annotated branching nodes of the syntax tree. It is defined recursively where the base case is a single lexical entry, so the function returns the semantics of that entry, and the recursive case is the case of a branching node. In the recursive case, the function calls itself recursively on all of its constituents and then passes those constituents into the composition rule for that node.

~~~~
var pronounce = function(utterance) {
  if (utterance.hasOwnProperty('pronunciation')) {
    // base case: single lexical entry
    return utterance.pronunciation
  } else {
    var constituents = utterance.slice(0, utterance.length-1)
    var pronouncedConstituents = filter(function(c) {return c != ""}, map(pronounce, constituents))
    return pronouncedConstituents.join(" ")
  }
}

var interpretSemantics = function(utterance, state, theta) {
  // returns a truth value for declarative sentences
  if (utterance.hasOwnProperty('pronunciation')) {
    // base case: single lexical entry
    return utterance.semantics
  } else {
    // otherwise, use the composition rule for that consituent
    var constituents = utterance.slice(0, utterance.length-1)
    var interpretedConstituents = map(function(cons) {
      return interpretSemantics(cons, state, theta)
    }, constituents)
    var compositionRule = utterance[utterance.length-1]
    var result = compositionRule(interpretedConstituents, state, theta)
    //display(result)
    return result
  }
}

var interpretSemanticsWithPrior = function(utterance, state, thresholdPrior) {
  // wrapper to sample over language-based priors
  var theta = sample(thresholdPrior)
  return interpretSemantics(utterance, state, theta)
}

editor.put('pronounce', pronounce)
editor.put('interpretSemantics', interpretSemantics)
editor.put('interpretSemanticsWithPrior', interpretSemanticsWithPrior)
~~~~

Here we define the literal listener, which assumes that its input is a syntax tree (not a string).

~~~~
var interpretSemanticsWithPrior = editor.get('interpretSemanticsWithPrior')

// creates the world state, allowing for multiple kinds
var getWorldState = function(makeFeaturePrior) {
  return {
    "elephants": sample(makeFeaturePrior(true))
  }
}

// models the literal listener, assuming the complete utterance has already been
// parsed into a syntax tree
var listener0Complete = function(utterance, makeFeaturePrior, thresholdPrior, qud) {
  Infer({model: function() {
    var state = getWorldState(makeFeaturePrior)
    var result = interpretSemanticsWithPrior(utterance, state, thresholdPrior)
    // condition on utterance being true
    condition(result)
    return state[qud]
  }, method: 'enumerate'});
}

editor.put('listener0Complete', listener0Complete)
~~~~

The following example utterances are shown:
- `Elephants live in Africa.` The original model predictions are recovered.
- `Elephants live in Africa and live in Asia. (S1)` The original model predictions are recovered.
- `Elephants live in Africa and live in Asia. (S2)` This represents the continuization where and outscopes gen. The lower triangle of the posterior is eliminated due to the meaning of the predicate as denoting _both_. The highest probability lies on the diagonal, and there is uniform probability on the upper triangle, reflecting the fact that for world states that violate the mutual exclusivity assumption, the ones that violate it the least (the fewest elephants living in both places) are preferred.
- `Some elephants live in Africa and live in Asia. (S2)` This represents the continuization where and outscopes gen. The lower triangle is eliminated and the upper triangle is uniform. The diagonal again has the highest probability, with slightly higher probability on states in which the split of living in Africa/living in Asia is skewed. This may reflect the fact that when _both_ has low prevalence in the prior, the highest probability lies on lower prevalence values for _live in Africa_ and _live in Asia_.

In general, the model struggles with inference because the space of threshold states is large, so we set the threshold to be the same value for all three predicates.

~~~~
var lexicalEntries = editor.get('lexicalEntries')
var phrases = editor.get('phrases')
var listener0Complete = editor.get('listener0Complete')
var makePrevalencePrior = editor.get('makePrevalencePrior')
var makeThetaPrior = editor.get('makeThetaPrior')
var pronounce = editor.get('pronounce')

print("example utterances")

var plainUtterance = [ 
  [
    lexicalEntries.gen, 
    lexicalEntries.elephants,
    phrases.qp
  ],
  lexicalEntries.africa,
  phrases.s1
]

print(pronounce(plainUtterance))

viz(marginalize(
  listener0Complete(plainUtterance, makePrevalencePrior, makeThetaPrior(), "elephants"),
  function(x) {
    return {
      africa: x.africa,
      asia: x.asia
    }
  }
))

var conjunctiveUtteranceGen = [
  [
    lexicalEntries.gen,
    lexicalEntries.elephants,
    phrases.qp
  ],
  [
    lexicalEntries.africa,
    lexicalEntries.conj,
    lexicalEntries.asia,
    phrases.andP
  ],
  phrases.s1
]

print(pronounce(conjunctiveUtteranceGen))
print('sentence continuization 1')

viz(marginalize(
  listener0Complete(conjunctiveUtteranceGen, makePrevalencePrior, makeThetaPrior(), "elephants"),
  function(x) {
    return {
      africa: x.africa,
      asia: x.asia
    }
  }
))

var conjunctiveUtteranceAnd = [
  [
    lexicalEntries.gen,
    lexicalEntries.elephants,
    phrases.qp
  ],
  [
    lexicalEntries.africa,
    lexicalEntries.conj,
    lexicalEntries.asia,
    phrases.andP
  ],
  phrases.s2
]

print(pronounce(conjunctiveUtteranceAnd))
print('sentence continuization 2')

viz(marginalize(
  listener0Complete(conjunctiveUtteranceAnd, makePrevalencePrior, makeThetaPrior(), "elephants"),
  function(x) {
    return {
      africa: x.africa,
      asia: x.asia
    }
  }
))

var conjunctiveUtteranceSome = [
  [
    lexicalEntries.some,
    lexicalEntries.elephants,
    phrases.qp
  ],
  [
    lexicalEntries.africa,
    lexicalEntries.conj,
    lexicalEntries.asia,
    phrases.andP
  ],
  phrases.s2
]

print(pronounce(conjunctiveUtteranceSome))

viz(marginalize(
  listener0Complete(conjunctiveUtteranceSome, makePrevalencePrior, makeThetaPrior(), "elephants"),
  function(x) {
    return {
      africa: x.africa,
      asia: x.asia
    }
  }
))
~~~~

**TODO:**
- Decide what to do about inference taking too long when the threshold values are specified separately.
- Fully articulate the structure of the syntax tree (add PP and NP nodes).
- Add the composition rule annotations to the PCFG.
