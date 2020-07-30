---
layout: model
title: elephants
model-language: webppl
model-language-version: v0.9.13
---

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
  var applicableFeaturePrevalences = map(function(f) {return worldState[kind][f]}, applicableFeatures)
  // all features are mutually exclusive, so addition doesn't double count any individuals
  // if feature is "africa" ("asia"), this will add the prevalence of elephants living
  // in Africa (Asia) and the prevalence of elephants living in both
  // if feature is "both", only the prevalence of elephants living in both will remain
  return sum(applicableFeaturePrevalences) > threshold
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
  var applicableFeaturePrevalences = map(function(f) {return worldState[kind][f]}, applicableFeatures)
  // all features are mutually exclusive, so addition doesn't double count any individuals
  // if feature is "africa" ("asia"), this will add the prevalence of elephants living
  // in Africa (Asia) and the prevalence of elephants living in both
  // if feature is "both", only the prevalence of elephants living in both will remain
  return sum(applicableFeaturePrevalences) > 0.5
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
  var applicableFeaturePrevalences = map(function(f) {return worldState[kind][f]}, applicableFeatures)
  // all features are mutually exclusive, so addition doesn't double count any individuals
  // if feature is "africa" ("asia"), this will add the prevalence of elephants living
  // in Africa (Asia) and the prevalence of elephants living in both
  // if feature is "both", only the prevalence of elephants living in both will remain
  return sum(applicableFeaturePrevalences) > 0
}
var someFunc = function(noun, worldState, threshold) {
  return function(continuation) {
    return noun(function(predicate) {
      some(predicate, continuation, worldState)
    })
  }
}
var all = function(kindChecker, featureChecker, worldState) {
  var kinds = Object.keys(worldState)
  var applicableKinds = filter(function(k) {return kindChecker(k)}, kinds)
  if (applicableKinds.length != 1) {
    error('cannot find unique kind')
  }
  var kind = applicableKinds[0]
  var features = Object.keys(worldState[kind])
  var applicableFeatures = filter(function(f) {return featureChecker(f)}, features)
  var applicableFeaturePrevalences = map(function(f) {return worldState[kind][f]}, applicableFeatures)
  // all features are mutually exclusive, so addition doesn't double count any individuals
  // if feature is "africa" ("asia"), this will add the prevalence of elephants living
  // in Africa (Asia) and the prevalence of elephants living in both
  // if feature is "both", only the prevalence of elephants living in both will remain
  return sum(applicableFeaturePrevalences) > .99
}
var allFunc = function(noun, worldState, threshold) {
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
editor.put('allFunc', allFunc)
editor.put('conjFunc', conjFunc)
editor.put('makePredicateSemantics', makePredicateSemantics)
~~~~

In the following box we define the lexicon. Each lexical entry contains both a pronunciation and a semantics, where the semantics is simply a function that will be composed by one of the phrasal composition rules.

~~~~
var genFunc = editor.get('genFunc')
var mostFunc = editor.get('mostFunc')
var someFunc = editor.get('someFunc')
var allFunc = editor.get('allFunc')
var conjFunc = editor.get('conjFunc')
var makePredicateSemantics = editor.get('makePredicateSemantics')

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
  "all": {
    pronunciation: "all",
    semantics: allFunc,
  },
  "conj": {
    pronunciation: "and",
    semantics: conjFunc,
  },
  "asia": {
    pronunciation: "Asia",
    semantics: "asia",
  },
  "elephants": {
    pronunciation: "elephants",
    semantics: elephantsFunc,
  },
  "africa": {
    pronunciation: "Africa",
    semantics: "africa",
  },
  "live": {
    pronunciation: "live",
    semantics: makePredicateSemantics
  },
  "in": {
    pronunciation: "in"
  }
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
  if (_.isEqual(constituents[0], constituents[2])) {
    return constituents[0]
  }
  var and = constituents[1]
  var rightConjunct = constituents[2]
  return and(leftConjunct, rightConjunct)
}

var ppFunc = function(constituents, worldState, threshold) {
  // PP -> P NP
  // denotation of PP: (semantically vacuous)
  // NP
  var np = constituents[1]
  return np
}

var vpFunc = function(constituents, worldState, threshold) {
  // VP -> V PP
  // denotation of VP: (semantically vacuous)
  // PP
  var v = constituents[0]
  var pp = constituents[1]
  return v(pp)
}

var phrases = {
  's1': s1Func,
  's2': s2Func,
  'qp': qpFunc,
  'andP': andPFunc,
  'pp': ppFunc,
  'vp': vpFunc
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
var makePrevalencePrior = function() {
  return Infer({model: function(){
    var me = flip(0.99)
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
      both: totalPrevalence > 1 ? totalPrevalence - 1 : 0, // elephants live in both
      // prevalence cannot add up to greater than 1, so leftover prevalence must be
      // elephants living in both Africa and Asia
      me: me
    } 
    // if correlated, force mutual exclusivity by decreasing probability of there
    // being elephants that live in both places
    factor(me ? totalPrevalence > 1 ? -k*Math.log(totalPrevalence) : 0 : 0)
    return p
  }})
}

print("prevalence prior")
viz(marginalize(makePrevalencePrior(), function(x) {
  return {
    africa: x.africa,
    asia: x.asia,
  }
}))
viz(marginalize(makePrevalencePrior(), function(x) {
  return {
    africa: x.africa,
    both: x.both,
  }
}))
viz(marginalize(makePrevalencePrior(), function(x) {
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

Here we define the pronunciation function for linearizing a syntax tree.

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

Here we define the PCFG.

~~~~
var lexicalEntries = editor.get('lexicalEntries')
var phrases = editor.get('phrases')
var pronounce = editor.get('pronounce')

// PCFG

var pcfg = {
  'QP': [
    {
      expansion: ['Q', 'N'], 
      annotation: phrases.qp, 
      p: 1,
      recursive: false
    }
  ],
  'PP': [
    {
      expansion: ['P', 'NP'], 
      annotation: phrases.pp,
      p: .9,
      recursive: false
    },
    {
      expansion: ['PP', 'connective', 'PP'], 
      annotation: phrases.andP, 
      p: .1,
      recursive: true
    }
  ],
  'VP': [
    {
      expansion: ['V', 'PP'],
      annotation: phrases.vp, 
      p: .9,
      recursive: false
    },
    {
      expansion: ['VP', 'connective', 'VP'],
      annotation: phrases.andP, 
      p: .1,
      recursive: true
    }
  ],
  'S': [
    {
      expansion: ['QP', 'VP'], 
      annotation: phrases.s1, 
      p: .4,
      recursive: false
    }, 
    {
      expansion: ['QP', 'VP'],
      annotation: phrases.s2, 
      p: .6,
      recursive: false
    }
  ],
  'N': [
    {
      expansion: lexicalEntries.elephants,
      p: 1,
      recursive: false
    }
  ],
  'NP': [
    {
      expansion: lexicalEntries.africa,
      p: 0.5,
      recursive: false
    },
    {
      expansion: lexicalEntries.asia,
      p: 0.5,
      recursive: false
    }
  ],
  'Q': [
    {
      expansion: lexicalEntries.gen,
      p: 0.4,
      recursive: false
    },
    {
      expansion: lexicalEntries.most,
      p: 0.2,
      recursive: false
    },
    {
      expansion: lexicalEntries.all,
      p: 0.2,
      recursive: false
    },
    {
      expansion: lexicalEntries.some,
      p: 0.2,
      recursive: false
    }
  ],
  'V': [
    {
      expansion: lexicalEntries.live,
      p: 1,
      recursive: false
    }
  ],
  'P': [
    {
      expansion: lexicalEntries.in,
      p: 1,
      recursive: false
    }
  ],
  'connective': [
    {
      expansion: lexicalEntries.conj,
      p: 1,
      recursive: false
    }
  ]
}

editor.put('pcfg', pcfg)
~~~~

The PCFG can generate an infinite number of sentences due to the recursive nature of coordination. Here we limit the space to a finite number of sentences by pruning the search space at every possible expansion to contain only the most likely trees.

~~~~
var phrases = editor.get('phrases')
var pcfg = editor.get('pcfg')
var lexicalEntries = editor.get('lexicalEntries')
var pronounce = editor.get('pronounce')

var deepExtendErrorMessage = 'ill-formed target/source for deepExtend'

// helper for deepExtend
var copyOver = function(target, obj) {
  return reduce(function(x, acc) {
    if (Array.isArray(obj[x])) {
      if (x === 'children') {
        return extend(acc, {
          children: map(function(e) {
            return deepExtend({}, e)
          }, obj[x])
        })
      } else {
        error(deepExtendErrorMessage)
      }
    } else if (typeof obj[x] === 'number') {
      if (x === 'p') {
        return extend(acc, {
          p: obj[x]
        })
      } else if (x === 'tempP') {
        return extend(acc, {
          tempP: obj[x]
        })
      } else if (x === 'id') {
        return extend(acc, {
          id: obj[x]
        })
      } else {
        error(deepExtendErrorMessage)
      }
    } else if (typeof obj[x] === 'string') {
      if (x === 'label') {
        return extend(acc, {
          label: obj[x]
        })
      } else {
        error(deepExtendErrorMessage)
      }
    } else if (obj[x].hasOwnProperty('label')) {
      return extend(acc, deepExtend({}, obj[x]))
    } else {
      if (x === 'annotation') {
        return extend(acc, {
          annotation: obj[x]
        })
      } else if (obj[x].hasOwnProperty('pronunciation') & x === 'children') {
        return extend(acc, {
          children: obj[x]
        })
      } else {
        error(deepExtendErrorMessage)
      }
    }
  }, target, Object.keys(obj))
}

// used to build up trees in expand
var deepExtend = function(target, source) {
  // consists of objects, arrays, strings, and numbers
  // properties of source override those of target
  var intermediateObject = copyOver({}, target)
  return copyOver(intermediateObject, source)
}

var beamSize = 100

// prune possible expansions to most likely ones
var prune = function(expansions) {
  return _.sortBy(
    expansions, "p"
  ).reverse().slice(
    0, beamSize > expansions.length ? expansions.length : beamSize
  )
}

// helper for expand
var expandLabel = function(tree, possibleExpansions, depth) {
  var expansions = _.flatten(map(function(possibleExpansion) {
    if (possibleExpansion.expansion.hasOwnProperty('pronunciation')) {
      return [deepExtend(
        tree, {
          children: extend(
            possibleExpansion.expansion, {p: 1}
          ),
          p: possibleExpansion.p
        }
      )]
    } else {
      var newChildren = map(function(e) {
        return {
          label: e,
        }
      }, possibleExpansion.expansion)
      return expand(deepExtend(tree, {
        children: newChildren,
        annotation: possibleExpansion.annotation,
        tempP: possibleExpansion.p
      }), depth + 1)
    }
  }, possibleExpansions), true)
  return prune(expansions)
}

// recursively expands a tree until either the maximum recursion depth is reached
// or all nodes have been fully expanded
var expand = function(tree, depth) {
  if (tree.hasOwnProperty('children')) {
    if (tree.children.hasOwnProperty('pronunciation')) { // leaf node
      return deepExtend(tree, {
        p: tree.tempP
      })
    } else { // branching node
      if (tree.children.length === 1) { // only one child to expand
        var leftExpansions = expand(tree.children[0], depth + 1)
        var expansions = map(function(l) {
          return deepExtend(tree, {
            children: [l],
            p: l.p*tree.tempP
          })
        }, leftExpansions)
        return prune(expansions)
      } else { // multiple children to expand
        var leftExpansions = expand(tree.children[0], depth + 1)
        var restChildren = tree.children.slice(1, tree.children.length)
        var restExpansions = expand(deepExtend(tree, {
          children: restChildren
        }), depth+1)
        var expansions = _.flatten(map(function(l) {
          return map(function(r) {
            return deepExtend(tree, {
              children: [l].concat(r.children),
              p: l.p*product(map(function(x) {return x.p}, r.children))*tree.tempP
            })
          }, restExpansions)
        }, leftExpansions), true)
        return prune(expansions)
      }
    }
  } else { // need to expand based on label of branching node (or preterminal)
    if (depth > 5) {
      var possibleExpansions = filter(function(e) {
        return !e.recursive
      }, pcfg[tree.label])
      return expandLabel(tree, possibleExpansions, depth)
    } else {
      var possibleExpansions = pcfg[tree.label]
      var result = expandLabel(tree, possibleExpansions, depth)
      return result
    }
  }
}

// convert tree representation from object form to array form that is used by
// the semantic interpretation function
var convertTree = function(tree) {
  if (tree.hasOwnProperty('pronunciation')) {
    return tree
  } else if (tree.hasOwnProperty('children')) {
    if (tree.children.hasOwnProperty('pronunciation')) {
      return tree.children
    } else {
      var intermediateResult = map(function(x) {
        return convertTree(x)
      }, tree.children)
      return intermediateResult.concat([tree.annotation])
    }
  }
}

// array of most likely parses, with ids to differentiate them
var likelyParses = map2(function(a,b) {
  return deepExtend(a, {
    id: b
  })
}, expand({
  label: 'S'
}, 0), _.range(0, beamSize))

editor.put('expand', expand)
editor.put('convertTree', convertTree)
editor.put('likelyParses', likelyParses)
~~~~

Here we test that the likely parses obtained above have the correct probabilities according to the PCFG, for a small number of most likely cases.

~~~~
var likelyParses = editor.get('likelyParses')
var convertTree = editor.get('convertTree')
var pronounce = editor.get('pronounce')
var phrases = editor.get('phrases')
var pcfg = editor.get('pcfg')

map(function(x) {
  if ([
    "elephants live in Africa", "elephants live in Asia"
  ].includes(pronounce(convertTree(x)))) {
    if (convertTree(x)[2] === phrases.s1) {
      if (Math.abs(
        x.p-pcfg.S[0].p*pcfg.Q[0].p*pcfg.VP[0].p*pcfg.PP[0].p*pcfg.NP[0].p
      )>1e-10) {
        error('wrong probability')
      }
    } else if (convertTree(x)[2] === phrases.s2) {
      if (Math.abs(
        x.p-pcfg.S[1].p*pcfg.Q[0].p*pcfg.VP[0].p*pcfg.PP[0].p*pcfg.NP[0].p
      )>1e-10) {
        error('wrong probability')
      }
    }
  } else if ([
    "all elephants live in Africa",
    "all elephants live in Asia",
    "most elephants live in Africa",
    "most elephants live in Asia",
    "some elephants live in Africa",
    "some elephants live in Asia"
  ].includes(pronounce(convertTree(x)))) {
    if (convertTree(x)[2] === phrases.s1) {
      if (Math.abs(
        x.p-pcfg.S[0].p*pcfg.Q[1].p*pcfg.VP[0].p*pcfg.PP[0].p*pcfg.NP[0].p
      )>1e-10) {
        error('wrong probability')
      }
    } else if (convertTree(x)[2] === phrases.s2) {
      if (Math.abs(
        x.p-pcfg.S[1].p*pcfg.Q[1].p*pcfg.VP[0].p*pcfg.PP[0].p*pcfg.NP[0].p
      )>1e-10) {
        error('wrong probability')
      }
    }
  }
}, likelyParses)

print('PCFG tested.')
~~~~

Here we define the literal listener, which assumes that its input is a syntax tree (not a string).

~~~~
var interpretSemanticsWithPrior = editor.get('interpretSemanticsWithPrior')
var pronounce = editor.get('pronounce')
var convertTree = editor.get('convertTree')
var likelyParses = editor.get('likelyParses')
var phrases = editor.get('phrases')
var lexicalEntries = editor.get('lexicalEntries')

// creates the world state, allowing for multiple kinds
var getWorldState = function(makeFeaturePrior) {
  return {
    "elephants": sample(makeFeaturePrior())
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

var possibleSentences = Categorical({
  vs: likelyParses,
  ps: map(function(y) {
    return y.p
  }, likelyParses)
})

var inferStructure = function(utteranceString) {
  Infer({model: function() {
    var unprocessedTree = sample(possibleSentences)
    var utteranceTree = convertTree(unprocessedTree)
    if (utteranceString.endsWith('.')) {
      condition(pronounce(utteranceTree) === utteranceString.slice(
        0, utteranceString.length-1
      ))
    } else {
      condition(pronounce(utteranceTree).startsWith(utteranceString))
    }
    return unprocessedTree
  }, method: 'enumerate'})
}

var listener0 = function(utteranceString, makeFeaturePrior, thresholdPrior, qud) {
  Infer({model: function() {
    var state = getWorldState(makeFeaturePrior)
    var utteranceTree = convertTree(sample(inferStructure(utteranceString)))
    var result = interpretSemanticsWithPrior(utteranceTree, state, thresholdPrior)
    condition(result)
    return state[qud]
  }, method: 'enumerate'})
}

editor.put('listener0Complete', listener0Complete)
editor.put('listener0', listener0)
editor.put('inferStructure', inferStructure)
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

print("example complete utterances")

var plotPosteriors = function(dist) {
  // plotting utility: plots the marginal distributions of:
  // 2-way joint of Africa and Asia
  // single variable of both
  // plots the mean of the three marginal distributions
  var marginalMe = marginalize(dist, function(x) {
    return x.me
  })
  viz(marginalMe)
  var marginalBoth = marginalize(dist, function(x) {
    return x.both
  })
  viz(marginalize(dist, function(x) {
    return {
      africa: x.africa,
      asia: x.asia
    }
  }))
  viz(marginalBoth)
  viz.bar(['Africa', 'Asia', 'both'], [
    expectation(marginalize(dist, function(x) {
      return x.africa
    })),
    expectation(marginalize(dist, function(x) {
      return x.asia
    })),
    expectation(marginalBoth)
  ])
}

editor.put('plotPosteriors', plotPosteriors)

var plainUtterance = [ 
  [
    lexicalEntries.gen, 
    lexicalEntries.elephants,
    phrases.qp
  ],
  [
    lexicalEntries.live,
    [
      lexicalEntries.in,
      lexicalEntries.africa,
      phrases.pp
    ],
    phrases.vp
  ],
  phrases.s2
]

print(pronounce(plainUtterance))
var posteriorPlainUtterance = listener0Complete(
  plainUtterance, makePrevalencePrior, makeThetaPrior(), "elephants"
)
plotPosteriors(posteriorPlainUtterance)

var conjunctiveUtteranceGen = [
  [
    lexicalEntries.gen,
    lexicalEntries.elephants,
    phrases.qp
  ],
  [
    [
      lexicalEntries.live,
      [
        lexicalEntries.in,
        lexicalEntries.africa,
        phrases.pp
      ],
      phrases.vp
    ],
    lexicalEntries.conj,
    [
      lexicalEntries.live,
      [
        lexicalEntries.in,
        lexicalEntries.asia,
        phrases.pp
      ],
      phrases.vp
    ],
    phrases.andP
  ],
  phrases.s1
]

print(pronounce(conjunctiveUtteranceGen))
print('sentence continuization 1')

var posteriorConjunctiveUtteranceGen = listener0Complete(
  conjunctiveUtteranceGen, makePrevalencePrior, makeThetaPrior(), "elephants"
)
plotPosteriors(posteriorConjunctiveUtteranceGen)

var conjunctiveUtteranceAnd = [
  [
    lexicalEntries.gen,
    lexicalEntries.elephants,
    phrases.qp
  ],
  [
    [
      lexicalEntries.live,
      [
        lexicalEntries.in,
        lexicalEntries.africa,
        phrases.pp
      ],
      phrases.vp
    ],
    lexicalEntries.conj,
    [
      lexicalEntries.live,
      [
        lexicalEntries.in,
        lexicalEntries.asia,
        phrases.pp
      ],
      phrases.vp
    ],
    phrases.andP
  ],
  phrases.s2
]

print(pronounce(conjunctiveUtteranceAnd))
print('sentence continuization 2')

var posteriorConjunctiveUtteranceAnd = listener0Complete(
  conjunctiveUtteranceAnd, makePrevalencePrior, makeThetaPrior(), "elephants"
)
plotPosteriors(posteriorConjunctiveUtteranceAnd)

var conjunctiveUtteranceSome = [
  [
    lexicalEntries.some,
    lexicalEntries.elephants,
    phrases.qp
  ],
  [
    [
      lexicalEntries.live,
      [
        lexicalEntries.in,
        lexicalEntries.africa,
        phrases.pp
      ],
      phrases.vp
    ],
    lexicalEntries.conj,
    [
      lexicalEntries.live,
      [
        lexicalEntries.in,
        lexicalEntries.asia,
        phrases.pp
      ],
      phrases.vp
    ],
    phrases.andP
  ],
  phrases.s2
]

print(pronounce(conjunctiveUtteranceSome))
print('sentence continuization 2')

var posteriorConjunctiveUtteranceSome = listener0Complete(
  conjunctiveUtteranceSome, makePrevalencePrior, makeThetaPrior(), "elephants"
)
plotPosteriors(posteriorConjunctiveUtteranceSome)
~~~~

~~~~
var lexicalEntries = editor.get('lexicalEntries')
var phrases = editor.get('phrases')
var listener0 = editor.get('listener0')
var makePrevalencePrior = editor.get('makePrevalencePrior')
var makeThetaPrior = editor.get('makeThetaPrior')
var pronounce = editor.get('pronounce')
var plotPosteriors = editor.get('plotPosteriors')
var inferStructure = editor.get('inferStructure')

print('example string utterances')

print("elephants live in Africa and live in Asia.")
var posteriorAmbiguous = listener0(
  "elephants live in Africa and live in Asia.", 
  makePrevalencePrior, makeThetaPrior(), "elephants"
)
plotPosteriors(posteriorAmbiguous)

print("elephants live in Africa and...")
var posteriorIncomplete = listener0(
  "elephants live in Africa and", 
  makePrevalencePrior, makeThetaPrior(), "elephants"
)
plotPosteriors(posteriorIncomplete)
~~~~
