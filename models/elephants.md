---
layout: model
title: elephants
model-language: webppl
model-language-version: v0.9.13
---

Here are the lexical entries.

~~~~
var genFunc = function(kind, state, theta) {
  return function(feature) {
    var prevalence = state[kind][feature]
    var threshold = theta[feature]
    return prevalence > threshold
  }
}

var mostFunc = function(kind, state) {
  return function(feature) {
    var prevalence = state[kind][feature]
    return prevalence >= 0.5
  }
}

var allFunc = function(kind, state) {
  return function(feature) {
    var prevalence = state[kind][feature]
    return prevalence >= .99
  }
}

var someFunc = function(kind, state) {
  return function(feature) {
    var prevalence = state[kind][feature]
    return prevalence > 0
  }
}

var generalizedConjFunc = function(a, b) {
  if (typeof a === "boolean" && typeof b === "boolean") {
    return a && b
  } else {
    return function(f) {
      return generalizedConjFunc(f(a), f(b))
    }
  }
}

var generalizedDisjFunc = function(a, b) {
  if (typeof a === "boolean" && typeof b === "boolean") {
    return a || b
  } else {
    return function(f) {
      return generalizedDisjFunc(f(a), f(b))
    }
  }
}

var semanticallyVacuousFunc = function(x) {
  return x
}

var lexicalEntries = {
  "gen": {
    pronounciation: "",
    semantics: genFunc,
    type: {
      in: "p", // p for probability distribution
      out: {
        in: "p",
        out: "t"
      }
    },
    contextDependent: true
  },
  "most": {
    pronounciation: "most",
    semantics: mostFunc,
    type: {
      in: "p", // p for probability distribution
      out: {
        in: "p",
        out: "t"
      }
    },
    contextDependent: true
  },
  "all": {
    pronounciation: "all",
    semantics: allFunc,
    type: {
      in: "p", // p for probability distribution
      out: {
        in: "p",
        out: "t"
      }
    },
    contextDependent: true
  },
  "some": {
    pronounciation: "some",
    semantics: someFunc,
    type: {
      in: "p", // p for probability distribution
      out: {
        in: "p",
        out: "t"
      }
    },
    contextDependent: true
  },
  "disj": {
    pronounciation: "or",
    semantics: generalizedDisjFunc,
    type: {
      in: "t",
      out: {
        "in": "t",
        "out": "t"
      }
    },
    contextDependent: false
  },
  "conj": {
    pronounciation: "and",
    semantics: generalizedConjFunc,
    type: {
      in: "t",
      out: {
        in: "t", 
        out: "t"
      }
    },
    contextDependent: false
  },
  "asia": {
    pronounciation: "Asia",
    semantics: "asia",
    type: "p",
    contextDependent: false
  },
  "elephants": {
    pronounciation: "elephants",
    semantics: "elephants",
    type: "p",
    contextDependent: false
  },
  "africa": {
    pronounciation: "Africa",
    semantics: "africa",
    type: "p",
    contextDependent: false
  },
  "live": {
    pronounciation: "live",
    semantics: semanticallyVacuousFunc,
    type: {
      in: "p",
      out: "p"
    },
    contextDependent: false
  },
  "in": {
    pronounciation: "in",
    semantics: semanticallyVacuousFunc,
    type: {
      in: "p",
      out: "p"
    },
    contextDependent: false
  },
  "eat": {
    pronounciation: "eat",
    semantics: "vacuous",
    type: "p",
    contextDependent: false
  },
  "rcBugs": {
    pronounciation: "where there are bugs",
    semantics: semanticallyVacuousFunc,
    type: {
      in: "p",
      out: "p"
    },
    contextDependent: false
  },
  'because': {
    pronounciation: "because",
    semantics: "vacuous",
    type: {
      in: "t",
      out: "t"
    }
  }
}
~~~~

The PCFG is defined below.

~~~~
var pcfgTransition = function(symbol) {
  var rules = {'S': {rhs: [['S', 'SBar'], ['NP', 'VP']], probs: [0.07, 0.93]},
               'NP': {rhs: [['Q', 'N']], probs: [1.0]},
               'SBar': {rhs: [['C', 'S']], probs: [1.0]},
               'DNP': {rhs: [['PN', 'RC'],
                             ['PN', 'Conj', 'PN']], probs: [0.84, 0.16]},
               'VP': {rhs: [['V', 'PP'],
                            ['VP', 'Conj', 'VP']], probs: [0.1, 0.9]},
               'PP': {rhs: [['P', 'DNP']], probs: [1.0]} }
  return rules[symbol].rhs[ discrete(rules[symbol].probs) ]
}

var preTerminal = function(symbol) {
  return symbol=='N' | symbol=='V' | symbol=='P' | symbol=='RC' |
    symbol=='C' | symbol=='Conj' | symbol=='PN' | symbol=='Q'
}

var terminal = function(symbol) {
  var rules = {
    'Q': {
      words: [
        lexicalEntries.gen, lexicalEntries.some, lexicalEntries.most, lexicalEntries.all
      ],
      probs: [0.25, 0.25, 0.25, 0.25]
      //probs: [0.004, 0.286, 0.229, 0.481]
    },
    'N': 
    {
      words: [lexicalEntries.elephants],  
      probs: [1.0]
    },
    'PN': 
    {
      words: [
        lexicalEntries.asia, 
        lexicalEntries.africa
      ], 
      probs: [0.382, 0.618]
    },
    'V': {
      words: [
        lexicalEntries.live,
        lexicalEntries.eat
      ],  
      probs: [0.731, 0.269]
    },
    'P': {words: [lexicalEntries.in], probs: [1.0]},
    'RC': {words: [lexicalEntries.rcBugs], probs: [1.0]},
    'C': {words: [lexicalEntries.because], probs: [1.0]},
    'Conj': {words: [lexicalEntries.conj], probs: [1.0]}
  }
  return rules[symbol].words[ discrete(rules[symbol].probs) ]
}

var pcfgGenerator = function(symbol) {
  preTerminal(symbol) ? [terminal(symbol)] : [expand(pcfgTransition(symbol))]
}

var expand = function(symbols) {
  if(symbols.length==0) {
    return null
  } else {
    var f = pcfgGenerator(symbols[0])
    var value = expand(symbols.slice(1));
    if (value == null) {
      return f
    } else {
      f.concat(value)
    }
  }
}

// distribution over possible sentences
var pcfg = Infer({model: function() {
  return pcfgGenerator("S")[0]
}, method: 'enumerate', maxExecutions: 100});
~~~~

The pronounciation function flattens a syntax tree into a string.

~~~~
// given an utterance as a syntax tree, return a string representing its
// pronounciation
var pronounce = function(struct) {
  if (struct.hasOwnProperty('type')) {
    return struct.pronounciation
  } else if (struct.length === 3) {
    var leftPronounciation = pronounce(struct[0])
    var rightPronounciation = pronounce(struct[2])
    var connective = pronounce(struct[1])
    return leftPronounciation + ' ' + connective + ' ' + rightPronounciation
  } else {
    var leftPronounciation = pronounce(struct[0])
    var rightPronounciation = pronounce(struct[1])
    if (leftPronounciation === "" || rightPronounciation === "") {
      return leftPronounciation + rightPronounciation
    } else {
      return leftPronounciation + ' ' + rightPronounciation
    }
  }
}
~~~~

Here we define the world knowledge.

~~~~
// wrapper
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

// makes 2-way joint on living in africa, and living in asia
var makePrevalencePrior = function(me) {
  return Infer({model: function(){
    var p =  {
      asia: sample(
        // binned beta distribution on prior prevalence of living in Asia
        makeComponent(priorParameters)
      ),
      africa: sample(
        // binned beta distribution on prior prevalence of living in Africa
        makeComponent(priorParameters)
      ),
    }
    var totalPrev = p.asia + p.africa 
    // if correlated, force mutual exclusivity by decreasing probability of the total
    // prevalence being greater than 1
    factor(me ? totalPrev > 1 ? -50*Math.log(totalPrev) : 0 : 0)
    return p
  }})
}

print('prevalence prior')
viz(makePrevalencePrior(true))
~~~~

The semantic interpretation function and associated utilities are defined below.

~~~~
// independent threshold values for both predicates
var makeThetaPrior = function() {
  return Infer({model: function(){
    var sampleTheta = function(){ uniformDraw(theta_bins) }
    var asia = sampleTheta();
    var africa = sampleTheta();
    return { asia, africa }
  }})
};

var functionallyApply = function(func, arg, state, theta) {
  var funcSemantics = func.semantics;
  var argSemantics = arg.semantics;
  return {
    type: func.type.out,
    semantics: func.contextDependent ?
    funcSemantics(argSemantics, state, theta) :
    funcSemantics(argSemantics),
    contextDependent: false
  }
}

var coordinate = function(thunk, applicator, state, theta) {
  var thunkSemantics = thunk.semantics;
  var applicatorSemantics = applicator.semantics;
  return {
    type: applicator.type.out,
    semantics: applicator.contextDependent ?
    thunkSemantics(applicatorSemantics, state, theta) :
    thunkSemantics(applicatorSemantics),
    contextDependent: false,
    coord: true
  }
}

var interpretSemantics = function(utterance, state, theta) {
  // returns a truth value for declarative sentences
  if (utterance.hasOwnProperty('type')) {
    // base case: single lexical entry
    return utterance
  } else if (utterance.length > 2) {
    // ternary branching: coordination
    var left = interpretSemantics(utterance[0], state, theta)
    var right = interpretSemantics(utterance[2], state, theta)
    if (left.type != right.type) {
      print('type mismatch in coordination')
      print(left.type)
      print(right.type)
      print(utterance)
    }
    if (left.semantics === "vacuous") {
      return right
    }
    if (right.semantics === "vacuous") {
      return left
    }
    if (left.semantics === right.semantics) {
      return left
    }
    var coordFunc = utterance[1].semantics
    return {
      type: left.type,
      semantics: coordFunc(left.semantics, right.semantics),
      contextDependent: false,
      coord: true
    }
  } else {
    var left = interpretSemantics(utterance[0], state, theta)
    var right = interpretSemantics(utterance[1], state, theta)
    if (right.semantics === "vacuous") {
      return {
        semantics: "vacuous",
        type: right.type
      }
    } else if (left.semantics === "vacuous") {
      return {
        semantics: "vacuous",
        type: left.type
      }
    } else if (left.coord && !right.coord) {
      return coordinate(left, right, state, theta)
    } else if (right.coord && !left.coord) {
      return coordinate(right, left, state, theta)
    } else if (typeof left.type === 'object' 
               && typeof right.type !== 'object' 
               && left.type.in === right.type) {
      return functionallyApply(left, right, state, theta)
    } else if (typeof right.type === 'object' 
               && typeof left.type !== 'object' 
               && right.type.in === left.type) {
      return functionallyApply(right, left, state, theta)
    } else if (left.type.in === right.type) {
      return functionallyApply(left, right, state, theta)
    } else if (right.type.in === left.type) {
      return functionallyApply(right, left, state, theta)
    } else {
      print('error')
      print(utterance)
    }
  }
}

var interpretSemanticsWithPrior = function(utterance, state, thresholdPrior) {
  // wrapper to sample over language-based priors
  var theta = sample(thresholdPrior)
  return interpretSemantics(utterance, state, theta)
}
~~~~

The literal listener is defined here.

~~~~
var getWorldState = function(makeFeaturePrior) {
  return {
    "elephants": sample(makeFeaturePrior(true))
  }
}

var listener0Complete = function(utterance, makeFeaturePrior, thresholdPrior, qud) {
  Infer({model: function() {
    var state = getWorldState(makeFeaturePrior)
    var result = interpretSemanticsWithPrior(utterance, state, thresholdPrior)
    // condition on utterance being true
    condition(result.semantics)
    return state[qud]
  }, method: 'enumerate'});
}

// models the literal listener: given an utterance fragment, the listener samples a
// complete utterance from the pcfg, conditions on the pronounciation of the utterance
// being consistent with the fragment, and conditions on the complete utterance
// being true
var listener0 = function(utteranceFragment, pcfg, makeFeaturePrior, thresholdPrior, qud) {
  Infer({model: function() {
    var sampledUtterance = sample(pcfg)
    var state = getWorldState(makeFeaturePrior)
    // condition on utterance being consistent with fragment
    condition(pronounce(sampledUtterance).startsWith(utteranceFragment))
    var completeUtterance = sampledUtterance[1][0].pronounciation === "because" ? 
        sampledUtterance[0] : sampledUtterance
    var result = interpretSemanticsWithPrior(completeUtterance, state, thresholdPrior)
    // condition on utterance being true
    condition(result.semantics)
    return state[qud]
  }, method: 'enumerate'});
}
~~~~

Examples of model interpretations.

~~~~
var conjunctiveUtterance = [ 
  [
    lexicalEntries.most, 
    lexicalEntries.elephants
  ],
  [
    lexicalEntries.live,
    [
      lexicalEntries.in,
      [
        lexicalEntries.africa,
        lexicalEntries.conj,
        lexicalEntries.asia
      ]
    ]
  ]
]

print(pronounce(conjunctiveUtterance))

viz(marginalize(
  listener0Complete(conjunctiveUtterance, makePrevalencePrior, makeThetaPrior(), "elephants"),
  function(x) {
    return {
      africa: x.africa,
      asia: x.asia
    }
  }
))

var plainUtterance = [ 
  [
    lexicalEntries.gen, 
    lexicalEntries.elephants
  ],
  [
    lexicalEntries.live,
    [
      lexicalEntries.in,
      lexicalEntries.africa,
    ]
  ]
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

print("elephants live in Africa and")
viz(marginalize(
  listener0("elephants live in Africa and", pcfg, makePrevalencePrior, makeThetaPrior(), "elephants"),
  function(x) {
    return {
      africa: x.africa,
      asia: x.asia
    }
  }
))
~~~~
