---
layout: model
title: Incremental pragmatics
model-status: code
model-category: Reasoning about Reasoning
model-tags: language, pragmatics, reference, incrementality
model-language: webppl
model-language-version: v0.9.7
---

[Cohn-Gordon et al. (2019)](https://scholarworks.umass.edu/scil/vol2/iss1/10/) incrementalize the RSA model of referring expression production. On their proposal, the speaker production probability of a word reflects the incremental communicative utility of producing that word (given the speaker's communicative intention and what the speaker has said thus far).  

The following is a WebPPL replication of Cohn-Gordon et al. (2019)'s calculations (Figure 1 of their paper). See the paper for prose explication of the models. 

~~~
var semantics = function (state) {
  return {
    red: ["R1", "R3"].includes(state) ? 1 : 0,
    dress: ["R1", "R2"].includes(state) ? 1 : 0,
    object: 1,
    STOP: 1,
    START: 1,
  };
};

var alpha = 1;

var wordCost = {
  red: 0,
  dress: 0,
  object: 0,
  STOP: 0,
  START: 0,
};

var states = ["R1", "R2", "R3"];

var words = ["red", "dress", "object", "STOP", "START"];

var utterances = [
  "START dress STOP",
  "START red dress STOP",
  "START red object STOP",
];

// safeDivide, getTransitions, licitTransitions: helper functions for incremental models

var safeDivide = function (x, y) {
  if (y == 0) {
    return 0;
  } else {
    return x / y;
  }
};

var getTransitions = function (str) {
  var result = [];
  var splitStr = str.split(" ");
  var indices = _.range(splitStr.length);
  map(function (i) {
    var transition = splitStr.slice(0, i + 1).join(" ");
    result.push(transition);
  }, indices);
  return result;
};

var licitTransitions = _.uniq(
  _.flatten(
    map(function (x) {
      return getTransitions(x);
    }, utterances)
  )
);

var wordPrior = function () {
  return uniformDraw(words);
};

var stringCost = function (string) {
  var wordcosts = map(function (x) {
    return wordCost[x];
  }, string);
  return sum(wordcosts);
};

var stringMeanings = function (context, state) {
  var cSplit = context.split(" ");
  var meaning = semantics(state);
  return reduce(
    function (x, acc) {
      return meaning[x] * acc;
    },
    1,
    cSplit
  );
};

// stringSemantics: defined according to Cohn-Gordon et al. (2019), in prose on the bottom of page 83
// outputs values on the interval [0,1]: a string s's semantic value at a world w
// is the sum of semantic values of complete continuations of s true at w,
// divided by the total number of complete continuations of s:
var stringSemantics = function (context, state) {
  var allContinuations = filter(function (x) {
    return x.startsWith(context);
  }, utterances);
  var trueContinuations = reduce(
    function (x, acc) {
      return stringMeanings(x, state) + acc;
    },
    0,
    allContinuations
  );
  return safeDivide(trueContinuations, allContinuations.length);
};

// the normal, utterance-level RSA literal listener
var globalLiteralListener = function (utterance) {
  return Infer({model: function () {
    var state = uniformDraw(states);
    var meaning = stringMeanings(utterance, state);
    condition(meaning);
    return state;
  }
               });
};

// the normal, utterance-level RSA pragmatic speaker
var globalUtteranceSpeaker = cache(function (state) {
  return Infer({
    model: function () {
      var utterance = uniformDraw(utterances);
      var listener = globalLiteralListener(utterance);
      factor(
        alpha * (listener.score(state) - stringCost(utterance.split(" ")))
      );
      return utterance;
    },
  });
});

// L0^{WORD} from Cohn Gordon et al. (2019): defined according to equation (4) of that paper
var incrementalLiteralListener = function (string) {
  return Infer({
    model: function () {
      var state = uniformDraw(states);
      var meaning = Math.log(stringSemantics(string, state));
      factor(meaning);
      return state;
    },
  });
};

// S1^{WORD} from Cohn Gordon et al. (2019): defined according to equation (5) of that paper
var wordSpeaker = function (context, state) {
  return Infer({
    model: function () {
      var word = wordPrior();
      var newContext = context.concat([word]);
      // grammar constraint: linear order must be allowed in language
      condition(licitTransitions.includes(newContext.join(" ")));
      // note: condition basically goes away
      var result =
        stringMeanings(context.join(" "), state) == 0
          ? 1
          : alpha *
            (incrementalLiteralListener(newContext.join(" ")).score(
              state
            ) -
              stringCost(newContext));
      factor(result);
      return word;
    },
  });
};

// L1^{WORD} from Cohn Gordon et al. (2019): defined according to equation (6) of that paper
var pragmaticWordListener = function (word, context) {
  return Infer({
    model: function () {
      var state = uniformDraw(states);
      factor(wordSpeaker(context,state).score(word));
      return state;
    },
  });
};

// S1^{UTT-IP} from Cohn Gordon et al. (2019): defined according to equation (7) of that paper
var incrementalUtteranceSpeaker = cache(function (utt, state) {
  var string = utt.split(" ");
  var indices = _.range(string.length);
  var probs = map(function (i) {
    var context = string.slice(0, i);
    //print(context)
    return Math.exp(wordSpeaker(context, state).score(string[i]));
  }, indices);
  return reduce(
    function (x, acc) {
      return x * acc;
    },
    1,
    probs
  );
}, 100000);

print("Replicating Figure 1-c: incremental word-level predictions from S1^{WORD} (for R1):")

print("P(word|c = START, w= R1):")

viz.table(wordSpeaker(["START"], "R1"));

print("P(word|c = START red, w= R1):")

viz.table(wordSpeaker(["START", "red"], "R1"));

print("--------------------------")

print("Replicating Figure 1-d: incremental listener predictions from L1^{WORD} (for 'red'):")

viz.table(pragmaticWordListener('red', ["START"]))

print("Replicating Figure 1-e: incremental utterance-level predictions from S1^{UTT-IP}:")

print("P('dress'|R1): " + incrementalUtteranceSpeaker("START dress STOP", "R1"))
print("P('red dress'|R1): " + incrementalUtteranceSpeaker("START red dress STOP", "R1"))
print("P('red object'|R1): " + incrementalUtteranceSpeaker("START red object STOP", "R1"))
print("P('dress'|R2): " + incrementalUtteranceSpeaker("START dress STOP", "R2"))
print("P('red dress'|R2): " + incrementalUtteranceSpeaker("START red dress STOP", "R2"))
print("P('red object'|R2): " + incrementalUtteranceSpeaker("START red object STOP", "R2"))
print("P('dress'|R3): " + incrementalUtteranceSpeaker("START dress STOP", "R3"))
print("P('red dress'|R3): " + incrementalUtteranceSpeaker("START red dress STOP", "R3"))
print("P('red object'|R3): " + incrementalUtteranceSpeaker("START red object STOP", "R3"))
~~~
