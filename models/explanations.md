---
layout: model
title: Explanations
model-status: code
model-language: webppl
---

<script src='http://web.stanford.edu/~erindb/webppl-viz/webppl.min.js'></script>  
<link rel='stylesheet' href='http://web.stanford.edu/~erindb/webppl-viz/viz.css'>

<script src='https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML'></script>

* toc
{:toc}

We explore the hypothesis that explanations can be thought of as counterfactual statements that are especially relevant and informative in a particular context.

## Basic counterfactual model: bacon example

We use Lucas and Kemp's Extended Structural Model (ESM) of counterfactuals ([Lucas & Kemp, 2015](http://philpapers.org/archive/LUCAIP.pdf)) as the underlying counterfactual reasoning for our explanations models.

In their paper, Lucas & Kemp use the following example:

We usually cook bacon for dinner, and when we do the smoke alarm usually goes off. Our neighbors are sometimes annoyed with us for other reasons (or for no reason at all), but they will *always* be angry if the smoke alarm goes off.

Given this underlying model, suppose we know that bacon was cooked, the smoke alarm went off, and the neighbors are angry. Counterfactually, if the neighbors weren't angry, would bacon have been cooked?

~~~
// ======== model specification =========

// exogenous random variables
var rand = function() {
  return {
    uB: flip(0.9), // bacon is a priori likely
    uS: flip(0.9), // smoke alarm is likely, given bacon
    uN: flip(0.1) // probability of neighbors being angry even if the smoke alarm doesn't go off is low
  };
};

// endogenous variables are functions of the random state
var vars = function(rVs) {
  var bacon = rVs.uB;
  var smokeAlarm = and(bacon, rVs.uS); // smoke alarm will only go off if bacon is cooked, but it might not even if you do cook bacon.
  var neighbors = or(smokeAlarm, rVs.uN); // neighbors *will get angry* if the smoke alarm goes off. they might even if it doesn't, though.
  return {
    bacon: bacon,
    smokeAlarm: smokeAlarm,
    neighbors: neighbors
  };
};

// ======== parameters and counterfactual functions =========

// parameter for how similar counterfactual world is to actual world
var stickiness = 0.5;

var stickyRand = function(actualRVs) {
  var freshRVs = rand();
  return mapObj(function(val, key) {
    return flip(stickiness) ? actualRVs[key] : freshRVs[key];
  }, actualRVs);
};

var contextualEval = cache(function(proposition, world) {
  var context = 'var bacon = ' + JSON.stringify(world.bacon) + ';' +
      'var smokeAlarm = ' + JSON.stringify(world.smokeAlarm) + ';' +
      'var neighbors = ' + JSON.stringify(world.neighbors) + ';';
  return(webpplEval(context + proposition));
});

var counterfactualERP = function(ifA, thenB, actualRVs) {
  return Enumerate(function() {
    var counterfactualRVs = stickyRand(actualRVs);
    var counterfactualWorld = vars(counterfactualRVs);
    condition(contextualEval(ifA, counterfactualWorld));
    return contextualEval(thenB, counterfactualWorld);
  });
};

// ======== example =========

var actualRVs = {uB: true, uS: true, uN: true};

// in the counterfactual world, if the neighbors were angry, P(bacon) ~ 0.487
print(counterfactualERP('!neighbors', 'bacon', actualRVs));

// as long as the smoke alarm went off, the neighbors would *always* be angry
//print(counterfactualERP('smokeAlarm', 'neighbors', actualRVs));
~~~

## "Literal explanation": bacon example

What if we give the explanation, "The neighbors are angry because I cooked bacon."" What can we infer from this?

Intuitively, we can infer that the smoke alarm actually went off. If it hadn't, then the link between bacon and the neighbors being angry would be less strong.

We treat the "literal meaning" of such an explanation as "The neighbors are angry, I cooked bacon, and if (counterfactually) I hadn't cooked bacon, the neighbors wouldn't have been angry.""

If we use this literal interpretation, then a listener who hears "The neighbors are angry *because* I cooked bacon," will be *slightly* more likely to believe that the smoke alarm went off than if they heard the sentence, "The neighbors are angry *and* I cooked bacon." But the difference is small, since the probability of smoke alarm given bacon and neighbors is already basically at ceiling.

~~~
///fold:
// ======== model specification =========

// exogenous random variables
var rand = function() {
  return {
    uB: flip(0.9), // bacon is a priori likely
    uS: flip(0.9), // smoke alarm is likely, given bacon
    uN: flip(0.1) // probability of neighbors being angry even if the smoke alarm doesn't go off is low
  };
};

// endogenous variables are functions of the random state
var vars = function(rVs) {
  var bacon = rVs.uB;
  var smokeAlarm = and(bacon, rVs.uS); // smoke alarm will only go off if bacon is cooked, but it might not even if you do cook bacon.
  var neighbors = or(smokeAlarm, rVs.uN); // neighbors *will get angry* if the smoke alarm goes off. they might even if it doesn't, though.
  return {
    bacon: bacon,
    smokeAlarm: smokeAlarm,
    neighbors: neighbors
  };
};

// ======== parameters and counterfactual functions =========

// parameter for how similar counterfactual world is to actual world
var stickiness = 0.5;

var stickyRand = function(actualRVs) {
  var freshRVs = rand();
  return mapObj(function(val, key) {
    return flip(stickiness) ? actualRVs[key] : freshRVs[key];
  }, actualRVs);
};

var contextualEval = cache(function(proposition, world) {
  var context = 'var bacon = ' + JSON.stringify(world.bacon) + ';' +
      'var smokeAlarm = ' + JSON.stringify(world.smokeAlarm) + ';' +
      'var neighbors = ' + JSON.stringify(world.neighbors) + ';';
  return(webpplEval(context + proposition));
});

var counterfactualERP = function(ifA, thenB, actualRVs) {
  return Enumerate(function() {
    var counterfactualRVs = stickyRand(actualRVs);
    var counterfactualWorld = vars(counterfactualRVs);
    condition(contextualEval(ifA, counterfactualWorld));
    return contextualEval(thenB, counterfactualWorld);
  });
};
///
// ======== literal meaning functions =========

var because = function(conditionA, resultB, actualWorld, actualRVs) {
  var actuallyA = contextualEval(conditionA, actualWorld);
  var actuallyB = contextualEval(resultB, actualWorld);

  // debugging:
  //print(conditionA + "? " + actuallyA);
  //print(resultB + "? " + actuallyB);

  if (actuallyA & actuallyB) {
    // debugging:
    //print("both true!");

    var ifNotA = "!" + conditionA;
    var thenNotB = "!" + resultB;

    // debugging:
    //print("if " + ifNotA + " then " + thenNotB);

    counterfactualERP(ifNotA, thenNotB, actualRVs).score([], true);
  } else {
    return -Infinity;
  }
};

/*
   meaning_factor is -Infinity if the utterance is completely false
   in the world, 0 if the utterance is true, and for "because" utterances
   it is the proportion of counterfactual worlds satisfying conditionA
   in which resultB is also true.
*/
var meaning_factor = cache(function(utterance, world, rVs) {
  if (utterance == 'nothing') {
    return 0;
  } else {
    var words = utterance.split(' ');
    if (words.length < 2 | words[1] != 'because') {
      return contextualEval(utterance, world) ? 0 : -Infinity;
    } else {
      // "resultB because conditionA"
      var resultB = words[0];
      var conditionA = words[2];

      // debugging:
      //print("because " + conditionA + ", " + resultB);

      return because(conditionA, resultB, world, rVs);
    }
  }
});

var literalERP = function(utterance, QUD) {
  return Enumerate(function() {
    var actualRVs = rand();
    var actualWorld = vars(actualRVs);
    factor(meaning_factor(utterance, actualWorld, actualRVs));
    if (QUD) {
      // take subset of world if question under discussion is specified
      return actualWorld[QUD];
    } else {
      return actualWorld;
    }
  });
};

// ======== example =========

/*
// check meaning_factor:
var actualRVs = {uB: true, uS: true, uN: true};
var actualWorld = vars(actualRVs);

print(meaning_factor('bacon', actualWorld, actualRVs));
print(meaning_factor('!bacon', actualWorld, actualRVs));
print(meaning_factor('neighbors because bacon', actualWorld, actualRVs));
*/

print(literalERP('neighbors & bacon', 'smokeAlarm'));
print(literalERP('neighbors because bacon', 'smokeAlarm'));
~~~

## Pragmatic interpretation: bacon example

Adding a layer of pragmatic inference doesn't change this example much, but it does strengthen the inferences.

Note: the alternatives in the speaker model here make a difference to the exact inferences that are made. A much fuller set of alternatives results in similar (albeit *much* slower) inferences for this example.

~~~
///fold:
// ======== model specification =========

// exogenous random variables
var rand = function() {
  return {
    uB: flip(0.9), // bacon is a priori likely
    uS: flip(0.9), // smoke alarm is likely, given bacon
    uN: flip(0.1) // probability of neighbors being angry even if the smoke alarm doesn't go off is low
  };
};

// endogenous variables are functions of the random state
var vars = function(rVs) {
  var bacon = rVs.uB;
  var smokeAlarm = and(bacon, rVs.uS); // smoke alarm will only go off if bacon is cooked, but it might not even if you do cook bacon.
  var neighbors = or(smokeAlarm, rVs.uN); // neighbors *will get angry* if the smoke alarm goes off. they might even if it doesn't, though.
  return {
    bacon: bacon,
    smokeAlarm: smokeAlarm,
    neighbors: neighbors
  };
};

// ======== parameters and counterfactual functions =========

// parameter for how similar counterfactual world is to actual world
var stickiness = 0.5;

var stickyRand = function(actualRVs) {
  var freshRVs = rand();
  return mapObj(function(val, key) {
    return flip(stickiness) ? actualRVs[key] : freshRVs[key];
  }, actualRVs);
};

var contextualEval = cache(function(proposition, world) {
  var context = 'var bacon = ' + JSON.stringify(world.bacon) + ';' +
      'var smokeAlarm = ' + JSON.stringify(world.smokeAlarm) + ';' +
      'var neighbors = ' + JSON.stringify(world.neighbors) + ';';
  return(webpplEval(context + proposition));
});

var checkPossible = function(unfactoredERP) {
  var factorERP = Enumerate(function() {
    return sample(unfactoredERP).factor;
  });
  if (factorERP.score([], -Infinity) == 0) {
    // a sort of way of throwing errors if we try to evaluate an impossible utterance
    return 'impossible';
  } else {
    return Enumerate(function() {
      var result = sample(unfactoredERP);
      factor(result.factor);
      return result.result;
    });
  }
};

var counterfactualERP = function(ifA, thenB, actualRVs) {
  //print(ifA);
  //print(thenB);
  //print(actualRVs);
  var unfactoredERP = Enumerate(function() {
    var counterfactualRVs = stickyRand(actualRVs);
    //print(counterfactualRVs);
    var counterfactualWorld = vars(counterfactualRVs);
    //print(counterfactualWorld);
    return {
      result: contextualEval(thenB, counterfactualWorld),
      factor: (contextualEval(ifA, counterfactualWorld) ? 0 : -Infinity)
    }
  });
  //unfactoredERP.print();
  return checkPossible(unfactoredERP);
};

// ======== literal meaning functions =========

var because = function(conditionA, resultB, actualWorld, actualRVs) {
  var actuallyA = contextualEval(conditionA, actualWorld);
  var actuallyB = contextualEval(resultB, actualWorld);

  // debugging:
  //print(conditionA + "? " + actuallyA);
  //print(resultB + "? " + actuallyB);

  if (actuallyA & actuallyB) {
    // debugging:
    //print("both true!");

    var ifNotA = "!" + conditionA;
    var thenNotB = "!" + resultB;

    // debugging:
    //print("if " + ifNotA + " then " + thenNotB);

    var cfERP = counterfactualERP(ifNotA, thenNotB, actualRVs);
    //print("because " + conditionA + ", " + resultB);
    //cfERP.print();
    return cfERP == 'impossible' ? -Infinity : cfERP.score([], true);;
  } else {
    return -Infinity;
  }
};

/*
   meaning_factor is -Infinity if the utterance is completely false
   in the world, 0 if the utterance is true, and for "because" utterances
   it is the proportion of counterfactual worlds satisfying conditionA
   in which resultB is also true.
*/
var meaning_factor = cache(function(utterance, world, rVs) {
  if (utterance == 'nothing') {
    return 0;
  } else {
    var words = utterance.split(' ');
    if (words.length < 2 | words[1] != 'because') {
      return contextualEval(utterance, world) ? 0 : -Infinity;
    } else {
      // "resultB because conditionA"
      var resultB = words[0];
      var conditionA = words[2];

      // debugging:
      //print("because " + conditionA + ", " + resultB);

      return because(conditionA, resultB, world, rVs);
    }
  }
});

var literalERP = cache(function(utterance) {
  var unfactoredERP = Enumerate(function() {
    var actualRVs = rand();
    var actualWorld = vars(actualRVs);
    //print(actualWorld);
    return {
      result: actualWorld,
      factor: meaning_factor(utterance, actualWorld, actualRVs)
    };
  });
  return checkPossible(unfactoredERP);
});

// a small set of alternative utterances
var utterances = [
  'nothing',
  'bacon', 'smokeAlarm', 'neighbors',
  '!bacon', '!smokeAlarm', '!neighbors',
  'bacon & smokeAlarm', 'bacon & neighbors', 'smokeAlarm & neighbors',
  'neighbors because bacon'
];

// full set of alternatives
// WARNING: TAKES A COUPLE MINUTES TO RUN
/*var utterances = [
  'nothing',

  'bacon', 'smokeAlarm', 'neighbors',
  '!bacon', '!smokeAlarm', '!neighbors',

  'bacon & smokeAlarm', 'bacon & neighbors', 'bacon & !smokeAlarm', 'bacon & !neighbors',
  'smokeAlarm & neighbors', 'smokeAlarm & !neighbors',
  '!bacon & smokeAlarm', '!bacon & neighbors', '!bacon & !smokeAlarm', '!bacon & !neighbors',
  '!smokeAlarm & neighbors', '!smokeAlarm & !neighbors',

  'bacon because smokeAlarm', 'bacon because neighbors', 'bacon because !smokeAlarm', 'bacon because !neighbors',
  'smokeAlarm because neighbors', 'smokeAlarm because !neighbors',
  '!bacon because smokeAlarm', '!bacon because neighbors', '!bacon because !smokeAlarm', '!bacon because !neighbors',
  '!smokeAlarm because neighbors', '!smokeAlarm because !neighbors',

  'smokeAlarm because bacon', 'neighbors because bacon', '!smokeAlarm because bacon', '!neighbors because bacon',
  'neighbors because smokeAlarm', '!neighbors because smokeAlarm',
  'smokeAlarm because !bacon', 'neighbors because !bacon', '!smokeAlarm because !bacon', '!neighbors because !bacon',
  'neighbors because !smokeAlarm', '!neighbors because !smokeAlarm',

  'bacon & smokeAlarm & neighbors', 'bacon & smokeAlarm & !neighbors',
  'bacon & !smokeAlarm & neighbors', 'bacon & !smokeAlarm & !neighbors',
  '!bacon & smokeAlarm & neighbors', '!bacon & smokeAlarm & !neighbors',
  '!bacon & !smokeAlarm & neighbors', '!bacon & !smokeAlarm & !neighbors'
];*/

var alpha = 5;

var costs = map(function(utterance) {
  if (utterance == "nothing") {
    return 0;
  } else {
    var words = utterance.split(" ");
    var chars = utterance.split("");
    var negs = filter(function(x) {return x == "!";}, chars);
    return words.length;
  }
}, utterances);

var utterancePrior = function() {
  var probabilities = map(function(x) {return Math.exp(-x);}, costs);
  return utterances[discrete(probabilities)];
};

var speakerERP = cache(function(world) {
  var unfactoredERP = Enumerate(function() {
    var utterance = utterancePrior();
    //print(utterance);
    var interpretation = literalERP(utterance);
    return {
      factor: interpretation == 'impossible' ? -Infinity : interpretation.score([], world) * alpha,
      result: utterance
    };
  });
  return checkPossible(unfactoredERP);
});

var listenerERP = function(utterance) {
  return Enumerate(function() {
    var actualRVs = rand();
    var actualWorld = vars(actualRVs);
    //print(actualWorld);
    var description = speakerERP(actualWorld);
    factor(
      description == 'impossible' ? -Infinity : description.score([], utterance) * alpha
    );
    return actualWorld.smokeAlarm;
  });
};

// ======== example =========

//print(speakerERP({bacon: true, smokeAlarm: true, neighbors: true}))

/*
// check meaning_factor:
var actualRVs = {uB: true, uS: true, uN: true};
var actualWorld = vars(actualRVs);

print(meaning_factor('bacon', actualWorld, actualRVs));
print(meaning_factor('!bacon', actualWorld, actualRVs));
print(meaning_factor('neighbors because bacon', actualWorld, actualRVs));
*/

print(listenerERP('bacon & neighbors'));
print(listenerERP('neighbors because bacon'));
~~~

## Tug-of-war

Suppose Alice and Bob are playing tug-of-war with each other. Alice pulls on one end of the rope and Bob pulls on the other. Either player might be strong or weak, lazy or determined. If a strong player is lazy, they will lose around half the time to a weak but determined player. A lazy player will always lose to a player with the same strength as them who is trying hard.

Think about the following sentences and the inferences we can draw from them:

* "Alice won because Bob was lazy."
  - This seems to imply that Bob is strong and Alice is weak.
* "Alice won because Alice is strong."
  - Bob probably tried.
* "Alice won because Alice tried."
  - Alice and Bob probably have similar strengths.*
* "Alice won because Bob was weak."
  - Alice is probably also weak.*

These inferences may be sensitive to this set of alternatives in the model.

~~~
// ======== model specification =========

// exogenous random variables
var rand = function() {
  return {
    uAS: flip(0.5),
    uAL: flip(0.3),
    uBS: flip(0.5),
    uBL: flip(0.3),
    uAW: flip(0.5)
  };
};

// endogenous variables are functions of the random state
var vars = function(rVs) {
  // strong is strength = 2; weak is strength = 1
  var aliceStrong = rVs.uAS;
  var aliceStrength = aliceStrong ? 2 : 1;
  var aliceLazy = rVs.uAL;
  var bobStrong = rVs.uBS;
  var bobStrength = bobStrong ? 2 : 1;
  var bobLazy = rVs.uBL;
  // laziness decreases strength by half
  var alicePulling = aliceLazy ? aliceStrength/2 : aliceStrength;
  var bobPulling = bobLazy ? bobStrength/2 : bobStrength;
  var aliceWin = (alicePulling>bobPulling)?true:(bobPulling>alicePulling)?false:rVs.uAW;
  
  return {
    aliceStrong: aliceStrong,
    aliceLazy: aliceLazy,
    bobStrong: bobStrong,
    bobLazy: bobLazy,
    aliceWin: aliceWin
  };
};

// ======== parameters and counterfactual functions =========

// parameter for how similar counterfactual world is to actual world
var stickiness = 0.5;

var stickyRand = function(actualRVs) {
  var freshRVs = rand();
  return mapObj(function(val, key) {
    return flip(stickiness) ? actualRVs[key] : freshRVs[key];
  }, actualRVs);
};

var contextualEval = cache(function(proposition, world) {
  var context = 'var aliceStrong = ' + JSON.stringify(world.aliceStrong) + ';' +
      'var aliceLazy = ' + JSON.stringify(world.aliceLazy) + ';' +
      'var bobStrong = ' + JSON.stringify(world.bobStrong) + ';' +
      'var bobLazy = ' + JSON.stringify(world.bobLazy) + ';' +
      'var aliceWin = ' + JSON.stringify(world.aliceWin) + ';';
  //print(context + proposition);
  return(webpplEval(context + proposition));
});

var checkPossible = function(unfactoredERP) {
  var factorERP = Enumerate(function() {
    return sample(unfactoredERP).factor;
  });
  if (factorERP.score([], -Infinity) == 0) {
    // a sort of way of throwing errors if we try to evaluate an impossible utterance
    return 'impossible';
  } else {
    return Enumerate(function() {
      var result = sample(unfactoredERP);
      factor(result.factor);
      return result.result;
    });
  }
};

var counterfactualERP = function(ifA, thenB, actualRVs) {
  //print(ifA);
  //print(thenB);
  //print(actualRVs);
  var unfactoredERP = Enumerate(function() {
    var counterfactualRVs = stickyRand(actualRVs);
    //print(counterfactualRVs);
    var counterfactualWorld = vars(counterfactualRVs);
    //print(counterfactualWorld);
    return {
      result: contextualEval(thenB, counterfactualWorld),
      factor: (contextualEval(ifA, counterfactualWorld) ? 0 : -Infinity)
    }
  });
  //unfactoredERP.print();
  return checkPossible(unfactoredERP);
};

// ======== literal meaning functions =========

var because = function(conditionA, resultB, actualWorld, actualRVs) {
  var actuallyA = contextualEval(conditionA, actualWorld);
  var actuallyB = contextualEval(resultB, actualWorld);

  // debugging:
  //print(conditionA + "? " + actuallyA);
  //print(resultB + "? " + actuallyB);

  if (actuallyA & actuallyB) {
    // debugging:
    //print("both true!");

    var ifNotA = "!" + conditionA;
    var thenNotB = "!" + resultB;

    // debugging:
    //print("if " + ifNotA + " then " + thenNotB);

    var cfERP = counterfactualERP(ifNotA, thenNotB, actualRVs);
    //print("because " + conditionA + ", " + resultB);
    //cfERP.print();
    return cfERP == 'impossible' ? -Infinity : cfERP.score([], true);;
  } else {
    return -Infinity;
  }
};

/*
   meaning_factor is -Infinity if the utterance is completely false
   in the world, 0 if the utterance is true, and for "because" utterances
   it is the proportion of counterfactual worlds satisfying conditionA
   in which resultB is also true.
*/
var meaning_factor = cache(function(utterance, world, rVs) {
  if (utterance == 'nothing') {
    return 0;
  } else {
    var words = utterance.split(' ');
    if (words.length < 2 | words[1] != 'because') {
      return contextualEval(utterance, world) ? 0 : -Infinity;
    } else {
      // "resultB because conditionA"
      var resultB = words[0];
      var conditionA = words[2];

      // debugging:
      //print("because " + conditionA + ", " + resultB);

      return because(conditionA, resultB, world, rVs);
    }
  }
});

var literalERP = cache(function(utterance) {
  var unfactoredERP = Enumerate(function() {
    var actualRVs = rand();
    var actualWorld = vars(actualRVs);
    //print(actualWorld);
    return {
      result: actualWorld,
      factor: meaning_factor(utterance, actualWorld, actualRVs)
    };
  });
  return checkPossible(unfactoredERP);
});

var utterances = [
  'nothing',
  
  'aliceStrong', '!aliceStrong',
  'aliceLazy', '!aliceLazy',
  'bobStrong', '!bobStrong',
  'bobLazy', '!bobLazy',
  'aliceWin', '!aliceWin',
  
  'aliceWin & aliceStrong',
  'aliceWin & !aliceLazy',
  'aliceWin & !bobStrong',
  'aliceWin & bobLazy',
  
  'aliceWin because aliceStrong',
  'aliceWin because !aliceLazy',
  'aliceWin because !bobStrong',
  'aliceWin because bobLazy'
];

var alpha = 5;

var costs = map(function(utterance) {
  if (utterance == "nothing") {
    return 0;
  } else {
    var words = utterance.split(" ");
    var chars = utterance.split("");
    var negs = filter(function(x) {return x == "!";}, chars);
    return words.length;
  }
}, utterances);

var utterancePrior = function() {
  var probabilities = map(function(x) {return Math.exp(-x);}, costs);
  return utterances[discrete(probabilities)];
};

var speakerERP = cache(function(world) {
  var unfactoredERP = Enumerate(function() {
    var utterance = utterancePrior();
    //print(utterance);
    var interpretation = literalERP(utterance);
    return {
      factor: interpretation == 'impossible' ? -Infinity : interpretation.score([], world) * alpha,
      result: utterance
    };
  });
  return checkPossible(unfactoredERP);
});

var listenerERP = function(utterance) {
  return Enumerate(function() {
    var actualRVs = rand();
    var actualWorld = vars(actualRVs);
    //print(actualWorld);
    var description = speakerERP(actualWorld);
    factor(
      description == 'impossible' ? -Infinity : description.score([], utterance) * alpha
    );
    return actualWorld;
  });
};

// ======== example =========

//print(Enumerate(utterancePrior));
/*speakerERP({
  aliceStrong: true, aliceLazy: true,
  bobStrong: true, bobLazy: true,
  aliceWin: true
});*/

// WARNING: this code will take a while to run
vizPrint(literalERP('aliceWin & bobLazy'));
vizPrint(literalERP('aliceWin because bobLazy'));

// WARNING: THIS WILL TAKE A *VERY* LONG TIME TO RUN
// the results are that alice is probably weak and bob is
// probably strong when we say "because" whereas alice is
// almost certainly strong and bob almost certainly weak
// when we say "and"
/*
print('aliceWin & bobLazy');
vizPrint(listenerERP('aliceWin & bobLazy'));
print('aliceWin because bobLazy');
vizPrint(listenerERP('aliceWin because bobLazy'));
*/

// WARNING: THIS WILL TAKE A *VERY* LONG TIME TO RUN\
/*
print('aliceWin because bobLazy');
vizPrint(listenerERP('aliceWin because bobLazy'));
print('aliceWin because aliceStrong');
vizPrint(listenerERP('aliceWin because aliceStrong'));
print('aliceWin because !aliceLazy');
vizPrint(listenerERP('aliceWin because !aliceLazy'));
print('aliceWin because !bobStrong');
vizPrint(listenerERP('aliceWin because !bobStrong'));
*/
~~~

## Evaluating felicity of different explanations

Going back to the bacon example, suppose that we want to get a felicity judgment for the explanation, "The neighbors are angry because I cooked bacon." How good of a thing is that to say?

We can add another layer of pragmatic reasoning above the pragmatic listener, where a pragmatic speaker can either agree or disagree with that explanation.

~~~
///fold:
// ======== model specification =========

// exogenous random variables
var rand = function() {
  return {
    uB: flip(0.9), // bacon is a priori likely
    uS: flip(0.9), // smoke alarm is likely, given bacon
    uN: flip(0.1) // probability of neighbors being angry even if the smoke alarm doesn't go off is low
  };
};

// endogenous variables are functions of the random state
var vars = function(rVs) {
  var bacon = rVs.uB;
  var smokeAlarm = and(bacon, rVs.uS); // smoke alarm will only go off if bacon is cooked, but it might not even if you do cook bacon.
  var neighbors = or(smokeAlarm, rVs.uN); // neighbors *will get angry* if the smoke alarm goes off. they might even if it doesn't, though.
  return {
    bacon: bacon,
    smokeAlarm: smokeAlarm,
    neighbors: neighbors
  };
};

// ======== parameters and counterfactual functions =========

// parameter for how similar counterfactual world is to actual world
var stickiness = 0.5;

var stickyRand = function(actualRVs) {
  var freshRVs = rand();
  return mapObj(function(val, key) {
    return flip(stickiness) ? actualRVs[key] : freshRVs[key];
  }, actualRVs);
};

var contextualEval = cache(function(proposition, world) {
  var context = 'var bacon = ' + JSON.stringify(world.bacon) + ';' +
      'var smokeAlarm = ' + JSON.stringify(world.smokeAlarm) + ';' +
      'var neighbors = ' + JSON.stringify(world.neighbors) + ';';
  //print(context + proposition);
  return(webpplEval(context + proposition));
});

var checkPossible = function(unfactoredERP) {
  var factorERP = Enumerate(function() {
    return sample(unfactoredERP).factor;
  });
  if (factorERP.score([], -Infinity) == 0) {
    // a sort of way of throwing errors if we try to evaluate an impossible utterance
    return 'impossible';
  } else {
    return Enumerate(function() {
      var result = sample(unfactoredERP);
      factor(result.factor);
      return result.result;
    });
  }
};

var counterfactualERP = function(ifA, thenB, actualRVs) {
  //print(ifA);
  //print(thenB);
  //print(actualRVs);
  var unfactoredERP = Enumerate(function() {
    var counterfactualRVs = stickyRand(actualRVs);
    //print(counterfactualRVs);
    var counterfactualWorld = vars(counterfactualRVs);
    //print(counterfactualWorld);
    return {
      result: contextualEval(thenB, counterfactualWorld),
      factor: (contextualEval(ifA, counterfactualWorld) ? 0 : -Infinity)
    }
  });
  //unfactoredERP.print();
  return checkPossible(unfactoredERP);
};

// ======== literal meaning functions =========

var because = function(conditionA, resultB, actualWorld, actualRVs) {
  var actuallyA = contextualEval(conditionA, actualWorld);
  var actuallyB = contextualEval(resultB, actualWorld);

  // debugging:
  //print(conditionA + "? " + actuallyA);
  //print(resultB + "? " + actuallyB);

  if (actuallyA & actuallyB) {
    // debugging:
    //print("both true!");

    var ifNotA = "!" + conditionA;
    var thenNotB = "!" + resultB;

    // debugging:
    //print("if " + ifNotA + " then " + thenNotB);

    var cfERP = counterfactualERP(ifNotA, thenNotB, actualRVs);
    //print("because " + conditionA + ", " + resultB);
    //cfERP.print();
    return cfERP == 'impossible' ? -Infinity : cfERP.score([], true);;
  } else {
    return -Infinity;
  }
};

/*
   meaning_factor is -Infinity if the utterance is completely false
   in the world, 0 if the utterance is true, and for "because" utterances
   it is the proportion of counterfactual worlds satisfying conditionA
   in which resultB is also true.
*/
var meaning_factor = cache(function(utterance, world, rVs) {
  if (utterance == 'nothing') {
    return 0;
  } else {
    var words = utterance.split(' ');
    var negation = words[0] == "itisnotthecasethat";
    var doubleNegation = negation ? words[1] == "itisnotthecasethat" : false;
    var restOfWords = doubleNegation ? words.slice(2) : negation ? words.slice(1) : words;
    if (restOfWords.length < 2 | restOfWords[1] != 'because') {
      var isTrue = contextualEval(utterance, world);
      // if negation, check that negated utterance is true
      return (doubleNegation ? isTrue : negation ? !isTrue : isTrue) ? 0 : -Infinity;
    } else {
      // "resultB because conditionA"
      var resultB = restOfWords[0];
      var conditionA = restOfWords[2];

      // debugging:
      //print("because " + conditionA + ", " + resultB);

      var becauseIsTrue = because(conditionA, resultB, world, rVs);

      return doubleNegation ? becauseIsTrue : negation ? !becauseIsTrue : becauseIsTrue;
    }
  }
});

var literalERP = cache(function(utterance) {
  var unfactoredERP = Enumerate(function() {
    var actualRVs = rand();
    var actualWorld = vars(actualRVs);
    //print(actualWorld);
    return {
      result: actualWorld,
      factor: meaning_factor(utterance, actualWorld, actualRVs)
    };
  });
  return checkPossible(unfactoredERP);
});

var alpha = 5;

var getCostUtterance = function(utterance) {
  if (utterance == "nothing") {
    return 0;
  } else {
    var words = utterance.split(" ");
    var chars = utterance.split("");
    var negs = filter(function(x) {return x == "!";}, chars);
    return words.length;
  }
};

var getUtterancePrior = function(utterance) {
  var basePropositions = ['bacon', 'smokeAlarm', 'neighbors'];
  var getAlternativeUtterances = function(utterance) {
    var negPropositions = map(function(p) {return "!" + p;}, basePropositions);
    var simplePropositions = negPropositions.concat(basePropositions);
    return [utterance, "nothing", "itisnotthecasethat " + utterance].concat(
      simplePropositions
    );
  };
  var utterances = getAlternativeUtterances(utterance);
  var costs = map(getCostUtterance, utterances);
  return function() {
    var probabilities = map(function(x) {return Math.exp(-x);}, costs);
    return utterances[discrete(probabilities)];
  };
};

var speakerERP = cache(function(world, baseUtterance) {
  var utterancePrior = getUtterancePrior(baseUtterance);
  var unfactoredERP = Enumerate(function() {
    var utterance = utterancePrior();
    //print(utterance);
    var interpretation = literalERP(utterance);
    return {
      factor: interpretation == 'impossible' ? -Infinity : interpretation.score([], world),
      result: utterance
    };
  });
  return checkPossible(unfactoredERP);
});

var listenerERP = cache(function(utterance) {
  return Enumerate(function() {
    var actualRVs = rand();
    var actualWorld = vars(actualRVs);
    //print(actualWorld);
    var description = speakerERP(actualWorld, utterance /*give speaker utterance to generate relevant alternatives*/);
    factor(
      description == 'impossible' ? -Infinity : description.score([], utterance) * alpha
    );
    return actualWorld;
  });
});
///
var speaker2ERP = function(utterance, world) {
  var s2Utterances = [utterance, "itisnotthecasethat " + utterance];
  var s2costs = map(getCostUtterance, s2Utterances);
  var s2UtterancePrior = function() {
    var probabilities = map(function(x) {return Math.exp(-x);}, s2costs);
    return s2Utterances[discrete(probabilities)];
  };
  return Enumerate(function() {
    var utt = s2UtterancePrior();
    var interpretation = listenerERP(utt);
    factor( interpretation.score([], world) );
    return utt == utterance ? "yes" : "no";
  });
};

// ======== examples =========

print('neighbors because bacon when smokeAlarm is true');
print(speaker2ERP('neighbors because bacon', {bacon: true, smokeAlarm: true, neighbors: true}));

print('neighbors because bacon when smokeAlarm is false');
print(speaker2ERP('neighbors because bacon', {bacon: true, smokeAlarm: false, neighbors: true}));
~~~

<!-- ## Inferring a causal model: simple version

## Inferring a causal model: cooler version -->

<!-- ## Empirical phenomena to explore -->

<!--
coherent plan for quarter!!!!!!!!!!! (last section of writeup)
list of some phenomena that we think are predicted by these sorts of models that we could think about looking at experimentally.
simple way to start doing experiments that still has rich phenomena
-->

<!--
other

causal vs formal might be a qud effect of whether we (want to) know the causal model or the state of the world.
someday maybe show that causal, formal, and teleological are all representable within this framework.
acknowledge interns

check in with tobi and josh
invite them to be authors
modeling the pragmatics of explanation

alice won because bob wanted her to win

alice tried hard so that she would win

alice tried hard because bob was strong

teleological explanation 'agent chose A so that B' is applicable when an agent
  chooses to do A
  wants B
  and believes that B because A
-->


## To Do

* more kinds of intuitive theories, e.g. formal and functional explanations.
  - e.g. social inference in tug-of-war. assuming others choose randomly, should you be lazy? "Alice didn't try because Bob is weak."
* S2 model for felicity of different explanations
* presuppositions: lifted variable background knowledge (SALT?)
* infer causal relationships: simple and NN