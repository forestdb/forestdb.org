---
layout: model
title: Simple 2-Boolean-Variable Explanations
model-status: code
model-language: webppl
---

<script src="http://web.stanford.edu/~erindb/webppl-viz/webppl.min.js"></script>  
<link rel="stylesheet" href="http://web.stanford.edu/~erindb/webppl-viz/viz.css">

* toc
{:toc}

## Generative model

~~~
// for running with different kinds of models:
/// fold:
// model parameters
var pA = 0.5;
var pB = 0.5;
var stickiness = 0.5;

// functions of (parent and) exogenous random variable (rV)
// that could represent a causal relationship
var ifCause = function(parent, rV) {
  return rV ? parent : !parent;
};
var andCause = function(parent, rV) {
  return and(parent, rV);
};
var orCause = function(parent, rV) {
  return or(parent, rV);
};
var echo = function(rV) {
  return rV;
};
///

// exogenous random variables
var randomness = function() {
  return {
    uA: flip(pA),
    uB: flip(pB)
  };
};

// evaluate world, given exogenous random variables
var world = function(randomness) {
  var A = echo(randomness.uA);
  var B = andCause(A, randomness.uB);
  return { A:A, B:B };
};

// graph possible world states
vizPrint(Enumerate(function() {
  var actual_randomness = randomness();
  var actual_world = world(actual_randomness);
  return actual_world;
}));
~~~

## Meaning function

~~~
///fold:
// for running with different kinds of models:
// model parameters
var pA = 0.5;
var pB = 0.5;
var stickiness = 0.5;

// functions of (parent and) exogenous random variable (rV)
// that could represent a causal relationship
var ifCause = function(parent, rV) {
  return rV ? parent : !parent;
};
var andCause = function(parent, rV) {
  return and(parent, rV);
};
var orCause = function(parent, rV) {
  return or(parent, rV);
};
var echo = function(rV) {
  return rV;
};

// exogenous random variables
var randomness = function() {
  return {
    uA: flip(pA),
    uB: flip(pB)
  };
};

// evaluate world, given exogenous random variables
var world = function(randomness) {
  var A = echo(randomness.uA);
  var B = andCause(A, randomness.uB);
  return { A:A, B:B };
};
///

// check that a predicate (a string representing a webppl expression of world variables "A" and "B") is true of a world
var checkTrue = function(pred, world) {
  //var worldArr = obj2Arr(world);
  var prog = "var A=" + world.A + ";" +
      "var B=" + world.B + ";" +
      pred + ";";
  return webpplEval(prog);
};

// meaning function
var meaning = function(pred, wrld) {
  var elements = pred.split(" ");
  if (elements.length == 0) {
    return true;
  } else if (elements.length == 1) {
    return checkTrue(pred, wrld);
  } else {
    print("WARNING: your input predicate (" + pred +
          ") has no meaning defined");
    return true;
  }
};

// graph possible world states and confirm meaning is consistent
vizPrint(Enumerate(function() {
  var actual_randomness = randomness();
  var actual_world = world(actual_randomness);
  return {
    A: actual_world.A,
    meaningA: meaning("A", actual_world),
    B: actual_world.B,
    meaningB: meaning("B", actual_world)
  }
}));
~~~

## Counterfactuals

~~~
///fold:
// for running with different kinds of models:
// model parameters
var pA = 0.5;
var pB = 0.5;
var stickiness = 0.5;

// functions of (parent and) exogenous random variable (rV)
// that could represent a causal relationship
var ifCause = function(parent, rV) {
  return rV ? parent : !parent;
};
var andCause = function(parent, rV) {
  return and(parent, rV);
};
var orCause = function(parent, rV) {
  return or(parent, rV);
};
var echo = function(rV) {
  return rV;
};

// exogenous random variables
var randomness = function() {
  return {
    uA: flip(pA),
    uB: flip(pB)
  };
};

// evaluate world, given exogenous random variables
var world = function(randomness) {
  var A = echo(randomness.uA);
  var B = andCause(A, randomness.uB);
  return { A:A, B:B };
};

// check that a predicate (a string representing a webppl expression of world variables "A" and "B") is true of a world
var checkTrue = function(pred, world) {
  //var worldArr = obj2Arr(world);
  var prog = "var A=" + world.A + ";" +
      "var B=" + world.B + ";" +
      pred + ";";
  return webpplEval(prog);
};
///

var n_particles = 10;

// negation: take 1
var not = function(pred) {
  return "!(" + pred + ")";
}

// counterfactual reasoning

var sticky_randomness = function(actual) {
  var fresh = randomness();
  return mapObj(function(val, key) {
    return flip(stickiness) ? actual[key] : fresh[key];
  }, actual);
};

var counterfactual = function(cond, concl, actual_randomness) {
  return ParticleFilter(function() {
    var counterfactual_randomness = sticky_randomness(actual_randomness);
    var counterfactual_world = world(counterfactual_randomness);
    condition(checkTrue(cond, counterfactual_world));
    return concl;
  }, n_particles);
};

// evaluation of "because"

var because = function(cond, concl, actual_randomness, actual_world) {
  // because condition, conclusion.
  var counterfactual_randomness = sticky_randomness(actual_randomness);
  var counterfactual_world = world(counterfactual_randomness);
  return and(
    // condition is true
    checkTrue(cond, actual_world),
    and(
      // conclusion is true
      checkTrue(concl, actual_world),
      // if not condition, then not conclusion
      sample(counterfactual(not(cond), not(concl), actual_randomness))
    )
  );
};

// meaning function (now with because)

var meaning = cache(function(pred, wrld, rand) {
  var elements = pred.split(" ");
  if (elements.length == 0) {
    return true;
  } else if (elements.length == 1) {
    return checkTrue(pred, wrld);
  } else if (elements[0] == "because" & elements.length == 3) {
    return because(elements[1], elements[2], rand, wrld);
  } else {
    print("WARNING: your input predicate (" + pred +
          ") has no meaning defined");
    return true;
  }
});

vizPrint(ParticleFilter(function() {
  var actual_randomness = randomness();
  var actual_world = world(actual_randomness);
  condition(meaning("because A B", actual_world, actual_randomness));
  return actual_world;
}, n_particles));
~~~