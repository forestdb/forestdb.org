---
layout: model
title: Tug of War Explanations
model-status: code
model-language: webppl
---

<script src="http://web.stanford.edu/~erindb/webppl-viz/webppl.min.js"></script>  
<link rel="stylesheet" href="http://web.stanford.edu/~erindb/webppl-viz/viz.css">

* toc
{:toc}

## Simplest generative model

Alice and Bob are playing a game of tug-of-war. I'm starting with a very simple model to begin with.

Each player is either weak or strong. If their strengths are matched, the winner is up to chance, otherwise the stronger player wins.


~~~
var model = function(rvs) {
  var A = rvs.uA ? 2 : 1;
  var B = rvs.uB ? 2 : 1;
  var W = (A>B) ? "A" : (B>A) ? "B" : rvs.uW ? "A" : "B";
  return {
    A: A==2 ? "strong" : "weak",
    B: B==2 ? "strong" : "weak",
    W: W
  }
};

var randomness = function() {
  return {
    uA: flip(0.5),
    uB: flip(0.5),
    uW: flip(0.5)
  }
}

var inference = function() {
  var actual_randomness = randomness();
  var actual_world = model(actual_randomness);
  return actual_world;
};

vizPrint(Enumerate(inference));
~~~

## Explanations: Take 1

~~~
///fold:
var model = function(rvs) {
  var A = rvs.uA ? 2 : 1;
  var B = rvs.uB ? 2 : 1;
  var W = (A>B) ? "A" : (B>A) ? "B" : rvs.uW ? "A" : "B";
  return {
    A: A==2 ? "strong" : "weak",
    B: B==2 ? "strong" : "weak",
    W: W
  }
};

var randomness = function() {
  return {
    uA: flip(0.5),
    uB: flip(0.5),
    uW: flip(0.5)
  }
}
///

var stickiness = 0.5;
var sticky_randomness = function(actual) {
  var fresh = randomness();
  return {
    uA: flip(stickiness) ? actual.uA : fresh.uA,
    uB: flip(stickiness) ? actual.uB : fresh.uB,
    uW: flip(stickiness) ? actual.uW : fresh.uW
  };
};

var counterfactualERP = function(actual_randomness, cond, cond_bool, concl, concl_bool) {
  // if value of cond is cond_bool, then the value of concl is concl_bool
  return Enumerate(function() {
    var counterfactual_randomness = sticky_randomness(actual_randomness);
    var counterfactual_world = model(counterfactual_randomness);
    condition(cond(counterfactual_world) == cond_bool);
    return concl(counterfactual_world) == concl_bool;
  });
};

var because = function(condition, conclusion, actual_world, actual_randomness) {
  // "because condition, conclusion"
  return and(
    condition(actual_world),
    and(
      conclusion(actual_world),
      // if not condition, then not conclusion:
      sample(counterfactualERP(actual_randomness, condition, false, conclusion, false))
    )
  );
};

var alice_strong = function(world) {
  return world.A == "strong";
};
var bob_strong = function(world) {
  return world.B == "strong";
};
var alice_win = function(world) {
  return world.W == "A";
};
var bob_win = function(world) {
  return world.W == "B";
};

var inference = function() {
  var actual_randomness = randomness();
  var actual_world = model(actual_randomness);
  //condition(and(alice_strong(actual_world), alice_win(actual_world)));
  condition(because(alice_strong, alice_win, actual_world, actual_randomness));
  return actual_world;
};

vizPrint(Enumerate(inference));
~~~

## Slightly more interesting generative model

Now, with probability 0.3, a player might be lazy. If they're lazy, they pull at half their strength (which was 2 if strong and 1 if weak). So a strong but lazy player can be beaten by a weak but determined player.

~~~
var model = function(rvs) {
  var A = rvs.uA ? 2 : 1;
  var B = rvs.uB ? 2 : 1;
  var LA = rvs.uLA;
  var LB = rvs.uLB;
  var PA = LA ? A/2 : A;
  var PB = LB ? B/2 : B;
  var W = (PA>PB) ? "A" : (PB>PA) ? "B" : rvs.uW ? "A" : "B";
  return {
    A: A==2 ? "strong" : "weak",
    B: B==2 ? "strong" : "weak",
    LA: LA,
    LB: LB,
    W: W
  }
};

var randomness = function() {
  return {
    uA: flip(0.5),
    uB: flip(0.5),
    uW: flip(0.5),
    uLA: flip(0.3),
    uLB: flip(0.3)
  }
}

var inference = function() {
  var actual_randomness = randomness();
  var actual_world = model(actual_randomness);
  return actual_world;
};

vizPrint(Enumerate(inference));
~~~

## Explanations: Take 2

> Alice won because Bob was lazy.

My intuition is that this sentence means that it’s more likely that Bob is strong and that Alice is weak.

Relative to "and", these interpretations kind of bear out...

~~~
///fold:
var model = function(rvs) {
  var A = rvs.uA ? 2 : 1;
  var B = rvs.uB ? 2 : 1;
  var LA = rvs.uLA;
  var LB = rvs.uLB;
  var PA = LA ? 0.5*A : A;
  var PB = LB ? 0.5*B : B;
  var W = (PA>PB) ? "A" : (PB>PA) ? "B" : rvs.uW ? "A" : "B";
  return {
    A: A==2 ? "strong" : "weak",
    B: B==2 ? "strong" : "weak",
    LA: LA,
    LB: LB,
    W: W
  }
};

var randomness = function() {
  return {
    uA: flip(0.5),
    uB: flip(0.5),
    uLA: flip(0.3),
    uLB: flip(0.3),
    uW: flip(0.5)
  };
};
///

var stickiness = 0.5;
var sticky_randomness = function(actual) {
  var fresh = randomness();
  return {
    uA: flip(stickiness) ? actual.uA : fresh.uA,
    uB: flip(stickiness) ? actual.uB : fresh.uB,
    uLA: flip(stickiness) ? actual.uLA : fresh.uLA,
    uLB: flip(stickiness) ? actual.uLB : fresh.uLB,
    uW: flip(stickiness) ? actual.uW : fresh.uW
  };
};

var counterfactualERP = function(actual_randomness, cond, cond_bool, concl, concl_bool) {
  // if value of cond is cond_bool, then the value of concl is concl_bool
  return Enumerate(function() {
    var counterfactual_randomness = sticky_randomness(actual_randomness);
    var counterfactual_world = model(counterfactual_randomness);
    condition(cond(counterfactual_world) == cond_bool);
    return concl(counterfactual_world) == concl_bool;
  });
};

var because = function(condition, conclusion, actual_world, actual_randomness) {
  // "because condition, conclusion"
  return and(
    condition(actual_world),
    and(
      conclusion(actual_world),
      // if not condition, then not conclusion:
      sample(counterfactualERP(actual_randomness, condition, false, conclusion, false))
    )
  );
};

var alice_strong = function(world) {
  return world.A == "strong";
};
var bob_strong = function(world) {
  return world.B == "strong";
};
var alice_win = function(world) {
  return world.W == "A";
};
var bob_win = function(world) {
  return world.W == "B";
};
var alice_lazy = function(world) {
  return world.LA;
}
var bob_lazy = function(world) {
  return world.LB;
}

var inference = function() {
  var actual_randomness = randomness();
  var actual_world = model(actual_randomness);
  //condition(and(bob_lazy(actual_world), alice_win(actual_world)));
  condition(because(bob_lazy, alice_win, actual_world, actual_randomness));
  return actual_world;
};

vizPrint(Enumerate(inference));
~~~

## Pragmatics

As we add pragmatics and increase rationality and cost of explaining, the probabitliy that Alice is weak and Bob is strong increases.

Our alternative utterances include a bunch of different explanations for why Alice won.  Here are the different explanations and my intuition of what they mean:

> Alice won because Bob was lazy.

Bob is probably strong, and Alice probably weak.

> Alice won because Alice is strong.

Bob probably tried.

> Alice won because Alice tried.

Alice and Bob have similar strengths.

> Alice won because Bob was weak.

Alice is probably also weak.

~~~
///fold:
var model = function(rvs) {
  var A = rvs.uA ? 2 : 1;
  var B = rvs.uB ? 2 : 1;
  var LA = rvs.uLA;
  var LB = rvs.uLB;
  var PA = LA ? 0.5*A : A;
  var PB = LB ? 0.5*B : B;
  var W = (PA>PB) ? "A" : (PB>PA) ? "B" : rvs.uW ? "A" : "B";
  return {
    A: A==2 ? "strong" : "weak",
    B: B==2 ? "strong" : "weak",
    LA: LA,
    LB: LB,
    W: W
  }
};

var randomness = function() {
  return {
    uA: flip(0.5),
    uB: flip(0.5),
    uLA: flip(0.3),
    uLB: flip(0.3),
    uW: flip(0.5)
  };
};

var stickiness = 0.5;
var sticky_randomness = function(actual) {
  var fresh = randomness();
  return {
    uA: flip(stickiness) ? actual.uA : fresh.uA,
    uB: flip(stickiness) ? actual.uB : fresh.uB,
    uLA: flip(stickiness) ? actual.uLA : fresh.uLA,
    uLB: flip(stickiness) ? actual.uLB : fresh.uLB,
    uW: flip(stickiness) ? actual.uW : fresh.uW
  };
};

var counterfactualERP = function(actual_randomness, cond, cond_bool, concl, concl_bool) {
  // if value of cond is cond_bool, then the value of concl is concl_bool
  return Enumerate(function() {
    var counterfactual_randomness = sticky_randomness(actual_randomness);
    var counterfactual_world = model(counterfactual_randomness);
    condition(cond(counterfactual_world) == cond_bool);
    return concl(counterfactual_world) == concl_bool;
  });
};

var because = function(condition, conclusion, actual_world, actual_randomness) {
  // "because condition, conclusion"
  return and(
    condition(actual_world),
    and(
      conclusion(actual_world),
      // if not condition, then not conclusion:
      sample(counterfactualERP(actual_randomness, condition, false, conclusion, false))
    )
  );
};

var alice_strong = function(world) {
  return world.A == "strong";
};
var bob_strong = function(world) {
  return world.B == "strong";
};
var alice_win = function(world) {
  return world.W == "A";
};
var bob_win = function(world) {
  return world.W == "B";
};
var alice_lazy = function(world) {
  return world.LA;
};
var bob_lazy = function(world) {
  return world.LB;
};
var alice_not_lazy = function(world) {
  return !world.LA;
};
var bob_not_lazy = function(world) {
  return !world.LB;
};
var alice_weak = function(world) {
  return world.A == "weak";
};
var bob_weak = function(world) {
  return world.B == "weak";
};
///

var alpha = 5;

// <3 eval...
var meaning = function(utterance, actual_world, actual_randomness) {
  if (utterance == "because alice_strong alice_win") {
    return because(alice_strong, alice_win, actual_world, actual_randomness); 
  } else if (utterance == "because bob_lazy alice_win") {
    return because(bob_lazy, alice_win, actual_world, actual_randomness); 
  } else if (utterance == "and alice_strong alice_win") {
    return and(alice_strong(actual_world), alice_win(actual_world));
  } else if (utterance == "and bob_lazy alice_win") {
    return and(bob_lazy(actual_world), alice_win(actual_world));
  } else if (utterance == "because alice_not_lazy alice_win") {
    return because(alice_not_lazy, alice_win, actual_world, actual_randomness); 
  } else if (utterance == "and alice_not_lazy alice_win") {
    return and(alice_not_lazy(actual_world), alice_win(actual_world));
  } else if (utterance == "because bob_weak alice_win") {
    return because(bob_weak, alice_win, actual_world, actual_randomness); 
  } else if (utterance == "and bob_weak alice_win") {
    return and(bob_weak(actual_world), alice_win(actual_world));
  } else if (utterance == "alice_win") {
    return alice_win(actual_world);
  } else if (utterance == "bob_win") {
    return bob_win(actual_world);
  } else if (utterance == "alice_lazy") {
    return alice_lazy(actual_world);
  } else if (utterance == "bob_lazy") {
    return bob_lazy(actual_world);
  } else if (utterance == "alice_not_lazy") {
    return !alice_lazy(actual_world);
  } else if (utterance == "bob_not_lazy") {
    return !bob_lazy(actual_world);
  } else if (utterance == "alice_strong") {
    return alice_strong(actual_world);
  } else if (utterance == "bob_strong") {
    return bob_strong(actual_world);
  } else if (utterance == "alice_weak") {
    return !alice_strong(actual_world);
  } else if (utterance == "bob_weak") {
    return !bob_strong(actual_world);
  } else {
    return true;
  }
};

var utterance_prior = function() {
  var utterances = [
    "because alice_strong alice_win",
    "and alice_strong alice_win",
    "because bob_lazy alice_win",
    "and bob_lazy alice_win",
    "because alice_not_lazy alice_win",
    "and alice_not_lazy alice_win",
    "because bob_weak alice_win",
    "and bob_weak alice_win",
    "alice_win", "bob_win",
    "alice_lazy", "bob_lazy",
    "alice_not_lazy", "bob_not_lazy",
    "alice_strong", "alice_weak",
    "bob_strong", "bob_weak"
  ];
  var costs = [
    6, 6,
    6, 6,
    6, 6,
    6, 6,
    1, 1,
    1, 1,
    1, 1,
    1, 1,
    1, 1
  ];
  var probabilities = map(function(x) {return Math.exp(-x);}, costs);
  return utterances[discrete(probabilities)];
}

var literalERP = cache(function(utterance) {
  return Enumerate(function() {
    var actual_randomness = randomness();
    var actual_world = model(actual_randomness);
    condition(meaning(utterance, actual_world, actual_randomness));
    return actual_world;
  });
});

var speakerERP = cache(function(world) {
  return Enumerate(function() {
    var utterance = utterance_prior();
    factor( alpha * literalERP(utterance).score([], world) );
    return utterance;
  });
});

var listenerERP = function(utterance) {
  return Enumerate(function() {
    var actual_randomness = randomness();
    var actual_world = model(actual_randomness);
    factor( alpha * speakerERP(actual_world).score([], utterance) );
    return actual_world;
  })
};

vizPrint(listenerERP("because bob_lazy alice_win"));
~~~