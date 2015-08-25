---
layout: model
title: Explanations
model-status: code
model-language: webppl
---

<script src="http://web.stanford.edu/~erindb/webppl-viz/webppl.min.js"></script>  
<link rel="stylesheet" href="http://web.stanford.edu/~erindb/webppl-viz/viz.css">

* toc
{:toc}

## Model specification

~~~
// model has rand (exogenous random variables) and
// vars (functions of exogenous random variables)
var baconModel = {
  rand: function() {
    return {
      uB: flip(0.9),
      uS: flip(0.9),
      uN: flip(0.1)
    };
  },
  vars: function(rVs) {
    var bacon = rVs.uB;
    var smokeAlarm = and(bacon, rVs.uS);
    var neighbors = or(smokeAlarm, rVs.uN);
    return {
      bacon: bacon,
      smokeAlarm: smokeAlarm,
      neighbors: neighbors
    };
  }
};

//more models:
///fold:
var andModel = {
  rand: function() {
    return {
      uA: flip(0.5),
      uB: flip(0.5)
    };
  },
  vars: function(rVs) {
    var A = rVs.uA;
    var B = and(A, rVs.uB);
    return {
      A: A,
      B: B
    };
  }
};

var orModel = {
  rand: function() {
    return {
      uA: flip(0.5),
      uB: flip(0.5)
    };
  },
  vars: function(rVs) {
    var A = rVs.uA;
    var B = or(A, rVs.uB);
    return {
      A: A,
      B: B
    };
  }
};

var echoModel = {
  rand: function() {
    return {
      uA: flip(0.5),
      uB: flip(0.5)
    };
  },
  vars: function(rVs) {
    var A = rVs.uA;
    var B = rVs.uB ? A : !A;
    return {
      A: A,
      B: B
    };
  }
};
///

// to generate worlds using these models,
var generate = function(model) {
  var rand = model.rand;
  var vars = model.vars;
  return Enumerate(function() {
    var actualRV = rand();
    var actualWorld = vars(actualRV);
    return actualWorld;
  })
};

vizPrint(generate(baconModel));
~~~

## Conditioning

~~~
// model specifications:
///fold:
// model has rand (exogenous random variables) and
// vars (functions of exogenous random variables)
var baconModel = {
  rand: function() {
    return {
      uB: flip(0.9),
      uS: flip(0.9),
      uN: flip(0.1)
    };
  },
  vars: function(rVs) {
    var bacon = rVs.uB;
    var smokeAlarm = and(bacon, rVs.uS);
    var neighbors = or(smokeAlarm, rVs.uN);
    return {
      bacon: bacon,
      smokeAlarm: smokeAlarm,
      neighbors: neighbors
    };
  }
};

var andModel = {
  rand: function() {
    return {
      uA: flip(0.5),
      uB: flip(0.5)
    };
  },
  vars: function(rVs) {
    var A = rVs.uA;
    var B = and(A, rVs.uB);
    return {
      A: A,
      B: B
    };
  }
};

var orModel = {
  rand: function() {
    return {
      uA: flip(0.5),
      uB: flip(0.5)
    };
  },
  vars: function(rVs) {
    var A = rVs.uA;
    var B = or(A, rVs.uB);
    return {
      A: A,
      B: B
    };
  }
};

var echoModel = {
  rand: function() {
    return {
      uA: flip(0.5),
      uB: flip(0.5)
    };
  },
  vars: function(rVs) {
    var A = rVs.uA;
    var B = rVs.uB ? A : !A;
    return {
      A: A,
      B: B
    };
  }
};
///

// eval tools
var cacheEval = cache(webpplEval);
var getEnv = function(world) {
  var makeDefnStatement = function(pair) {
    var key = 0;
    var val = 1;
    return "var " + pair[key] + " = " + pair[val] + ";";
  };
  var definitionStatements = map(makeDefnStatement, _.pairs(world));
  return definitionStatements.join(" ");
};
var meaning = cache(function(pred, world) {
  var environment = getEnv(world);
  return cacheEval(environment + pred);
});

// to generate worlds using these models,
// conditioned on the literal truth of the utterance
var literalERP = function(utterance, model) {
  var rand = model.rand;
  var vars = model.vars;
  return Enumerate(function() {
    var actualRV = rand();
    var actualWorld = vars(actualRV);
    condition(meaning(utterance, actualWorld));
    return actualWorld;
  });
};

// condition on bacon being cooked
vizPrint(literalERP("bacon", baconModel));
~~~

## Complex utterances

~~~
// model specifications:
///fold:
// model has rand (exogenous random variables) and
// vars (functions of exogenous random variables)
var baconModel = {
  rand: function() {
    return {
      uB: flip(0.9),
      uS: flip(0.9),
      uN: flip(0.1)
    };
  },
  vars: function(rVs) {
    var bacon = rVs.uB;
    var smokeAlarm = and(bacon, rVs.uS);
    var neighbors = or(smokeAlarm, rVs.uN);
    return {
      bacon: bacon,
      smokeAlarm: smokeAlarm,
      neighbors: neighbors
    };
  }
};

var andModel = {
  rand: function() {
    return {
      uA: flip(0.5),
      uB: flip(0.5)
    };
  },
  vars: function(rVs) {
    var A = rVs.uA;
    var B = and(A, rVs.uB);
    return {
      A: A,
      B: B
    };
  }
};

var orModel = {
  rand: function() {
    return {
      uA: flip(0.5),
      uB: flip(0.5)
    };
  },
  vars: function(rVs) {
    var A = rVs.uA;
    var B = or(A, rVs.uB);
    return {
      A: A,
      B: B
    };
  }
};

var echoModel = {
  rand: function() {
    return {
      uA: flip(0.5),
      uB: flip(0.5)
    };
  },
  vars: function(rVs) {
    var A = rVs.uA;
    var B = rVs.uB ? A : !A;
    return {
      A: A,
      B: B
    };
  }
};
///
// eval tools:
///fold:
var cacheEval = cache(webpplEval);
var getEnv = function(world) {
  var makeDefnStatement = function(pair) {
    var key = 0;
    var val = 1;
    return "var " + pair[key] + " = " + pair[val] + ";";
  };
  var definitionStatements = map(makeDefnStatement, _.pairs(world));
  return definitionStatements.join(" ");
};
///

var evalSimpleMeaning = function(pred, world) {
  var environment = getEnv(world);
  return cacheEval(environment + pred);
};
var meaning = cache(function(pred, world) {
  if (pred == "nothing") {
    return true;
  } else {
    var elements = pred.split(" ");
    if (elements[0] == "not") {
      var negatedPred = elements.slice(1).join(" ");
      return !evalSimpleMeaning(negatedPred, world);
    } else if (elements[1] == "and") {
      // "a and b"
      var a = elements[0];
      var b = elements[2];
      var aMeaning = evalSimpleMeaning(a, world);
      var bMeaning = evalSimpleMeaning(b, world);
      return and(aMeaning, bMeaning);
    } else {
      evalSimpleMeaning(pred, world);
    }
  }
});

// to generate worlds using these models,
// conditioned on the literal truth of the utterance
var literalERP = function(utterance, model) {
  var rand = model.rand;
  var vars = model.vars;
  return Enumerate(function() {
    var actualRV = rand();
    var actualWorld = vars(actualRV);
    condition(meaning(utterance, actualWorld));
    return actualWorld;
  });
};

// condition on bacon being cooked
vizPrint(literalERP("bacon and neighbors", baconModel));
~~~

## Counterfactuals

> ... we assume that B (bacon cooked), S (smoke alarm activates) and N (neighbors disturbed) are true in the real world, and that S' is false in the counterfactual world. Figure 3 shows how the extended network can be used to reason about a counterfactual observation of S'. In this case we set B, S and N to true and S' to false, then compute the resulting probability distribution on B. As described in Appendix A, this computation can be carried out using any standard algorithm for inference in Bayesian networks. When s = 0.5, the model in Figure 3a predicts that P(B = t) = 0.487. In other words, the model makes a backtracking inference and concludes that bacon was relatively unlikely to have been cooked in the counterfactual world. (Lucas & Kemp, 2015)

~~~
// model specifications:
///fold:
// model has rand (exogenous random variables) and
// vars (functions of exogenous random variables)
var baconModel = {
  rand: function() {
    return {
      uB: flip(0.9),
      uS: flip(0.9),
      uN: flip(0.1)
    };
  },
  vars: function(rVs) {
    var bacon = rVs.uB;
    var smokeAlarm = and(bacon, rVs.uS);
    var neighbors = or(smokeAlarm, rVs.uN);
    return {
      bacon: bacon,
      smokeAlarm: smokeAlarm,
      neighbors: neighbors
    };
  }
};

var andModel = {
  rand: function() {
    return {
      uA: flip(0.5),
      uB: flip(0.5)
    };
  },
  vars: function(rVs) {
    var A = rVs.uA;
    var B = and(A, rVs.uB);
    return {
      A: A,
      B: B
    };
  }
};

var orModel = {
  rand: function() {
    return {
      uA: flip(0.5),
      uB: flip(0.5)
    };
  },
  vars: function(rVs) {
    var A = rVs.uA;
    var B = or(A, rVs.uB);
    return {
      A: A,
      B: B
    };
  }
};

var echoModel = {
  rand: function() {
    return {
      uA: flip(0.5),
      uB: flip(0.5)
    };
  },
  vars: function(rVs) {
    var A = rVs.uA;
    var B = rVs.uB ? A : !A;
    return {
      A: A,
      B: B
    };
  }
};
///
// eval tools:
///fold:
var cacheEval = cache(webpplEval);
var getEnv = function(world) {
  var makeDefnStatement = function(pair) {
    var key = 0;
    var val = 1;
    return "var " + pair[key] + " = " + pair[val] + ";";
  };
  var definitionStatements = map(makeDefnStatement, _.pairs(world));
  return definitionStatements.join(" ");
};
var evalSimpleMeaning = function(pred, world) {
  var environment = getEnv(world);
  return cacheEval(environment + pred);
};
///

// util that will be in webppl later
var mapObject = function(fn, obj) {
  return _.object(
    map(
      function(kv) {
        return [kv[0], fn(kv[0], kv[1])]
      },
      _.pairs(obj))
  );
};

// parameter for how similar counterfactual world is to actual world
var stickiness = 0.5;

var stickyRand = function(actualRVs, rand) {
  var freshRVs = rand();
  return mapObject(function(key, val) {
    return flip(stickiness) ? actualRVs[key] : freshRVs[key];
  }, actualRVs);
};

var baconCounterfactual = Enumerate(function() {
  var rand = baconModel.rand;
  var vars = baconModel.vars;
  var rVs = rand();
  var actualWorld = vars(rVs);
  var cfRVs = stickyRand(rVs, rand);
  var cfWorld = vars(cfRVs);
  condition(
    and(
      actualWorld.bacon,
      and(
        actualWorld.smokeAlarm,
        and(
          actualWorld.neighbors,
          !cfWorld.smokeAlarm
        )
      )
    )
  );
  return cfWorld.bacon;
});

print(baconCounterfactual);
~~~

## Counterfactual utterances

~~~
// model specifications:
///fold:
// model has rand (exogenous random variables) and
// vars (functions of exogenous random variables)
var baconModel = {
  rand: function() {
    return {
      uB: flip(0.9),
      uS: flip(0.9),
      uN: flip(0.1)
    };
  },
  vars: function(rVs) {
    var bacon = rVs.uB;
    var smokeAlarm = and(bacon, rVs.uS);
    var neighbors = or(smokeAlarm, rVs.uN);
    return {
      bacon: bacon,
      smokeAlarm: smokeAlarm,
      neighbors: neighbors
    };
  }
};

var andModel = {
  rand: function() {
    return {
      uA: flip(0.5),
      uB: flip(0.5)
    };
  },
  vars: function(rVs) {
    var A = rVs.uA;
    var B = and(A, rVs.uB);
    return {
      A: A,
      B: B
    };
  }
};

var orModel = {
  rand: function() {
    return {
      uA: flip(0.5),
      uB: flip(0.5)
    };
  },
  vars: function(rVs) {
    var A = rVs.uA;
    var B = or(A, rVs.uB);
    return {
      A: A,
      B: B
    };
  }
};

var echoModel = {
  rand: function() {
    return {
      uA: flip(0.5),
      uB: flip(0.5)
    };
  },
  vars: function(rVs) {
    var A = rVs.uA;
    var B = rVs.uB ? A : !A;
    return {
      A: A,
      B: B
    };
  }
};
///
var models = {
  bacon: baconModel,
  and: andModel,
  or: orModel,
  echo: echoModel
};
// eval tools:
///fold:
var cacheEval = cache(webpplEval);
var getEnv = function(world) {
  var makeDefnStatement = function(pair) {
    var key = 0;
    var val = 1;
    return "var " + pair[key] + " = " + pair[val] + ";";
  };
  var definitionStatements = map(makeDefnStatement, _.pairs(world));
  return definitionStatements.join(" ");
};
var evalSimpleMeaning = function(pred, world) {
  var environment = getEnv(world);
  return cacheEval(environment + pred);
};
///

// util that will be in webppl later
var mapObject = function(fn, obj) {
  return _.object(
    map(
      function(kv) {
        return [kv[0], fn(kv[0], kv[1])]
      },
      _.pairs(obj))
  );
};
// tool for negation
var not = function(pred) {
  return "!(" + pred + ")";
};

// parameter for how similar counterfactual world is to actual world
var stickiness = 0.5;

var stickyRand = function(actualRVs, rand) {
  var freshRVs = rand();
  return mapObject(function(key, val) {
    return flip(stickiness) ? actualRVs[key] : freshRVs[key];
  }, actualRVs);
};

// "if (counterfactually) cond, then concl"
var counterfactualERP = cache(function(cond, concl, rVs, modelLabel) {
  return Enumerate(function() {
    var rand = (models[modelLabel]).rand;
    var vars = (models[modelLabel]).vars;
    var cfRVs = stickyRand(rVs, rand);
    var cfWorld = vars(cfRVs);
    var counterfactualConditionMeaning = evalSimpleMeaning(cond, cfWorld);
    var counterfactualConclusionMenaing = evalSimpleMeaning(concl, cfWorld);
    condition( counterfactualConditionMeaning ); // condition is true in counterfactual world
    return counterfactualConclusionMenaing; // return value of conclusion in counterfactual world
  });
});

// "if (counterfactually) cond, then concl"
var counterfactual = function(cond, concl, rVs, modelLabel) {
  return sample(counterfactualERP(cond, concl, rVs, modelLabel));
};

var getWorld = function(rVs, modelLabel) {
  var vars = (models[modelLabel]).vars;
  return vars(rVs);
}

// "condition because conclusion"
var because = function(cond, concl, rVs, modelLabel) {
  var world = getWorld(rVs, modelLabel);
  // if because contains "presuppositions":
  return and(
    // sample from ERP
    // ("because" := "cond and concl and if not cond then not concl")
    counterfactual( not(cond), not(concl), rVs, modelLabel ),
    and(
      evalSimpleMeaning(cond, world),
      evalSimpleMeaning(concl, world)
    )
  );
  // if not:
  // ("because" := "if not cond then not concl")
  // return counterfactual( not(cond), not(concl), rVs );
};

var meaning = cache(function(pred, rVs, modelLabel) {
  if (pred == "nothing") {
    return true;
  } else {
    var elements = pred.split(" ");
    if (elements.length == 1) {
      var world = getWorld(rVs, modelLabel);
      evalSimpleMeaning(pred, world);
    } else {
      if (elements[0] == "not") {
        var world = getWorld(rVs, modelLabel);
        var negatedPred = elements.slice(1).join(" ");
        return !evalSimpleMeaning(negatedPred, world);
      } else if (elements[1] == "and") {
        var world = getWorld(rVs, modelLabel);
        // "a and b"
        var a = elements[0];
        var b = elements[2];
        var aMeaning = evalSimpleMeaning(a, world);
        var bMeaning = evalSimpleMeaning(b, world);
        return and(aMeaning, bMeaning);
      } else if (elements[1] == "because") {
        // "concl because cond"
        var cond = elements[2];
        var concl = elements[0];
        return because(cond, concl, rVs, modelLabel);
      } else {
        print("i can't parse: '" + pred + "'");
        return true;
      }
    }
  }
});

var marginalize = function(erp, variable) {
  return Enumerate(function() {
    var result = sample(erp);
    return result[variable];
  });
};

// we might want to check counterfactuals that are impossible under the model
var checkCondition = function(jointERP) {
  var conditionERP = marginalize(jointERP, "condition");
  if (conditionERP.score([], true) == -Infinity) {
    return "impossible";
  } else {
    return Enumerate(function() {
      var result = sample(jointERP);
      condition(result.condition);
      return result.world;
    });
  }
};

var literalERP = function(utterance, modelLabel) {
  var rand = (models[modelLabel]).rand;
  var vars = (models[modelLabel]).vars;
  return checkCondition(Enumerate(function() {
    var actualRV = rand();
    var actualWorld = vars(actualRV);
    return {
      world: actualWorld,
      condition: meaning(utterance, actualRV, modelLabel)
    };
  }));
};

// probability of smokeAlarm if "neighbors are mad because of cooking bacon"
var becauseERP = marginalize(
  literalERP("neighbors because bacon", "bacon"),
  "smokeAlarm"
);
// probability of smokeAlarm if "i cooked bacon and the neighbors are mad"
var andERP = marginalize(
  literalERP("neighbors and bacon", "bacon"),
  "smokeAlarm"
);
print("probability of smokeAlarm if 'neighbors are mad because of cooking bacon': ");
print(Math.exp(becauseERP.score([], true)));
print("probability of smokeAlarm if 'i cooked bacon and the neighbors are mad': ");
print(Math.exp(andERP.score([], true)));
~~~