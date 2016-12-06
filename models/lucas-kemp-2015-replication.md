---
layout: model
title: Lucas & Kemp 2015 Replication
model-language: webppl
model-language-version: pre-v0.7
---

<script src="http://web.stanford.edu/~erindb/webppl-viz/webppl.min.js"></script>  
<link rel="stylesheet" href="http://web.stanford.edu/~erindb/webppl-viz/viz.css">

<script src='https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML'></script>

* toc
{:toc}

## Representing Causal Knowledge: Causal Bayes Nets vs Functional Causal Models

[Lucas & Kemp (2015)](http://philpapers.org/archive/LUCAIP.pdf) find evidence that *Functional Causal Models* rather than *Causal Bayes Nets* accurately reflect people's intuitions about cause and counterfactuals.

### Causal Bayes Nets

A *Causal Bayes Net* (CBN) assumes that causal relationships can be inherently stochastic.

![0.9 --> X ; X --0.9--> Y](../assets/img/CBN.png)

~~~
var BCN = function() {
  var X = flip(0.9);
  var Y = flip(0.9) ? X : false;
  return { X: X, Y: Y };
}
vizPrint(Enumerate(BCN));
~~~

### Functional Causal Models

A *Functional Causal Model* (FCM) assumes that all causes are actually determinisitic, but some are unknown. Every observable variable $$X$$ is a deterministic function of its parent observable variables $$par(X)$$ and an exogenous random variable $$U_{X}$$. The randomness of the system lies in the unobserved and unobservable variables $$U_{X}$$.

![0.9 --> Ux ; Ux --> X ; X --> Y ; Uy --> Y](../assets/img/FCM.png)

~~~
// the causal relationships are deterministic
var X = function(Ux, Uy) {return Ux;};
var Y = function(Ux, Uy) {return and(X(Ux, Uy), Uy)};

// but other factors are at play that we have uncertainty about
var FCM = function() {
  var Ux = flip(0.9);
  var Uy = flip(0.9);
  return { X: X(Ux, Uy), Y: Y(Ux, Uy) };
}

vizPrint(Enumerate(FCM));
~~~

### Example

Suppose we decide to cook bacon, which we often do. The smoke alarm goes off, which it usually does when we cook bacon. Our neighbors get angry, which sometimes happens for other reasons but *definitely* happens when our smoke alarm goes off.

Here's the causal knowledge and random state priors that can represent this story:

~~~
var world2String = function(world) {
  map(function(x) {
    return x[1] ? x[0] : '-';
  }, _.pairs(world)).join('');
};
var strPrint = function(erp) {
  print(Enumerate(function() {
    return world2String(sample(erp));
  }));
  return true;
};
var stories = {
  bacon: {
    //random state priors
    rand : function() {
      return {
        //we will probably cook bacon
        uB: flip(0.9),
        //the smoke alarm is probably working
        uS: flip(0.9),
        //even if the smoke alarm doesn't go off,
        //the neighbors might still be angry for
        //some other reason
        uN: flip(0.1)
      };
    },
    //causal knowledge
    vars: function(rVs) {
      var B = rVs.uB;
      //the smoke alarm will only go off if we're cooking
      //bacon (B) and it's working (uS)
      var S = B & rVs.uS ? true : false;
      //the neighbors will definitely be angry if the smoke
      //alarm goes off. otherwise, they might still be angry
      var N = S | rVs.uN ? true : false;
      return {
        B: B,
        S: S,
        N: N
      };
    }
  }
};
var rand = stories.bacon.rand;
var vars = stories.bacon.vars;
var baconERP = Enumerate(function() {
  var rVs = rand(); //random sate
  var world = vars(rVs); //observable world state
  return world;
});
print('joint distribution');
strPrint(baconERP);
print('marginals');
vizPrint(baconERP);
~~~

## Representing Counterfactual Simulation: Extended Structural Model

The logic behind Lucas & Kemp's Extended Structural Model (ESM) is that wen thinking counterfactually, we keep a lot of the random state of the actual world, but perturb it slightly to get slightly different worlds.

For example, in the situation whether we cooked bacon, the smoke alarm went off and the neighbors are angry, we can imagine what would have happened if the smoke alarm hadn't gone off. How likely is it that we would still have been cooking bacon? This depends on the causal link between cooking bacon and the smoke alarm going off. It depends on what the prior probability of cooking bacon was. It also depends on the fact that in the actually world *we actually did* cook bacon.

The model includes a parameter for the degree of dependence on the actual world. This parameter, s (for "stickiness"), determines whether the parts of *random state* get resampled. The random state variables are independent from one another and the causal information is encoded in the function that maps from the random state to the world state. Therefore, the "sticky" dependence of the counterfactual world on the actual world can be separated in the model from the dependence of downstream variables in the world on their causes (and the "backtracking" statistical dependence of the value of upstream variables on the values of their observed consequents).

~~~
///fold:
// overall utilities

var world2String = function(world) {
  map(function(x) {
    return x[1] ? x[0] : '-';
  }, _.pairs(world)).join('');
};
var strPrint = function(erp) {
  print(Enumerate(function() {
    return world2String(sample(erp));
  }));
  return true;
};
var mapObject = function(fn, obj) {
  return _.object(
      map(
      function(kv) {
        return [kv[0], fn(kv[0], kv[1])]
      },
      _.pairs(obj))
  );
};

// deals with erps that have no support
// (e.g. because conditions on something impossible)
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

// enumerate with an extra "check" step
var checkEnumerate = function(thunk) {
  return checkPossible(Enumerate(thunk));
};

// get probability of erp result "true"
var getP = function(erp, value) {
  var value = value ? value : true;
  return Math.exp(erp.score([], value));
};

var stories = {
  bacon: {
    rand : function() {
      return {
        uB: flip(0.9),
        uS: flip(0.9),
        uN: flip(0.1)
      };
    },
    vars: function(rVs) {
      var B = rVs.uB;
      var S = B & rVs.uS ? true : false;
      var N = S | rVs.uN ? true : false;
      return {
        B: B,
        S: S,
        N: N
      };
    }
  }
};
var rand = stories.bacon.rand;
var vars = stories.bacon.vars;
///

var stickiness = 0.5;

// for counterfactual simulation,
// sometimes (with probability "stickiness") sample the actual
// random statue variable. othertimes, sample a new one from the prior
var stickyRand = function(actualRVs) {
  var freshRVs = rand();
  return mapObject(function(key, val) {
    return flip(stickiness) ? actualRVs[key] : freshRVs[key];
  }, actualRVs);
};

// get value of propsition in a world
var contextualEval = cache(function(proposition, world) {
  var context = reduce(
    function(keyVal, acc) {
      return acc + ' var ' + keyVal[0] + ' = ' + JSON.stringify(keyVal[1]) + ';';
    },
    "",
    _.pairs(world)
  );
  return webpplEval(context + proposition + ';');
});

// counterfactual ERP, conditioning on actual world
var cfERP = function(cfParams) {
  return checkEnumerate(function() {
    //checks to handle different usage
///fold:
    // if no cfAntecedent, condition cfERP on true
    // if no cfConsequent, return world
    // if no actualRVs, use actualWorld
    // if no actualWorld and no actualRVs....sample a completely random world

    if (cfParams.actualRVs & cfParams.actualWorld) {
      print([
        'warning 1234:',
        '*both* RVs and world were given as input!',// for pretty printing
        'actualRVs=',
        JSON.stringify(cfParams.actualRVs),
        '; actualWorld=',
        JSON.stringify(cfParams.actualWorld)
      ].join(' '));
    };

    // if we know the actualRVs, use 'em
    var actualRVs = cfParams.actualRVs ? cfParams.actualRVs : rand();
    var actualWorld = vars(actualRVs);

    // if we know *some* stuff about the actualWorld, condition on that
    var cond3 = (cfParams.actually ?
                 contextualEval(cfParams.actually, actualWorld) :
                 true);
///

    // if we know the actualWorld, condition on that
    var cond1 = (cfParams.actualWorld ?
                 _.isEqual(cfParams.actualWorld, actualWorld) :
                 true);

    var cfRVs = stickyRand(actualRVs);
    var cfWorld = vars(cfRVs);

    // if we are given a counterfactual antecedent, condition on that
    var cond2 = (cfParams.cfAntecedent ?
                 contextualEval(cfParams.cfAntecedent, cfWorld) :
                 true);

    // if cfConsequent specified, return it, else return whole cfWorld
    var result = (cfParams.cfConsequent ?
                  contextualEval(cfParams.cfConsequent, cfWorld) :
                  cfWorld);

    return {
      result: result,
      factor: cond1 & cond2 & cond3 ? 0 : -Infinity
    }
  });
};

var antecedent = '!S';
var consequent = 'B';

getP(cfERP({
  cfAntecedent: '!S',
  cfConsequent: 'B',
  actualWorld: {B: true, S: true, N: true}
}));
~~~

In their paper, Lucas & Kemp found P(B')=0.487 (where B' is the counterfactual value of B).

## Experiments

#### Basic Experiment Design

##### First slide (my words)

We will give you descriptions of different labs that are studying hormones in rats.

Different labs are studying different hormones, so the hormone that one lab calls "A" will not be the same hormone that another lab calls "A".

##### Subsequent slides

Present short story.

In random order, present questions.

e.g.

> If hormone A were observed to be absent, would hormone A be present? <br/>
> <input type='radio' name='response' value='yes'>Yes <br/>
> <input type='radio' name='response' value='no'>No <br/>
> <br/>
> How confident are you in your answer? <br/>
> <input type='radio' name='confidence' value='1'>Not at all confident <br/>
> <input type='radio' name='confidence' value='2'> <br/>
> <input type='radio' name='confidence' value='3'> <br/>
> <input type='radio' name='confidence' value='4'> <br/>
> <input type='radio' name='confidence' value='5'> <br/>
> <input type='radio' name='confidence' value='5'>Completely confident

L&K converted these ratings to subjective probabilities by treating "Not at all confident" responses as 0.5 and linearly interpolating between 0.5 and either 0 or 1, where the direction of interpolation depended on the answer to the yes/no question.

##### Last slide

To get prior probabilities, at the end of each experiment, L&K asked,

> How many, out of 100 mice, does 'a very small number' correspond to?

and rounded the median to the nearest multiple of 5.

These kinds of questions were included as attention checks:

* If you had observed hormone A to be present, would hormone A be present?

Participants who answered these questions incorrectly were excluded.

### Causal Stories

#### story 1: independent

A and B are *a priori* unlikely and causally unrelated. Both happen to be true in this situation.

<img src='../assets/img/story1.png' width='150px'/>

#### story 2: causal chain

A is unlikely *a priori*. A usually causes B and B usually causes C. All happen to be true in this situation.

<img src='../assets/img/story2.png' width='150px'/>

#### story 3: simple cause

A is kind of unlikely *a priori*. A usually causes C (but sometimes B gets in the way).
In this situation, A and B are true but (since B prevents A from causing C) C is false.

<img src='../assets/img/story3.png' width='150px'/>

#### story 4: multiple causes with same strength

A and B are both likely *a priori*. If either (or both) is true, then C is true. C always causes D.
In this situation, all of the variables (A, B, C, D) happen to be false.

<img src='../assets/img/story4.png' width='120px'/>

#### story 5: multiple causes with different strengths

A and B are both somewhat likely *a priori*. A always causes C, but in the absense of A B usually causes C. C always causes D.
In this situation, all of the variables happen to be true.

*Note:* We're not sure whether the link between B and C is on in this case, because either way, A would have caused C.

<img src='../assets/img/story5.png' width='200px'/>

#### story 6: xor causal chain

A is *a priori* kind of unlikely. B is almost always the same as A, but sometimes it's the opposite. C is almost always the same as B, but sometimes it's the opposite. In this situation, A is false and successfully causes B to be false. However, even though B is false, C happens to be true.

<img src='../assets/img/story6.png' width='150px'/>

### Model

~~~
//utilities
///fold:

var mapObject = function(fn, obj) {
  return _.object(
      map(
      function(kv) {
        return [kv[0], fn(kv[0], kv[1])]
      },
      _.pairs(obj))
  );
};

var defaultStickiness = 0.53;

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

var checkEnumerate = function(thunk) {
  return checkPossible(Enumerate(thunk));
};

var getP = function(erp, value) {
  var value = value ? value : true;
  return Math.exp(erp.score([], value));
};
///

// ESM model
// (written slightly more abstractly so we can vary e.g. how stickiness works
//  and what causal story we're using)
///fold:
var model = function(params) {

  // default functions
  var defaultStickyRand = function(rand, stickiness) {
    return function(actualRVs) {
      var freshRVs = rand();
      return mapObject(function(key, val) {
        return flip(stickiness) ? actualRVs[key] : freshRVs[key];
      }, actualRVs);
    };
  };

  // read in parameters of model (e.g. stories, stickiness)
  var rand = params.rand;
  var vars = params.vars;
  var stickiness = params.stickiness ? params.stickiness : defaultStickiness; // from L&K
  var stickyRandMaker = (params.stickyRand ?
                         params.stickyRand :
                         defaultStickyRand);
  var stickyRand = stickyRandMaker(rand, stickiness);
  var impliesComponents = params.impliesComponents ? params.impliesComponents : true;

  // get value of proposition in a particular world
  var contextualEval = cache(function(proposition, world) {
    var context = reduce(
      function(keyVal, acc) {
        return acc + ' var ' + keyVal[0] + ' = ' + JSON.stringify(keyVal[1]) + ';';
      },
      "",
      _.pairs(world)
    );
    return webpplEval(context + proposition + ';');
  });

  // counterfactual ERP, conditioning on actual world
  var cfERP = function(cfParams) {
    return checkEnumerate(function() {
      //checks to handle different usage
      ///fold:
      // if no cfAntecedent, condition cfERP on true
      // if no cfConsequent, return world
      // if no actualRVs, use actualWorld
      // if no actualWorld and no actualRVs....sample a completely random world

      if (cfParams.actualRVs & cfParams.actualWorld) {
        print([
          'warning 1234:',
          '*both* RVs and world were given as input!',// for pretty printing
          'actualRVs=',
          JSON.stringify(cfParams.actualRVs),
          '; actualWorld=',
          JSON.stringify(cfParams.actualWorld)
        ].join(' '));
      };

      // if we know the actualRVs, use 'em
      var actualRVs = cfParams.actualRVs ? cfParams.actualRVs : rand();
      var actualWorld = vars(actualRVs);

      // if we know *some* stuff about the actualWorld, condition on that
      var cond3 = (cfParams.actually ?
                   contextualEval(cfParams.actually, actualWorld) :
                   true);
      ///

      // if we know the actualWorld, condition on that
      var cond1 = (cfParams.actualWorld ?
                   _.isEqual(cfParams.actualWorld, actualWorld) :
                   true);

      var cfRVs = stickyRand(actualRVs);
      var cfWorld = vars(cfRVs);

      // if we are given a counterfactual antecedent, condition on that
      var cond2 = (cfParams.cfAntecedent ?
                   contextualEval(cfParams.cfAntecedent, cfWorld) :
                   true);

      // if cfConsequent specified, return it, else return whole cfWorld
      var result = (cfParams.cfConsequent ?
                    contextualEval(cfParams.cfConsequent, cfWorld) :
                    cfWorld);

      return {
        result: result,
        factor: cond1 & cond2 & cond3 ? 0 : -Infinity
      }
    });
  };

  // run desired function to print results, give it relevant ERPs
  var resultFn = params.resultFn;
  resultFn({
    cfERP: cfERP
  });

  return true;
};

//represent all causal stories in experiment
///fold:
var stories = {
  bacon: {
    rand : function() {
      return {
        uB: flip(0.9),
        uS: flip(0.9),
        uN: flip(0.1)
      };
    },
    vars: function(rVs) {
      var B = rVs.uB;
      var S = B & rVs.uS ? true : false;
      var N = S | rVs.uN ? true : false;
      return {
        B: B,
        S: S,
        N: N
      };
    }
  },
  story1: {
    rand : function() {
      return {
        uA: flip(0.1),
        uB: flip(0.1)
      };
    },
    vars: function(rVs) {
      var A = rVs.uA;
      var B = rVs.uB;
      return {
        A: A,
        B: B
      };
    }
  },
  story2: {
    rand : function() {
      return {
        uA: flip(0.1),
        uAB: flip(0.75),
        uBC: flip(0.75)
      };
    },
    vars: function(rVs) {
      var A = rVs.uA;
      var B = A & rVs.uAB ? true : false;
      var C = B & rVs.uBC ? true : false;
      return {
        A: A,
        B: B,
        C: C
      };
    }
  },
  story3: {
    rand : function() {
      return {
        uA: flip(0.25),
        uB: flip(0.1)
      };
    },
    vars: function(rVs) {
      var A = rVs.uA;
      var B = rVs.uB;
      var C = (A & !B) | (!A & B) ? true : false;
      return {
        A: A,
        B: B,
        C: C
      };
    }
  },
  story4: {
    rand : function() {
      return {
        uA: flip(0.9),
        uB: flip(0.9)
      };
    },
    vars: function(rVs) {
      var A = rVs.uA;
      var B = rVs.uB;
      var C = (A | B) ? true : false;
      var D = C;
      return {
        A: A,
        B: B,
        C: C,
        D: D
      };
    }
  },
  story5: {
    rand : function() {
      return {
        uA: flip(0.75),
        uB: flip(0.75),
        uBC: flip(0.9)
      };
    },
    vars: function(rVs) {
      var A = rVs.uA;
      var B = rVs.uB;
      var C = A ? true : ((B & rVs.uBC) ? true : false);
      var D = C;
      return {
        A: A,
        B: B,
        C: C,
        D: D
      };
    }
  },
  story6: {
    rand : function() {
      return {
        uA: flip(0.25),
        uAB: flip(0.1),
        uBC: flip(0.1)
      };
    },
    vars: function(rVs) {
      var A = rVs.uA;
      var B = (A & !rVs.uAB) | (!A & rVs.uAB) ? true : false;
      var C = (B & !rVs.uBC) | (!B & rVs.uBC) ? true : false;
      return {
        A: A,
        B: B,
        C: C
      };
    }
  }
};
///

// print model results for each story,
// including data from original experiment and replication
///fold:

// a function to pass in to the model
// to run the model on different causal stories and
// print everything out nicely in CSV format
var resultFn = function(resultParams) {
  var story = resultParams.story;
  var antecedent = resultParams.antecedent;
  var consequents = resultParams.consequents;
  var actualWorld = resultParams.actualWorld;
  var stickiness = resultParams.stickiness ? resultParams.stickiness : defaultStickiness;
  var lkModel = resultParams.lkModel;
  var lkData = resultParams.lkData;
  var repData = resultParams.repData;
  var indices = _.range(consequents.length);
  return function(params) {
    var cfERP = params.cfERP;

    map(function(index) {
      var consequent = consequents[index];
      var lkM = lkModel[index];
      var lkD = lkData[index];
      var repD = repData[index];
      var p = getP(cfERP(
        {
          cfAntecedent: antecedent,
          cfConsequent: consequent,
          actualWorld: actualWorld
        }
      ));
      print([
        story,
        stickiness,
        antecedent,
        consequent,
        p,
        lkM,
        lkD,
        repD].join(','));
      return true;
    }, indices);
  };
};

// a function for printing out results of CF model
// on L&K data in CSV format
var printCfModel = function(params) {
  var story = params.story;
  var antecedent = params.antecedent;
  var consequents = params.consequents;
  var actualWorld = params.actualWorld;
  var stickiness = params.stickiness ? params.stickiness : defaultStickiness;
  model({
    rand: (stories[story]).rand,
    vars: (stories[story]).vars,
    stickiness: stickiness,
    resultFn: resultFn(params)
  });
  return true;
};

print([
  'story', 'stickiness', 'antecedent',
  'consequent', 'replicationModelProbability',
  'originalModelProbability',
  'originalDataProbability',
  'replicationDataProbability'].join(','))
printCfModel({
  story: 'story1',
  antecedent: '!A',
  consequents: ['A', 'B'],
  actualWorld: {A: true, B: true},
  lkModel: [0.00, 0.58],
  lkData: [0.00, 0.53],
  repData: [0.309, 0.411]});
printCfModel({
  story: 'story2',
  antecedent: '!B',
  consequents: ['A', 'B', 'C'],
  actualWorld: {A: true, B: true, C: true},
  lkModel: [0.14, 0.00, 0.00],
  lkData: [0.31, 0.03, 0.19],
  repData: [0.391, 0.151, 0.225]});
printCfModel({
  story: 'story3',
  antecedent: '!A',
  consequents: ['A', 'C', 'A', 'B', 'C'],
  actualWorld: {A: true, B: true, C: false},
  lkModel: [0.00, 0.58, 0.00, 0.58, 0.58],
  lkData: [0.03, 0.59, 0.04, 0.41, 0.68],
  repData: [0.0638, 0.0205, 0.4095, 0.4019, 0.4890]});
printCfModel({
  story: 'story4',
  antecedent: 'C',
  consequents: ['A', 'B', 'C', 'D'],
  actualWorld: {A: false, B: false, C: false, D: false},
  lkModel: [0.63, 0.63, 1.00, 1.00],
  lkData: [0.68, 0.70, 0.97, 0.97],
  repData: [0.758, 0.68, 0.975, 0.927]});
printCfModel({
  story: 'story5',
  antecedent: '!C',
  consequents: ['A', 'B', 'C', 'D'],
  actualWorld: {A: true, B: true, C: true, D: true},
  lkModel: [0.00, 0.43, 0.00, 0.00],
  lkData: [0.23, 0.43, 0.03, 0.23],
  repData: [0.351, 0.303, 0.101, 0.162]});
printCfModel({
  story: 'story6',
  antecedent: 'B',
  consequents: ['A', 'B', 'C'],
  actualWorld: {A: false, B: false, C: true},
  lkModel: [0.73, 1.00, 0.42],
  lkData: [0.73, 0.97, 0.51],
  repData: [0.666, 0.896, 0.739]});
print('finished');
///

// this model takes about a minute or two to run and prints out a CSV.
// results are shown in the graphs below.
~~~

L&K used a simple grid search to find parameter values that minimized sum squared error. They considered considered 1000 uniformly spaced parameter values between 0 and 1. Best-fit parameter was s=0.53.

Our model simulations show the same results as Lucas & Kemp 2015. ([caveat](./lucas-kemp-2015-replication.html#embedded-counterfactuals))

<img src='../assets/img/lkrepcf_model.png' width='50%'/>

### Replication Results

Overall our replication wasn't a perfect match to what L&K found, and the model fit isn't perfect to the data. L&K showed that the ESM model did better than other probabilistic models of counterfactuals.

<img src='../assets/img/lkrepcf.png' width='95%'/>

## Notes

### Embedded counterfactuals

See writeup of [embedded counterfactuals](./embedded-counterfactuals.html) (same model, but in Church).
