---
layout: model
title: Questions and Answers
model-language: webppl
---

### Clark (1979) whiskey example

Depending on the context, liquor merchants will be more or less likely to give over-informative answers. Our model accounts for this via inference about the underlying goal, given the context.

~~~~
///fold:
var condition = function(x){
  var score = x ? 0 : -Infinity;
  factor(score);
};

var mean = function(thunk){
  return expectation(Enumerate(thunk), function(v){return v;});
};

var KL = function(erpTrue, erpApprox){
  var values = erpTrue.support([]);
  var xs = map(
    function(value){
      var p = Math.exp(erpTrue.score([], value));
      var q = Math.exp(erpApprox.score([], value));
      if (p == 0.0){
        return 0.0;
      } else {
        return p * Math.log(p / q);
      }
    },
    values);
  return sum(xs);
};

var uniformDraw = function (xs) {
  return xs[randomInteger(xs.length)];
};

///

//   ---------------
// | World knowledge |
//   ---------------

var buyWhiskeyContext = "I'd like to buy some whiskey.";
var spendFiveDollarsContext = "I only have $5 to spend.";

var prices = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

var isNumeric = function(x){
  return _.contains(prices, x);
};

var worldPrior = function(){
  return [uniformDraw(prices)];
};

var price = function(world){
  return world[0];
};

//  -------------------
// | Question knowledge |
//  -------------------

var isMoreThanFiveQuestion = "Does Jim Beam cost more than $5?";

var isMoreThanFiveQuestionMeaning = function(world){
  return price(world) < 5;
};

var questions = [isMoreThanFiveQuestion];

var questionPrior = function(){
  return uniformDraw(questions);
};

//  -----------------
// | Answer knowledge |
//  -----------------

var literalAnswers = ["yes, the whiskey costs more than $5",
                      "no, the whiskey costs less than $5"];

var priceAnswers = map(function(p) {return "the whiskey costs $" + p;}, prices);

// evenly distribute probability across response types
var answerPrior = function(){
  return flip(0.5) ? uniformDraw(literalAnswers) : uniformDraw(priceAnswers);
};

var numericAnswerMeaning = function(number){
  return function(world){
    return price(world) == number;
  };
};

var booleanAnswerMeaning = function(bool){
  return function(world){
    return (price(world) > 5) == bool;
  };
};

//   -----------
// | Interpreter |
//   -----------

var meaning = function(utterance){
  return ((utterance === "yes, the whiskey costs more than $5") ? booleanAnswerMeaning(true) :
          (utterance === "no, the whiskey costs less than $5") ? booleanAnswerMeaning(false) :
          (utterance === isMoreThanFiveQuestion) ? isMoreThanFiveQuestionMeaning :
          _.contains(priceAnswers, utterance) ? numericAnswerMeaning(last(utterance.split('$'))*1) :
          console.error('unknown utterance!', utterance));
};

var interpreter = cache(function(answer){
  return Enumerate(function(){
    var world = worldPrior();
    var answerMeaning = meaning(answer);
    condition(answerMeaning(world));
    return world;
  });
});

var makeTruthfulAnswerPrior = function(trueWorld) {
  var truthfulAnswerPrior = Enumerate(function(){
    var answer = answerPrior();
    factor(interpreter(answer).score([], trueWorld));
    return answer;
  });
  return truthfulAnswerPrior;
};

//  ------
// | QUDs |
//  ------

var qudPrice = function(world){return price(world);};
var qudPriceGreaterThan5 = function(world){return price(world) > 5;};

var qudPrior = function(context){
  var p = ((context === buyWhiskeyContext) ? 0.5 :
           (context === spendFiveDollarsContext) ? 0.99 :
           console.error('unknown context'));
  return (flip(p) ? "qudPriceGreaterThan5" : "qudPrice");
};

var nameToQUD = function(qudName){
  return (qudName == "qudPriceGreaterThan5" ? qudPriceGreaterThan5 :
          qudName == "qudPrice" ? qudPrice :
          qudName == isMoreThanFiveQuestion ? isMoreThanFiveQuestionMeaning :
          console.error('unknown qud name', qudName));
};

//  -------
// | Models |
//  -------

var explicitAnswerer = cache(function(question, trueWorld, rationality) {
  var qud = nameToQUD(question);
  return Enumerate(function(){
    var truthfulAnswerPrior = makeTruthfulAnswerPrior(trueWorld);
    var answer = sample(truthfulAnswerPrior);
    var score = mean(function(){
      var inferredWorld = sample(interpreter(answer));
      return (qud(trueWorld) == qud(inferredWorld) ? 1 : 0);
    });
    factor(Math.log(score) * rationality);
    return answer;
  });
});

var explicitQuestioner = cache(function(qudName, rationality) {
  var qud = nameToQUD(qudName);
  return Enumerate(function(){
    var question = questionPrior();
    var prior = Enumerate(function(){
      return qud(worldPrior());});
    var expectedKL = mean(function(){
      var trueWorld = worldPrior();
      var answer = sample(explicitAnswerer(question, trueWorld, rationality));
      var posterior = Enumerate(function(){
        var world = sample(interpreter(answer));
        return qud(world);
      });
      return KL(posterior, prior);
    });
    factor(expectedKL * rationality);
    return question;
  });
});

var pragmaticAnswerer = function(context, question, trueWorld, rationality){
  var qudPosterior = Enumerate(function(){
    var qudName = qudPrior(context);
    var qud = nameToQUD(qudName);
    var q_erp = explicitQuestioner(qudName, rationality);
    factor(q_erp.score([], question));
    return qudName;
  });
  return Enumerate(function(){
    var qud = nameToQUD(sample(qudPosterior));
    var truthfulAnswerPrior = makeTruthfulAnswerPrior(trueWorld);
    var answer = sample(truthfulAnswerPrior);
    var score = mean(
      function(){
        var inferredWorld = sample(interpreter(answer));
        return (qud(trueWorld) == qud(inferredWorld)) ? 1.0 : 0.0;
      });
    factor(Math.log(score) * rationality);
    return answer;
  });
};

var world = [4];

print(buyWhiskeyContext + " " + isMoreThanFiveQuestion);
print(pragmaticAnswerer(buyWhiskeyContext, isMoreThanFiveQuestion, world, 1));

print(spendFiveDollarsContext + " " + isMoreThanFiveQuestion);
print(pragmaticAnswerer(spendFiveDollarsContext, isMoreThanFiveQuestion, world, 1));

~~~~

### Mention-some example

We set up the mention-some disambiguation problem below, using the same structure as the previous model. This example uses a more structured world space and answer prior, but uses the same mechanism where context shifts the QUD prior.

~~~~
///fold:
var condition = function(x){
  var score = x ? 0 : -Infinity;
  factor(score);
};

var mean = function(thunk){
  return expectation(Enumerate(thunk), function(v){return v;});
};

var allTrue = function(boolList) {
  return reduce(function(val, memo) {
    return val && memo;
  }, true, boolList)
}

var allFalse = function(boolList) {
  return reduce(function(val, memo) {
    return !val && memo;
  }, true, boolList)
}

var getFilteredCafeList = function(world) {
  var cafeList = map(function(value) {
    var hasNewspaper = world[value][1];
    if(hasNewspaper) {
      return value;
    } else {
      return []
    }
  }, _.keys(world))

  var filteredCafeList = filter(function(val){
    if(_.isEmpty(val))
      return false
    else
      return true
  }, cafeList)

  return filteredCafeList;
}

var pickAllNewspaperCafes = function(world) {
  var filt = getFilteredCafeList(world)
  if(_.isEmpty(filt))
    return 'none'
  else
    return filt
}

var pickClosestNewspaperCafe = function(world) {
  var filt = getFilteredCafeList(world);
  if(_.isEmpty(filt)) {
    return 'none'
  } else {
    return minWith(function(k) {
      return world[k][0];
    }, filt)[0]
  }
}                               

var KL = function(erpTrue, erpApprox){
  var values = erpTrue.support([]);
  var xs = map(
    function(value){
      var p = Math.exp(erpTrue.score([], value));
      var q = Math.exp(erpApprox.score([], value));
      if (p == 0.0){
        return 0.0;
      } else {
        return p * Math.log(p / q);
      }
    },
    values);
  return sum(xs);
};

var flatten = function(xs){
  if (xs.length == 0) {
    return [];
  } else {
    return xs[0].concat(flatten(xs.slice(1)));
  }
};

var powerset = function(set) {
  if (set.length == 0)
    return [[]];
  else {
    var rest = powerset(set.slice(1));
    return map(
      function(element) {
        return [set[0]].concat(element);
      },
      rest).concat(rest);
  }
}

var all = function(p,l) { 
  return mapReduce1(function(a,b){ return a & b; }, p, l); };

var butLast = function(xs){
  return xs.slice(0, xs.length-1);
};

var uniformDraw = function (xs) {
  return xs[randomInteger(xs.length)];
};
///

//   ---------------
// | World knowledge |
//   ---------------

var distances = [1,3]

var cafes = ['cafe1', 'cafe2', 'cafe3', 'cafe4']
var cafePowerset = powerset(cafes);

var touristContext = "I'm new in town.";
var businesspersonContext = "I'm trying to set up a newspaper distribution business.";

var isCafeList = function(x){
  return allTrue(map(function(v) {return _.contains(cafes, v)}, x));
};

var countCafeCombinations = function(n) {
  return filter(function(l) {return l.length == n}, cafePowerset).length;
};

var worldPrior = function(){
  return {
    'cafe1' : [uniformDraw(distances), flip(.5)],
    'cafe2' : [uniformDraw(distances), flip(.5)],
    'cafe3' : [uniformDraw(distances), flip(.5)],
    'cafe4' : [uniformDraw(distances), flip(.5)]
  }
}

var hasNewspaper = function(world, cafe) {
  if(_.contains(_.keys(world), cafe))
    return world[cafe][1]
  else
    return false
}

// Returns the distance of a cafe
var distance = function(world, cafe) {
  return world[cafe][0]
}

//  -------------------
// | Question knowledge |
//  -------------------

var newspaperQuestion = "Where can one buy an Italian newspaper?";

// projects from a world to the relevant properties for the desired answer
var newspaperQuestionMeaning = function(world){
  return _.object(map(function(cafe){
    return [cafe, hasNewspaper(world, cafe)]
  }, cafes))
};

var questions = [newspaperQuestion]

var questionPrior = function(){
  return uniformDraw(questions);
};

//  -----------------
// | Answer knowledge |
//  -----------------

// (truncated) geometric distribution
var answerPrior = function(){
  var tempAnswer = uniformDraw(cafePowerset)
  var score = (Math.pow(.5, tempAnswer.length + 1)
               / countCafeCombinations(tempAnswer.length))
  factor(Math.log(score))
  return (tempAnswer.length == 0 ? ['none'] : tempAnswer)
};

var cafeAnswerMeaning = function(cafeList){
  return function(world){
    var doTheyHaveNewspapers = map(function(cafe) {
      hasNewspaper(world, cafe)
    }, cafeList);
    return allTrue(doTheyHaveNewspapers);
  };
};

var noneMeaning = function() {
  return function(world){
    var doTheyHaveNewspapers = map(function(cafe) {
      hasNewspaper(world, cafe)
    }, cafes);
    return allFalse(doTheyHaveNewspapers);
  }
}

//   -----------
// | Interpreter |
//   -----------

var meaning = function(utterance){
  return (isCafeList(utterance) ? cafeAnswerMeaning(utterance) :
         (_.isEqual(utterance, [ "none" ])) ? noneMeaning() : 
         (utterance === newspaperQuestion) ? newspaperQuestionMeaning :
         console.error('unknown utterance!', utterance));
};

var interpreter = cache(function(answer){
  return Enumerate(function(){
    var world = worldPrior();
    var answerMeaning = meaning(answer);
    condition(answerMeaning(world));
    return world;
  });
});

var makeTruthfulAnswerPrior = function(trueWorld) {
  var truthfulAnswerPrior = Enumerate(function(){
    var answer = answerPrior();
    factor(interpreter(answer).score([], trueWorld));
    return answer
  });
  return truthfulAnswerPrior;
};

//  ------
// | QUDs |
//  ------

var qudAll = function(world){return pickAllNewspaperCafes(world);};
var qudClosest = function(world){return pickClosestNewspaperCafe(world);};

var qudPrior = function(context){
  var p = ((context === businesspersonContext) ? 0.9 :
           (context === touristContext) ? 0.1 :
           console.error('unknown context'));
  return (flip(p) ? "qudAll" :
          "qudClosest");
};

var nameToQUD = function(qudName){
  return (qudName == "qudClosest" ? qudClosest :
          qudName == "qudAll" ? qudAll :
          qudName == newspaperQuestion ? newspaperQuestionMeaning :
          console.error('unknown qud name', qudName));
};

//  -------
// | Models |
//  -------

var explicitAnswerer = cache(function(question, trueWorld, rationality) {
  var qud = nameToQUD(question);
  return Enumerate(function(){
    var truthfulAnswerPrior = makeTruthfulAnswerPrior(trueWorld);
    var answer = sample(truthfulAnswerPrior);
    var score = mean(function(){
      var inferredWorld = sample(interpreter(answer));
      return (_.isEqual(qud(trueWorld), qud(inferredWorld)) ? 1 : 0);
    });
    factor(Math.log(score) * rationality);
    return answer;
  });
});  

var explicitQuestioner = function(qudName, rationality) {
  var qud = nameToQUD(qudName);
  return Enumerate(function(){
    var question = questionPrior();
    var prior = Enumerate(function(){
      return qud(worldPrior());
    });
    var expectedKL = mean(function(){
      var trueWorld = worldPrior();
      var answer = sample(explicitAnswerer(question, trueWorld, rationality));
      var posterior = Enumerate(function(){
        var world = sample(interpreter(answer));
        return qud(world);
      });
      return KL(posterior, prior);
    });
    factor(expectedKL * rationality);
    return question;
  });
};

var pragmaticAnswerer = function(context, question, trueWorld, rationality){
  var qudPosterior = Enumerate(function(){
    var qudName = qudPrior(context);
    var qud = nameToQUD(qudName);
    var q_erp = explicitQuestioner(qudName, rationality);
    factor(q_erp.score([], question));
    return qudName;
  });
  return Enumerate(function(){
    var qudName = sample(qudPosterior);
    var qud = nameToQUD(qudName);
    // Pick answer conditioned on communicating question predicate value
    var truthfulAnswerPrior = makeTruthfulAnswerPrior(trueWorld);
    var answer = sample(truthfulAnswerPrior);
    var score = mean(
      function(){
        var inferredWorld = sample(interpreter(answer));
        return (_.isEqual(qud(trueWorld), qud(inferredWorld))) ? 1.0 : 0.0;
      });
    factor(Math.log(score) * rationality);
    return answer;
  });
};

var world = {'cafe1' : [3, false],
             'cafe2' : [1, true],
             'cafe3' : [3, true],
             'cafe4' : [3, true]}

print("world", world);

print(businesspersonContext, newspaperQuestion);
print(pragmaticAnswerer(businesspersonContext, newspaperQuestion, world,1));

print(touristContext, newspaperQuestion);
print(pragmaticAnswerer(touristContext, newspaperQuestion, world,1));

~~~~

### Gibbs Jr. & Bryant (2008) time example

Here, we use a more sophisticated family of QUDs, and inference takes place over these alternatives instead of using a context.

~~~~

///fold:
var identity = function(x){return x;};

var negate = function(predicate){
  return function(x){
    return !predicate(x);
  };
};

var condition = function(x){
  var score = x ? 0 : -Infinity;
  factor(score);
};

var mean = function(thunk){
  return expectation(Enumerate(thunk), function(v){return v;});
};

var KL = function(erpTrue, erpApprox){
  var values = erpTrue.support([]);
  var xs = map(
    function(value){
      var p = Math.exp(erpTrue.score([], value));
      var q = Math.exp(erpApprox.score([], value));
      if (p == 0.0){
        return 0.0;
      } else {
        return p * Math.log(p / q);
      }
    },
    values);
  return sum(xs);
};

var flatten = function(xs){
  if (xs.length == 0) {
    return [];
  } else {
    return xs[0].concat(flatten(xs.slice(1)));
  }
};

var uniformDraw = function (xs) {
  return xs[randomInteger(xs.length)];
};

var timeDiff = function(timeString1, timeString2) {
  var hr1 = timeString1.split(":")[0]
  var hr2 = timeString2.split(":")[0]
  var min1 = 60*hr1 + 1*timeString1.split(":")[1]
  var min2 = 60*hr2 + 1*timeString2.split(":")[1]
  return Math.abs(min2 - min1)
}

var getEveryFifthTime = function(list) {
  return filter(function(num) {
    return num[3] % 5 == 0;
  }, list);
};

var getNonFifthTimes = function(list) {
  return filter(function(num) {
    return num[3] % 5 != 0;
  }, list);
};

var roundToNearest = function(time) {
  var hour = time.split(":")[0]
  var minutes = time.split(":")[1]
  var roundedMinutes = 5 * Math.round(minutes / 5)
  return hour + ":" + roundedMinutes
}

///

//   ---------------
// | World knowledge |
//   ---------------

var times = map(function(n) {return "3:" + n;}, _.range(30, 60));

// World state is just the true current time
var worldPrior = function(){
  return uniformDraw(times)
};

//  -------------------
// | Question knowledge |
//  -------------------

var timeQuestion = "What time is it?";

// projects from a world to the relevant properties for the desired answer
var timeQuestionMeaning = function(world){
  return world
};

var questions = [timeQuestion] 

var questionPrior = function(){
  return uniformDraw(questions);
};

//  -----------------
// | Answer knowledge |
//  -----------------

// saying rounded times is easier (b/c analog watches)
var answerPrior = function(){
  var ans = (flip(.25) ?
             uniformDraw(getNonFifthTimes(times)) :
             uniformDraw(getEveryFifthTime(times)));
  return ans;
};

var timeAnswerMeaning = function(currTime){
  return function(world){
    return timeDiff(currTime, world) == 0;
  };
};

//   -----------
// | Interpreter |
//   -----------

var meaning = function(utterance){
  return (_.contains(times, utterance) ? timeAnswerMeaning(utterance) :
         (utterance === timeQuestion) ? timeQuestionMeaning :
         console.error('unknown utterance!', utterance));
};

var interpreter = cache(function(answer){
  return Enumerate(function(){
    var world = worldPrior();
    var answerMeaning = meaning(answer);
    condition(answerMeaning(world));
    return world;
  });
});

//  ------
// | QUDs |
//  ------

var qudFactory = function(threshold) {
  return function(world){
    if(world < threshold) {
      return roundToNearest(world);
    } else {
      return world;
    }
  };
};

// Family of quds parameterized by threshold at which "running late"
// Thresholds closer to the appointment time (provided by context) are more likely
var qudPrior = function(context){
  var timeDiffs = map(function(time) {return timeDiff(time, context)},
                      times);
  var maxDiff = Math.max.apply(null, timeDiffs);
  var timeProbs = map(function(timeDiff) {return maxDiff - timeDiff;},
                      timeDiffs);
  var qudWord = (flip(0.25) ?
                 "Exact" :
                 categorical(timeProbs, times));
  return "qud" + qudWord;
};

var nameToQUD = function(qudName){
  if (qudName == timeQuestion) {
    return timeQuestionMeaning;
  } else if (qudName === "qudExact") {
    return (function(world) {return world;});
  } else {
    var threshold = qudName.slice(3);
    return qudFactory(threshold);
  }
};

//  -------
// | Models |
//  -------

var explicitAnswerer = cache(function(question, trueWorld, rationality) {
  var qud = nameToQUD(question);
  return Enumerate(function(){
    var answer = answerPrior();
    var score = mean(function(){
      var inferredWorld = sample(interpreter(answer));
      return (_.isEqual(qud(trueWorld), qud(inferredWorld)) ? 1 : 0);
    });
    factor(Math.log(score) * rationality);
    return answer;
  });
});  

var explicitQuestioner = function(qudName, rationality) {
  var qud = nameToQUD(qudName);
  return Enumerate(function(){
    var question = questionPrior();
    var prior = Enumerate(function(){
      return qud(worldPrior());
    });
    var expectedKL = mean(function(){
      var trueWorld = worldPrior();
      var answer = sample(explicitAnswerer(question, trueWorld, rationality));
      var posterior = Enumerate(function(){
        var world = sample(interpreter(answer));
        return qud(world);
      });
      return KL(posterior, prior);
    });
    factor(expectedKL * rationality);
    return question;
  });
};

var pragmaticAnswerer = function(context, question, trueWorld, rationality){
  var qudPosterior = Enumerate(function(){
    var qudName = qudPrior(context);
    var qud = nameToQUD(qudName);
    var q_erp = explicitQuestioner(qudName, rationality);
    factor(q_erp.score([], question));
    return qudName;
  });
  return Enumerate(function(){
    var qudName = sample(qudPosterior);
    var qud = nameToQUD(qudName);
    var answer = answerPrior(); // note we DON'T use truthfulAnswerPrior here
    var score = mean(function(){
      var inferredWorld = sample(interpreter(answer));
      return (_.isEqual(qud(trueWorld), qud(inferredWorld))) ? 1.0 : 0.0;
    });
    factor(Math.log(score) * rationality);
    return answer;
  });
};

var appointmentContext = "4:00";

var runModel = function(group) {
  return mean(function(){
    var trueWorld = worldPrior();
    var ansERP = pragmaticAnswerer(appointmentContext, timeQuestion, trueWorld, 1);
    condition(group === "early" ?
	      trueWorld.slice(2) < 45 :
	      trueWorld.slice(2) > 45);
    return Math.exp(ansERP.score([], roundToNearest(trueWorld)));
  });
};

print(Enumerate(function(){return qudPrior(appointmentContext)}));

print("early rounds p = " + runModel("early"));
print("late rounds p = " + runModel("late"));

~~~~

### Clark (1979) credit cards example

Our final example demonstrates how our pragmatic answerer can use Gricean reasoning to infer underlying goals directly from the question utterance (via a model of the questioner) instead of using broader context clues.

~~~~





///fold:
var identity = function(x){return x;};

var negate = function(predicate){
  return function(x){
    return !predicate(x);
  };
};

var condition = function(x){
  var score = x ? 0 : -Infinity;
  factor(score);
};

var mean = function(thunk){
  return expectation(Enumerate(thunk), function(v){return v;});
};

var KL = function(erpTrue, erpApprox){
  var values = erpTrue.support([]);
  var xs = map(
    function(value){
      var p = Math.exp(erpTrue.score([], value));
      var q = Math.exp(erpApprox.score([], value));
      if (p == 0.0){
	return 0.0;
      } else {
	return p * Math.log(p / q);
      }
    },
    values);
  return sum(xs);
};

var flatten = function(xs){
  if (xs.length == 0) {
    return [];
  } else {
    return xs[0].concat(flatten(xs.slice(1)));
  }
};

var powerset = function(set) {
  if (set.length == 0)
    return [[]];
  else {
    var rest = powerset(set.slice(1));
    return map(
      function(element) {
	return [set[0]].concat(element);
      },
      rest).concat(rest);
  }
}

var getFilteredCardList = function(world) {
  var cardList = map(function(value) {
    var hasCard = world[value];
    if(hasCard) {
      return value;
    } else {
      return []
    }
  }, _.keys(world))

  var filteredCardList = filter(function(val){
    if(_.isEmpty(val))
      return false
    else
      return true
  }, cardList)

  return filteredCardList;
}

var allFalse = function(boolList) {
  return reduce(function(val, memo) {
    return !val && memo;
  }, true, boolList)
}

var butLast = function(xs){
  return xs.slice(0, xs.length-1);
};

var uniformDraw = function (xs) {
  return xs[randomInteger(xs.length)];
};

///


// --------------------------------------------------------------------

//   ---------------
// | World knowledge |
//   ---------------

var cardTypes = ['Visa','MasterCard', 'AmericanExpress', 'Diners', 'CarteBlanche'];

var cardPowerSet = powerset(cardTypes);

var cardLikelihoods = {
  'Visa' : 0.72,
  'MasterCard' : 0.71,
  'AmericanExpress' : 0.5,
  'Diners' : 0.12,
  'CarteBlanche' : 0.10
};

// Reflect real probabilities of acceptance from Clark
var worldPrior = function(){
  return {
    'Visa' : flip(cardLikelihoods['Visa']),
    'MasterCard' : flip(cardLikelihoods['MasterCard']),
    'AmericanExpress' : flip(cardLikelihoods['AmericanExpress']),
    'Diners' : flip(cardLikelihoods['Diners']),
    'CarteBlanche' : flip(cardLikelihoods['CarteBlanche'])
  };
};

var interestedInCard = function(world, card) {
  if(_.contains(_.keys(world), card))
    return world[card];
  else
    return false;
};

//  -------------------
// | Question knowledge |
//  -------------------

var masterCardQuestion = "Do you accept Master Card?";
var masterCardQuestionMeaning = function(world){
  return world['MasterCard'];
};

var VisaQuestion = "Do you accept Visa card?";
var VisaQuestionMeaning = function(world){
  return world['Visa'];
};

var dinersQuestion = "Do you accept Diners card?";
var dinersQuestionMeaning = function(world){
  return world['Diners'];
};

var carteBlancheQuestion = "Do you accept Carte Blanche?";
var carteBlancheQuestionMeaning = function(world){
  return world['CarteBlanche'];
};

var AmericanExpressQuestion = "Do you accept American Express?";
var AmericanExpressQuestionMeaning = function(world){
  return world['AmericanExpress'];
};

var creditCardsQuestion = "Do you accept credit cards?";
var creditCardsQuestionMeaning = function(world){
  return _.some(_.values(world));
};

var questions = [masterCardQuestion, AmericanExpressQuestion, VisaQuestion,
		 dinersQuestion, carteBlancheQuestion,
		 creditCardsQuestion];

// Penalize questions for their length
var questionPrior = function(){
  var q = uniformDraw(questions);
  factor(- Math.log(q.split(' ').length));
  return q;
};

//  -----------------
// | Answer knowledge |
//  -----------------

var cardAnswerSpace = powerset(cardTypes);

var countAnswerCombinations = function(n) {
  return filter(function(l) {return l.length == n;}, cardAnswerSpace).length;
};

var booleanAnswerSpace = ["yes", "no"];

// Say 'yes' 'no' or some combination of cards
var answerPrior = function(){
  // prefer yes/no over detailed answer
  return (flip(0.5) ?
	  uniformDraw(booleanAnswerSpace) :
	  uniformDraw(cardAnswerSpace));
};

var cardAnswerMeaning = function(cardList){
  return function(questionMeaning){
    return function(world){
      return _.every(map(function(card) {
	return world[card];
      }, cardList));
    };
  };
};

var booleanAnswerMeaning = function(bool){
  return function(questionMeaning){
    return function(world){
      if (questionMeaning == masterCardQuestionMeaning){
	return (world['MasterCard'] == bool);
      } else if (questionMeaning == VisaQuestionMeaning) {
	return (world['Visa'] == bool);
      } else if (questionMeaning == carteBlancheQuestionMeaning) {
	return (world['CarteBlanche'] == bool);
      } else if (questionMeaning == dinersQuestionMeaning) {
	return (world['Diners'] == bool);
      } else if (questionMeaning == AmericanExpressQuestionMeaning){
	return (world['AmericanExpress'] == bool);
      } else if (questionMeaning == creditCardsQuestionMeaning){
	return (_.some(_.values(world)) == bool);
      } else {
	console.error("unknown question meaning");
      }
    };
  };
};

var noneMeaning = function() {
  return function(questionMeaning){
    return function(world){
      var areTheyInterestedInCards = map(function(card) {
	isInterestedInCard(world, card);
      }, cardTypes);
      return allFalse(areTheyInterestedInCards);
    };
  };
};

var cardUtterance = function(utterance) {
  var filteredList = filter(function(x) {
    return _.isEqual(x, utterance);
  }, cardAnswerSpace);
  return !_.isEmpty(filteredList);
};

var booleanUtterance = function(utterance) {
  var filteredList = filter(function(x) {
    return _.isEqual(x, utterance);
  }, booleanAnswerSpace);
  return !_.isEmpty(filteredList);
};

//   -----------
// | Interpreter |
//   -----------

var meaning = function(utterance){
  return (utterance === "yes" ? booleanAnswerMeaning(true) :
	  utterance === "no" ? booleanAnswerMeaning(false) :
	  cardUtterance(utterance) ? cardAnswerMeaning(utterance) :
	  _.isEqual(utterance, [ "none" ]) ? noneMeaning() :
	  (utterance === masterCardQuestion) ? masterCardQuestionMeaning :
	  (utterance === VisaQuestion) ? VisaQuestionMeaning :
	  (utterance === dinersQuestion) ? dinersQuestionMeaning :
	  (utterance === carteBlancheQuestion) ? carteBlancheQuestionMeaning :
	  (utterance === AmericanExpressQuestion) ? AmericanExpressQuestionMeaning :
	  (utterance === creditCardsQuestion) ? creditCardsQuestionMeaning :
	  console.error('unknown utterance in meaning!', utterance));
};

var interpreter = cache(function(question, answer){
  return Enumerate(function(){
    var world = worldPrior();
    var answerMeaning = meaning(answer);
    var questionMeaning = meaning(question);
    condition(answerMeaning(questionMeaning)(world));
    return world;
  });
});

var checkExhaustive = function(answer, trueWorld) {
  var cardsAccepted = filter(function(key){
    return trueWorld[key];
  }, _.keys(trueWorld));
  return _.isEqual(answer, cardsAccepted);
};

var makeTruthfulAnswerPrior = function(question, trueWorld) {
  var truthfulAnswerPrior = Enumerate(function(){
    var answer = answerPrior();
    var possibleWorlds = interpreter(question, answer);
    var containsTrueWorld = _.some(map(function(v){
      return _.isEqual(trueWorld, v);
    }, possibleWorlds.support()));
    var exhaustive = (cardUtterance(answer)
		      ? checkExhaustive(answer, trueWorld)
		      : true);
    condition(containsTrueWorld & exhaustive);
    return answer;
  });
  return truthfulAnswerPrior;
};

//  ------
// | QUDs |
//  ------

var qudFactory = function(cardString) {
  var cardList = cardString.split(",");
  return function(world){
    return _.some(map(function(card){
      return world[card];
    }, cardList));
  };
};

var cardSetLikelihood = function(cardSet) {
  var cardsNotInSet = _.difference(cardTypes, cardSet);
  var inSetScore = reduce(function(v, memo){
    return memo * cardLikelihoods[v];
  }, 1, cardSet);
  var outOfSetScore = reduce(function(v, memo) {
    return memo * (1 - cardLikelihoods[v]);
  }, 1, cardsNotInSet);
  return inSetScore * outOfSetScore;
};

var qudPrior = function(){
  var cardSet = cardSetPrior();
  return "QUD" + cardSet;
};

var cardSetPrior = function(){
  var filteredPowerSet = filter(function(cardSet){
    return !_.isEmpty(cardSet);
  }, cardPowerSet);
  var cardSet = uniformDraw(filteredPowerSet);
  factor(cardSetLikelihood(cardSet));
  return cardSet;
};

var nameToQUD = function(qudName){
  if (qudName == masterCardQuestion) {
    return masterCardQuestionMeaning;
  } else if (qudName == VisaQuestion) {
    return VisaQuestionMeaning;
  } else if (qudName == dinersQuestion) {
    return dinersQuestionMeaning;
  } else if (qudName == carteBlancheQuestion) {
    return carteBlancheQuestionMeaning;
  } else if (qudName == AmericanExpressQuestion) {
    return AmericanExpressQuestionMeaning;
  } else if (qudName == creditCardsQuestion) {
    return creditCardsQuestionMeaning;
  } else if (qudName.slice(0,3) === "QUD") {
    return qudFactory(qudName.slice(3));
  }
};

//  -------
// | Models |
//  -------

var explicitAnswerer = cache(function(question, trueWorld, rationality) {
  var qud = nameToQUD(question);
  return Enumerate(function(){
    var truthfulAnswerPrior = makeTruthfulAnswerPrior(question, trueWorld);
    var answer = sample(truthfulAnswerPrior);
    var score = mean(function(){
      var inferredWorld = sample(interpreter(question, answer));
      return (qud(trueWorld) == qud(inferredWorld) ? 1 : 0);
    });
    factor(Math.log(score) * rationality);
    return answer;
  });
});

var explicitQuestioner = cache(function(qudName, rationality) {
  var qud = nameToQUD(qudName);
  return Enumerate(function(){
    var question = questionPrior();
    var prior = Enumerate(function(){
      return qud(worldPrior());});
    var expectedKL = mean(function(){
      var trueWorld = worldPrior();
      var answer = sample(explicitAnswerer(question, trueWorld, rationality));
      var posterior = Enumerate(function(){
	var world = sample(interpreter(question, answer));
	return qud(world);
      });
      return KL(posterior, prior);
    });
    factor(expectedKL * rationality);
    return question;
  });
});

var pragmaticAnswerer = function(question, trueWorld, rationality){
  var qudPosterior = Enumerate(function(){
    var qudName = qudPrior();
    var qud = nameToQUD(qudName);
    var q_erp = explicitQuestioner(qudName, rationality);
    factor(q_erp.score([], question));
    return qudName;
  });
  //  print(qudPosterior);
  return Enumerate(function(){
    var qud = nameToQUD(sample(qudPosterior));
    var truthfulAnswerPrior = makeTruthfulAnswerPrior(question, trueWorld);
    var answer = sample(truthfulAnswerPrior);
    var score = mean(
      function(){
	var inferredWorld = sample(interpreter(question, answer));
	return (qud(trueWorld) == qud(inferredWorld)) ? 1.0 : 0.0;
      });
    factor(Math.log(score) * rationality);
    return answer;
  });
};

var world = {
  'Visa' : true,
  'MasterCard' : false,
  'AmericanExpress' : true,
  'Diners' : false,
  'CarteBlanche' : false
};

var runModel = function(question) {
  return mean(function(){
    var trueWorld = worldPrior();
    var ansERP = pragmaticAnswerer(question, trueWorld, 100000);
    var literalAns = Math.exp(ansERP.score([], "yes")) || Math.exp(ansERP.score([], "no"));
    return literalAns;
  });
};

var visualizeQUDPosterior = function(question, rationality) {
  print(Enumerate(function(){
    var qudName = qudPrior();
    var qud = nameToQUD(qudName);
    var q_erp = explicitQuestioner(qudName, rationality);
    factor(q_erp.score([], question));
    return qudName;
  }));
};

print(creditCardsQuestion);
print(runModel(creditCardsQuestion));

print(masterCardQuestion);
print(runModel(masterCardQuestion));

print(AmericanExpressQuestion);
print(runModel(AmericanExpressQuestion));

visualizeQUDPosterior(AmericanExpressQuestion, 10000);
visualizeQUDPosterior(creditCardsQuestion, 10000);

~~~~

