---
layout: model
title: Questions and Answers
model-language: webppl
model-language-version: v0.9.9
---

### Sensitivity to question

Answerer should respond to 'what time do you close?' differently than 'what is the price of a fifth of Jim Beam?'

~~~~
///fold:
var toPairs = function(obj){
  return _.sortBy(_.toPairs(obj));
}

var toObject = function(pairs){
  return _.zipObject(pairs);
}

var makeObjectScorer = function(dist) {
  var newDist = Infer({method: 'enumerate'}, function(){
    return toPairs(sample(dist));
  });
  return function(value) {
    return newDist.score(toPairs(value));
  }
}

var KL = function(erpTrue, erpApprox){                       
  var values = erpTrue.support([]);                          
  return sum(map(                                            
    function(value){                                         
      var scoreP = erpTrue.score(value);                     
      var scoreQ = erpApprox.score(value);                   
      var probP = Math.exp(scoreP);                          
      return probP == 0.0 ? 0.0 : probP * (scoreP - scoreQ); 
    }, values));                                             
};                                                           
///

//   ---------------
// | World knowledge |
//   ---------------

var prices = [4, 5, 6];
var closingTimes = [8, 9];

var worldPrior = function(){
  return {price: uniformDraw(prices),
          time: uniformDraw(closingTimes)}
};

//  -------------------
// | Question knowledge |
//  -------------------

var timeQuestion = "What time do you close tonight?";
var priceQuestion = "What is the price of a fifth of Jim Beam?";

var timeQuestionMeaning = function(w) {return w.time}
var priceQuestionMeaning = function(w) {return w.price}

var questions = [timeQuestion, priceQuestion];
var questionPrior = function(){return uniformDraw(questions)};

//  -----------------
// | Answer knowledge |
//  -----------------

var priceAnswers = map(function(p) {return "the whiskey costs $" + p;}, prices);
var timeAnswers = map(function(t) {return "we close at " + t;}, closingTimes);

// evenly distribute probability across response types
var answerPrior = function(){
  return flip(.5) ? uniformDraw(priceAnswers) : uniformDraw(timeAnswers)
};

var timeAnswerMeaning = function(time){
  return function(world){return world.time == time};
};

var priceAnswerMeaning = function(price){
  return function(world){return world.price == price};
};

//   -----------
// | Interpreter |
//   -----------

var meaning = function(utterance){
  return ((utterance === timeQuestion) ? timeQuestionMeaning :
          (utterance === priceQuestion) ? priceQuestionMeaning :
          _.includes(timeAnswers, utterance) ? timeAnswerMeaning(last(utterance.split(' '))*1.0) :
          _.includes(priceAnswers, utterance) ? priceAnswerMeaning(last(utterance.split('$'))*1.0) :
          console.error('unknown utterance!', utterance));
};
                                                                              
var interpreter = cache(function(answer){                                     
  return Infer({method: 'enumerate'}, function(){                             
    var world = worldPrior();                                                 
    var answerMeaning = meaning(answer);                                      
    condition(answerMeaning(world));                                          
    return world;                                                             
  });                                                                         
});                                                                           

var makeTruthfulAnswerPrior = function(trueWorld) {
  return Infer({method: 'enumerate', model: function(){
    var answer = answerPrior();
    var interpScorer = makeObjectScorer(interpreter(answer));
    factor(interpScorer(trueWorld))
    return answer;
  }});
};

//  ------
// | QUDs |
//  ------

var qudPrice = function(world){return world.price;};
var qudTime = function(world){return world.time;};

var qudPrior = function(){
  return (flip(.5) ? "qudTime" : "qudPrice");
};

var QUDPosteriorFromMeaning = function(question) {
  var correspondingQUD = question === 'timeQuestion' ? 'qudTime' : 'qudPrice';
  return Delta({v: correspondingQUD})
};

var QUDPosteriorFromInference = function(question){
  return Infer({method: 'enumerate'}, function() {
    var qudName = qudPrior();
    var q_erp = questioner('explicit', qudName);
    observe(q_erp, question);
    return qudName;
  });
};

var nameToQUD = function(qudName){
  return (qudName == "qudTime" ? qudTime :
          qudName == "qudPrice" ? qudPrice :
          qudName == "identity" ? function(v) {return v} :
          console.error('unknown qud name', qudName));
};

//  -------
// | Models |
//  -------
                                                                         
var getProjectedWorldPrior = function(qudName) {                             
  var qud = nameToQUD(qudName);                                              
  return Infer({method: 'enumerate'}, function(){                            
    return qud(worldPrior());                                                
  });                                                                        
};                                                                           

var rationality = 1;

var questioner = function(type, qudName) {
  return Infer({method: 'enumerate'}, function() {
    var question = questionPrior();
    var possibleAnswer = Infer({method: 'enumerate'}, function(){
      var trueWorld = worldPrior();
      return sample(answerer(type, question, trueWorld));
    });
    var infGain = function(answer){
      var prior = getProjectedWorldPrior(qudName);
      var posterior = Infer({method: 'enumerate'}, function(){
        var world = worldPrior();
        observe(answerer(type, question, world), answer);
        return nameToQUD(qudName)(world);
      });
      return KL(posterior, prior);
    };
    factor(expectation(possibleAnswer, infGain) * rationality);
    return question;
  });
};

var answerer = function(type, question, trueWorld) {
  var truthfulAnswerPrior = makeTruthfulAnswerPrior(trueWorld);
  var qudPosterior = (type === 'pragmatic' ? QUDPosteriorFromInference(question) :
                      type === 'explicit' ? QUDPosteriorFromMeaning(question) :
                      type === 'literal' ? Delta({v: 'identity'}) :
                      console.error('type not recognized'));
  return Infer({method: 'enumerate'}, function(){
    var qud = nameToQUD(sample(qudPosterior));
    var answer = sample(truthfulAnswerPrior);
    var utility = expectation(interpreter(answer), function(possibleWorld) {
      return _.isEqual(qud(possibleWorld), qud(trueWorld))
    });
    factor(Math.log(utility) * rationality);      
    return answer;
  });
};

var world = {price: 4, time: 8};

print('A0:');
viz.table(answerer('literal', timeQuestion, world))
viz.table(answerer('literal', priceQuestion, world))

print('A1:');
viz.table(answerer('explicit', timeQuestion, world))
viz.table(answerer('explicit', priceQuestion, world))

print('A2:')
viz.table(answerer('pragmatic', timeQuestion, world))
viz.table(answerer('pragmatic', priceQuestion, world))
~~~~

### Sensitivity to context

Depending on the context, liquor merchants will be more or less likely to give over-informative answers. Our model accounts for this via inference about the underlying goal, given the context.

~~~~
///fold:
var KL = function(erpTrue, erpApprox){                       
  var values = erpTrue.support([]);                          
  return sum(map(                                            
    function(value){                                         
      var scoreP = erpTrue.score(value);                     
      var scoreQ = erpApprox.score(value);                   
      var probP = Math.exp(scoreP);                          
      return probP == 0.0 ? 0.0 : probP * (scoreP - scoreQ); 
    }, values));                                             
};                                                           
///

//   ---------------
// | World knowledge |
//   ---------------

var buyWhiskeyContext = "I'd like to buy some whiskey.";
var spendFiveDollarsContext = "I only have $5 to spend.";

var prices = [1,2,3,4, 5, 6,7,8,9,10];

var isNumeric = function(x){
  return _.includes(prices, x);
};

var worldPrior = function(){
  return uniformDraw(prices);
};

//  -------------------
// | Question knowledge |
//  -------------------

var isMoreThanFiveQuestion = "Does Jim Beam cost more than $5?";

var isMoreThanFiveQuestionMeaning = function(world){
  return world > 5;
};

var questions = [isMoreThanFiveQuestion];

var questionPrior = function(){
  return uniformDraw(questions);
};

//  -----------------
// | Answer knowledge |
//  -----------------

var literalAnswers = ["yes", "no"];

var priceAnswers = map(function(p) {return "the whiskey costs $" + p;}, prices);

// evenly distribute probability across response types
var answerPrior = function(){
  return flip(0.5) ? uniformDraw(literalAnswers) : uniformDraw(priceAnswers);
};

var numericAnswerMeaning = function(number){
  return function(world){
    return world == number;
  };
};

var booleanAnswerMeaning = function(bool){
  return function(world){
    return (world > 5) == bool;
  };
};

//   -----------
// | Interpreter |
//   -----------

var meaning = function(utterance){
  return ((utterance === "yes") ? booleanAnswerMeaning(true) :
          (utterance === "no") ? booleanAnswerMeaning(false) :
          (utterance === isMoreThanFiveQuestion) ? isMoreThanFiveQuestionMeaning :
          _.includes(priceAnswers, utterance) ? numericAnswerMeaning(last(utterance.split('$'))*1.0) :
          console.error('unknown utterance!', utterance));
};
                                                                              
var interpreter = cache(function(answer){                                     
  return Infer({method: 'enumerate'}, function(){                             
    var world = worldPrior();                                                 
    var answerMeaning = meaning(answer);                                      
    condition(answerMeaning(world));                                          
    return world;                                                             
  });                                                                         
});                                                                           

var makeTruthfulAnswerPrior = function(trueWorld) {
  return Infer({method: 'enumerate', model: function(){
    var answer = answerPrior();
    factor(interpreter(answer).score(trueWorld));
    return answer;
  }});
};

//  ------
// | QUDs |
//  ------

var qudPrice = function(world){return world;};
var qudPriceGreaterThan5 = function(world){return world > 5;};

var qudPrior = function(context){
  var p = ((context === buyWhiskeyContext) ? 0.5 :
           (context === spendFiveDollarsContext) ? 0.99 :
           console.error('unknown context'));
  return (flip(p) ? "qudPriceGreaterThan5" : "qudPrice");
};

var QUDPosteriorFromMeaning = function(question) {
  var questionMeaning = (question === isMoreThanFiveQuestion ? 
                         "qudPriceGreaterThan5" : 
                         console.error('unknown question'));
  return Delta({v: questionMeaning})
};

var QUDPosteriorFromInference = function(question, context){
  return Infer({method: 'enumerate'}, function() {
    var qudName = qudPrior(context);
    var q_erp = questioner('explicit', qudName);
    observe(q_erp, question);
    return qudName;
  });
};

var nameToQUD = function(qudName){
  return (qudName == "qudPriceGreaterThan5" ? qudPriceGreaterThan5 :
          qudName == "qudPrice" ? qudPrice :
          qudName == "identity" ? function(v) {return v} :
          console.error('unknown qud name', qudName));
};

//  -------
// | Models |
//  -------
                                                                         
var getProjectedWorldPrior = function(qudName) {                             
  var qud = nameToQUD(qudName);                                              
  return Infer({method: 'enumerate'}, function(){                            
    return qud(worldPrior());                                                
  });                                                                        
};                                                                           

var rationality = 1;

var questioner = function(type, qudName) {
  return Infer({method: 'enumerate'}, function() {
    var question = questionPrior();
    var possibleAnswer = Infer({method: 'enumerate'}, function(){
      var trueWorld = worldPrior();
      return sample(answerer(type, question, trueWorld));
    });
    var infGain = function(answer){
      var prior = getProjectedWorldPrior(qudName);
      var posterior = Infer({method: 'enumerate'}, function(){
        var world = worldPrior();
        observe(answerer(type, question, world), answer);
        return nameToQUD(qudName)(world);
      });
      return KL(posterior, prior);
    };
    factor(expectation(possibleAnswer, infGain) * rationality);
    return question;
  });
};

var answerer = function(type, question, trueWorld, context) {
  var truthfulAnswerPrior = makeTruthfulAnswerPrior(trueWorld);
  var qudPosterior = (type === 'pragmatic' ? QUDPosteriorFromInference(question, context) :
                      type === 'explicit' ? QUDPosteriorFromMeaning(question) :
                      type === 'literal' ? Delta({v: 'identity'}) :
                      console.error('type not recognized'));
  return Infer({method: 'enumerate'}, function(){
    var qud = nameToQUD(sample(qudPosterior));
    var answer = sample(truthfulAnswerPrior);
    var utility = expectation(interpreter(answer), function(possibleWorld) {
      return qud(possibleWorld) === qud(trueWorld);
    });
    factor(Math.log(utility) * rationality);      
    return answer;
  });
};

var world = 4;

print('A0:');
viz.table(answerer('literal', isMoreThanFiveQuestion, world,  buyWhiskeyContext))
viz.table(answerer('literal', isMoreThanFiveQuestion, world,  spendFiveDollarsContext))

print('A1:');
viz.table(answerer('explicit', isMoreThanFiveQuestion, world,  buyWhiskeyContext))
viz.table(answerer('explicit', isMoreThanFiveQuestion, world,  spendFiveDollarsContext))

print('A2:')
viz.table(answerer('pragmatic', isMoreThanFiveQuestion, world,  buyWhiskeyContext))
viz.table(answerer('pragmatic', isMoreThanFiveQuestion, world,  spendFiveDollarsContext))
~~~~

### Clark (1979) credit cards example

Our final example demonstrates how our pragmatic answerer can use Gricean reasoning to infer underlying goals directly from the question utterance (via a model of the questioner) instead of using broader context clues.

~~~~
///fold:
var KL = function(erpTrue, erpApprox){
  var values = erpTrue.support([]);
  return sum(map(
    function(value){
      var scoreP = erpTrue.score(value);
      var scoreQ = erpApprox.score(value);
      var probP = Math.exp(scoreP);
      return probP == 0.0 ? 0.0 : probP * (scoreP - scoreQ);
    }, values));
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

var checkExhaustive = function(answer, trueWorld) {
  var cardsAccepted = filter(function(key){
    return trueWorld[key];
  }, _.keys(trueWorld));
  return _.isEqual(answer, cardsAccepted);
};

var cardLikelihoods = {
  'Visa' : 0.72,
  'MasterCard' : 0.71,
  'AmericanExpress' : 0.5,
  'Diners' : 0.12,
  'CarteBlanche' : 0.10
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

var getCardFromQuestion = function(q) {
  return butLast(last(q.split(' ')))
}
///


// --------------------------------------------------------------------

//   ---------------
// | World knowledge |
//   ---------------

var cardTypes = ['Visa','MasterCard', 'AmericanExpress', 'Diners', 'CarteBlanche'];

var cardPowerSet = powerset(cardTypes);

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

//  -------------------
// | Question knowledge |
//  -------------------

var cardQuestions = map(function(s) {return "Do you accept " + s + "?"}, cardTypes);
var cardQuestionMeaning = function(card) {
  return function(world) {return world[card]}
}

var creditCardsQuestion = "Do you accept credit cards?";
var creditCardsQuestionMeaning = function(world){
  return _.some(_.values(world));
};

var questions = cardQuestions.concat([creditCardsQuestion])
var questionPrior = function(){return uniformDraw(questions)};

//  -----------------
// | Answer knowledge |
//  -----------------

var cardAnswerSpace = powerset(cardTypes);
var booleanAnswerSpace = ["yes", "no"];

// 'yes' 'no' or some combination of cards
var answerPrior = function(){
  return (flip(0.5) ?
          uniformDraw(booleanAnswerSpace) :
          uniformDraw(cardAnswerSpace));
};

var cardAnswerMeaning = function(cardList){
  return function(questionMeaning){
    return function(world){
      return _.every(map(function(card) {return world[card]}, cardList));
    };
  };
};

var booleanAnswerMeaning = function(bool){
  return function(questionMeaning){
    return function(world){return questionMeaning(world) == bool}
  }
}

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
          _.includes(cardQuestions, utterance) ? cardQuestionMeaning(getCardFromQuestion(utterance)) :
          utterance === creditCardsQuestion ? creditCardsQuestionMeaning :
          console.error('unknown utterance in meaning!', utterance));
};

var interpreter = cache(function(question, answer){
  return Infer({method: 'enumerate', model: function(){
    var world = worldPrior();
    var answerMeaning = meaning(answer);
    var questionMeaning = meaning(question);
    condition(answerMeaning(questionMeaning)(world));
    return world;
  }});
}); 

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

var qudPrior = function(){
  var filteredPowerSet = filter(function(set){return !_.isEmpty(set)}, cardPowerSet);
  var cardSet = uniformDraw(filteredPowerSet);
  factor(cardSetLikelihood(cardSet));
  return "QUD" + cardSet;
};

var nameToQUD = function(qudName) {
  var cardList = qudName.slice(3).split(",");
  return function(world){
    return _.some(map(function(card){return world[card]}, cardList));
  };
};

var QUDPosteriorFromInference = function(question){
  return Infer({method: 'enumerate'}, function() {
    var qudName = qudPrior();
    var q_erp = questioner('explicit', qudName);
    observe(q_erp, question);
    return qudName;
  });
};

var QUDPosteriorFromMeaning = function(question){
  var correspondingQUD = (_.includes(cardQuestions, question) ?
                          "QUD" + getCardFromQuestion(question) :
                          "QUD" + cardTypes.join(','));
  return Delta({v: correspondingQUD});
};

//  -------
// | Models |
//  -------

var getProjectedWorldPrior = function(qudName) {
  var qud = nameToQUD(qudName);
  return Infer({method: 'enumerate'}, function(){return qud(worldPrior())});
};

var rationality = 10000;

var questioner = function(type, qudName) {
  return Infer({method: 'enumerate'}, function() {
    var question = questionPrior();
    var possibleAnswer = Infer({method: 'enumerate'}, function(){
      var trueWorld = worldPrior();
      return sample(answerer(type, question, trueWorld));
    });
    var infGain = function(answer){
      var prior = getProjectedWorldPrior(qudName);
      var posterior = Infer({method: 'enumerate'}, function(){
        var world = worldPrior();
        observe(answerer(type, question, world), answer);
        return nameToQUD(qudName)(world);
      });
      return KL(posterior, prior);
    };
    factor(expectation(possibleAnswer, infGain) * rationality);
    return question;
  });
};

var answerer = dp.cache(function(type, question, trueWorld) {
  var truthfulAnswerPrior = makeTruthfulAnswerPrior(question, trueWorld);
  var qudPosterior = (type === 'pragmatic' ? QUDPosteriorFromInference(question) :
                      type === 'explicit' ? QUDPosteriorFromMeaning(question) :
                      type === 'literal' ? Delta({v: 'identity'}) :
                      console.error('type not recognized'));
  return Infer({method: 'enumerate'}, function(){
    var qud = nameToQUD(sample(qudPosterior));
    var answer = sample(truthfulAnswerPrior);
    var utility = expectation(interpreter(question, answer), function(possibleWorld) {
      return qud(possibleWorld) === qud(trueWorld);
    });
    factor(Math.log(utility) * rationality);      
    return answer;
  });
})

var world = {
  'Visa' : false,
  'MasterCard' : true,
  'AmericanExpress' : false,
  'Diners' : false,
  'CarteBlanche' : false
};

var context = 'credit cards';
var runModel = function(question) {
  return expectation(Infer({method: 'enumerate', model: function(){
    var trueWorld = worldPrior();
    var ans = answerer('pragmatic', question, trueWorld);
    return Math.exp(ans.score("yes")) + Math.exp(ans.score("no"));
  }}));
};

print(questions[1]);
print(runModel(questions[1]));

print(questions[2]);
print(runModel(questions[2]));

print(last(questions))
print(runModel(last(questions)));
~~~~

### Mention-some example

We set up the mention-some disambiguation problem below, using the same structure as the previous model. This example uses a more structured world space and answer prior, but uses the same mechanism where context shifts the QUD prior.

~~~~
var filterWorld = function(world) {
  return _.fromPairs(reduce(function(value, memo) {
    var hasNewspaper = world[value]['hasNewspaper'];
    return hasNewspaper ? memo.concat([[value, world[value]]]) : memo;
  }, [], _.keys(world)));
}

var pickAllNewspaperCafes = function(world) {
  var filt = filterWorld(world);
  return _.isEmpty(filt) ? ['none'] : _.keys(filt);
}

var pickClosestNewspaperCafe = function(world) {
  var filt = filterWorld(world);
  if(_.isEmpty(filt)) {
    return ['none'];
  } else {
    var minDist = _.min(map(function(v){return v['distance']}, _.values(filt)));
    return sort(filter(function(v) {return filt[v]['distance'] == minDist},
		       _.keys(filt)));
  }
};

var KL = function(erpTrue, erpApprox){
  var values = erpTrue.support([]);
  return sum(map(
    function(value){
      var scoreP = erpTrue.score(value);
      var scoreQ = erpApprox.score(value);
      var probP = Math.exp(scoreP);
      return probP == 0.0 ? 0.0 : probP * (scoreP - scoreQ);
    }, values));
};
///

//   ---------------
// | World knowledge |
//   ---------------

var distances = [1,3]

var cafes = ['cafe1', 'cafe2', 'cafe3', 'cafe4']

var touristContext = "I'm new in town.";
var businesspersonContext = "I'm trying to set up a newspaper distribution business.";

var isCafeList = function(l){
  var predicate = function(s) {return _.includes(cafes, s);};
  return all(predicate, l);
};

var worldPrior = function(){
  return {
    'cafe1' : {'distance' : uniformDraw(distances), 'hasNewspaper' : flip(.5)},
    'cafe2' : {'distance' : uniformDraw(distances), 'hasNewspaper' : flip(.5)},
    'cafe3' : {'distance' : uniformDraw(distances), 'hasNewspaper' : flip(.5)},
    'cafe4' : {'distance' : uniformDraw(distances), 'hasNewspaper' : flip(.5)}
  }
}

//  -------------------
// | Question knowledge |
//  -------------------

var newspaperQuestion = "Where can one buy an Italian newspaper?";

// projects from a world to the relevant properties for the desired answer
var newspaperQuestionMeaning = function(world){return pickAllNewspaperCafes(world);};

var questions = [newspaperQuestion]

var questionPrior = function(){
  return uniformDraw(questions);
};

//  -----------------
// | Answer knowledge |
//  -----------------

// (truncated) geometric distribution
var answerPrior = function(){
  return flip(.8) ? [] : sort([uniformDraw(cafes)].concat(answerPrior()));
};

var cafeAnswerMeaning = function(cafeList){
  return function(world){
    var predicate = function(cafe) {return world[cafe]['hasNewspaper'];};
    return all(predicate, cafeList);
  };
};

var noneMeaning = function() {
  return function(world){
    var predicate = function(cafe) {return !world[cafe]['hasNewspaper'];};
    return all(predicate, cafes);
  };
};

//   -----------
// | Interpreter |
//   -----------

var meaning = function(utterance){
  return (isCafeList(utterance) ? cafeAnswerMeaning(utterance) :
         (_.isEqual(utterance, [])) ? noneMeaning() : 
         (utterance === newspaperQuestion) ? newspaperQuestionMeaning :
         console.error('unknown utterance!', utterance));
};

var interpreter = cache(function(answer){
  return Infer({method: 'enumerate'}, function(){
    var world = worldPrior();
    var answerMeaning = meaning(answer);
    condition(answerMeaning(world));
    return world;
  });
});

//  ------
// | QUDs |
//  ------

var qudAll = function(world){return pickAllNewspaperCafes(world);};
var qudClosest = function(world){return pickClosestNewspaperCafe(world);};

var qudPrior = function(context){
  var p = ((context === businesspersonContext) ? 1 :
           (context === touristContext) ? 0 :
           console.error('unknown context'));
  return (flip(p) ? "qudAll" : "qudClosest");
};

var nameToQUD = function(qudName){
  return (qudName === "qudClosest" ? qudClosest :
          qudName === "qudAll" ? qudAll :
          qudName === newspaperQuestion ? newspaperQuestionMeaning :
          console.error('unknown qud name', qudName));
};

//  -------
// | Models |
//  -------

var getConsistentWorlds = function(answer, qudName) {
  var qud = nameToQUD(qudName);
  return Infer({method: 'enumerate'}, function(){
    var inferredWorld = sample(interpreter(answer));
    return qud(inferredWorld);
  });
};

var getProjectedWorldPrior = function(qudName) {
  var qud = nameToQUD(qudName);
  return Infer({method: 'enumerate'}, function(){
    return qud(worldPrior());
  });
};

var explicitAnswerer = cache(function(question, trueWorld, rationality) {
  var qud = nameToQUD(question);
  return Infer({method: 'enumerate', maxExecutions: 1}, function(){
    var answer = answerPrior();
    var consistentWorldDist = getConsistentWorlds(answer, question);
    factor(consistentWorldDist.score(qud(trueWorld)) * rationality);
    return answer;
  });
});  

var explicitQuestioner = cache(function(qudName, rationality) {
  return Infer({method: 'enumerate'}, function(){
    var question = questionPrior();
    var prior = getProjectedWorldPrior(qudName);
    var informationGainDist = Infer({method: 'enumerate'}, function(){
      var trueWorld = worldPrior();
      var possibleAnswer = sample(explicitAnswerer(question, trueWorld, rationality));
      var posterior = getConsistentWorlds(possibleAnswer, qudName);
      return KL(posterior, prior);
    });
    factor(expectation(informationGainDist) * rationality);
    return question;
  });
});

var inferQUD = cache(function(context, question, rationality){
  return Infer({method: 'enumerate'}, function() {
    var qudName = qudPrior(context);
    var q_erp = explicitQuestioner(qudName, rationality);
    observe(q_erp, question);
    return qudName;
  });
});

var pragmaticAnswerer = cache(function(context, question, trueWorld, rationality){
  var qudPosterior = inferQUD(context, question, rationality);
  return Infer({method: 'enumerate', maxExecutions: 500}, function(){
    var qudName = sample(qudPosterior);
    var answer = answerPrior();
    var consistentWorldDist = getConsistentWorlds(answer, qudName);
    factor(consistentWorldDist.score(nameToQUD(qudName)(trueWorld)) * rationality);
    return answer;
  });
});

var world = {'cafe1' : {'distance' : 3, 'hasNewspaper' : false},
             'cafe2' : {'distance' : 1, 'hasNewspaper' :true},
             'cafe3' : {'distance' : 3, 'hasNewspaper' :true},
             'cafe4' : {'distance' : 3, 'hasNewspaper' :true}}

console.log(businesspersonContext, newspaperQuestion);
console.log(pragmaticAnswerer(businesspersonContext, newspaperQuestion, world,10));

console.log(touristContext, newspaperQuestion);
console.log(pragmaticAnswerer(touristContext, newspaperQuestion, world, 10));
~~~~

### Gibbs Jr. & Bryant (2008) time example

Here, we use a more sophisticated family of QUDs, and inference takes place directly over these alternatives, relative to the true world, instead of using an explicit context.

~~~~
var KL = function(erpTrue, erpApprox){
  var values = erpTrue.support([]);
  return sum(map(
    function(value){
      var scoreP = erpTrue.score(value);
      var scoreQ = erpApprox.score(value);
      var probP = Math.exp(scoreP);
      return probP == 0.0 ? 0.0 : probP * (scoreP - scoreQ);
    }, values));
};

var flatten = function(xs){
  if (xs.length == 0) {
    return [];
  } else {
    return xs[0].concat(flatten(xs.slice(1)));
  }
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
  return (_.includes(times, utterance) ? timeAnswerMeaning(utterance) :
         (utterance === timeQuestion) ? timeQuestionMeaning :
         console.error('unknown utterance!', utterance));
};

var interpreter = cache(function(answer){
  return Infer({method: 'enumerate'}, function(){
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

var getConsistentWorlds = function(answer, qudName) {
  var qud = nameToQUD(qudName);
  return Infer({method: 'enumerate'}, function(){
    var inferredWorld = sample(interpreter(answer));
    return qud(inferredWorld);
  });
};

var getProjectedWorldPrior = function(qudName) {
  var qud = nameToQUD(qudName);
  return Infer({method: 'enumerate'}, function(){
    return qud(worldPrior());
  });
};

var explicitAnswerer = cache(function(question, trueWorld, rationality) {
  var qud = nameToQUD(question);
  return Infer({method: 'enumerate'}, function(){
    var answer = answerPrior();
    var consistentWorldDist = getConsistentWorlds(answer, question);
    factor(consistentWorldDist.score(qud(trueWorld)) * rationality);
    return answer;
  });
});  

var explicitQuestioner = cache(function(qudName, rationality) {
  return Infer({method: 'enumerate'}, function(){
    var question = questionPrior();
    var prior = getProjectedWorldPrior(qudName);
    var informationGainDist = Infer({method: 'enumerate'}, function(){
      var trueWorld = worldPrior();
      var possibleAnswer = sample(explicitAnswerer(question, trueWorld, rationality));
      var posterior = getConsistentWorlds(possibleAnswer, qudName);
      return KL(posterior, prior);
    });
    factor(expectation(informationGainDist) * rationality);
    return question;
  });
});

var inferQUD = cache(function(context, question, rationality){
  return Infer({method: 'enumerate'}, function() {
    var qudName = qudPrior(context);
    var q_erp = explicitQuestioner(qudName, rationality);
    observe(q_erp, question);
    return qudName;
  });
});

var pragmaticAnswerer = cache(function(context, question, trueWorld, rationality){
  var qudPosterior = inferQUD(context, question, rationality);
  return Infer({method: 'enumerate'}, function(){
    var qudName = sample(qudPosterior);
    var answer = answerPrior();
    var consistentWorldDist = getConsistentWorlds(answer, qudName);
    factor(consistentWorldDist.score(nameToQUD(qudName)(trueWorld)) * rationality);
    return answer;
  });
});

var appointmentContext = "4:00";

var runModel = function(group) {
  return expectation(Infer({method: 'enumerate', model: function(){
    var trueWorld = worldPrior();
    var ansERP = pragmaticAnswerer(appointmentContext, timeQuestion, trueWorld, 1);
    condition(group === "early" ?
              trueWorld.slice(2) < 45 :
              trueWorld.slice(2) > 45);
    return Math.exp(ansERP.score(roundToNearest(trueWorld)));
  }}));
};

viz(Infer({method: 'enumerate', model: function(){return qudPrior(appointmentContext)}}))

print("early rounds p = " + runModel("early"));
print("late rounds p = " + runModel("late"));
~~~~

