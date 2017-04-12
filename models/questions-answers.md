---
layout: model
title: Questions and Answers
model-language: webppl
model-language-version: v0.9.7
---

### Clark (1979) whiskey example

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

var prices = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

var isNumeric = function(x){
  return _.includes(prices, x);
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
          _.includes(priceAnswers, utterance) ? numericAnswerMeaning(last(utterance.split('$'))*1) :
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
  var truthfulAnswerPrior = makeTruthfulAnswerPrior(trueWorld);
  return Infer({method: 'enumerate'}, function(){                    
    var answer = sample(truthfulAnswerPrior);                                                        
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
  var truthfulAnswerPrior = makeTruthfulAnswerPrior(trueWorld);
  return Infer({method: 'enumerate'}, function(){                 
    var qudName = sample(qudPosterior);                                               
    var answer = sample(truthfulAnswerPrior);                                                        
    var consistentWorldDist = getConsistentWorlds(answer, qudName);                   
    factor(consistentWorldDist.score(nameToQUD(qudName)(trueWorld)) * rationality);   
    return answer;                                                                    
  });                                                                                 
});     

var world = [4];

print(buyWhiskeyContext + " " + isMoreThanFiveQuestion);
viz(pragmaticAnswerer(buyWhiskeyContext, isMoreThanFiveQuestion, world, 1));

print(spendFiveDollarsContext + " " + isMoreThanFiveQuestion);
viz(pragmaticAnswerer(spendFiveDollarsContext, isMoreThanFiveQuestion, world, 1));
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

### Clark (1979) credit cards example

Our final example demonstrates how our pragmatic answerer can use Gricean reasoning to infer underlying goals directly from the question utterance (via a model of the questioner) instead of using broader context clues.

~~~~
///fold:
var mean = function(thunk){
  return expectation(Enumerate(thunk), function(v){return v;});
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
  return Infer({method: 'enumerate', model: function(){
    var world = worldPrior();
    var answerMeaning = meaning(answer);
    var questionMeaning = meaning(question);
    condition(answerMeaning(questionMeaning)(world));
    return world;
  }});
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

var qudPrior = function(context){
  return "QUD" + cardSetPrior();
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

var getConsistentWorlds = function(question, answer, qudName) {
  var qud = nameToQUD(qudName);
  return Infer({method: 'enumerate'}, function(){
    var inferredWorld = sample(interpreter(question, answer));
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
  var truthfulAnswerPrior = makeTruthfulAnswerPrior(question, trueWorld)
  return Infer({method: 'enumerate'}, function(){
    var answer = sample(truthfulAnswerPrior);
    var consistentWorldDist = getConsistentWorlds(question, answer, question);
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
      var posterior = getConsistentWorlds(question, possibleAnswer, qudName);
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
  var truthfulAnswerPrior = makeTruthfulAnswerPrior(question, trueWorld);
  return Infer({method: 'enumerate'}, function(){
    var qudName = sample(qudPosterior);
    var answer = sample(truthfulAnswerPrior);
    var consistentWorldDist = getConsistentWorlds(question, answer, qudName);
    factor(consistentWorldDist.score(nameToQUD(qudName)(trueWorld)) * rationality);
    return answer;
  });
});

var world = {
  'Visa' : true,
  'MasterCard' : false,
  'AmericanExpress' : true,
  'Diners' : false,
  'CarteBlanche' : false
};

var context = 'credit cards';
// // TODO: fix score argument here to sum over possible keys
var runModel = function(question) {
  return expectation(Infer({method: 'enumerate', model: function(){
    var trueWorld = worldPrior();
    var ansERP = pragmaticAnswerer(context, question, trueWorld, 10000);
    return Math.exp(ansERP.score("yes")) + Math.exp(ansERP.score("no"));
  }}));
};

print(masterCardQuestion);
print(runModel(masterCardQuestion));

print(AmericanExpressQuestion);
print(runModel(AmericanExpressQuestion));

print(creditCardsQuestion);
print(runModel(creditCardsQuestion));

inferQUD(context, AmericanExpressQuestion, 10000);
inferQUD(context, creditCardsQuestion, 10000);
~~~~

