---
layout: model
title: Conventions
model-language: webppl
model-language-version: v0.9.6
---

~~~~
///fold:
var colors = ["yellow","orange","red","pink",
	      "green","blue","brown","black"];
var objects = [["blue","apple"], ["green","apple"], ["red","apple"],
	       ["black","avocado"], ["green","avocado"], ["red","avocado"],
	       ["blue","banana"], ["brown","banana"], ["yellow","banana"],
	       ["brown","carrot"], ["orange","carrot"], ["pink","carrot"],
	       ["green","pear"], ["orange","pear"], ["yellow","pear"],
	       ["green","tomato"], ["pink","tomato"], ["red","tomato"],	       
	       ["black","pepper"], ["green","pepper"], ["orange","pepper"],
	       ["red","pepper"]];

// for all objects in a given context, can name the color, type, or both
var getPossibleUtts = function(context) {
  return _.uniq(_.flatten(map(function(itemArr) {
    return [itemArr[0], itemArr[1], itemArr.join('_')];
  },context)));
};
///

var initializeModel = function(params) {

  // construct prior over possible misperceptions, based on version in params
  // 1. if 'none', there's no noise, and this prior is a delta on true context
  // 2. if 'addition', then we supplement the context with a 'ghost object'
  // 3. if 'replacement', then we swap out one of the objects with a 'misperception'
  var getNoisyContextPrior = function(trueContext) {
    return Infer({method: 'enumerate'}, function() {
      if(params.noiseType === 'none' || flip(1 - params.noiseRate))
	return trueContext;
      else if(params.noiseType === 'addition')
	return trueContext.concat([uniformDraw(objects)]);
      else if(params.noiseType === 'replacement') {
	var replaceIndex = randomInteger(trueContext.length);
	var replaceObj = uniformDraw(objects);
	return (trueContext.slice(0,replaceIndex)
		.concat([replaceObj])
		.concat(trueContext.slice(replaceIndex+1,trueContext.length)));
      } else {
	console.error("unknown noiseType: " + params.noiseType);
      }
    });
  };
  
  // Cost of full utterances is sum of individual word costs,
  // unless you just use a color, in which case you incur
  // an additional costx
  var getUtteranceCost = function(utt) {
    var split = utt.split("_");
    if (split.length == 2) {
      return params.cost_color + params.cost_type;
    } else {
      return (_.includes(colors, split[0]) ?
	      params.cost_color ://  + params.color_only_cost :
	      params.cost_type);
    }
  };

  // Looks up meaning in given lexicon
  // (if no entry, listener assigns vanishingly small probability)
  var meaning = function(utt, object) {
    var objStr = object.join("_");
    var lexicalEntry = params.lexicon[utt];
    return _.has(lexicalEntry, objStr) ? lexicalEntry[objStr] : -100; 
  };

  // Selects among objects in context using lexicon
  var literalListener = cache(function(utt, context){
    return Infer({method:'enumerate'},function(){
      var object = uniformDraw(context);
      factor(meaning(utt,object)); 
      return object;
    });
  });

  // Selects among utterances given informativity in context and cost of production,
  // marginalizing over possible noise in perception of context
  // Timeit note: marginalizing over listener takes about 200-300ms per utt
  var speaker = function(context) {
    var target = context[0];
    var possibleutts = getPossibleUtts(context);
    var noisyContextPrior = getNoisyContextPrior(context);
    return Infer({method:'enumerate'},function(){
      var utt = uniformDraw(possibleutts);
      var listener = Infer({method: 'enumerate'}, function() {
	var noisyContext = sample(noisyContextPrior);
	return sample(literalListener(utt, noisyContext));
      });

      factor(params.alpha * listener.score(target)
	     - getUtteranceCost(utt));
      return utt;
    });
  };

  return speaker;
};

var params = {
  lexicon: 'realValued', // (['realValued', 'truthConditional']),
  noiseType: 'none', // (['none', 'addition', 'replacement']),
  noiseRate: .5, // ({a: 0, b: 1, width: 1/20}),
  alpha : 10, // uniformDrift({a:0,b:40,width:40/20}),
  cost_color : .5, //uniformDrift({a:0,b:3,width:3/20}),
  cost_type : .5  // uniformDrift({a:0,b:3,width:3/20}),
//    color_only_cost : uniformDrift({a:0,b:3,width:3/20})
};  

var context = [
  [ 'green', 'apple' ],        
  [ 'blue', 'banana' ],        
  [ 'brown', 'carrot' ]
];

var speaker = initializeModel(params);
speaker(context[0], context);
~~~~
