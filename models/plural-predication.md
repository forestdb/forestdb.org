---
layout: model
title: Plural Predication
model-status: code
model-category: Reasoning about Reasoning
model-tags: language, ambiguity, plurality, interpretation noise
model-language: webppl
model-language-version: v0.9.6
---

This is the model of ambiguity resolution in plural predication from Scontras and Goodman 2016.

	// helper functions
	// error function
	var erf = function(x) {
	  var a1 =  0.254829592;
	  var a2 = -0.284496736;
	  var a3 =  1.421413741;
	  var a4 = -1.453152027;
	  var a5 =  1.061405429;
	  var p  =  0.3275911;
	  var sign = x < 0 ? -1 : 1
	  var z = Math.abs(x);
	  var t = 1.0/(1.0 + p*z);
	  var y = 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*Math.exp(-z*z);
	  var answer = sign*y
	  return answer
	}

	// check array identity
	var arraysEqual = function(a1,a2) {
	  return JSON.stringify(a1)==JSON.stringify(a2);
	}

	// get ERP probabilities
	var erpProbs = function(ERP,support) {
	  var scores = function(val) {
	    return Math.exp(ERP.score(val))
	  }
	  return map(scores,support)
	}

	// KL divergence
	var KL = function(P,Q) {
	  var diverge = function(xp,xq) {
	    return xp == 0 ? 0 : (xp * Math.log(xp / xq) )
	  }
	  return sum(map2(diverge,P,Q))
	}



	// wrapper for plural predication model
	var pluralPredication = function(numberObjects,
	                                  collectiveNoise,
	                                  knowledge,
	                                  thingSaid
	                                 ) {

	  var utterances = [
	    "null",
	    "ambiguous-pos",
	    "each-pos",
	    "together-pos"
	  ];

	  // null > ambiguous > unambiguous 
	  var utterancePrior = function() {
	    return categorical([3,2,1,1],utterances)
	  };

	  // possible object sizes
	  var objects = [2,3,4];
	  var objectPrior = function() {
	    uniformDraw(objects);
	  }

	  // build states with n many objects
	  var statePrior = function(nObjLeft,stateSoFar) {
	    var stateSoFar = stateSoFar == undefined ? [] : stateSoFar
	    if (nObjLeft == 0) {
	      return stateSoFar
	    } else {
	      var newObj = objectPrior()
	      var newState = stateSoFar.concat([newObj])
	      return statePrior(nObjLeft - 1,newState)
	    }
	  }

	  // threshold priors
	  var distThetaPrior = function(){return objectPrior()};  
	  var collThetaPrior = function(){return uniformDraw([2,3,4,5,6,7,8,9,10,11,12])};

	  // noise variance
	  var noiseVariance = collectiveNoise == "no" ? 0.01 :
	  collectiveNoise == "low" ? 1 :
	  collectiveNoise == "mid" ? 2 : 3

	  // x > theta interpretations
	  var collInterpretationPos = function(state, collTheta,noise) {
	    var weight = 1 - (0.5 * (1 + erf((collTheta - sum(state)) / 
	                                     (noise * Math.sqrt(2)))))
	    return weight == 0 ? flip(0.01) : weight == 1 ? flip(0.99) : flip(weight)
	  }

	  var distInterpretationPos = function(state, distTheta) {
	    return all(function(x){x >= distTheta}, state) ? flip(0.99) : flip(0.01)
	  }
	  
	//   // x < theta interpretations
	//   var collInterpretationPos = function(state, collTheta,noise) {
	//     var weight = 1 - (0.5 * (1 + erf((sum(state) - collTheta) / 
	//                                      (noise * Math.sqrt(2)))))
	//     return weight == 0 ? flip(0.01) : weight == 1 ? flip(0.99) : flip(weight)
	//   }

	//   var distInterpretationPos = function(state, distTheta) {
	//     return all(function(x){x <= distTheta}, state) ? flip(0.99) : flip(0.01)
	//   }

	  // meaning function
	  var meaning = function(utt,state,distThetaPos,collThetaPos,isCollective,noise) {
	    return  utt == "null" ? true :
	    utt == "each-pos" ? distInterpretationPos(state,distThetaPos) :
	    utt == "together-pos" ? collInterpretationPos(state,collThetaPos,noise) :
	    isCollective ? collInterpretationPos(state,collThetaPos,noise) :
	    distInterpretationPos(state,distThetaPos)
	  }

	  var alpha = 10

	  var literal = cache(function(utterance,distThetaPos,collThetaPos,isCollective) {
	    Infer({method:"enumerate"}, function(){
	      var state = statePrior(numberObjects);
	      var noise = noiseVariance
	      condition(meaning(utterance,state,distThetaPos,collThetaPos,isCollective,noise));
	      return state;
	    })
	  });

	  var speakerBelief = cache(function(state,speakerKnows) {
	    Infer({method:"enumerate"}, function(){
	      var obs = function(s) {
	        return speakerKnows ? s : sum(s) 
	      }
	      var bState = statePrior(numberObjects)
	      condition(arraysEqual(obs(bState),obs(state)))
	      return bState
	    })
	  })

	  var speaker = cache(function(state,distThetaPos,collThetaPos,isCollective,speakerKnows) {
	    Infer({method:"enumerate"}, function(){
	      var utterance = utterancePrior()
	      var bDist = speakerBelief(state,speakerKnows)
	      var lDist = literal(utterance,distThetaPos,collThetaPos,isCollective)
	      factor(-1 *
	             KL(erpProbs(bDist,bDist.support()),
	                erpProbs(lDist,bDist.support())
	               ))
	      return utterance
	    })
	  });

	  var listener = cache(function(utterance,speakerKnows) {
	    Infer({method:"enumerate"}, function(){
	      var state = statePrior(numberObjects);
	      var isCollective = flip(0.8)
	      var distThetaPos = distThetaPrior();
	      var collThetaPos = collThetaPrior();
	      factor(alpha * 
	             speaker(state,distThetaPos,collThetaPos,isCollective,speakerKnows).score(utterance) 
	            );
	      return isCollective
	    });
	  });

	  return listener(thingSaid,knowledge)
	}


	print(pluralPredication(3,"no",true,"ambiguous-pos"))
	print(pluralPredication(3,"no",false,"ambiguous-pos"))

	print(pluralPredication(3,"low",true,"ambiguous-pos"))
	print(pluralPredication(3,"low",false,"ambiguous-pos"))

	print(pluralPredication(3,"mid",true,"ambiguous-pos"))
	print(pluralPredication(3,"mid",false,"ambiguous-pos"))

	print(pluralPredication(3,"high",true,"ambiguous-pos"))
	print(pluralPredication(3,"high",false,"ambiguous-pos"))


