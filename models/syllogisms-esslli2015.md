---
layout: model
title: Syllogistic reasoning with real-world content
model-status: code
model-language: webppl
---

A model of content effects in syllogistic reasoning.

Ref:tessler2014syllogisms presented a model of argument strength for syllogistic reasoning. 
Argument strength is calculating by a generative model of idealized situations: situations are composed of objects with (Boolean) properties.
For simplicity, properties were assumed to be *independent and identically distributed*.
Here, we relax that assumption and measure the prior over properties empirically. 

The original model also accounted for pragmatic effects in syllogistic reasoning. 
That model uses an inference-about-inference setup to explicitly model the premises as coming from an (informative) experimenter.
That model, written in Church, can be found here: [here](http://forestdb.org/models/syllogisms-cogsci14.html).

This model doesn't include the pragmatic compoenent as of yet, focusing instead on the computation of argument strength.
This model is written in WebPPL.

~~~~
/// model parameter: number of objects

var n_objects = 4
// var priorDM = 'plausibility'

// mht's gensym
globalStore.gensymCounter = 0
var generateLabel = function(){
  globalStore.gensymCounter = globalStore.gensymCounter+1
  return "g" + globalStore.gensymCounter
}

// each object has a label
var objects = repeat(n_objects, generateLabel)

// empiricalPrior order corresponds with propertyTuples
var propertyTuples = [[0,0,0],[0,0,1],[0,1,0],[0,1,1],
                      [1,0,0],[1,0,1],[1,1,0],[1,1,1]]

var empiricalPrior = { lightbulb: 
   [ 0.24602846414362,  0.11758230643016374, 0.03002454453076707, 0.021605845425660415,
     0.09246959793518443, 0.11613603192957513, 0.1264134724274912, 0.24973973717753792 ],
  tomatoplant: 
   [ 0.18614347383728444, 0.06674594292556965, 0.11079513687730845, 0.08840397548035497,
     0.10099067042833954, 0.14302944460469302, 0.10284310244194857, 0.20104825340450147 ],
  cracker: 
   [ 0.13958560292608718, 0.18984121615975305, 0.16532489934289613, 0.1007158901785135,
     0.1266459089369192,  0.07255761758737525, 0.15269095970967317, 0.052637905158782465 ],
  strawberry: 
   [ 0.1467538037897898, 0.13814401342789567, 0.13997190080307098, 0.2058429216389246, 
     0.2192599751611972, 0.04206648163997186, 0.07778468583564677, 0.030176217703503164 ],
  painting: 
   [ 0.15868429644835172, 0.11032846084219958, 0.17739637719643314, 0.0587054407378321,
     0.07530371510193026, 0.17220187338109763, 0.13590947277186263, 0.11147036352029296 ],
  knife: 
   [ 0.16397053492239244, 0.07206225246448014, 0.19227967473750193, 0.052169702678688396,
     0.0808569287839623,  0.2048175888790572,  0.11834816118323957, 0.11549515635067803 ] }



var uniformDraw = function(x){
  return x[randomInteger(x.length)]
}

var multinomialProbabilities = function(br){
  return [Math.pow(1-br,3),Math.pow(1-br,2)*br,Math.pow(1-br,2)*br,Math.pow(br,2)*(1-br),
          Math.pow(1-br,2)*br,Math.pow(br,2)*(1-br),Math.pow(br,2)*(1-br),Math.pow(br,3)]
}


// define the syllogistic space

var quantifiers = ["all","some","none", "not all"];
var sentenceForms = [ ["A","B"],["B","A"],
                      ["B","C"],["C","B"],
                      ["A","C"],["C","A"]];

var premiseForms = {"1":[["B","A"],["C","B"]],
                    "2":[["A","B"],["C","B"]],
                    "3":[["B","A"],["B","C"]],
                    "4":[["A","B"],["B","C"]]}

// propertyOrder corresponds to propertyTuples above
var propertyOrder = ["A","B","C"];

// all possible syllogistic sentences
var syllogisticSentences = _.flatten(map(function(x)
  {return map(function(y){return [y,x]} ,sentenceForms)}, quantifiers),true)


// check is a given sentence is of the conclusion form: C--A
// uncomment alternative lines to allow for conclusions of different forms 
// (A--C or both C--A and A--C)
var isConclusion = function(x){
//  return (x[0][0]=='A' || x[0][0]=='C') && (x[0][1]=='A' || x[0][1]=='C')
//  return (x[0][0]=='A') && (x[0][1]=='C')
  return (x[0][0]=='C') && (x[0][1]=='A')
}

// quantifier logic and helpers

// check to see which objects have property "term"
var hasProperty = function(props,term)
  {return props[propertyOrder.indexOf(term)]}

// check if the situation has at least one object with each property
var plentifulWorlds = function(propObject){
  var pO = map(second,_.pairs(propObject)) // function designed for list of lists
  var fOR = function(v1,v2){
    return map2(function(e1,e2){return e1 || e2}, v1, v2)
  }
  return reduce(function(e1,e2){return e1 && e2}, 1, (reduce(fOR,[0,0,0],pO)))
}





var allSentence = function(propObject, termOne, termTwo){
  var allOverObjectVals = function(fn, obj){
    return all(function(kv){return fn(kv[1])}, _.pairs(obj))
  }

  return allOverObjectVals(function(val){
    return hasProperty(val,termOne)? hasProperty(val,termTwo) : 1
  }, propObject)
}

var someSentence = function(propObject, termOne, termTwo){
  var anyOverObjectVals = function(fn, obj){
    return any(function(kv){return fn(kv[1])}, _.pairs(obj))
  }
  return anyOverObjectVals(function(val){
    return hasProperty(val,termOne)? hasProperty(val,termTwo) : 0
  }, propObject)
}

var notallSentence = function(propObject, termOne, termTwo){
  return 1 - allSentence(propObject,termOne,termTwo)
}

var noneSentence = function(propObject, termOne, termTwo){
  return 1 - someSentence(propObject,termOne,termTwo)
}


// equivalence class transformation

var equivalentTransform = cache(function(objects, backgroundPrior){

    var pruneERP = function(myERP){
      var scr = map(function(lst)
                     {var y = myERP.score([],lst);
                      return y},
                     myERP.support())
      var prnScr=filter(function(lst){return lst[0] > -Infinity}, _.zip(scr,myERP.support()))
      var ps = map(function(x){return Math.exp(first(x))}, prnScr)
      var vs = map(second,prnScr)
      return Enumerate(function(){return vs[discrete(ps)]});
    }

  var getProperties = function(obj) {
      var p = propertyTuples[discrete(backgroundPrior)]
      return p
    }

  var prePrunedERP = Enumerate(function(){

    var propertiesOfObjects = _.object(_.zip(objects,map(getProperties,objects)))

    var meaning = function(quantifier) {
      return quantifier=="all"? allSentence :
             quantifier=="some"? someSentence :
             quantifier=="none"? noneSentence :
             quantifier=="not all"? notallSentence : 
             true
    }

    var nonEmptyWorld = plentifulWorlds(propertiesOfObjects) 
    factor(nonEmptyWorld?0:-Infinity)

    return map(function(sentence)
      {return meaning(sentence[1])(propertiesOfObjects,
                                    sentence[0][0],
                                    sentence[0][1])}, 
      syllogisticSentences)
  })

  return pruneERP(prePrunedERP)
})




/// helpers for sentences

var stateToSentence = function(state){
  return filter(
    function(x){return state[syllogisticSentences.indexOf(x)]}, 
    syllogisticSentences)
}

var flattenSentences = function(sentences){
  return map(function(sentence){
    return [sentence[0][0], sentence[0][1], sentence[1]].join()
  },sentences)
}


var argumentStrength_abstractWorld = function(premises, br) {
  Enumerate(function(){

    var equivalentWorlds = equivalentTransform(objects, multinomialProbabilities(br))
    var state = sample(equivalentWorlds)

    var trueSentences = stateToSentence(state)
    var premisesTrue = flattenSentences(trueSentences).indexOf(premises[0].join())!=-1 &&
                        flattenSentences(trueSentences).indexOf(premises[1].join())!=-1
    var trueConclusions = filter(isConclusion, trueSentences)

    var conclusion = uniformDraw(trueConclusions)

    factor(premisesTrue?0:-Infinity)

    return conclusion
  })
}


var argumentStrength_realWorld = function(premises, domain) {
  Enumerate(function(){

    var prior = empiricalPrior[domain]
    // run equivalence class transformation
    var equivalentWorlds = equivalentTransform(objects, prior)
    // sample a situation
    var situation = sample(equivalentWorlds)

    // which sentences are true of that situation?
    var trueSentences = stateToSentence(situation)

    // are the premises included in that list of true sentences?
    var premisesTrue = flattenSentences(trueSentences).indexOf(premises[0].join())!=-1 &&
                        flattenSentences(trueSentences).indexOf(premises[1].join())!=-1
    // which of the true sentences are of the conclusion form?
    var trueConclusions = filter(isConclusion, trueSentences)

    // sample a true conclusion
    var conclusion = uniformDraw(trueConclusions)

    // conditioning on the premises being true
    factor(premisesTrue?0:-Infinity)

   return conclusion
  })
}

print(argumentStrength_realWorld([ 
                          [ [ 'B', 'A' ], 'some' ], 
                          [ [ 'C', 'B' ], 'all' ] ], 
                          'lightbulb'))

print(argumentStrength_abstractWorld([ 
                          [ [ 'B', 'A' ], 'some' ], 
                          [ [ 'C', 'B' ], 'all' ] ], 
                          0.25))

~~~~

References:

- Cite:tessler2014syllogisms
- Cite:tessler2015syllogisms-esslli