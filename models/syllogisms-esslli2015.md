---
layout: model
title: Syllogistic reasoning with real-world content
model-status: code
model-language: webppl
---

A model of content effects in syllogistic reasoning.

### Under heavy development.


~~~~
var n_objects = 4
var priorDM = 'plausibility'

globalStore.gensymCounter = 0
var gensym = function(){
  globalStore.gensymCounter = globalStore.gensymCounter+1
  return "g" + globalStore.gensymCounter
}

var objects = repeat(n_objects, gensym)

var mapObject = function(fn, obj){ 
  return _.object(
    map(
      function(kv){
        return [kv[0], fn(kv[0], kv[1])]
      }, 
      _.pairs(obj))
  );
}

var uniformDraw = function(x){
  return x[randomInteger(x.length)]
}

var makeERPfromObject = function(obj){
     return Enumerate(function(){return _.keys(obj)[discrete(_.values(obj))]});
    }


var writeERP = function(myERP){
  return map(
          function(value){
            value.concat(Math.exp(myERP.score([], value)))
          },
          myERP.support([]))
}

// load prior data


var priorClean = utils_wppl.parsePriorData(priorDM)

var propertyTuples = [[0,0,0],[0,0,1],[0,1,0],[0,1,1],
                      [1,0,0],[1,0,1],[1,1,0],[1,1,1]]


var domainsSingular = ['cracker','knife','strawberry','lightbulb']


var csvInput = utils_wppl.readReasoningData();




var domains = _.uniq(map(function(row){return row[domainCol]}, radioData))
var syllogisms = _.uniq(map(function(row){return row[syllCol]}, radioData))



// for joint, map row into string of 1s and 0s
var mapObject2 = function(fn, obj){  
  return  map(function(x){return fn(x[1])}, _.pairs(obj))
}

var rowToObject = function(row){
  return mapObject2(function(index){return row[index]}, radioIndices)
}

// for marginal, map row into "YES" conclusions
var mapObject3 = function(fn, obj){  
  return  map(function(x){return fn(x[1])==1 ? x[0] : ''}, _.pairs(obj))
}

var rowToConclusion = function(row){
  return mapObject3(function(index){return row[index]}, radioIndices)
}

// organize by domain and syllogism , 
// and then within each subject, organized into an object from conclusions --> binary
// structuredData[domainName][syllName]

var structuredDataJoint = _.object(map(function(domain){
              return [domain, _.object(map(function(syll){
                return [syll, map(rowToObject,filter(function(row){
                    return (row[domainCol]==domain && row[syllCol] == syll)
                  }, radioData))
                ]
              }, syllogisms))]
            }, domains))


// organize by domain and syllogism , 
// and then within each subject, organized into an object from conclusions --> binary
// structuredData[domainName][syllName]

var structuredDataMarginal = _.object(map(function(domain){
              return [domain, _.object(map(function(syll){
                return [syll, 
                          _.filter(
                            _.flatten(
                                map(rowToConclusion,
                                    filter(function(row){
                                      return (row[domainCol]==domain && row[syllCol] == syll)
                           }, radioData))), Boolean)
                       ]
              }, syllogisms))]
            }, domains))

////

var domainSingularPlural = {
  "cracker":"crackers",
  "lightbulb":"lightbulbs",
  "strawberry":"strawberries",
  "knife":"knives"
}



// creating the syllogistic space

var quantifiers = ["all","some","none", "not all"];
var sentenceForms = [ ["A","B"],["B","A"],
                      ["B","C"],["C","B"],
                      ["A","C"],["C","A"]];

var propertyOrder = ["A","B","C"];


var conclusionOrder = [ [["C","A"],"all"],
                        [["C","A"],"none"],
                        [["C","A"],"some"],
                        [["C","A"],"not all"]];

var conclusionListOrder = [ [["C","A"],"all"],
                          [["C","A"],"none"],
                          [["C","A"],"some"],
                          [["C","A"],"not all"],
                          [[["C","A"],"all"],[["C","A"],"some"]],
                          [[["C","A"],"some"],[["C","A"],"not all"]],
                          [[["C","A"],"none"],[["C","A"],"not all"]]];


var binarizedConclusionSet = ['1000',
                              '0100',
                              '0010',
                              '0001',
                              '1010',
                              '0011',
                              '0101'];


var syllogisticSentences = _.flatten(map(function(x)
  {return map(function(y){return [y,x]} ,sentenceForms)}, quantifiers),true)

var premiseForms = {"1":[["B","A"],["C","B"]],
                  "2":[["A","B"],["C","B"]],
                  "3":[["B","A"],["B","C"]],
                  "4":[["A","B"],["B","C"]]}

var scholasticDict = {"all":"A","none":"E","some":"I","not all":"O"}

var premisesToScholasticCode = function(premises){
  var figure = _.invert(premiseForms)[premises[0][0]+','+premises[1][0]]
  var code = map(function(x){return scholasticDict[x[1]]}, premises).join('')
  return code+figure
}


var scholasticCodeToPremises = function(code){
  var invertedDict = _.invert(scholasticDict)
  var p = premiseForms[code[2]]
  var premise1 = [p[0], invertedDict[code[0]]]
  var premise2 = [p[1], invertedDict[code[1]]]
  return [premise1, premise2]
}


var isPremise = function(sentence,figure,premiseNo){
  return sentence[0] == premiseForms[figure][premiseNo-1]
}

var isConclusion = function(x){
//  return (x[0][0]=='A' || x[0][0]=='C') && (x[0][1]=='A' || x[0][1]=='C')
//  return (x[0][0]=='A') && (x[0][1]=='C')
  return (x[0][0]=='C') && (x[0][1]=='A')
}


var stateToSentence = function(state){
  return filter(function(x){return state[syllogisticSentences.indexOf(x)]
  }, syllogisticSentences)
}

var flattenSentences = function(sentences){
  return map(function(sentence){
    return [sentence[0][0], sentence[0][1], sentence[1]].join()
  },sentences)
}


var syllogisticPremisesNested = map(function(x)
  {return map(function(y)
    {return map(function(q1)
      {return map(function(q2)
        {return [[y,q1],[x,q2]]},
        quantifiers)},
      quantifiers)},
    sentenceForms.slice(0,2))},
  sentenceForms.slice(2,4))

var syllogisticPremises = _.flatten(
  _.flatten(
    _.flatten(syllogisticPremisesNested,
      true),
    true),
  true)


var premiseDictionary = {"1": _.flatten(_.flatten(syllogisticPremisesNested,true)[3],true),
                          "2": _.flatten(_.flatten(syllogisticPremisesNested,true)[2],true),
                          "3": _.flatten(_.flatten(syllogisticPremisesNested,true)[1],true),
                          "4": _.flatten(_.flatten(syllogisticPremisesNested,true)[0],true)}



// quantifier logic and helpers

var hasProperty = function(props,term)
  {return props[propertyOrder.indexOf(term)]}



var plentifulWorlds = function(propObject){
  var pO = map(second,_.pairs(propObject)) // function designed for list of lists
  var fOR = function(v1,v2){
    return map2(function(e1,e2){return e1 || e2}, v1, v2)
  }
  return reduce(function(e1,e2){return e1 && e2}, 1, (reduce(fOR,[0,0,0],pO)))
}

var allOverObjectVals = function(fn, obj)
  {return all(function(kv){return fn(kv[1])}, _.pairs(obj))}

var anyOverObjectVals = function(fn, obj)
  {return any(function(kv){return fn(kv[1])}, _.pairs(obj))}

var allSentence = function(propObject, termOne, termTwo){
  return allOverObjectVals(function(val){return hasProperty(val,termOne)? hasProperty(val,termTwo) : 1},
    propObject)
}

var someSentence = function(propObject, termOne, termTwo){
  return anyOverObjectVals(function(val){return hasProperty(val,termOne)? hasProperty(val,termTwo) : 0},
    propObject)
}

var notallSentence = function(propObject, termOne, termTwo){
  return 1 - allSentence(propObject,termOne,termTwo)
}

var noneSentence = function(propObject, termOne, termTwo){
  return 1 - someSentence(propObject,termOne,termTwo)
}

var mu = function(propObject, termOne, termTwo){
  return 1
}


// equivalence class transformation


var multinomialProbabilities = function(br){
  return [Math.pow(1-br,3),Math.pow(1-br,2)*br,Math.pow(1-br,2)*br,Math.pow(br,2)*(1-br),
          Math.pow(1-br,2)*br,Math.pow(br,2)*(1-br),Math.pow(br,2)*(1-br),Math.pow(br,3)]
}


//  propertyTuples = [[0,0,0],[0,0,1],[0,1,0],[0,1,1],
//                       [1,0,0],[1,0,1],[1,1,0],[1,1,1]]

var multinomialProbabilitiesFromid = function(a,b,c){
  return [(1-a)*(1-b)*(1-c), (1-a)*(1-b)*c, (1-a)*b*(1-c), (1-a)*b*c,
          a*(1-b)*(1-c), a*(1-b)*c, a*b*(1-c), a*b*c]
}


var equivalentTransform = cache(function(objects, backgroundPrior){

//  var backgroundPrior = [1,1,1,1,1,1,1,1]
//  var backgroundPrior = multinomialProbabilities(0.25)


  var getProperties = function(obj) {
      var p = propertyTuples[discrete(backgroundPrior)]
      return p
    }

  Enumerate(function(){

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
})

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


/// helpers for sentences

var flattenSentences = function(sentences){
    return map(function(sentence){
      return [sentence[0][0], sentence[0][1], sentence[1]].join()
    },sentences)
  }

// var binarizeConclusionSet = function(trueConcl){
//   return map(function(conclusion){
//     return (flattenSentences(trueConcl).indexOf(conclusion) > -1) ? 1 : 0
//   }, flattenSentences(conclusionOrder)).join('')
// }


// this considers "multiple conclusions" explicitly
var trueLists = function(trueConcls){
      _.flatten([map(function(x){return flattenSentences(conclusionListOrder).indexOf(x)}, 
                    flattenSentences(trueConcls)), 
                 flattenSentences(conclusionListOrder).indexOf(flattenSentences(trueConcls).join())])
}


// this considers "multiple conclusions" implicitly
// var trueLists = function(trueConcls){
//       map(function(x){return flattenSentences(conclusionListOrder).indexOf(x)}, 
//                     flattenSentences(trueConcls))
// }

var reasoner0_independent = cache(function(premises, a, b, c, domain) {
  Enumerate(function(){

    var equivalentWorlds = equivalentTransform(objects, multinomialProbabilitiesFromid(a,b,c))

    var state = sample(equivalentWorlds)
    var trueSentences = stateToSentence(state)
        // this could probably be optimized
    var flattenedSentences = flattenSentences(trueSentences) 

    var premisesTrue = flattenedSentences.indexOf(premises[0].join())!=-1 &&
                        flattenedSentences.indexOf(premises[1].join())!=-1

    var trueConclusions = filter(isConclusion, trueSentences)

    var listConclusions = trueLists(trueConclusions)
//    var whichConclusions = binarizedConclusionSet[uniformDraw(listConclusions)]
   var whichConclusions = uniformDraw(listConclusions)
//    var conclusion = uniformDraw(trueConclusions)

    factor(premisesTrue?0:-Infinity)
//    return conclusion // what is the conclusion?
    return whichConclusions // which conclusions are true?
  })
})


var reasoner0_empirical = cache(function(premises, domain) {
  Enumerate(function(){

    var backgroundPrior = priorClean[domain]
    var equivalentWorlds = pruneERP(equivalentTransform(objects, backgroundPrior))
    var state = sample(equivalentWorlds)


    var trueSentences = stateToSentence(state)
        // this could probably be optimized
    var flattenedSentences = flattenSentences(trueSentences) 

    var premisesTrue = flattenedSentences.indexOf(premises[0].join())!=-1 &&
                        flattenedSentences.indexOf(premises[1].join())!=-1

    var trueConclusions = filter(isConclusion, trueSentences)

    var listConclusions = trueLists(trueConclusions)
//    var whichConclusions = binarizedConclusionSet[uniformDraw(listConclusions)]
   var whichConclusions = uniformDraw(listConclusions)
//    var conclusion = uniformDraw(trueConclusions)

    factor(premisesTrue?0:-Infinity)
//    return conclusion // what is the conclusion?
    return whichConclusions // which conclusions are true?
  })
})

~~~~