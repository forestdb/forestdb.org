---
layout: model
title: CCG scope ambiguity
model-language: webppl
---

**A model of scope ambiguity using a combinatory categorical grammar and the RSA framework, by Carina Kaltenbach and Nicola Estrefallaces.**

This model is based on a semantic parser combined with the RSA framework by Goodman and Stuhlmüller (http://dippl.org/examples/zSemanticPragmaticMashup.html). In addition it defines new lexical entries and adds a parameter "scope" to resolve scope ambiguity in sentences like "All of the blond people are not nice". 

~~~~
var makeObj = function() {
  return { blond: flip(0.5), nice: flip(0.5)}
}
var scopePrior = function(){
    return categorical([3,7],["inverse","surface"]);
}
var worldPrior = function(nObjLeft, meaningFn, worldSoFar, prevFactor) {
  var worldSoFar = worldSoFar==undefined ? [] : worldSoFar
  var prevFactor = prevFactor==undefined ? 0 : prevFactor
  if (nObjLeft==0) {
    factor(-prevFactor)
    return worldSoFar
  } else {
    var newObj = makeObj()
    var newWorld = worldSoFar.concat([newObj])
    var newFactor = meaningFn(newWorld)?0:-100
    factor(newFactor - prevFactor)
    return worldPrior(nObjLeft-1, meaningFn, newWorld, newFactor)
  }
}


var meaning = function(utterance, scope) {
  var utt = utterance.split(" ")
  if ((utt.indexOf("all")!=-1) & (utt.indexOf("not")!=-1)){
    return combineMeanings(filter(function(m){return !(m.sem==undefined)},
                                 map2(lexicalMeaning, utt, map(function(x) { return scope }, utt))))
  } else {
   return combineMeanings(filter(function(m){return !(m.sem==undefined)},
                                 map2(lexicalMeaning, utt, map(function(x) { return "surface" }, utt))))
  
  }
}

var lexicalMeaning = function(word, scope) {

  var wordMeanings = {

    "blond" : {
      sem: function(world){return function(obj){return obj.blond}},
      syn: {dir:'L', int:'NP', out:'S'} },

    "nice" : {
      sem: function(world){return function(obj){return obj.nice}},
      syn: {dir:'L', int:'NP', out:'S'} },

    "some" : {
      sem: function(world){return function(P){return function(Q){return filter(Q, filter(P, world)).length>0}}},
      syn: {dir:'R',
            int:{dir:'L', int:'NP', out:'S'},
            out:{dir:'R',
                 int:{dir:'L', int:'NP', out:'S'},
                 out:'S'}} },

    "all" : {
      sem: function(world){return function(P){return function(Q){return filter(neg(Q), filter(P, world)).length==0}}},
      syn: {dir:'R',
            int:{dir:'L', int:'NP', out:'S'},
            out:{dir:'R',
                 int:{dir:'L', int:'NP', out:'S'},
                 out:'S'}} },

    "none" : {
      sem: function(world){return function(P){return function(Q){return filter(Q, filter(P, world)).length==0}}},
      syn: {dir:'R',
            int:{dir:'L', int:'NP', out:'S'},
            out:{dir:'R',
                 int:{dir:'L', int:'NP', out:'S'},
                 out:'S'}} },
     "not" : {
      sem: function(world){return function(P){return neg(P)} },
      syn: {dir: 'R', 
            int: {dir:'L', int:'NP', out: 'S'}, 
            out: {dir:'L', int:'NP', out:'S'} }}, 
    "not-all": {
      sem: function(world){return function(P){return function(Q){return filter(neg(Q), filter(P, world)).length>0}}},
      syn: {dir:'R',
            int:{dir:'L', int:'NP', out:'S'},
            out:{dir:'R',
                 int:{dir:'L', int:'NP', out:'S'},
                 out:'S'}} },
    "null": {
      sem: function(world){return true},
      syn: 'S'
  }
  }
  //if scope is inverse and sentence is ambiguous (contains all and not) use "Not all..." structure
  if(scope == "inverse"){
    if (word == "all"){
      var meaning = wordMeanings["not-all"];
      return meaning;
    }else if (word =="not") {
      return meaning == undefined?{sem: undefined, syn: ''}:meaning;
    } else{ 
      var meaning = wordMeanings[word];
      return meaning == undefined?{sem: undefined, syn: ''}:meaning;
    }
      
  } else {
     var meaning = wordMeanings[word];
  return meaning == undefined?{sem: undefined, syn: ''}:meaning;
  }
 
}

var neg = function(Q){
  return function(x){return !Q(x)}
}


//assume that both f and a will give their actual semantic value after being applied to a world. make a new meaning that passes on world arg.
var applyWorldPassing = function(f,a) {
  return function(w){return f(w)(a(w))}
}

var combineMeaning = function(meanings) {
  var possibleComb = canApply(meanings,0)
  var i = possibleComb[randomInteger(possibleComb.length)]
  var s = meanings[i].syn
  if (s.dir == 'L') {
    var f = meanings[i].sem
    var a = meanings[i-1].sem
    var newmeaning = {sem: applyWorldPassing(f,a), syn: s.out}
    return meanings.slice(0,i-1).concat([newmeaning]).concat(meanings.slice(i+1))
  }
  if (s.dir == 'R') {
    var f = meanings[i].sem
    var a = meanings[i+1].sem
    var newmeaning = {sem: applyWorldPassing(f,a), syn: s.out}
    return meanings.slice(0,i).concat([newmeaning]).concat(meanings.slice(i+2))
  }
}

//make a list of the indexes that can (syntactically) apply.
var canApply = function(meanings,i) {
  if(i==meanings.length){
    return []
  }
  var s = meanings[i].syn
  if (s.hasOwnProperty('dir')){ //a functor
    var a = ((s.dir == 'L')?syntaxMatch(s.int, meanings[i-1].syn):false) |
        ((s.dir == 'R')?syntaxMatch(s.int, meanings[i+1].syn):false)
    if(a){return [i].concat(canApply(meanings,i+1))}
  }
  return canApply(meanings,i+1)
}


// The syntaxMatch function is a simple recursion to
// check if two syntactic types are equal.
var syntaxMatch = function(s,t) {
  return !s.hasOwnProperty('dir') ? s==t :
  s.dir==t.dir & syntaxMatch(s.int,t.int) & syntaxMatch(s.out,t.out)
}


// Recursively do the above until only one meaning is
// left, return it's semantics.
var combineMeanings = function(meanings){
  return meanings.length==1 ? meanings[0].sem : combineMeanings(combineMeaning(meanings))
}


var utterancePrior = function() {
  var utterances = ["all of the blond people are nice",
                    "none of the blond people are nice",
                    "null",
                   "all of the blond people are not nice", 
                   "none of the blond people are not nice",
                   ]
  return uniformDraw(utterances)
}


var literalListener = cache(function(utterance, scope) {
  Infer({ 
    model() {
      var m = meaning(utterance, scope)
      var world = worldPrior(2,m)
      factor(m(world)?0:-Infinity)
      return world
    }
  })
})

var speaker = cache(function(world, scope) {
  Infer({ 
    model() {
      var utterance = utterancePrior()
      var L = literalListener(utterance, scope)
      factor(L.score(world))
      return utterance
    }
  })
})


var listener = cache(function(utterance) {
  Infer({ 
    model() {
      var world = worldPrior(2, function(w){return 1}) //use vacuous meaning to avoid any guide by literal meaning
      var scope = scopePrior()
      var S = speaker(world, scope)
      observe(S,utterance)
      return (world)
    }
  })
})

var pragmaticSpeaker = cache(function(world){
  Infer({model() {
    var utterance = utterancePrior();
    factor(listener(utterance).score(world))
    return utterance
  }})
})

viz.table(literalListener("all of the blond people are not nice", "surface"))
viz.table(speaker([{blond: true, nice:false}, {blond:true, nice:false}],"surface"))

//listener("all of the blond people are not nice")
pragmaticSpeaker([{blond: true, nice:false}, {blond:true, nice:false}])
~~~~
