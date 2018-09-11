---
layout: model
title: Homogeneity in Plural Definites
model-status: code
model-category: Reasoning about Reasoning
model-tags: natural language, rsa
model-language: webppl
model-language-version: v0.9.13
---

Benjamin Spector's RSA model of homogeneity in plural definites from [this paper](http://lingbuzz.auf.net/lingbuzz/003826/current.pdf?_s=BIkv3dFcNw3VfDMh).

Implementation by Lucas Champollion.

~~~~
// worlds
var w = {
  no : 'No',
  justSome : 'Just some',
  all : 'All'
}
// messages
var m = ['some','all','no','not all','the','not the']
// interpretation function (lexicon) 1: 'the' means 'some'
var l1 = {
  'some' : [w.justSome, w.all],
  'all' : [w.all],
  'no': [w.no],
  'not all' : [w.no, w.justSome],
  'the': [w.justSome, w.all],
  'not the': [w.no]
}
// interpretation function (lexicon) 2: 'the' means 'all'
var l2 = {
  'some' : [w.justSome, w.all],
  'all' : [w.all],
  'no': [w.no],
  'not all' : [w.no, w.justSome],
  'the': [w.all],
  'not the': [w.no, w.justSome]
}
// lexica
var lexica = [l1, l2]
// questions under discussion
var q = {
  isSome : 'Is Some?',
  isAll : 'Is All?',
  whatsTheCase : 'What\'s the case?'
}
// cost
var cost = {
  'the' : 0,
  'not the' : 1,
  'some' : 1,
  'all' : 1,
  'no' : 1.5,
  'not all' : 2
}
// prior on worlds
var worldPrior = function() {
  return w[uniformDraw(Object.keys(w))]
}
// prior on utterances
var utterancePrior = function() {
  return uniformDraw(m)
}
// prior on lexica (interpretation functions)
var lexiconPrior = Infer(function() {
  uniformDraw(lexica)
})
// check if an utterance is true
// u : utterance
// w : world
// i : lexicon (interpretation function)
var trueAt = function(u,w,l) { 
  return l[u].includes(w)
}
// projection function: takes a qud in string format, and 
// returns an equivalence class over worlds
var lookup = function(qud) {
  if (qud === q.isSome) {
    return [[w.no], [w.justSome, w.all]]
  } else if (qud === q.isAll) {
    return [[w.no, w.justSome], [w.all]]
  } else if (qud === q.whatsTheCase) {
    return [[w.no], [w.justSome], [w.all]]
  }
}

// are two worlds equivalent given a QUD?
var equiv = function(w1, w2, qud) { 
  var containsBoth = function(cell) { return (cell.includes(w1) && cell.includes(w2))}
  return any(containsBoth, lookup(qud))
}
// given a world and a QUD, return the cell of the QUD that contains that world
// Q(w)
var cellOf = function(world, qud) {
  var includesWorld = function(cell) {return (cell.includes(world))}
  return find(includesWorld, lookup(qud))
}
// L_0
// literal listener
// takes an utterance and a lexicon, and guesses a proposition and a QUD
// L_0 = P(Q(w), Q | u, L)
var literalListener = dp.cache(function(utterance,lexicon) {
  return Infer({model: function(){
    var world = worldPrior()
    var qud = qudPrior()
    var qudCell = cellOf(world,qud)
    condition(trueAt(utterance,world,lexicon))
    return { qudCell : qudCell, qud : qud }
  }})
})
// U_1
// expected utility across lexica : U_1(u,w,Q) = \sum_L P(L)*U_1(u,w,Q,L)
var expectedUtility = function(utterance,world,qud) {
  // utility given a lexicon and a QUD: U_1(u,w,Q,L) = log L_0(Q(w),Q|u,L) - c(u)
  var utilityForLiteralListener = function(lexicon) {
    var qudCell = cellOf(world,qud)
    return literalListener(utterance,lexicon).score({"qudCell":qudCell, "qud": qud}) 
      - cost[utterance]
  }
  var weightedUtility = function(lexicon) {
    var weight = Math.exp(lexiconPrior.score(lexicon))
    return weight * utilityForLiteralListener(lexicon)
  }
  return sum(map(weightedUtility,lexica))
}
// S_n for n >= 1
// speaker S_1(u|w,Q) \propto exp(lambda*U_1(u|w,Q))
var speaker = dp.cache(function(world, qud, n) {
  Infer(
    function() {
      var utterance = utterancePrior()
      factor(lambda * utility(utterance, world, qud, n))
      return utterance
    }
  )
})

// L_n for n >= 1
// pragmatic listener
var pragmaticListener = dp.cache(function(utterance,n) {
  return Infer({model: function(){
    var world = worldPrior()
    var qud = qudPrior()
    observe(speaker(world, qud, n),utterance)
    return { world: world, qud : qud }
  }})
})
// U_n for n >= 1
var utility = function(utterance,world,qud,n) {
  if (n == 1) {
    return expectedUtility(utterance,world,qud) 
  } else {
    return pragmaticListener(utterance,n-1).score({"world" : world, "qud": qud})
      - cost[utterance]
  }
}
// print function 'condProb2Table' for conditional probability tables
// from problang.org
///fold:
var condProb2Table = function(condProbFct, row_names, col_names, precision){
 var matrix = map(function(row) {
   map(function(col) {
     _.round(Math.exp(condProbFct(row).score(col)),precision)}, 
       col_names)}, 
                  row_names)
 var max_length_col = _.max(map(function(c) {c.length}, col_names))
 var max_length_row = _.max(map(function(r) {r.length}, row_names))
 var header = _.repeat(" ", max_length_row + 2)+ col_names.join("    ") + "\n"
 var row = mapIndexed(function(i,r) { _.padEnd(r, max_length_row, " ") + "  " + 
   mapIndexed(function(j,c) {
   _.padEnd(matrix[i][j], c.length+2," ")}, 
              col_names).join("  ") + "\n" }, 
                      row_names).join("")
 return header + row
}
///
var listener = function(utterance) {
  return marginalize(pragmaticListener(utterance, recursionDepth), "world")
}
// rationality parameter
var lambda = 5
// recursion depth
var recursionDepth = 5
// prior 1 on questions under discussion: "what's the case?" is likely
var probablyWTC = function() {
  categorical({
    vs: [q.whatsTheCase, q.isSome, q.isAll],
    ps: [0.8,            0.1,      0.1    ]
  })
}
// prior 2 on questions under discussion: "is some?" is likely
var probablyIsSome = function() {
  categorical({
    vs: [q.whatsTheCase, q.isSome, q.isAll],
    ps: [0.1,            0.8,      0.1    ]
  })
}
// toggle between qudPrior1 and qudPrior2 manually
var qudPrior = probablyWTC 
//var qudPrior = probablyIsSome
var condProbFct = listener
var rowNames = m // messages
var colNames = Object.values(w) // worlds as strings
var precision = 2 // how many decimals displayed
display(condProb2Table(condProbFct,rowNames,colNames,precision))

~~~~


Here's a different implementation of the same model by Greg Scontras:

~~~~
// worlds
var worlds = ["none", "justSome", "all"]
// prior on worlds
var worldPrior = function() {
 return uniformDraw(worlds)
}

// utterances
var utterances = ['some','all','no','not all','the','not the']
// prior on utterances
var utterancePrior = function() {
 return uniformDraw(utterances)
}
// utterance cost
var cost = {
 'the' : 0,
 'not the' : 1,
 'some' : 1,
 'all' : 1,
 'no' : 1.5,
 'not all' : 2
}

// questions under discussion
var QUDs = ["some?","all?","what?"]
// QUD function to partition world space
var qudFun = function(qud,w) {
 if (qud === "some?") {
   return w != "none"
 } else if (qud === "all?") {
   return w == "all"
 } else if (qud === "what?") {
   return w
 }
}

// possible lexica
var lexica = ["l1","l2"]
// prior on lexica (interpretation functions)
var lexiconPrior = function() {
 uniformDraw(lexica)
}
// interpretation function (lexicon) 1: 'the' means 'some'
var l1Fun = function(u,w) {
 if (u=="some") {
   return w != "none"
 } else if (u=="all") {
   return w == "all" 
 } else if (u=="no") {
   return w == "none" 
 } else if (u=="not all") {
   return w != "all"
 } else if (u=="the") {
   return w != "none"
 } else if (u=="not the") {
   return w == "none"
 }
}
// interpretation function (lexicon) 2: 'the' means 'all'
var l2Fun = function(u,w) {
 if (u=="some") {
   return w != "none"
 } else if (u=="all") {
   return w == "all" 
 } else if (u=="no") {
   return w == "none" 
 } else if (u=="not all") {
   return w != "all"
 } else if (u=="the") {
   return w == "all"
 } else if (u=="not the") {
   return w != "all"
 }
}
// check if an utterance is true
// u : utterance
// w : world
// l : lexicon
var meaning = function(u,w,l) {
 l == "l1" ? l1Fun(u,w) :
 l2Fun(u,w)
}


// L_0
// literal listener
// takes an utterance and a lexicon, and guesses a proposition and a QUD
// L_0 = P(Q(w), Q | u, L)
var literalListener = dp.cache(function(utterance,lexicon) {
 return Infer({model: function(){
   var world = worldPrior()
   var qud = qudPrior()
   var qudVal = qudFun(qud,world)
   condition(meaning(utterance,world,lexicon))
   return {qudVal: qudVal, qud: qud}
 }})
})

// S1 takes into account all the possible lexica
var speakerUtility = function(utterance,world,qud) {
 var l0Utility = function(lexicon) {
   var qudVal = qudFun(qud,world)
   return literalListener(utterance,lexicon).score({qudVal: qudVal, qud: qud}) -
     cost[utterance]
 }
 sum(map(l0Utility,lexica))*(1/lexica.length)
}

var speaker1 = function(world,qud,lexicon) {
 return Infer({model: function() {
   var utterance = utterancePrior()
   factor(lambda*speakerUtility(utterance,world,qud))
   return utterance
 }})
}

var listener1 = dp.cache(function(utterance) {
 return Infer({model: function() {
   var world = worldPrior()
   var qud = qudPrior()
   observe(speaker1(world,qud),utterance)
   return {qud: qud,w: world}
 }})
})

// here comes the recursion
var speaker2 = dp.cache(function(world,qud) {
 return Infer({model: function() {
   var utterance = utterancePrior()
   factor(lambda * (listener1(utterance).score({qud: qud,w: world})-cost[utterance]))
   return utterance
 }})
})

var listener2 = dp.cache(function(utterance) {
 return Infer({model: function() {
   var world = worldPrior()
   var qud = qudPrior()
   observe(speaker2(world,qud),utterance)
   return {qud: qud,w: world}
 }})
})

var speaker3 = dp.cache(function(world,qud) {
 return Infer({model: function() {
   var utterance = utterancePrior()
   factor(lambda * (listener2(utterance).score({qud: qud,w: world})-cost[utterance]))
   return utterance
 }})
})

var listener3 = dp.cache(function(utterance) {
 return Infer({model: function() {
   var world = worldPrior()
   var qud = qudPrior()
   observe(speaker3(world,qud),utterance)
   return {qud: qud,w: world}
 }})
})

var speaker4 = dp.cache(function(world,qud) {
 return Infer({model: function() {
   var utterance = utterancePrior()
   factor(lambda * (listener3(utterance).score({qud: qud,w: world})-cost[utterance]))
   return utterance
 }})
})

var listener4 = dp.cache(function(utterance) {
 return Infer({model: function() {
   var world = worldPrior()
   var qud = qudPrior()
   observe(speaker4(world,qud),utterance)
   return {qud: qud,w: world}
 }})
})

var speaker5 = dp.cache(function(world,qud) {
 return Infer({model: function() {
   var utterance = utterancePrior()
   factor(lambda * (listener4(utterance).score({qud: qud,w: world})-cost[utterance]))
   return utterance
 }})
})

var listener5 = dp.cache(function(utterance) {
 return Infer({model: function() {
   var world = worldPrior()
   var qud = qudPrior()
   observe(speaker5(world,qud),utterance)
   return {qud: qud,w: world}
 }})
})

// print function for conditional probability tables from problang.org:
var condProb2Table = function(condProbFct, row_names, col_names, precision){
 var matrix = map(function(row) {
   map(function(col) {
     _.round(Math.exp(condProbFct(row).score(col)),precision)}, 
       col_names)}, 
                  row_names)
 var max_length_col = _.max(map(function(c) {c.length}, col_names))
 var max_length_row = _.max(map(function(r) {r.length}, row_names))
 var header = _.repeat(" ", max_length_row + 2)+ col_names.join("    ") + "\n"
 var row = mapIndexed(function(i,r) { _.padEnd(r, max_length_row, " ") + "  " + 
   mapIndexed(function(j,c) {
   _.padEnd(matrix[i][j], c.length+2," ")}, 
              col_names).join("  ") + "\n" }, 
                      row_names).join("")
 return header + row
}
// helper function to marginalize;
// change listener function to get predictions from different levels
var listener = function(utterance) {
 return marginalize(listener5(utterance), "w")
}
// settings for probability table
var condProbFct = listener
var rowNames = utterances
var colNames = worlds
var precision = 2 // how many decimals displayed

// rationality parameter
var lambda = 5

// QUD prior 1: "what's the case?" is likely
var probablyWTC = function() {
 categorical({
   vs: ["what?", "some?", "all?"],
   ps: [    0.8,     0.1,    0.1]
 })
}
// QUD prior 2: "is some?" is likely
var probablyIsSome = function() {
 categorical({
   vs: ["what?", "some?", "all?"],
   ps: [    0.1,     0.8,    0.1]
 })
}

// toggle between qudPrior1 and qudPrior2 manually
var qudPrior = probablyWTC 
// var qudPrior = probablyIsSome

// generate predictions from L5 for all possible utterances
display(condProb2Table(condProbFct,rowNames,colNames,precision))

~~~~
