---
layout: model
title: Adjective ordering parameter exploration
model-language: webppl
---

The following code implements the exploration of parameter settings for multi-adjective modification from Scontras, Degen, and Goodman (2018).

First, we need a way to generate sets of boxes of varying cardinality. These sets are constrained to have just a single member that is both small and brown (i.e., the intended referent of our multi-adjective modification).

~~~~
// generate single box
var objGenerator = function() {
  var brown = flip() // the box could be brown
  var small = flip() // the box could be small
  return {brown: brown, small: small}
}

// generate a set of n-many boxes
var objsGenerator = function(num,list) {
  var list = list == undefined ? [] : list
  if (num == 0) {
    return list
  } else {
    var newObj = objGenerator()
    var newList = list.concat([newObj])
    return objsGenerator(num - 1, newList)
  }
}

// generate all the possible sets with a single unique referent
var objSetsGenerator = function() {
  Infer({model: function() {
    var cardinalities = [2] // specifies the possible set cardinalities
    var cardinality = uniformDraw(cardinalities)
    var objs = objsGenerator(cardinality)
    var refNumber = _.filter(objs, {"brown":true,"small":true}).length
    condition(refNumber == 1)
    return objs
  }})
}

print("the possible sets of cardinality 2:")
viz.table(objSetsGenerator())
~~~~

Next, a noisy truth-functional adjective semantics:

~~~~
// a noisy truth-funcitonal adjective semantics
var adjMeaning = function(property,eps) {
  return function(obj) {
    obj[property] ? flip(1-eps) : flip(eps)
  }
}

print('check what "small" does with a small brown box') 
print('when "small" has an error rate of 0.64:')
adjMeaning("small",0.64)({"brown":true,"small":true})
~~~~

We want the error rates for a given adjective (i.e., `eps`) to depend on the size of the set to be classified. Here, we set the minimum `eps` and have it increase by 0.04 with each increase in the number of objects:

~~~~
// size-dependent error rates
var epsLookup = function(property,setSize,smallEps,brownEps) {
  property == "small" ? 
    smallEps + ((setSize - 1) * .04) :
  brownEps + ((setSize - 1) * .04) 
}

print('look up the error rate for "small" when')
print('it classifies a set with three members:')
epsLookup("small",3,0.64,0.20)
~~~~

We can use the truth-functional adjective semantics to restrict a set of boxes to just those elements that hold the relevant (noisy) property:

~~~~
///fold:
// a noisy truth-funcitonal adjective semantics
var adjMeaning = function(property,eps) {
  return function(obj) {
    obj[property] ? flip(1-eps) : flip(eps)
  }
}

// size-dependent error rates
var epsLookup = function(property,setSize,smallEps,brownEps) {
  property == "small" ? 
    smallEps + ((setSize - 1) * .04) :
  brownEps + ((setSize - 1) * .04) 
}
///

var restrictedSet = function(set,adj,smallEps,brownEps){
  var length = set.length
  var eps = epsLookup(adj,length,smallEps,brownEps) 
  var adjSem = adjMeaning(adj,eps)
  var filter = function(obj){ 
    adjSem(obj) ? obj : null
  }
  var filteredSet = map(filter,set)
  return filteredSet.filter(Boolean) // removes null
}

print('restrict a set containing a small brown box and a small box')
print('using the adjective "small":')
restrictedSet(
  [{"brown":true,"small":true},{"brown":false,"small":true}], // the originial set
  "small", // the property to restrict by
  0.64, // the minimum error rate for "small"
  0.20 // the minimum error rate for "brown"
)
~~~~

We can check what happens when both adjectives are used by having them restrict the set of boxes one at a time. In the following code, we calculate the probability that the multi-adjective modification will have correctly classified the intended referent: `{"brown":true,"small":true}`. 

*Note: In the paper, these calculations appear in (16) and (17).*

~~~~
///fold:
// a noisy truth-funcitonal adjective semantics
var adjMeaning = function(property,eps) {
  return function(obj) {
    obj[property] ? flip(1-eps) : flip(eps)
  }
}

// size-dependent error rates
var epsLookup = function(property,setSize,smallEps,brownEps) {
  property == "small" ? 
    smallEps + ((setSize - 1) * .04) :
  brownEps + ((setSize - 1) * .04) 
}

// restrict a set with noisy adjective
var restrictedSet = function(set,adj,smallEps,brownEps){
  var length = set.length
  var eps = epsLookup(adj,length,smallEps,brownEps) 
  var adjSem = adjMeaning(adj,eps)
  var filter = function(obj){ 
    adjSem(obj) ? obj : null
  }
  var filteredSet = map(filter,set)
  return filteredSet.filter(Boolean) // removes null
}
///

// check the default order
var defaultOrder = function(objs,smallEps,brownEps) {
  Infer({method: "enumerate",
         model: function(){ 
           var brownSet = restrictedSet(objs,"brown",smallEps,brownEps)
           var smallSet = restrictedSet(brownSet,"small",smallEps,brownEps)
           var newSet = _.filter(smallSet, {"brown":true,"small":true})
           newSet
         }})
}

print('probability of correct classification of the intended referent:')
print('default ordering (i.e., "small brown box"):')
display(defaultOrder([{"brown":true,"small":true},
                      {"brown":true,"small":false},
                      {"brown":false,"small":true}],
                     0.64, 0.20))

// check the reverse order
var reverseOrder = function(objs,smallEps,brownEps) {
  Infer({method: "enumerate",
         model: function(){ 
           var smallSet = restrictedSet(objs,"small",smallEps,brownEps)
           var brownSet = restrictedSet(smallSet,"brown",smallEps,brownEps)
           var newSet = _.filter(brownSet, {"brown":true,"small":true})
           newSet
         }})
}

print('reverse ordering (i.e., "brown small box"):')
display(reverseOrder([{"brown":true,"small":true},
                      {"brown":true,"small":false},
                      {"brown":false,"small":true}],
                     0.64, 0.20))
~~~~

We now have a way to generate different sets of boxes, and to restrictively modify those sets with multiple adjectives. In order to search the parameter space for those cases where the default order does not yield a higher probability of successful classification of the intended referent, we need a range of possible minimum values for `eps`, `epsBins`. We also need a way of systematically checking all of the possible combinations of parameters, `subjCheck`.

~~~~
///fold:
// generate single box
var objGenerator = function() {
  var brown = flip() // the box could be brown
  var small = flip() // the box could be small
  return {brown: brown, small: small}
}

// generate a set of n-many boxes
var objsGenerator = function(num,list) {
  var list = list == undefined ? [] : list
  if (num == 0) {
    return list
  } else {
    var newObj = objGenerator()
    var newList = list.concat([newObj])
    return objsGenerator(num - 1, newList)
  }
}

// generate all the possible sets with a single unique referent
var objSetsGenerator = function() {
  Infer({model: function() {
    var cardinalities = [2]
    var cardinality = uniformDraw(cardinalities)
    var objs = objsGenerator(cardinality)
    var refNumber = _.filter(objs, {"brown":true,"small":true}).length
    condition(refNumber == 1)
    return objs
  }})
}

// a noisy truth-funcitonal adjective semantics
var adjMeaning = function(property,eps) {
  return function(obj) {
    obj[property] ? flip(1-eps) : flip(eps)
  }
}

// size-dependent error rates
var epsLookup = function(property,setSize,smallEps,brownEps) {
  property == "small" ? 
    smallEps + ((setSize - 1) * .04) :
  brownEps + ((setSize - 1) * .04) 
}

// restrict a set with noisy adjective
var restrictedSet = function(set,adj,smallEps,brownEps){
  var length = set.length
  var eps = epsLookup(adj,length,smallEps,brownEps) 
  var adjSem = adjMeaning(adj,eps)
  var filter = function(obj){ 
    adjSem(obj) ? obj : null
  }
  var filteredSet = map(filter,set)
  return filteredSet.filter(Boolean) // removes null
}

// check the default order
var defaultOrder = function(objs,smallEps,brownEps) {
  Infer({method: "enumerate",
         model: function(){ 
           var brownSet = restrictedSet(objs,"brown",smallEps,brownEps)
           var smallSet = restrictedSet(brownSet,"small",smallEps,brownEps)
           var newSet = _.filter(smallSet, {"brown":true,"small":true})
           newSet
         }})
}

// check the reverse order
var reverseOrder = function(objs,smallEps,brownEps) {
  Infer({method: "enumerate",
         model: function(){ 
           var smallSet = restrictedSet(objs,"small",smallEps,brownEps)
           var brownSet = restrictedSet(smallSet,"brown",smallEps,brownEps)
           var newSet = _.filter(brownSet, {"brown":true,"small":true})
           newSet
         }})
}
///

// possible minimum values for epsilon
var epsBins = _.range(0.01, 0.8, 0.04)

// check those cases where the reverse order has higher probability
var subjCheck = cache(function() {
  Infer({model: function(){
    var objs = sample(objSetsGenerator())
    var smallEps = uniformDraw(epsBins)
    var brownEps = uniformDraw(epsBins)
    var defaultScore = defaultOrder(objs,smallEps,brownEps).score([{"brown":true,"small":true}])
    var reverseScore = reverseOrder(objs,smallEps,brownEps).score([{"brown":true,"small":true}])
    condition(smallEps > brownEps) // ensure that smallEps is larger
    condition(defaultScore < reverseScore) // ensure that the defaultScore is smaller
    return {objs, smallEps,brownEps}
  }})
})

print('parameter settings for which the reverse order yields a higher probability:')
viz.table(subjCheck())
~~~~

There are two classes of cases where the reverse ordering results in a higher probability of correct referent classification: 1) a larger number of objects from the less subjective category (e.g., one small brown box and one brown box) or 2) a larger number of objects from the more subjective category (e.g., one small brown box and one small box). However, even in these cases, not all values for `eps` yield a higher probability for the reverse order. In the following code, we systematically explore the parameter space for these two cases.

~~~~
///fold:
// helper function that converts an object into a string
var stringify = function(x){return JSON.stringify(x)}

// generate single box
var objGenerator = function() {
  var brown = flip() // the box could be brown
  var small = flip() // the box could be small
  return {brown: brown, small: small}
}

// generate a set of n-many boxes
var objsGenerator = function(num,list) {
  var list = list == undefined ? [] : list
  if (num == 0) {
    return list
  } else {
    var newObj = objGenerator()
    var newList = list.concat([newObj])
    return objsGenerator(num - 1, newList)
  }
}

// generate all the possible sets with a single unique referent
var objSetsGenerator = function() {
  Infer({model: function() {
    var cardinalities = [2]
    var cardinality = uniformDraw(cardinalities)
    var objs = objsGenerator(cardinality)
    var refNumber = _.filter(objs, {"brown":true,"small":true}).length
    condition(refNumber == 1)
    return objs
  }})
}

// a noisy truth-funcitonal adjective semantics
var adjMeaning = function(property,eps) {
  return function(obj) {
    obj[property] ? flip(1-eps) : flip(eps)
  }
}

// size-dependent error rates
var epsLookup = function(property,setSize,smallEps,brownEps) {
  property == "small" ? 
    smallEps + ((setSize - 1) * .04) :
  brownEps + ((setSize - 1) * .04) 
}

// restrict a set with noisy adjective
var restrictedSet = function(set,adj,smallEps,brownEps){
  var length = set.length
  var eps = epsLookup(adj,length,smallEps,brownEps) 
  var adjSem = adjMeaning(adj,eps)
  var filter = function(obj){ 
    adjSem(obj) ? obj : null
  }
  var filteredSet = map(filter,set)
  return filteredSet.filter(Boolean) // removes null
}

// check the default order
var defaultOrder = function(objs,smallEps,brownEps) {
  Infer({method: "enumerate",
         model: function(){ 
           var brownSet = restrictedSet(objs,"brown",smallEps,brownEps)
           var smallSet = restrictedSet(brownSet,"small",smallEps,brownEps)
           var newSet = _.filter(smallSet, {"brown":true,"small":true})
           newSet
         }})
}

// check the reverse order
var reverseOrder = function(objs,smallEps,brownEps) {
  Infer({method: "enumerate",
         model: function(){ 
           var smallSet = restrictedSet(objs,"small",smallEps,brownEps)
           var brownSet = restrictedSet(smallSet,"brown",smallEps,brownEps)
           var newSet = _.filter(brownSet, {"brown":true,"small":true})
           newSet
         }})
}

// possible minimum values for epsilon
var epsBins = _.range(0.01, 0.8, 0.04)

///

var subjCheck = cache(function() {
  Infer({model: function(){
    var objs = sample(objSetsGenerator())
    var objsSet = stringify(objs)
    var smallEps = uniformDraw(epsBins)
    var brownEps = uniformDraw(epsBins)
    var defaultScore = defaultOrder(objs,smallEps,brownEps).score([{"brown":true,"small":true}])
    var reverseScore = reverseOrder(objs,smallEps,brownEps).score([{"brown":true,"small":true}])
    condition(smallEps > brownEps) // ensure that smallEps is larger
    condition(defaultScore < reverseScore) // ensure that the defaultScore is smaller
    condition(stringify(objs) == stringify([{"brown":true,"small":true},{"brown":false,"small":true}])|
             stringify(objs) == stringify([{"brown":true,"small":true},{"brown":true,"small":false}]))
    return {smallEps, brownEps, objsSet}
  }})
})
///

print('parameter settings for which the reverse order yields a higher probability:')
viz.scatter(subjCheck().support(),{groupBy:"objsSet"})
~~~~

When there are **more brown objects than small objects** (i.e., the orange dots in the figure generated by the previous code box), `brownEps` must be sufficiently **small**, and as `brownEps` increases, the difference between it and `smallEps` must be sufficiently small. With these parameter values, it's better to first restrict the set of boxes with a relatively noiseless "small" to (correctly) remove the brown non-small boxes so that the set that "brown" operates over is smaller.

When there are **more small objects than brown objects** (i.e., the blue dots in the figure generated above), `brownEps` must be sufficiently **large** and the difference between it and `smallEps` must be small. With these parameter settings, it's better to first restrict the set of boxes with a relatively noisy "small" to (erroneously) remove the brown non-small boxes so that the set that "brown" operates over is smaller.

So far, we've only considered sets of boxes with cardinality 2, but these patterns hold for sets of greater sizes. The following code recreates the analysis reported in Scontras, Degen, and Goodman (2018), which looks at sets with cardinality 2 through 5. 

*Note: This code will take some minutes to run.*

~~~~
///fold:
// helper function that converts an object into a string
var stringify = function(x){return JSON.stringify(x)}

// generate single box
var objGenerator = function() {
  var brown = flip() // the box could be brown
  var small = flip() // the box could be small
  return {brown: brown, small: small}
}

// generate a set of n-many boxes
var objsGenerator = function(num,list) {
  var list = list == undefined ? [] : list
  if (num == 0) {
    return list
  } else {
    var newObj = objGenerator()
    var newList = list.concat([newObj])
    return objsGenerator(num - 1, newList)
  }
}

// generate all the possible sets with a single unique referent
var objSetsGenerator = function() {
  Infer({model: function() {
    var cardinalities = [2,3,4,5] // check sets of size 2 through 5
    var cardinality = uniformDraw(cardinalities)
    var objs = objsGenerator(cardinality)
    var refNumber = _.filter(objs, {"brown":true,"small":true}).length
    condition(refNumber == 1)
    return objs
  }})
}

// a noisy truth-funcitonal adjective semantics
var adjMeaning = function(property,eps) {
  return function(obj) {
    obj[property] ? flip(1-eps) : flip(eps)
  }
}

// size-dependent error rates
var epsLookup = function(property,setSize,smallEps,brownEps) {
  property == "small" ? 
    smallEps + ((setSize - 1) * .04) :
  brownEps + ((setSize - 1) * .04) 
}

// restrict a set with noisy adjective
var restrictedSet = function(set,adj,smallEps,brownEps){
  var length = set.length
  var eps = epsLookup(adj,length,smallEps,brownEps) 
  var adjSem = adjMeaning(adj,eps)
  var filter = function(obj){ 
    adjSem(obj) ? obj : null
  }
  var filteredSet = map(filter,set)
  return filteredSet.filter(Boolean) // removes null
}

// check the default order
var defaultOrder = function(objs,smallEps,brownEps) {
  Infer({method: "enumerate",
         model: function(){ 
           var brownSet = restrictedSet(objs,"brown",smallEps,brownEps)
           var smallSet = restrictedSet(brownSet,"small",smallEps,brownEps)
           var newSet = _.filter(smallSet, {"brown":true,"small":true})
           newSet
         }})
}

// check the reverse order
var reverseOrder = function(objs,smallEps,brownEps) {
  Infer({method: "enumerate",
         model: function(){ 
           var smallSet = restrictedSet(objs,"small",smallEps,brownEps)
           var brownSet = restrictedSet(smallSet,"brown",smallEps,brownEps)
           var newSet = _.filter(brownSet, {"brown":true,"small":true})
           newSet
         }})
}

// possible minimum values for epsilon
var epsBins = _.range(0.01, 0.8, 0.04)

///

var subjCheck = cache(function() {
  Infer({method: "enumerate",maxExecutions: Infinity, model: function(){
    var objs = sample(objSetsGenerator())
    var objsSet = stringify(objs)
    var smallEps = uniformDraw(epsBins)
    var brownEps = uniformDraw(epsBins)
    var defaultScore = defaultOrder(objs,smallEps,brownEps).score([{"brown":true,"small":true}])
    var reverseScore = reverseOrder(objs,smallEps,brownEps).score([{"brown":true,"small":true}])
    condition(smallEps > brownEps) // ensure that smallEps is larger
    condition(defaultScore < reverseScore) // ensure that the defaultScore is smaller
    return {objsSet, smallEps, brownEps}
  }})
})
///

print('parameter settings for which the reverse order yields a higher probability:')
viz.table(subjCheck())
~~~~