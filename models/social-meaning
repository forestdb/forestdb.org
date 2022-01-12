---
layout: model
title: Social meaning
model-status: code
model-category: Reasoning about Reasoning
model-tags: language, pragmatics, sociolinguistics, social meaning
model-language: webppl
model-language-version: v0.9.7
---

[Burnett (2019)](https://link.springer.com/article/10.1007/s10988-018-9254-y) combines insights from third-wave variationist sociolinguistics and Bayesian probabilistic pragmatics to model social inferences associated with the lingusitic variables (ING) (e.g. working vs. workin') and /t/ release.

The following is a WebPPL replication of Burnett (2019)'s calculations. See her paper for prose explication of the models. 

See as well [Qing and Cohn-Gordon (2019)](https://semanticsarchive.net/Archive/Tg3ZGI2M/Qing.pdf) for a revision of Burnett's account designed to capture repeated speaker productions of socially-meaningful linguistic variants.

~~~~
// Personae in (6)
var personae = [{name: "stern", competence : true, friendliness : false},
                 {name: "cool", competence : true, friendliness : true},
                 {name: "asshole", competence : false, friendliness : false},
                 {name: "doofus", competence : false, friendliness: true}]

// Table 2
var voterPrior = Categorical({ps: [0.3,0.2,0.3,0.2], vs: personae})

// Table 5
var journalistPrior = Categorical({ps: [0.2,0.2,0.3,0.3], vs: personae})

var personaePrior = voterPrior
// var personaePrior = journalistPrior

var variants = ["n","ng"]

var cost = {
  n : 0,
  ng : 0
}

// Eckert-Montague Semantics (Table 1)
var semantics = {
  ng: function(persona) { return (persona.competence == true | persona.friendliness == false) ; },
  n: function(persona) { return (persona.competence == false | persona.friendliness == true); },
}

// Definition in (11) - the 'literal listener'

var conditionalization = function(variant) {
  return Infer({model: function(){
    var persona = sample(personaePrior)
    var meaning = semantics[variant]
    condition(meaning(persona))
    return persona
  }}
)}

// Definition in (12)

var utility = function(persona, variant) {
  
  var informativity = conditionalization(variant).score(persona)
  return(informativity - cost[variant])
  
}

var alpha = 6

// Definition in (13) - soft-max choice rule

var speaker = function(persona) {
  return Infer(function() { 
   var variant = uniformDraw(variants)
  factor(alpha * utility(persona,variant))
  return(variant)
  }) 
}

// Table 6: persona selection function (the value system)

var mu = function(persona) {
  
  persona.name == "cool" ? 2 :
  persona.name == "stern" ? 1 :
  persona.name == "doofus" ? 1 :
  persona.name == "asshole" ? 0 :
  0
  
}

// Definition in (14): probability distribution over personae 

var alphaprime = 6

var personaDistribution = Infer(
  function() {
    var persona = sample(personaePrior)
    factor(alphaprime * mu(persona))
    return persona
  })

// Definition in (15): speaker with a value system

var valueSpeaker = function(variant) {
  
  // Array of utilities of variant for each persona, times probability of the persona
  var variantUtility = map(function(persona) {
    return Math.exp(personaDistribution.score(persona)) * Math.exp(speaker(persona).score(variant))  
  }, personae)
  
  return sum(variantUtility)
  
}

// Definition in (17): Listening with certainty about speaker's values 

var valueInformedListener = function(variant) {
  return Infer(function(){
  
  var persona = sample(personaePrior)
  factor(Math.exp(personaDistribution.score(persona)) * Math.exp(speaker(persona).score(variant)))
  return persona.name
    
  }) 
}

// Definition in (18): Naive listening 

var naiveListener = function(variant) {
  return Infer(function(){
  
  var persona = sample(personaePrior)
  factor(speaker(persona).score(variant))
  return persona.name
  }) 
}
  
print("Literal L's beliefs immediately after hearing -n at the barbecue")
viz.table(conditionalization('n'))

print("Literal L’s beliefs immediately after hearing -ng at the barbecue")
viz.table(conditionalization('ng'))

print("Obama wants to be the cool guy")
viz.table(speaker(personae[1]))

// set prior to journalistPrior above
print("Obama's overall probability of using -ng with the journalist")

print(valueSpeaker('ng'))

print("Obama's overall probability of using -n with the journalist")

print(valueSpeaker('n'))

print("Hearing Obama use -n (and you have a sense of Obama's values)")

viz(valueInformedListener('n'))

print("Hearing Obama use -n (and you're naive as to Obama's values)")

viz(naiveListener('n'))
~~~~
