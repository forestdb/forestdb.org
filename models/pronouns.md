---
layout: model
title: Pronouns
model-language: webppl
---

A model of pronoun ambiguity resolution by Alandi Bates and Cheryl Choi:

~~~~
// possible utterances
var utterances = ["he","null"]

// utterance Prior
var utterancePrior = function() {
  uniformDraw(utterances);
}

// possible referents -"Source","Goal","Other"
var referents = ["Source","Goal","Other"];

//referent Prior
var referentPrior = function() {
  uniformDraw(referents);
//   categorical([0.49,0.49,0.2],referents);
};

//possible interpretations for the pronoun "he"- refer to the Goal or the Source
var interpretations = ["Source","Goal"]

// interpretation Prior
var interpretationPrior = function(){ 
//   return uniformDraw(interpretations)
  return categorical([0.1,0.9],interpretations)
};

//meaning function - takes in an utterance, a referent, and an interpretation and returns a truth value
var meaning = function(u,r,i){
  if (u == "he"){
    if (i == "Source"){return r == "Source"}
    else {return r == "Goal"}}
  else {return true}  
}

// possible QUDs
var QUDs = ["Why?","What-next?","Who?"];

//QUD Prior
var QUDPrior = function() {
//   uniformDraw(QUDs);
  categorical([0.05,0.9,0.05],QUDs);
};

//QUD function - takes in a QUD and a referent and returns a truth value
//based on participant judgments in the Kehler & Rhode (2013) study
var QUDfun = function(QUD,referent){
  if (QUD == "Why?"){
    var check_Source = categorical([0.82,0.17,0.01],referents)
    return referent == check_Source}
  if (QUD == "What-next?"){
    var check_Goal = categorical([0.33,0.66,0.01],referents)
    return referent == check_Goal}
  else{
    var check_Other = uniformDraw(referents) 
    return referent == check_Other}
}

// Literal listener (L0) - takes in an utterance, interpretation, and a QUD and returns truth value
var literalListener = cache(function(utterance,interpretation,QUD) {
  Infer({model: function(){
    var referent = referentPrior() 
    var qReferent = QUDfun(QUD,referent)
    var meaning = meaning(utterance,referent,interpretation)
    condition(meaning)
    return qReferent}});
});

var alpha = 2.5 //seemed reasonable (same used in Savinelli et al. (2017))

// Speaker (S) - takes in an interpretation, a referent, and a QUD and returns a distribution over utterances
var speaker = function(interpretation,referent,QUD) {
  Infer({model: function(){
    var utterance = utterancePrior();
    var qreferent = QUDfun(QUD,referent);
    var L0 = literalListener(utterance,interpretation,QUD)
    factor(alpha * L0.score(qreferent));
    return utterance;
  }});
};

// Pragmatic listener (L1) - takes in an utterance and a QUD and returns a distribution over referents
var pragmaticListener = cache(function(utterance,QUD) {
  Infer({model: function(){
    var referent = referentPrior();
    var interpretation = interpretationPrior(QUD); 
    var S1 = speaker(interpretation,referent,QUD)
    observe(S1,utterance);
    return referent
  }});
});

// Pragmatic speaker (S2) - takes in a referent and a QUDand returns a distribution over utterances
var pragmaticSpeaker = cache(function(referent,QUD) {
  Infer({model: function(){
    var utterance = utterancePrior();
//     var QUD = QUDPrior()
    factor(pragmaticListener(utterance,QUD).score(referent))
    return utterance
  }})
});

~~~~
