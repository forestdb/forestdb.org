---
layout: model
title: Generic ID
model-status: hidden
model-category: Miscellaneous
model-language: webppl
model-language-version: v0.9.7
---

### Generic ID model

~~~~
// discretized range between 0 - 1
var bins = _.range(0.01, 1, 0.05);
var thresholdBins = _.range(0, 1, 0.05);

// function returns a discretized Beta PDF
var discretizeBeta = function(g, d){
  var shape_alpha =  g * d
  var shape_beta = (1-g) * d
  var betaPDF = function(x){
    return Math.pow(x,shape_alpha-1)*Math.pow((1-x),shape_beta-1)
  }
  return map(betaPDF, bins)
}


var displayObj = function(obj){
  display(JSON.stringify(obj));
};

var structuredPriorModel = function(params){
  Infer({method: "enumerate"}, function(){
    // unpack parameters
    var potential = params["potential"]
    var g = params["prevalenceWhenPresent"]
    var d = params["concentrationWhenPresent"]
    var propertyIsPresent = flip(potential)
    var prevalence = propertyIsPresent ?
        categorical(discretizeBeta(g,d), bins) :
    0
    return prevalence
  })
}
///

var alpha_1 = 5;
var alpha_2 = 1;

var utterances = ["Bears eat honey", "A bear eats honey", "silence"];
//                   "The bears eat honey", "null"];

var thresholdPrior = function() { return uniformDraw(thresholdBins) };
var utterancePrior = function() { return uniformDraw(utterances) }

var sensePrior = function(){
  return uniformDraw(["generic", "specific"])
}
var prevalencePrior = structuredPriorModel({
  potential: 0.3,
  prevalenceWhenPresent: 0.5,
  concentrationWhenPresent: 2
})


var meaning = function(utterance, prevalence, positiveExamples, threshold, sense, context) {
  utterance === "silence" ? true :
  sense === "generic" ? prevalence > threshold :
  context == 0 ? true : // this is a band-aid to make this run, but destroys the inferences
  utterance === "A bear eats honey" ? positiveExamples >= 1 :
  utterance === "The bears eat honey" ? positiveExamples >= 2 :
  utterance === "The bear eats honey" ? positiveExamples === 1 :
  utterance === "Bears eat honey" ? positiveExamples >= 2 :
  true
}

var literalListener = cache(function(utterance, threshold, sense, context) {
  Infer({method: "enumerate"}, function(){
    var prevalence = sample(prevalencePrior);
    // var context = flip() ? 0 : 5; // unlift context var
    var positiveExamples = context > 0 ? prevalence > 0 ? binomial({p: prevalence, n: context}) : 0 : 0;
    // var sense = uniformDraw(["generic", "specific"]); // unlift sense var
    var m = meaning(utterance, prevalence, positiveExamples, threshold, sense, context)
    condition(m)
    var qudVal = {prevalence, positiveExamples};
    return qudVal
  })
})

var speaker1 = cache(function(threshold, sense, context, prevalence, positiveExamples) {
  Infer({method: "enumerate"}, function(){
    var utterance = utterancePrior()
    var L0 = literalListener(utterance, threshold, sense, context)
    var qudVal = {prevalence, positiveExamples};
    factor( alpha_1*L0.score(qudVal) )
    return utterance
  })
})

// var s1  = speaker1(0, "specific", 0, 0, 0)
// var l0 = literalListener("A bear eats honey", 0, "specific", 0)

var pragmaticListener = function(utterance, observedContext) {
  Infer({method: "enumerate"}, function(){
    // abstract knowledge
    var prevalence = sample(prevalencePrior);
    // concrete knowledge
    // var context = flip() ? 5 : 0; // uncertainty over context
    var context = observedContext ? 5 : 0; // observed context
    // softer constraints on context var
    // categorical({
    //   vs: [0, 5],
    //   ps: observedContext ? [0.1, 0.9] : [0.9, 0.1]
    // });
    var positiveExamples = context > 0 ? prevalence > 0 ? binomial({p: prevalence, n: context}) : 0 : 0;

    // semantic knowledge
    var sense = uniformDraw(["generic", "specific"]);
    var threshold = sense === "generic" ? uniformDraw(thresholdBins) : -9;
    // displayObj({context, sense})
    var S1 = speaker1(threshold, sense, context, prevalence, positiveExamples);
    observe(S1, utterance);
    return sense
  })
}

var posterior = pragmaticListener("A bear eats honey", true);
display("specific interpretation (with context) : " + Math.exp(posterior.score("specific")))

var posterior2 = pragmaticListener("A bear eats honey", false);
display( "specific interpretation (no context) : " + Math.exp(posterior2.score("specific")))
~~~~
