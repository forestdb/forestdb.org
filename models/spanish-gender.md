---
layout: model
title: Yeaton Muelas Gil - Spanish gender
model-language: webppl
---

### Introduction

Introduction
We shall start by looking at an example from a news report informing on a group of researchers from Spain who developed a virtual coach to prevent depression with the headline:

_Médicos salmantinos crean un entrenador virtual que previene la depresion laboral_ (La Gaceta, 10/04/2021. Page 4)

[Doctors from Salamanca create a virtual coach that prevents depression at work]

If only the translation is observed, there is no indication of the gender of the doctors and researchers involved in the project; however, such gender is indicated in the original version (namely, the masculine generic). The difference simply comes from the way the English and Spanish languages indicate (or not) gender in their system: while the former is what is called a _natural gender language_, where most role nouns are not morphologically marked, the latter is a _grammatical gender language_, where nouns are usually marked for gender and therefore carry an additional cue to referential gender (Reali 2015). In other words, while Spanish language indicates the gender of words by means of an ending vowel (mainly -o for masculine, -a for feminine), there is no such marking in English. 

While this may seem a simple linguistic norm, it is of great importance for linguistic studies. When more than one gender is involved in a communicative or discursive situation in Spanish, or the gender is unknown or irrelevant, the Real Academia de la Lengua Española states that the masculine generic shall be used and is equally inclusive of all genders. However, many studies have proved that the use of the masculine generics inherently implies a lower depiction and projection of women in all situations. Scholars have recently investigated the consequences of using it in comparison to other more inclusive alternatives, such as the double form with a slash (_médicos/as_), the complete form of feminine and masculine gender (_médicas y medicos_) or even emerging options like the vowel _-e_ or the consonant _-x_. 

With the intention of continuing this line of research, a preliminary behavioral study was carried out to confirm whether participants do project a higher proportion of women when using the alternative forms in comparison to the normative one. Thus, 117 university students used a Likert scale to rate a list of 60 jobs as “mainly male” or “mainly women”; there were 3 different options of the survey, all containing the same jobs but presented in a different form (survey A: médicos, survey B: médicos/as; survey C: médicas y medicos”) and each participant would only answer one of the options. 

The results of this preliminary study show two main outcomes: there is a trend in general that shows how more women are in fact included in the scale when the job is presented in the alternative forms, compared to the masculine generic option; moreover, this trend is reversed for the jobs which are stereotypically female in society (such as hairdresser, nurse, nanny, etc.), meaning fewer women (or more men) are included in the alternative forms. This seems to agree and confirm the previous studies claiming that the use of inclusive language reduces the stereotype regardless of which gender is involved, and therefore should be used if the gender gap is to be more widely or easily accepted. 

Still, these preliminary results obtained in the behavioral study were nonetheless limited to a small part of Spanish population. For the purpose of this subject and this project, the present study applies the Rational Speech Act (RSA) framework to this problem and builds the semantics into a computational model that aims at confirming the aforementioned trend. The objective is not only to compare behavioral with computational results and therefore achieve a stronger statement, but also to be able to provide more general, applicable, justified and comparable model. 
____

### The model
#### Foundations

Our model is built on the foundations of the model of generic language covered in Chapter 7. We extend this model to address the issue of inferring the prevalence of women among a group of professionals in order to see whether the RSA can account for the observed behavioral effects, or whether some other mechanism must be in play. We lay out the architecture of the model below, building up. 

NB: For the purposes of this project, we assume that all group members are drawn from the man/woman binary. We operationalize this accordingly by treating the observed prevalence as the percent of women in the group. The percent of men in the group can therefore be inferred as 1 minus the observed prevalence of women. 

---

#### Possible utterances

Whereas the previous generics model had only two utterances (`generic` and `null`), we define four possible utterances:
- `mascpl`: masculine plural -- the so-called default
- `mslashf`: masculine/ feminine (e.g.: médicos/médicas) which is much higher cost than `mascpl` but can be applied to the same groups
- `fem`: feminine plural -- theoretically licensed only when the group contains exactly zero men (but we relax this condition as shown below)
- `uttnull`: the null utterance (silence)

The costs are shown in the code block below. `mascpl` and `uttnull` have the same low cost of `1` while the `fem` utterance is less frequent and therefore higher cost (`4`) and the higher effort `mslashf` utterance has a cost of `6`.

We define the utterance prior as a uniform draw from our four utterances.

~~~~
// Possible utterances
var utterances = ['mascpl', 'mslashf','fem','uttnull']

// Utterance costs
var cost = {
  'mascpl': 1,
  'mslashf': 6,
  'fem': 4,
  'uttnull': 1
}

// Utterance prior
var utterancePrior = function() { return uniformDraw(utterances) }

Infer(utterancePrior)
~~~~

#### Semantics

With the exception of the null utterance (which always returns `true`), the truth value of each of our utterances is defined according to some thresholds `thresholdM` or `thresholdF`. The parameters for these threshold are currently rather arbitrary, with `thresholdM` doing a uniform draw from the threshold bins (also more or less arbitrary), and `thresholdF` drawing from the same bins (for more discussion on this distribution, see [here](http://www.problang.org/chapters/07-generics.html)). In the future, we hope to estimate these thresholds empirically, but given the speaker intuition that the threshold for a `fem` utterance must be near `1`, we provide a potential parameterization of this distribution with a relatively high concentration of `50` and a mean of `0.85` , but for the purposes of simplicity, we use the same prior for both `thresholdM` and `thresholdF`.

We then define the truth conditions for the `mascpl` utterance as `true` when the observed prevalence is less than or equal to `thresholdM`. In the same way, `fem` is `true` when the observed prevalence is greater than or equal to `thresholdF`. The truth value of our high-cost `mslashf` utterance is defined as `true` when the observed prevalence is *either* less than or equal to `thresholdM` *or* greater than or equal to `thresholdF`--the disjunction of the other two utterances. A previous version used strictly greater than or less than operations for this utterance, but using those the greater than or equal to or less than or equal to operators does not seem to meaningfully impact the results.

Our `meaning` function therefore looks up the conditions based on the utterance and returns the corresponding truth value.

~~~~
// Threshold for using the masc utterance
var thresholdPriorM = function() { return uniformDraw(thresholdBins) }

// Threshold for using the fem utterance
// var thresholdPriorF = function() { sample(DiscreteBeta(0.85,50)) }

// Meaning functions by utterance
var uttFns = {
  uttnull : function(){return true},
  mascpl : function(prevalence,thresholdM,thresholdF){return prevalence <= thresholdM},
  mslashf : function(prevalence,thresholdM,thresholdF){return prevalence >= thresholdF 
                                                        || prevalence <= thresholdM},
  fem : function(prevalence,thresholdM,thresholdF){return prevalence >= thresholdF} // all women
}

// Select relevant meaning function
var meaning = function(utterance, prevalence, thresholdM,thresholdF) {
  var uttfn = uttFns[utterance]
  return uttfn(prevalence,thresholdM,thresholdF)
}
~~~~

#### Prior model
We then incorporate this into the prior model from Chapter 7 on generics. Whereas in that model, the prior was a mixture of both a stable and unstable distribution, we need only the stable distribution-- essentially what proportion of people in that profession are women. This prior distribution is drawn from a discretized Beta distribution with mean `prevalence`, and a constant concentration of `20`. We hope to more accurately fit this concentration parameter by profession in future versions.

For the purposes of this paper, we selected three exemplar professions:
- Truck drivers (_camioneros/as_) -- stereotypically male: 0.21
- Doctors (_medicos/as_) -- approximately even stereotype: 0.51
- Hairdressers (_peluqueros/as_) -- stereotypically female: 0.79

Prevalence is indicated as **percent women**. A prevalence of `0` would indicate all men, prevalence of `1` would indicate all women. We will sometimes intermix this by making reference to the prevalence of men going up which would simply be `1 - prevalence`.

~~~~
// Prior model
// Modified from Ch. 7 Generics for stable distribution only
var priorModel = function(params){
  Infer({model: function(){
    var g = params['prevalenceWhenPresent']
    var d = params['concentrationWhenPresent']
    var StableDistribution = DiscreteBeta(g, d)
    return sample(StableDistribution)
  }})
}

// Prevalence when present for representative professions
var professions = {camio: {prevalence: .21}, // truck driver
                   medic: {prevalence: .51}, // doctor
                   peluq: {prevalence: .79}} // hairdresser

// Get profession from lookup table
var prof_lookup = function(job){
  return professions[job]
}

// Query a profession and get the prevalence when present
var prof = 'peluq'
var prof_params = prof_lookup(prof)

// Define prior for a profession
var prior = priorModel({
  prevalenceWhenPresent: prof_params.prevalence,
  concentrationWhenPresent: 20
})
~~~~

#### Literal listener
Our literal listener $L_0$ is based on the $L_0$ from Chapter 7. As arguments it takes an `utterance`, a prior model of the prevalence for a profession (`statePrior`), and two thresholds corresponding to the prevalence of men required to trigger a `mascpl` utterance (`thresholdM`), and to the prevalence of women required to trigger a `fem` utterance (`thresholdF`). Given these elements, the listener performs a uniform draw from the discrete bins of the prior distribution. It then checks whether the observed `utterance` is consistent with that `prevalence` and the two given thresholds, and returns a truth value. If the `meaning` function returns `true`, then the listener returns the `prevalence`.

~~~~
// Literal listener (L0)
// cf. Ch. 7 Generic Language
var listener = function(utterance, statePrior,thresholdM,thresholdF) {
  Infer({model: function(){
    var prevalence = uniformDraw(bins)
    var m = meaning(utterance, prevalence, thresholdM,thresholdF)
    condition(m)
    return prevalence
  }})
}
~~~~

#### Literal listener behavior
We provide a basic demonstration of the $L_0$ behavior below. 

~~~~
// Literal listener behavior

// Bins
var bins = [0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1]
var thresholdBins = [0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]

// function returns a discretized Beta distribution
var DiscreteBeta = cache(function(g, d){
  var a =  g * d, b = (1-g) * d;
  var betaPDF = function(x){
    return Math.pow(x, a-1)*Math.pow((1-x), b-1)
  }
  var probs = map(betaPDF, bins);
  return Categorical({vs: bins, ps: probs})
})

var priorModel = function(params){
  Infer({model: function(){
    var g = params['prevalenceWhenPresent']
    var d = params['concentrationWhenPresent']
    var StableDistribution = DiscreteBeta(g, d)
    return sample(StableDistribution)
  }})
}

// Possible utterances
var utterances = ['mascpl', 'mslashf','fem','uttnull']

// Utterance costs
var cost = {
  'mascpl': 1,
  'mslashf': 6,
  'fem': 4,
  'uttnull': 1
}

// Utterance prior
var utterancePrior = function() { return uniformDraw(utterances) }

// Meaning functions by utterance
var uttFns = {
  uttnull : function(){return true},
  mascpl : function(prevalence,thresholdM,thresholdF){return prevalence <= thresholdM},
  mslashf : function(prevalence,thresholdM,thresholdF){return prevalence >= thresholdF 
                                                        || prevalence <= thresholdM},
  fem : function(prevalence,thresholdM,thresholdF){return prevalence >= thresholdF} // all women
}

// Select relevant meaning function
var meaning = function(utterance, prevalence, thresholdM,thresholdF) {
  var uttfn = uttFns[utterance]
  return uttfn(prevalence,thresholdM,thresholdF)
}

// Prior model
// Modified from Ch. 7 Generics for stable distribution only
var priorModel = function(params){
  Infer({model: function(){
    var g = params['prevalenceWhenPresent']
    var d = params['concentrationWhenPresent']
    var StableDistribution = DiscreteBeta(g, d)
    return sample(StableDistribution)
  }})
}

// Prevalence when present for representative professions
var professions = {camio: {prevalence: .21}, // truck driver
                   medic: {prevalence: .51}, // doctor
                   peluq: {prevalence: .79}} // hairdresser

// Get profession from lookup table
var prof_lookup = function(job){
  return professions[job]
}

// Literal listener (L0)
// cf. Ch. 7 Generic Language
var listener = function(utterance, statePrior,thresholdM,thresholdF) {
  Infer({model: function(){
    var prevalence = uniformDraw(bins)
    var m = meaning(utterance, prevalence, thresholdM,thresholdF)
    condition(m)
    return prevalence
  }})
}

// Query a profession and get the prevalence when present
var prof = 'peluq'
var prof_params = prof_lookup(prof)

// Define prior for a profession
var prior = priorModel({
  prevalenceWhenPresent: prof_params.prevalence,
  concentrationWhenPresent: 20
})

// Run literal listener
listener('mslashf',prior,.5,.5)
~~~~

Given the high-cost `mslashf` utterance and a prior model for `peluq` (hairdresser), and equal values for `thresholdM == thresholdF == 0.5`, the listener produces a bimodal distribution estimating the prevalence of women among _peluqueros/as_ to be either around 0.2 or around 0.8. This bimodal distribution makes sense given the disjunction logic used in the semantics for this utterance.

---

#### Speaker (S)
Our speaker, too, is based on the speaker from the generics model in Chapter 7, but modified in a crucial way. The original speaker would draw an `utterance`, and return the log odds of that `utterance` and `prevalence` under the literal listener. The utterance `cost` is then subtracted from these log odds, and the result is multiplied by the speaker optimality parameter `alpha` and returned.

Whereas in the original generics model, the costs of the utterances were constant, we now weight cost by strength of stereotype, i.e.: utterances used to describe states near the extremes (0 or 1) are costlier than utterances used to describe states near 0.5. We operationalize this by introducing `strength` which takes the absolute value of the `prevalence - 0.5` such that `prevalence` values near 0 or 1 will produce `strength` values close to 0.5, and `prevalence` values near 0.5 will produce `strength` values near 0.

This new `strength` value is then multiplied by the utterance's `cost` thus amplifying utterance cost more at the periphery.

~~~~
// Speaker (S)
var alpha = 4
// Modified from Ch. 7 Generics to weight cost by stereotype strength
var speaker = function(prevalence, statePrior,thresholdM,thresholdF){
  // get strength of stereotype
  var strength = Math.abs(prevalence -  0.5)
  Infer({model: function(){
    var utterance = utterancePrior();
    var L = listener(utterance, statePrior, thresholdM,thresholdF);
//     factor( alpha * (L.score(prevalence) - cost[utterance] ))
    factor( alpha * (L.score(prevalence) - (cost[utterance] * strength) ) )
    return utterance
  }})
}
~~~~

#### Speaker behavior
We provide a basic demonstration of $S$ behavior below.

~~~~
// Speaker behavior
// Bins
var bins = [0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1]
var thresholdBins = [0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]

// function returns a discretized Beta distribution
var DiscreteBeta = cache(function(g, d){
  var a =  g * d, b = (1-g) * d;
  var betaPDF = function(x){
    return Math.pow(x, a-1)*Math.pow((1-x), b-1)
  }
  var probs = map(betaPDF, bins);
  return Categorical({vs: bins, ps: probs})
})

var priorModel = function(params){
  Infer({model: function(){
    var g = params['prevalenceWhenPresent']
    var d = params['concentrationWhenPresent']
    var StableDistribution = DiscreteBeta(g, d)
    return sample(StableDistribution)
  }})
}

// Threshold for using the masc utterance
var thresholdPriorM = function() { return uniformDraw(thresholdBins) }

// Threshold for using the fem utterance
var thresholdPriorF = function() { sample(DiscreteBeta(0.85,50)) }

// Possible utterances
var utterances = ['mascpl', 'mslashf','fem','uttnull']

// Utterance costs
var cost = {
  'mascpl': 1,
  'mslashf': 6,
  'fem' : 4,
  'uttnull': 1
}

// Utterance prior
var utterancePrior = function() { return uniformDraw(utterances) }

// Meaning functions by utterance
var uttFns = {
  uttnull : function() {return true},
  mascpl : function(prevalence,thresholdM,thresholdF) {return prevalence <= thresholdM},
  mslashf : function(prevalence,thresholdM,thresholdF) {return prevalence > thresholdF || prevalence < thresholdM},
  fem : function(prevalence,thresholdM,thresholdF) {return prevalence >= thresholdF} // all women
}

// Select relevant meaning function
var meaning = function(utterance, prevalence, thresholdM,thresholdF) {
  var uttfn = uttFns[utterance]
  return uttfn(prevalence,thresholdM,thresholdF)
}

// Prior model
// Modified from Ch. 7 Generics for stable distribution only
var priorModel = function(params){
  Infer({model: function(){
    var g = params['prevalenceWhenPresent']
    var d = params['concentrationWhenPresent']
    var StableDistribution = DiscreteBeta(g, d)
    return sample(StableDistribution)
  }})
}

// Prevalence when present for representative professions
var professions = {camio: {prevalence: .21}, // truck driver
                   medic: {prevalence: .51}, // doctor
                   peluq: {prevalence: .79}} // hairdresser

// Get profession from lookup table
var prof_lookup = function(job){
  return professions[job]
}

// Literal listener (L0)
// cf. Ch. 7 Generic Language
var listener = function(utterance, statePrior,thresholdM,thresholdF) {
  Infer({model: function(){
    var prevalence = uniformDraw(bins)
    var m = meaning(utterance, prevalence, thresholdM,thresholdF)
    condition(m)
    return prevalence
  }})
}
var alpha = 4;
// Speaker (S)
// Modified from Ch. 7 Generics to weight cost by stereotype strength
var speaker = function(prevalence, statePrior,thresholdM,thresholdF){
  // get strength of stereotype
  var strength = Math.abs(prevalence -  0.5)
  Infer({model: function(){
    var utterance = utterancePrior();
    var L = listener(utterance, statePrior, thresholdM,thresholdF);
//     factor( alpha * (L.score(prevalence) - cost[utterance] ))
    factor( alpha * (L.score(prevalence) - (cost[utterance] * strength) ) )
    return utterance
  }})
}

// Query a profession and get the prevalence when present
var prof = 'peluq'
var prof_params = prof_lookup(prof)

// Define prior for a profession
var prior = priorModel({
  prevalenceWhenPresent: prof_params.prevalence,
  concentrationWhenPresent: 20
})

var thresholdM = 0.5 //thresholdPriorM()
var thresholdF = 0.5 //thresholdPriorF()
var prev = 0.3 //uniformDraw(bins)
display(prev)
speaker(prev,prior,thresholdM,thresholdF)
~~~~

Given `thresholdM = thresholdF = 0.5` and a `prevalence = 0.7 > 0.5`, the speaker will assign the most mass (~50%) to the `fem` utterance with marginally less given to `uttnull` and a tiny bit to the high-cost `mslashf`. The `mascpl` utterance is incompatible with the literal semantics in this case. If we take a `prevalence = 0.3 < 0.5`, then a markedly different pattern is observed. The speaker assigns more than 90% of posterior mass to the `mascpl`, with less than 10% assigned to `uttnull` and a negligible amount assigned to `mslashf`. The `fem` utterance is incompatible with the literal semantics here.

Importantly, under these conditions for the speaker and literal listener, there is no state where the `mslashf` utterance is favored.

---

#### Pragmatic listener
We base our Pragmatic listener ($L_1$) on the pragmatic listener from the Chapter 5 vagueness model. Whereas in that case, the $L_1$ was trying to infer the speaker's threshold for "expensiveness" and an item's true price. Here, the $L_1$ is attempting to infer the speaker's `thresholdM` and `thresholdF` values, as well as the true prevalence of women among the group of professionals that the speaker is talking about.

The pragmatic listener first draws a sample from its prior model for that profession, and then samples a `thresholdM` and a `thresholdF` which are passed to the speaker which returns the log odds for the observed utterance under that prevalence and those thresholds. The pragmatic listener then returns a 3-tuple object which contains the estimated prevalence and the two thresholds.

~~~~
// Pragmatic listener (L1)
// Modified from Ch. 5 Vagueness
var pragmaticListener = function(utterance,prior) {
  // run inference
  return Infer({method: 'enumerate'},
               function() {
    var prevalence = sample(prior)
    var thresholdM = thresholdPriorM()
//     var thresholdF = thresholdPriorF()
    var thresholdF = thresholdPriorM()
    factor( speaker(prevalence, prior,thresholdM,thresholdF).score(utterance) );
    return { prevalence: prevalence ,thresholdM: thresholdM, thresholdF: thresholdF};
  });
};
~~~~

#### Pragmatic listener behavior
As we incorporate the pragmatic listener, we now have a complete model which will infer the prevalence of women in some professional group based on their priors for that profession and its knowledge of the possible utterances.

Below, we run a simulation over the three professions (truck driver -- `camio`, doctor -- `medic`, hairdresser -- `peluq`) for two of the possible utterances: the "default" `mascpl` utterance, and the higher-cost `mslashf` utterance.

~~~~
// Put it all together
// Bins 
// var bins = [0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1]
// var thresholdBins = [0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]

// discretized range between 0 - 1
var bins = map(function(x){
  _.round(x, 2);
},  _.range(0.01, 1, 0.05));

var thresholdBins = map2(function(x,y){
  var d = (y - x)/ 2;
  return x + d
}, bins.slice(0, bins.length - 1), bins.slice(1, bins.length))

// function returns a discretized Beta distribution
var DiscreteBeta = cache(function(g, d){
  var a =  g * d, b = (1-g) * d;
  var betaPDF = function(x){
    return Math.pow(x, a-1)*Math.pow((1-x), b-1)
  }
  var probs = map(betaPDF, bins);
  return Categorical({vs: bins, ps: probs})
})

var priorModel = function(params){
  Infer({model: function(){
    var g = params['prevalenceWhenPresent']
    var d = params['concentrationWhenPresent']
    var StableDistribution = DiscreteBeta(g, d)
    return sample(StableDistribution)
  }})
}

// Threshold for using the masc utterance
var thresholdPriorM = function() { return uniformDraw(thresholdBins) }

// Threshold for using the fem utterance
var thresholdPriorF = function() { sample(DiscreteBeta(0.89,30)) }

// Possible utterances
var utterances = ['mascpl', 'mslashf','fem','uttnull']

// Utterance costs
var cost = {
  'mascpl': 1,
  'mslashf': 6,
  'fem': 4,
  'uttnull': 1
}

// Utterance prior
var utterancePrior = function() { return uniformDraw(utterances) }

// Meaning functions by utterance
var uttFns = {
  uttnull : function() {return true},
  mascpl : function(prevalence,thresholdM,thresholdF) {return prevalence <= thresholdM},
  mslashf : function(prevalence,thresholdM,thresholdF) {return prevalence >= thresholdF || prevalence <= thresholdM},
  fem : function(prevalence,thresholdM,thresholdF) {return prevalence >= thresholdF} // all women
}

// Select relevant meaning function
var meaning = function(utterance, prevalence, thresholdM,thresholdF) {
  var uttfn = uttFns[utterance]
  return uttfn(prevalence,thresholdM,thresholdF)
}

// Prior model
// Modified from Ch. 7 Generics for stable distribution only
var priorModel = function(params){
  Infer({model: function(){
    var g = params['prevalenceWhenPresent']
    var d = params['concentrationWhenPresent']
    var StableDistribution = DiscreteBeta(g, d)
    return sample(StableDistribution)
  }})
}

// Prevalence when present for representative professions
var professions = {camio: {prevalence: .21}, // truck driver
                   medic: {prevalence: .51}, // doctor
                   peluq: {prevalence: .79}} // hairdresser

// Get profession from lookup table
var prof_lookup = function(job){
  return professions[job]
}

// Query a profession and get the prevalence when present
var prof = 'peluq'
var prof_params = prof_lookup(prof)

// Define prior for a profession
var prior = priorModel({
  prevalenceWhenPresent: prof_params.prevalence,
  concentrationWhenPresent: 20
})

// Literal listener (L0)
// cf. Ch. 7 Generic Language
var listener = function(utterance, statePrior,thresholdM,thresholdF) {
  Infer({model: function(){
    var prevalence = uniformDraw(bins)
    var m = meaning(utterance, prevalence, thresholdM,thresholdF)
    condition(m)
    return prevalence
  }})
}
var alpha = 4;
// Speaker (S)
// Modified from Ch. 7 Generics to weight cost by stereotype strength
var speaker = function(prevalence, statePrior,thresholdM,thresholdF){
  // get strength of stereotype
  var strength = Math.abs(prevalence -  0.5)
  Infer({model: function(){
    var utterance = utterancePrior();
    var L = listener(utterance, statePrior, thresholdM,thresholdF);
//     factor( alpha * (L.score(prevalence) - cost[utterance] ))
    factor( alpha * (L.score(prevalence) - (cost[utterance] * strength) ) )
    return utterance
  }})
}

// Pragmatic listener (L1)
// Modified from Ch. 5 Vagueness
var pragmaticListener = function(utterance,prior) {
  // run inference
  return Infer({method: 'enumerate'},
               function() {
    var prevalence = sample(prior)
    var thresholdM = thresholdPriorM()
//     var thresholdF = thresholdPriorF()
    var thresholdF = thresholdPriorM()
    factor( speaker(prevalence, prior,thresholdM,thresholdF).score(utterance) )
    return { prevalence: prevalence ,thresholdM: thresholdM, thresholdF: thresholdF}
  })
}

// Define func to run model for mslashf, and mascpl utterances
var runsim = function(prior){
  map(function(utt){
    var L1 = pragmaticListener(utt,prior)
    var expect = expectation(marginalize(L1,'prevalence'))
    display('expected % women (' + utt + '): ' + expect)
    viz(marginalize(L1,'prevalence'))
    return expect
  }, ['mascpl','mslashf']) // ,'fem'
}

// Run model across the three professions
map(function(prof){
  var prof_prev = prof_lookup(prof).prevalence
  display(prof + '(prior mean: ' + prof_prev + ')')
  var prior = priorModel({
    prevalenceWhenPresent: prof_prev,
    concentrationWhenPresent: 20
  })
  runsim(prior)
},['camio','medic','peluq'])

true
~~~~

### Results 
For a stereotypically male profession (truck driver; prior mean = 0.21), the model estimates a higher prevalence of women under the high-cost utterance (~39%) relative to the low-cost "default" one (~20%). For a stereotypically neutral profession (doctor; prior mean = 0.51), the model estimates a marginally higher prevalence of women under the high-cost utterance (~51%) than the low-cost one (~47%). Finally, for a stereotypically female profession (hairdresser; prior mean = 0.79), the model estimates a *lower* prevalence of women under the high-cost utterance (~67%) compared to the low-cost one (~74%).

### Discussion

Contrary to the claims of the RAE, the "default" low-cost masculine plural utterance does not provide uniform expectations over gender distribution regardless of profession, a pattern we have been able to encode here. In the same vein, the behavioral data show that the higher-cost utterance does not necessarily mean "more women" than the low-cost utterance, but can also be used to indicate more men, again a behavior we have been able to mimic with our RSA model. We therefore posit that the high-cost utterance is not used to indicate "more women" but rather to contrast with the listener's prior expectations about the gender prevalence of the profession.

We find that the model produces a qualitatively human-like pattern with regard to its estimate of the prevalence of women in the professions given the selected utterances. Under the high cost utterance, the model draws posterior mass away from the extrema relative to the lower-cost utterance -- the pattern we observe in the human respondents. 

In order to achieve these effects, we have specifically innovated in two ways: semantics of the `mslashf` utterance, and the use of cost-weighting. While the `mascpl` and `fem` utterances have straightforward interpretations as meeting some required lower or upper threshold of women respectively, the `mslashf` utterance is not quite so obvious. By operationalizing the semantics of this utterance as the disjunction of the other two non-null utterances, we appear to have appropriately captured human behavior. 

In class, the question was raised as to whether the cost weighting was necessary to produce the observed behavioral results. We found that without the cost weighting, the model always predicted a greater prevalence of women under the `mslashf` utterance relative to the `mascpl` one (this can be observed by commenting out the `factor` statement in the speaker and uncommenting the line above). In essence, the `mslashf` utterance was serving as an intermediate point between the `mascpl` (lower prevalence of women) and `fem` (higher prevalence of women) utterances. It is unclear whether the mechanism we have implemented here is cognitively plausible, but it functions as needed for now.

In a previous version of the model which was presented in class, we used a higher prior for `thresholdF`, however this does not appear to be necessary to produce the observed results (but sometimes makes the differences larger).

### Conclusions and future directions
In short, we have a functioning model to capture an interesting pattern of human linguistic behavior. More work is needed, however, to work out some of the specifics and fit it more accurately to human behavior. One element of this would be a behavioral study which asked participants how many out of a set number (e.g.: 5) of professionals are women given the utterances. In this task we would also need to include a `fem` utterance in order to round out the priors of the model. This task would help provide a more direct mapping from human behavior to model predictions. 

On the modeling side, we hope to implement a pragmatic speaker which will select appropriate utterances for `{profession, prevalence}` pairs to match up with the pragmatic listener. An initial attempt was made at implementing this speaker but it did not immediately produce the desired effects so it was scrapped for the sake of time. 

In addition, we hope to further explore the parameter space for `thresholdM`, and `thresholdF` to understand how they impact model predictions and interact with one another. We also hope to include an even higher-cost utterance of the form _peluqueras o peluqueros_ which even more explicitly encodes the disjunction implemented in the semantics of the `mslashf` utterance here.
