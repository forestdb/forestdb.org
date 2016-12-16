---
layout: model
title: Conventions
model-language: webppl
model-language-version: v0.9.6
---

### Part 1: arbitrary mapping

~~~~
// possible states of the world
var statePrior =  Categorical({vs: ['tangram1', 'tangram2'], 
                               ps: [.5, .5]});
// possible utterances (include null utterance to make sure dists are well-formed)
var utterancePrior = Categorical({vs: ['label1', 'label2', 'n0'], 
                                  ps: [1/3,1/3,1/3]});

// meaning funtion to interpret the utterances
var nullMeaning = function(state){return true;};
var tangram1Meaning = function(state){return state == 'tangram1';};
var tangram2Meaning = function(state){return state == 'tangram2';};
var possibleMeanings = [nullMeaning, tangram1Meaning, tangram2Meaning];
var possibleLexicons = _.flatten(
  map(function(t1Meaning) {
    map(function(t2Meaning) {
      return _.extend(_.object(utterancePrior.support(), 
			       [t1Meaning, t2Meaning]),
                      {n0 : nullMeaning});
    }, possibleMeanings);
  }, possibleMeanings));

var lexiconPrior = Categorical({vs: possibleLexicons, 
                                ps: [1/9,1/9,1/9,1/9,1/9,1/9,1/9,1/9,1/9]});

// speaker optimality
var alpha = 1;

// null utterance costly; everything else cheap
var uttCost = function(utt) {
  return (utt == 'tangram1' || utt == 'tangram2' ? 1 : 10);
};

// literal listener
var L0 = function(utt, lexicon) {
  return Infer({method:"enumerate"}, function(){
    var state = sample(statePrior);
    var meaning = lexicon[utt];
    condition(meaning(state));
    return state;
  });
};

// pragmatic speaker
var S1 = function(state, lexicon) {
  return Infer({method:"enumerate"}, function(){
    var utt = sample(utterancePrior);
    factor(alpha * (L0(utt, lexicon).score(state)
    		    - uttCost(utt)));
    return utt;
  });
};

// pragmatic listener (needed for S)
var L2 = function(utt, lexicon) {
  return Infer({method: 'enumerate'}, function() {
    var state = sample(statePrior);
    observe(S1(state, lexicon), utt);
    return state;
  });
};

// conventional listener
var L = function(utt, data) {
  return Infer({method:"enumerate"}, function(){
    var state = sample(statePrior);
    var lexicon = sample(lexiconPrior);
    observe(S1(state, lexicon), utt);
    mapData({data: data}, function(datum){
      observe(S1(datum.obj, lexicon), datum.utt);
    });
    return state;
  });
};

// conventional speaker
var S = function(state, data) {
  return Infer({method:"enumerate"}, function(){
    var lexicon = sample(lexiconPrior);
    var utt = sample(utterancePrior);
    factor(alpha * (L2(utt, lexicon).score(state)
    		   - uttCost(utt)));
    mapData({data: data}, function(datum){
      observe(L2(datum.utt, lexicon), datum.obj);
    });
    return utt;
  });
};

console.log("listener hearing label1 after data:");
viz(L('label1', [{utt: 'label1', obj: 'tangram1'}]))

console.log("listener hearing label2 after data:");
viz(L('label2', [{utt: 'label1', obj: 'tangram1'}]))
~~~~

### Part 2: Shortening arbitrary utterances

~~~~
///fold:
var _powerset = function(set) {
  if (set.length == 0)
    return [[]];
  else {
    var rest = _powerset(set.slice(1));
    return map(function(element) {
      return [set[0]].concat(element);
    }, rest).concat(rest);
  }
};

var powerset = function(set, opts) {
  var res = _powerset(set);
  return opts.noNull ? filter(function(x){return !_.isEmpty(x);}, res) : res;
};

var cartesianProductOf = function(listOfLists) {
  return reduce(function(b, a) { 
    return _.flatten(map(function(x) {     
      return map(function(y) {             
        return x.concat([y]);                   
      }, b);                                       
    }, a), true);                                  
  }, [ [] ], listOfLists);                                   
};

// It takes too long to explore the real space, so we'll use the toy
// version that doesn't allow repeats (e.g. only allowed to insert or
// replace words with new ones that aren't already in the list)
var deleteWords = function(words) {
  if(_.isEmpty(words)) {
    return words;
  } else {
    var wordToOmit = uniformDraw(words);
    return remove(wordToOmit,words);
  }
};

var insertWords = function(words) {
  if(_.isEmpty(_.difference(possibleWords, words))) {
    return words;
  } else {
    var insertLoc = randomInteger(words.length + 1);
    var insertWord = uniformDraw(_.difference(possibleWords, words));
    return (words.slice(0,insertLoc)
            .concat(insertWord)
            .concat(words.slice(insertLoc, words.length)));
  }
};

var replaceWords = function(words) {
  if(_.isEmpty(_.difference(possibleWords, words))) {
    return words;
  } else {
    
    var replaceLoc = randomInteger(words.length);
    var replaceWord = uniformDraw(_.difference(possibleWords, words));
    return (words.slice(0,replaceLoc)
            .concat(replaceWord)
            .concat(words.slice(replaceLoc+1,words.length)));
  }
};

var nullMeaning = function(x) {return true;};
var constructAnyMeaning = function(label) {
  return function(trueState) {
    return any(function(labelState){
      return labelState == trueState;
    }, label.split('|'));
  }
};
var negate = function(f) {return function(x) {return !f(x)};}
var identity = function(x) {return x;};
var utteranceProbs = Infer({method: 'enumerate'}, function() {
  return normalize(repeat(5, function(){uniformDraw([.1, .5])}))
})
var getRatio = function(model) {
  return Math.exp(model.score('ki') - model.score('kima'))
}
///

// possible states of the world
var states = ['t1', 't2'];
var statePrior = Categorical({vs: states, ps: [1/2, 1/2]});

// possible utterances (include null utterance to make sure dists are well-formed)
var unconstrainedUtterances = ['kima', 'fuba'];
var derivedUtterances = ['n0'];
var utterances = unconstrainedUtterances.concat(derivedUtterances);
var utterancePrior = Categorical({vs: utterances, ps: [1/3,1/3,1/3]});

// longer utterances more costly (count chars)
var uttCost = cache(function(utt) {
  return utt == 'n0' ? 10 : utt.split('').length/2;
});

// meanings are possible disjunctions of states 
var meanings = map(function(l){return l.join('|');}, 
                   powerset(states, {noNull: true}));
var meaningSets = cartesianProductOf(repeat(unconstrainedUtterances.length, 
                                            function() {return meanings;}));

// Lexicons are maps from utterances to meanings 
// (null utterance always goes to null meaning)
var lexicons = map(function(meaningSet) {
  var unconstrainedMeanings = _.object(unconstrainedUtterances, meaningSet);
  return _.extend(unconstrainedMeanings, {'n0': 'null'});
}, meaningSets);
var lexiconPrior = Categorical({vs: lexicons, ps: repeat(lexicons.length, function(){return 1/lexicons.length})});

// Looks up the meaning of an utterance in a lexicon object
var meaning = cache(function(utt, lexicon) {  
  return (lexicon[utt] == 'null' ? 
          nullMeaning : 
          constructAnyMeaning(lexicon[utt]));
});

var exploreModel = function(params) {
  // Gives distribution over possible noisy versions of utt
  var noiseModel = cache(function(utt) {
    return Infer({method: 'enumerate'}, function() {
      if(flip(1-params.noiseRate)) {
        return utt;
      } else {
        return (utt === 'n0' ? 'n0' :
                utt === 'kima' ? categorical({vs: ['ki', 'kiba', 'fu', 'fuba'],
                                              ps: params.noiseProbs}) :
                utt === 'fuba' ? categorical({vs: ['fu', 'fuma', 'ki', 'kima'],
                                              ps: params.noiseProbs}) :
                console.error('unknown utt'));
      }
    });
  });

  // literal listener w/ noisy channel inference
  var L0 = cache(function(utt, lexicon) {
    return Infer({method:"enumerate"}, function(){
      var state = sample(statePrior);
      var intendedUtt = sample(utterancePrior);

      var uttMeaning = meaning(intendedUtt, lexicon);
      factor(uttMeaning(state) ?
             noiseModel(intendedUtt).score(utt) :
             -100);
      return state;
    });
  });

  // pragmatic speaker
  var S1 = cache(function(state, lexicon) {
    return Infer({method:"enumerate"}, function(){
      var intendedUtt = sample(utterancePrior);
      var noisyUtt = sample(noiseModel(intendedUtt));
      factor(params.alpha * (L0(noisyUtt, lexicon).score(state)
                             - uttCost(noisyUtt)));
      return intendedUtt;
    });
  });

  // pragmatic listener (needed for S)
  var L2 = cache(function(perceivedUtt, lexicon) {
    return Infer({method: 'enumerate'}, function() {
      var state = sample(statePrior);
      var intendedUtt = sample(utterancePrior);

      observe(noiseModel(intendedUtt), perceivedUtt);
      observe(S1(state, lexicon), intendedUtt);

      return state;
    });
  });

  // conventional listener
  // Assumes:
  // * all utterances, including current one, come from same lexicon
  // * intended utt for each past data point is independent
  // * speaker is S1 using utt to noisily communicate about some state 
  var L = cache(function(perceivedUtt, data) {
    return Infer({method:"enumerate"}, function(){
      var state = sample(statePrior);
      var lexicon = sample(lexiconPrior);
      var intendedUtt = sample(utterancePrior);

      observe(noiseModel(intendedUtt), perceivedUtt);
      observe(S1(state, lexicon), intendedUtt);

      mapData({data: data}, function(datum){
        var intendedUtt = sample(utterancePrior);
        observe(noiseModel(intendedUtt), datum.utt);
        observe(S1(datum.obj, lexicon), intendedUtt);
      });
      return state;
    });
  });

  // console.log(L('ki', [{utt:'kima', obj:'t1'}, {utt:'fuba', obj:'t2'}]));
  // console.log(L('ki', [{utt:'ki', obj:'t1'},{utt:'fu', obj:'t2'}]));

  // conventional speaker
  var S = function(state, data) {
    return Infer({method:"enumerate"}, function(){
      var lexicon = sample(lexiconPrior);
      var intendedUtt = sample(utterancePrior);
      var noisyUtt = sample(noiseModel(intendedUtt));

      factor(params.alpha * (L2(noisyUtt, lexicon).score(state)
                             - uttCost(noisyUtt)));

      mapData({data: data}, function(datum){
        var intendedUtt = sample(utterancePrior);

        observe(noiseModel(intendedUtt), datum.utt); // update beliefs about utterance dist
        observe(L2(datum.utt, lexicon), datum.obj); // update beliefs about lexicon
      });
      return noisyUtt;
    });
  };

  viz(S('t1', []))
  viz(S('t1', [{utt: 'kima', obj: 't1'}]));
  viz(S('t1', [{utt: 'kima', obj: 't1'},
               {utt: 'fuba', obj: 't2'}]))
  viz(S('t1', [{utt: 'kima', obj: 't1'},
               {utt: 'fuba', obj: 't2'},
               {utt: 'kima', obj: 't1'}]))

  print(getRatio(S('t1', [{utt: 'kima', obj: 't1'}])))
  print(getRatio(S('t1', [{utt: 'kima', obj: 't1'},
                          {utt: 'fuba', obj: 't2'}])))
  print(getRatio(S('t1', [{utt: 'kima', obj: 't1'},
                          {utt: 'fuba', obj: 't2'},
                          {utt: 'kima', obj: 't1'}])))
}

exploreModel({
  alpha: 1,
  noiseRate: .5,
  noiseProbs: [.8, .05, .05, .1]
})

// var paramRegimes = Infer({method: 'MCMC', samples: 2000, 
//                           callbacks: [editor.MCMCProgress()]}, function(){
//   var params = {
//     alpha: uniform({a: 0, b: 10}),
//     noiseRate: uniform({a: 0, b: .5}),
//     noiseProbs: T.toScalars(dirichlet(ones([4,1])))
//   }
  
//   condition(exploreModel(params))
  
//   return _.extend(_.omit(params, 'noiseProbs'),  
//                   _.object(['ki', 'kiba', 'fu', 'fuba'], params.noiseProbs));
// })

// viz.marginals(paramRegimes)
~~~~