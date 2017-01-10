---
layout: model
title: Conventions
model-language: webppl
model-language-version: v0.9.6
---

Why do Americans drive on the right side of the road while the British drive on the left? Why do English speakers use 'cat' to refer to a pet that goes 'meow' while the French use the word `chat'? These conventions or norms govern much of our everyday behavior. While there is a substantial literature using simple agent-based models showing how these arbitrary but stable patterns can emerge in large populations, there has been comparatively less work on the cognitive underpinnings of conventions.

Here, we focus on a classic case of conventionalization of language in a reference game (Clark & Wilkes-Gibbs, 1986). In the simplest version of this task, two players are presented with an array of objects constructed from tangrams which are not easily describable. One of them is designated as the 'target' for the speaker, and their goal is to produce an utterance that will allow the listener to distinguish the correct object from their array. 

Our model of conventionalization combines two innovations in modeling language understanding: lexical uncertainty and a noisy communication channel. The former introduces uncertainty over the exact meanings of words in the lexicon; the latter introduces a perceptual noise model by which utterances can be corrupted during transmission. We gradually build up to this combined model by tackling sub-problems.

### Part 1: arbitrary mapping

We begin by implementing the simplest lexical uncertainty model, used in Bergen, Levy, & Goodman (2016) to account for M-implicatures. In the simplified case we consider, there are just two labels and two tangrams. How does the pair converge on a mapping?

This is the simplest demonstration of conventions; even though neither party knows the meaning of a label at the outset, a random choice is taken to be evidence for a particular lexicon and it becomes the base for successful communication.

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

var cartesian = function(listOfLists) {
  return reduce(function(b, a) { 
    return _.flatten(map(function(x) {     
      return map(function(y) {             
        return x.concat([y]);                   
      }, b);                                       
    }, a), true);                                  
  }, [ [] ], listOfLists);                                   
};
var constructAnyMeaning = function(label) {
  return function(trueState) {
    return any(function(labelState){
      return labelState == trueState;
    }, label.split('|'));
  }
};
var nullMeaning = function(x) {return true;};
///

// possible states of the world
var states = ['t1', 't2'];
var statePrior =  Categorical({vs: states, ps: [1/2, 1/2]});

// possible utterances (include null utterance to make sure dists are well-formed)
var unconstrainedUtterances = ['label1', 'label2'];
var derivedUtterances = ['n0'];
var utterances = unconstrainedUtterances.concat(derivedUtterances);
var utterancePrior = Categorical({vs: utterances, ps: [1/3, 1/3, 1/3]});

// meanings are possible disjunctions of states
var meanings = map(function(l){return l.join('|');}, 
                   powerset(states, {noNull: true}));

// Lexicons are maps from utterances to meanings 
var meaningSets = cartesian(repeat(unconstrainedUtterances.length, function() {return meanings;}));
var lexicons = map(function(meaningSet) {
  var unconstrainedMeanings = _.object(unconstrainedUtterances, meaningSet);
  return _.extend(unconstrainedMeanings, {'n0': 'null'});
}, meaningSets);
var lexiconPrior = Categorical({vs: lexicons, ps: repeat(lexicons.length, function(){return 1/lexicons.length})});

// speaker optimality
var alpha = 1;

// null utterance costly; everything else cheap
var uttCost = function(utt) {
  return (utt == 't1' || utt == 't2' ? 1 : 10);
};

// Looks up the meaning of an utterance in a lexicon object
var meaning = cache(function(utt, lexicon) {  
  return (lexicon[utt] == 'null' ? 
          nullMeaning : 
          constructAnyMeaning(lexicon[utt]));
});

// literal listener
var L0 = function(utt, lexicon) {
  return Infer({method:"enumerate"}, function(){
    var state = sample(statePrior);
    var uttMeaning = meaning(utt, lexicon);
    condition(uttMeaning(state));
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

console.log("initial listener interpretation (first trial)");
viz(L('label1', []))

console.log("listener hearing label1 after data:");
viz(L('label1', [{utt: 'label1', obj: 't1'}]))

console.log("listener hearing label2 after data:");
viz(L('label2', [{utt: 'label1', obj: 't1'}]))
~~~~

The listener is initially uncertain about which tangram 'label1' is referring to, but after observing a trial in which 'label1' corresponded to 'tangram1', they update their beliefs about the likely lexicon and subsequently become more likely to interpret 'label1' as referring to 'tangram1.' Note that it is also more likely to interpret 'label2' as referring to 'tangram2', even though it has not observed any explicit usage of this label. This latter effect is a standard consequence of pragmatic reasoning. 

### Part 2: Shortening arbitrary utterances

In Clark & Wilkes-Gibbs (1986), one signature phenomenon is the shortening of referring expressions over time. On the first round, a speaker may refer to a tangram as "the one with their arms out front, like an ice skater," which becomes "ice skater" by the final round. To account for this qualitative phenomenon, we add a noisy communication channel to the above lexical uncertainty model & show (very weakly) that under certain parameters, the shorter utterance becomes *more likely* relative to the longer utterance as the number of observations increases.

To demonstrate this most clearly, and to explore the effect of different choices in the noise model, we briefly turn to a simpler example, where the pair of possible labels have two morphemes: "kima" and "fuba". The noisy channel can *delete* a morpheme (e.g. "kima" -> "ki"), or replace a morpheme (e.g. "kima" -> "kiba").

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

var cartesianProd = function(listOfLists) {
  return reduce(function(b, a) { 
    return _.flatten(map(function(x) {     
      return map(function(y) {             
        return x.concat([y]);                   
      }, b);                                       
    }, a), true);                                  
  }, [ [] ], listOfLists);                                   
};

var initList = function(n, val) {
  return repeat(n, function() {return val})
}

var uniformPs = function(vs) {
  return initList(vs.length, 1/vs.length)
}

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

var firstMorphs = ['ki', 'fu'];
var secondMorphs = ['ba', 'ma']
var performOp = function(utt, op, morph) {
  var morphs = utt.split(' ');
  if(morphs.length === 1) {
    var m = morphs[0];
    return (op === 'delete' ? ['n0'] :
            _.contains(firstMorphs, m) ? remove(m, firstMorphs) :
            _.contains(secondMorphs, m) ? remove(m, secondMorphs) :
           console.error('cannot perform op'));
  } else {
    if(op === 'delete') {
      return (morph === 'both' ? ['n0'] :
              morph === 'first' ? [morphs[1]] :
              morph === 'second' ? [morphs[0]] : 
              console.error('cannot perform op'))
    } else if(op === 'replace') {
      return (morph === 'first' ? remove(morphs[0], firstMorphs).concat(morphs[1]) :
              morph === 'second' ? [morphs[0]].concat(remove(morphs[1], secondMorphs)) :
              morph === 'both' ? remove(morphs[0], firstMorphs).concat(remove(morphs[1], secondMorphs)) :
              console.error('cannot perform op'))
    }
  }
};

var nullMeaning = function(x) {return true;};
var constructMeaning = function(label) {
  return function(trueState) {
    return any(function(labelState){
      return labelState == trueState;
    }, label.split('|'));
  }
};
var negate = function(f) {return function(x) {return !f(x)};}
var identity = function(x) {return x;};
var getRatio = function(model) {
  return Math.exp(model.score('ki') - model.score('ki ma'))
}
///

// possible states of the world
var states = ['t1', 't2'];
var statePrior = Categorical({vs: states, ps: uniformPs(states)});

// possible utterances (include null utterance to make sure dists are well-formed)
var unconstrainedUtts = ['ki ma', 'fu ba'];
var derivedUtts = ['n0'];
var intentionallyCorruptedUtts = ['ki', 'fu', 'ma', 'ba', 'ki ba', 'fu ma']
var grammaticalUtts = unconstrainedUtts.concat(derivedUtts);
var intendedUtts = grammaticalUtts.concat(intentionallyCorruptedUtts)
var utterancePrior = Categorical({vs: grammaticalUtts, ps: uniformPs(grammaticalUtts)});

// longer utterances more costly (count chars)
var uttCost = cache(function(utt) {
  return utt == 'n0' ? 10 : utt.split('').length/2;
});

// meanings are possible disjunctions of states 
var meanings = map(function(l){return l.join('|');}, 
                   powerset(states, {noNull: true}));
var meaningSets = cartesianProd(initList(unconstrainedUtts.length, meanings));

// Lexicons are maps from utterances to meanings 
// (null utterance always goes to null meaning)
var lexicons = map(function(meaningSet) {
  var unconstrainedMeanings = _.object(unconstrainedUtts, meaningSet);
  return _.extend(unconstrainedMeanings, {'n0': 'null'});
}, meaningSets);
var lexiconPrior = Categorical({vs: lexicons, ps: uniformPs(lexicons)});

// Looks up the meaning of an utterance in a lexicon object
var meaning = cache(function(utt, lexicon) {  
  var mStr = lexicon[utt];
  return (mStr == 'null' ? nullMeaning : constructMeaning(mStr));
});

// Simplified corruption function 
var corruptUtt = function(utt, params) {
  if(utt === 'n0') {
    return 'n0';
  } else if (_.contains(intendedUtts, utt)) {
    var operation = categorical({vs: ['delete', 'replace'],
                                 ps: [params.deleteProb, 1-params.deleteProb]});
    var morpheme = categorical({vs: ['both', 'first', 'second'],
                                ps: [params.bothProb, (1-params.bothProb)/2, (1-params.bothProb)/2]})
    return flip(params.randomProb) ? uniformDraw(intendedUtts) : performOp(utt, operation, morpheme).join(' ');
  } else {
    console.error('unknown utt')
  }
}

var params = {
  alpha: 2,
  noiseRate: 0.3,
  deleteProb: .6,
  bothProb: .25,
  randomProb: .25
}

// Gives distribution over possible noisy versions of intended utt
var noiseModel = cache(function(utt) {
  return Infer({method: 'enumerate'}, function() {
    return flip(1 - params.noiseRate) ? utt : corruptUtt(utt, params);
  });
});
  
// Inverts noise model, so that each corrupted utterances yields
// a distribution over possible intended utterances.
var invNoiseModel = cache(function(utt, opts){
  return Infer({method:'enumerate'}, function(){
    var intendedUtt = sample(utterancePrior)
    observe(noiseModel(intendedUtt), utt)
    return intendedUtt
  })
})

// literal listener w/ noisy channel inference
var L0 = cache(function(utt, lexicon) {
  return Infer({method:"enumerate"}, function(){
    var state = sample(statePrior);
    var intendedUtt = sample(invNoiseModel(utt));

    var uttMeaning = meaning(intendedUtt, lexicon);
    factor(uttMeaning(state) ? 0 : -100);
    return state;
  });
});

// pragmatic speaker marginalizing over perceptual corruption in L0
var S1 = cache(function(state, lexicon) {
  return Infer({method:"enumerate"}, function(){
    var intendedUtt = uniformDraw(intendedUtts)
    var listener = Infer({method: 'enumerate'}, function(){
      var corruptedUtt = sample(noiseModel(intendedUtt));
      return sample(L0(corruptedUtt,lexicon))
    })

    factor(params.alpha * (listener.score(state) - uttCost(intendedUtt)));
    return intendedUtt;
  });
});

// pragmatic listener (needed for S)
var L2 = cache(function(perceivedUtt, lexicon) {
  return Infer({method: 'enumerate'}, function() {
    var state = sample(statePrior);
    var intendedUtt = sample(invNoiseModel(perceivedUtt));
    observe(S1(state, lexicon), intendedUtt);
    return state;
  });
});

// conventional listener
var L = cache(function(perceivedUtt, data) {
  return Infer({method:"enumerate"}, function(){
    var state = sample(statePrior);
    var lexicon = sample(lexiconPrior);
    var intendedUtt = sample(invNoiseModel(perceivedUtt));
    
    observe(S1(state, lexicon), intendedUtt);
    mapData({data: data}, function(datum){
      observe(S1(datum.obj, lexicon), datum.utt);
    });
    return state;
  });
});

// conventional speaker
var S = function(state, data) {
  return Infer({method:"enumerate"}, function(){
    var lexicon = sample(lexiconPrior);
    var intendedUtt = uniformDraw(intendedUtts);
    var listener = Infer({method: 'enumerate'}, function(){
      var corruptedUtt = sample(noiseModel(intendedUtt))
      return sample(L2(corruptedUtt, lexicon))
    })
    
    factor(params.alpha * (listener.score(state) - uttCost(intendedUtt)));
    mapData({data: data}, function(datum){
      observe(L2(datum.utt, lexicon), datum.obj); // update beliefs about lexicon
    });
    return intendedUtt;
  });
};

// Listener is better able to understand 'ki' if they've observed full utterances
// than if they've only heard snippets
//   console.log(L('ki', [{utt:'ki ma', obj:'t1'}, {utt:'fu ba', obj:'t2'}]));
//   console.log(L('ki', [{utt:'ki', obj:'t1'},{utt:'fu', obj:'t2'}]));

console.log('speaker with no data ([])')
viz(S('t1', []))
console.log("speaker who observed 'kima'<=>'t1'")
viz(S('t1', [{utt: 'ki ma', obj: 't1'}]));
console.log("speaker who observed 'kima'<=>'t1' + 'fuba'<=>'t2'")  
viz(S('t1', [{utt: 'ki ma', obj: 't1'},
             {utt: 'fu ba', obj: 't2'}]))
console.log("speaker who observed 2x 'kima'<=>'t1' + 1x'fuba'<=>'t2'")
viz(S('t1', [{utt: 'ki ma', obj: 't1'},
             {utt: 'fu ba', obj: 't2'},
             {utt: 'ki ma', obj: 't1'}]))

console.log("respective ratios of 'ki' to 'kima' (increasing is good)")
print(getRatio(S('t1', [{utt: 'ki ma', obj: 't1'}])))
print(getRatio(S('t1', [{utt: 'ki ma', obj: 't1'},
                        {utt: 'fu ba', obj: 't2'}])))
print(getRatio(S('t1', [{utt: 'ki ma', obj: 't1'},
                        {utt: 'fu ba', obj: 't2'},
                        {utt: 'ki ma', obj: 't1'}])))
}
~~~~
