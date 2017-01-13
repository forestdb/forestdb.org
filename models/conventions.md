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
// possible states of the world
var states = ['t1', 't2'];
var statePrior =  Categorical({vs: states, ps: [1/2, 1/2]});

// possible utterances
var utterances = ['label1', 'label2'];
var utterancePrior = Categorical({vs: utterances, ps: [1/2, 1/2]});

// takes a sample from a (discretized) dirichlet distribution for each word,
// representing the extent to which that word describes each object
var lexiconPrior = Infer({method: 'enumerate'}, function(){
  var meanings = map(function(utt) {
    var t1Prob = uniformDraw([0.01, .25, .5, .75, 0.99]);
    return {'t1' : t1Prob, 't2' : 1-t1Prob};
  }, utterances);
  return _.object(utterances, meanings);
});

// speaker optimality
var alpha = 1;

// length-based cost (although they're all the same length here)
var uttCost = function(utt) {
  return utt.split(' ').length;
};

// literal listener (using real-valued lexicon)
var L0 = cache(function(utt, lexicon) {
  return Infer({method:"enumerate"}, function(){
    var state = sample(statePrior);
    factor(Math.log(lexicon[utt][state]));
    return state;
  });
});

// pragmatic speaker (opti
var S1 = cache(function(state, lexicon) {
  return Infer({method:"enumerate"}, function(){
    var utt = sample(utterancePrior);
    factor(alpha * (L0(utt, lexicon).score(state)
                    - uttCost(utt)));
    return utt;
  });
});

// conventional listener
var L1 = cache(function(utt, lexicon) {
  return Infer({method:"enumerate"}, function(){
    var state = sample(statePrior);
    observe(S1(state, lexicon), utt);
    return state;
  });
});

// compute lexicon posterior, taking into account some previous observations
// speakers do this by assuming data came from knowledgable listener, and vice versa
var lexiconPosterior = cache(function(originAgent, data) {
  return Infer({method: 'enumerate'}, function() {
    var lexicon = sample(lexiconPrior);
    mapData({data: data}, function(datum){
      if(originAgent === 'L') 
        observe(S1(datum.obj, lexicon), datum.utt);
      else if(originAgent === 'S') 
        observe(L1(datum.utt, lexicon), datum.obj);
    });
    return lexicon;
  });
});

// conventional listener (L1, marginalizing over lexicons)
var L = function(utt, data) {
  return Infer({method:"enumerate"}, function(){
    var lexicon = sample(lexiconPosterior('L', data));
    var state = sample(L1(utt, lexicon));
    return state;
  });
};

// conventional speaker (S1, reasoning about expected L1 behavior across lexicons)
var S = function(state, data) {
  return Infer({method:"enumerate"}, function(){
    var utt = sample(utterancePrior);
    var listener = Infer({method: 'enumerate'}, function() {
      var lexicon = sample(lexiconPosterior('S', data));
      return sample(L1(utt, lexicon))
    });
    factor(alpha * (listener.score(state) - uttCost(utt)));
    return utt;
  });
};

console.log("initial listener interpretation (first trial)");
viz(L('label1', []));

console.log("listener hearing label1 after data:");
viz(L('label1', [{utt: 'label1', obj: 't1'}]));

// console.log("listener hearing label2 after data:");
// viz(L('label2', [{utt: 'label1', obj: 't1'}]));

// console.log("listener hearing label1 after opposite data:");
// viz(L('label1', [{utt: 'label1', obj: 't2'}]));

// console.log("listener hearing label1 after more data:");
// viz(L('label1', [{utt: 'label1', obj: 't1'},
// 			 {utt: 'label1', obj: 't1'},
// 			 {utt: 'label2', obj: 't2'}]));

// console.log("speaker hearing label1 after more data:");
// viz(S('t1', [{utt: 'label1', obj: 't1'},
// 			 {utt: 'label1', obj: 't1'},
// 			 {utt: 'label2', obj: 't2'}]));
~~~~

The listener is initially uncertain about which tangram 'label1' is referring to, but after observing evidence in which a speaker produced 'label1' for 'tangram1', they update their beliefs about the likely lexicon and subsequently become more likely to interpret 'label1' as referring to 'tangram1.' Note that it is also more likely to interpret 'label2' as referring to 'tangram2', even though it has not observed any explicit usage of this label. This latter effect is a standard consequence of pragmatic reasoning. Uncomment for additional explorations of this model, and examine what the lexicon posterior looks like after seeing different data.

### Part 2: Dropping redundant information (conjunctions and modifiers)

In our data, several of the most frequently dropped utterances between the first round and the last round are modifying clauses. We account for this in an RSA model by enriching the lexicon with compositional semantics. In addition to bare labels, the speaker can form conjunctions of labels which have a meaning derived from the individual label meanings in the lexicon. We also introduce some initial bias for some labels toward one tangrams and other labels toward the other tangram. The intended behavior is that speakers are initially more likely to form conjunctions of the two labels with a bias toward the target tangram, in effect hedging against the possibility that they're in a world where one or the other of those labels don't turn out to mean the target tangram. As uncertainty over the lexicon decreases over multiple rounds, however, this information will become redundant and the benefit of hedging will not be worth the additional utterance cost. 

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
var constructAnyMeaning = function(label) {
  return function(trueState) {
    return any(function(labelState){
      return labelState == trueState;
    }, label.split('|'));
  }
};
var conjunction = function(meanings) {
  return function(trueState) {
    return all(function(meaning) {
      return meaning(trueState);
    }, meanings);
  }
}
var nullMeaning = function(x) {return true;};

var initList = function(n, val) {
  return repeat(n, function() {return val})
}

var uniformPs = function(vs) {
  return initList(vs.length, 1/vs.length)
}
///

// possible states of the world
var states = ['t1', 't2'];
var statePrior =  Categorical({vs: states, ps: [1/2, 1/2]});

// possible utterances (include null utterance to make sure dists are well-formed)
var unconstrainedUtterances = ['t1_a', 't1_b','t2_a','t2_b'];
var derivedUtterances = ['t1_a and t1_b', 't1_a and t2_a', 't1_a and t2_b',
                         't1_b and t2_a', 't1_b and t2_b', 't2_a and t2_b',
                         'n0'];
var utterances = unconstrainedUtterances.concat(derivedUtterances);
var utterancePrior = Categorical({vs: utterances, ps: uniformPs(utterances)});

// meanings are possible disjunctions of states
var meanings = map(function(l){return l.join('|');}, 
                   powerset(states, {noNull: true}));

var lexiconPrior = Infer({method: 'enumerate'}, function(){
  var meaningSet = map(function(utt) {
    var meaningProbs = {
      't1' : utt.split('_')[0] === 't1' ? 1/3 + 1/5 : 1/3 - 1/5,
      't2' : utt.split('_')[0] === 't1' ? 1/3 - 1/5 : 1/3 + 1/5,
      't1|t2' : 1/3
    }
    return categorical({vs: _.keys(meaningProbs), ps: _.values(meaningProbs)});
  }, unconstrainedUtterances)  
  return _.object(unconstrainedUtterances, meaningSet);
})

// speaker optimality
var alpha = 1;

// null utterance costly; everything else cheap
var uttCost = function(utt) {
  return (utt == 'n0' ? 10 : utt.split(' ').length/3);
};

// Looks up the meaning of an utterance in a lexicon object
var meaning = cache(function(utt, lexicon) {  
  if(utt === 'n0') {
    return nullMeaning;
  } else if (_.contains(unconstrainedUtterances, utt)) {
    return constructAnyMeaning(lexicon[utt]);
  } else {
    var baseMeanings = map(function(baseUtt) {
      return constructAnyMeaning(lexicon[baseUtt])
    }, utt.split(' and '));
    return conjunction(baseMeanings)
  }
});

// literal listener
var L0 = function(utt, lexicon) {
  return Infer({method:"enumerate"}, function(){
    var state = sample(statePrior);
    var uttMeaning = meaning(utt, lexicon);
    factor(uttMeaning(state) ? 0 : -100);
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

var L2 = cache(function(perceivedUtt, lexicon) {
  return Infer({method: 'enumerate'}, function() {
    var state = sample(statePrior);
    observe(S1(state, lexicon), perceivedUtt);
    return state;
  });
});

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
    var utt = sample(utterancePrior);
    var lexicon = sample(lexiconPrior);
    
    factor(alpha * (L2(utt, lexicon).score(state)
                    - uttCost(utt)));
    mapData({data: data}, function(datum){
      observe(L2(datum.utt, lexicon), datum.obj); // update beliefs about lexicon
    });
    return {utt, t1a: lexicon['t1_a'], t2a: lexicon['t2_a']};
  });
};

viz.marginals(S('t1', []))
viz.marginals(S('t1', [{utt: 't1_a', obj: 't1'}]))
viz.marginals(S('t1', [{utt: 't1_a', obj: 't1'},
                       {utt: 't1_b', obj: 't1'}]))
~~~~

### Part 3: Shortening arbitrary utterances

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

var possibleWords = ['ki', 'ma', 'fu', 'ba'];

var deleteWords = function(words) {
  if(_.isEmpty(words)) {
    return words;
  } else {
    var wordToOmit = uniformDraw(words);
    return remove(wordToOmit,words);
  }
};

var insertWords = function(words) {
  var insertLoc = randomInteger(words.length + 1);
  var insertWord = uniformDraw(possibleWords);
  return (words.slice(0,insertLoc)
         .concat(insertWord)
         .concat(words.slice(insertLoc, words.length)));
};

var replaceWords = function(words) {
  if(_.isEmpty(words)) {
    return words;
  } else {
    var replaceLoc = randomInteger(words.length);
    var replaceWord = uniformDraw(possibleWords);
    return (words.slice(0,replaceLoc)
            .concat(replaceWord)
            .concat(words.slice(replaceLoc+1,words.length)));
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

var params = {
  alpha: 1,
  noiseRate: 0.01,
  maxDepth: 1
}

// Recursively edit string to maxDepth (log prob proportional to levenstein distance)
var transform = function(words, currDepth) {
  if(flip(1 - params.noiseRate) || currDepth > params.maxDepth) {
    return _.isEmpty(words) ? ['n0'] : words;
  } else {
    var operations = [deleteWords, insertWords, replaceWords];
    var op = uniformDraw(operations);
    return transform(op(words), currDepth + 1);
  }
};

// Gives distribution over possible noisy versions of intended utt
var noiseModel = cache(function(utt) {
  return Infer({method: 'enumerate'}, function() {
    return (utt === 'n0' ? 'n0' :
            transform(utt.split(' '), 0).join(' '));
  });
});

// literal listener w/ noisy channel inference
// Note that the -100s are hacks to make it well-formed after capping recursion:
// a corruption of a corruption may not be reachable from a corruption of a grammatical utt,
// and it's possible that none of the reachable meanings are true of any of the states
var L0 = cache(function(utt, lexicon) {
  return Infer({method:"enumerate"}, function(){
    var state = sample(statePrior);
    var intendedUtt = sample(utterancePrior)

    var uttMeaning = meaning(intendedUtt, lexicon);
    var noiseScore = (_.contains(noiseModel(intendedUtt).support(), utt) ?
                      noiseModel(intendedUtt).score(utt) :
                      -100)
    factor(uttMeaning(state) ? 
           noiseScore : -100);
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
    return sample(noiseModel(intendedUtt));
  });
});

// S1('t2', {"ki ma":"t1|t2","fu ba":"t1|t2","n0":"null"})
// pragmatic listener (needed for S)
var L2 = cache(function(perceivedUtt, lexicon) {
  return Infer({method: 'enumerate'}, function() {
    var state = sample(statePrior);
    observe(S1(state, lexicon), perceivedUtt);
    return state;
  });
});

// conventional listener
var L = cache(function(perceivedUtt, data) {
  return Infer({method:"enumerate"}, function(){
    var state = sample(statePrior);
    var lexicon = sample(lexiconPrior);
    observe(S1(state, lexicon), perceivedUtt);
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
    return sample(noiseModel(intendedUtt));
  });
};

// Listener is better able to understand 'ki' if they've observed full utterances
// than if they've only heard snippets
// console.log(L('ki', [{utt:'ki ma', obj:'t1'},{utt:'fu ba', obj:'t2'}]));
// console.log(L('ki', [{utt:'ki', obj:'t1'},{utt:'fu', obj:'t2'}]));
// console.log('speaker with no data ([])')
// viz(S('t1', []))
// console.log("speaker who observed 'kima'<=>'t1'")
// viz(S('t1', [{utt: 'ki ma', obj: 't1'}]));
// console.log("speaker who observed 'kima'<=>'t1' + 'fuba'<=>'t2'")  
// viz(S('t1', [{utt: 'ki ma', obj: 't1'},
//              {utt: 'fu ba', obj: 't2'}]))
// console.log("speaker who observed 2x 'kima'<=>'t1' + 1x'fuba'<=>'t2'")
// viz(S('t1', [{utt: 'ki ma', obj: 't1'},
//              {utt: 'fu ba', obj: 't2'},
//              {utt: 'ki ma', obj: 't1'}]))

console.log("respective ratios of 'ki' to 'kima' (increasing is good)")
print(getRatio(S('t1', [{utt: 'ki ma', obj: 't1'}])))
print(getRatio(S('t1', [{utt: 'ki ma', obj: 't1'},
                        {utt: 'fu ba', obj: 't2'}])))
print(getRatio(S('t1', [{utt: 'ki ma', obj: 't1'},
                        {utt: 'fu ba', obj: 't2'},
                        {utt: 'ki ma', obj: 't1'}])))
~~~~
