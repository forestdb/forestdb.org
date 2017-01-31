---
layout: model
title: Conventions
model-language: webppl
model-language-version: v0.9.7
---

Why do Americans drive on the right side of the road while the British drive on the left? Why do English speakers use 'cat' to refer to a pet that goes 'meow' while the French use the word `chat'? These conventions or norms govern much of our everyday behavior. While there is a substantial literature using simple agent-based models showing how these arbitrary but stable patterns can emerge in large populations, there has been comparatively less work on the cognitive underpinnings of conventions.

Here, we focus on a classic case of conventionalization of language in a reference game (Clark & Wilkes-Gibbs, 1986). In the simplest version of this task, two players are presented with an array of objects constructed from tangrams which are not easily describable. One of them is designated as the 'target' for the speaker, and their goal is to produce an utterance that will allow the listener to distinguish the correct object from their array. 

Our model of conventionalization combines two innovations in modeling language understanding: lexical uncertainty and a noisy communication channel. The former introduces uncertainty over the exact meanings of words in the lexicon; the latter introduces a perceptual noise model by which utterances can be corrupted during transmission. We gradually build up to this combined model by tackling sub-problems.

### Part 1: arbitrary mapping

We begin by implementing the simplest lexical uncertainty model, used in Bergen, Levy, & Goodman (2016) to account for M-implicatures. In the simplified case we consider, there are just two labels and two tangrams. How does the pair converge on a mapping?

This is the simplest demonstration of conventions; even though neither party knows the meaning of a label at the outset, a random choice is taken to be evidence for a particular lexicon and it becomes the base for successful communication.

~~~~
///fold:
var getTrajectories = function(data) {
  var keys = _.keys(data[0]);
  return reduce(function(key, memo) {
    var timeBasedKeys = map(function(i) {return key + "." + i}, _.range(data.length));
    var vals = _.map(data, key);
    return extend(_.zipObject(timeBasedKeys, vals), memo)
  }, [], keys)
};
///

// set up speaker optimality & number of iterations
var params = {
  alpha : 5,
  beta : 1,
  numSteps : 6
};

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
  return _.zipObject(utterances, meanings);
});

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

// pragmatic speaker 
var S1 = cache(function(state, lexicon) {
  return Infer({method:"enumerate"}, function(){
    var utt = sample(utterancePrior);
    factor(params.alpha * (L0(utt, lexicon).score(state))
           - params.beta * uttCost(utt));
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
        observe(S1(datum.response, lexicon), datum.utt);
      else if(originAgent === 'S') 
        observe(L1(datum.utt, lexicon), datum.response);
    });
    return lexicon;
  });
});

// conventional listener (L1, marginalizing over lexicons)
var L = cache(function(utt, data) {
  return Infer({method:"enumerate"}, function(){
    var lexicon = sample(lexiconPosterior('L', data));
    var state = sample(L1(utt, lexicon));
    return state;
  });
});

// conventional speaker (S1, reasoning about expected L1 behavior across lexicons)
var S = cache(function(state, data) {
  return Infer({method:"enumerate"}, function(){
    var utt = sample(utterancePrior);
    var listener = Infer({method: 'enumerate'}, function() {
      var lexicon = sample(lexiconPosterior('S', data));
      return sample(L1(utt, lexicon))
    });
    factor(params.alpha * listener.score(state)
           - params.beta * uttCost(utt));
    return utt;
  });
});

var model = function() {
  var step = function(data) {
    if(data.length > params.numSteps) return getTrajectories(data);
    var state = sample(statePrior);
    var utt = sample(S(state, data));
    var response = sample(L(utt, data));
    var newDatum = {utt, response, intended: state, acc: state == response};
    return step(data.concat(newDatum));
  };
  step([]);
};

model();
~~~~

The listener is initially uncertain about which tangram 'label1' is referring to, but after observing evidence in which a speaker produced 'label1' for '1', they update their beliefs about the likely lexicon and subsequently become more likely to interpret 'label1' as referring to 't1.' Note that it is also more likely to interpret 'label2' as referring to 't2', even though it has not observed any explicit usage of this label. This latter effect is a standard consequence of pragmatic reasoning. Uncomment for additional explorations of this model, and examine what the lexicon posterior looks like after seeing different data.

### Part 2: Dropping redundant information (conjunctions and modifiers)

In our data, several of the most frequently dropped utterances between the first round and the last round are modifying clauses. We account for this in an RSA model by enriching the lexicon with compositional semantics. In addition to bare labels, the speaker can form conjunctions of labels which have a meaning derived from the individual label meanings in the lexicon. We also introduce some initial bias for some labels toward one tangrams and other labels toward the other tangram. The intended behavior is that speakers are initially more likely to form conjunctions of the two labels with a bias toward the target tangram, in effect hedging against the possibility that they're in a world where one or the other of those labels don't turn out to mean the target tangram. As uncertainty over the lexicon decreases over multiple rounds, however, this information will become redundant and the benefit of hedging will not be worth the additional utterance cost. 

~~~~
///fold:
var initList = function(n, val) {
  return repeat(n, function() {return val})
}

var uniformPs = function(vs) {
  return initList(vs.length, 1/vs.length)
}

var getTrajectories = function(data) {
  var keys = _.keys(data[0]);
  return reduce(function(key, memo) {
    var timeBasedKeys = map(function(i) {return key + "." + i}, _.range(data.length));
    var vals = _.map(data, key);
    return extend(_.zipObject(timeBasedKeys, vals), memo)
  }, [], keys)
};
///

// speaker optimality
var params = {
  alpha : 5,
  beta : 1,
  numSteps : 6
};

// possible states of the world
var states = [{type: 0, color: 0}, 
              {type: 1, color: 1}];
var statePrior =  Categorical({vs: states, ps: [1/2, 1/2]});

// possible base utterances, and possible conjunctions
var unconstrainedUtterances = ['type_a', 'type_b', 'color_a','color_b'];
var derivedUtterances = ['type_a type_b', 'type_a color_a','type_a color_b',
                         'type_b color_a','type_b color_b','color_a color_b'];
var utterances = unconstrainedUtterances.concat(derivedUtterances);
var utterancePrior = Categorical({vs: utterances, ps: uniformPs(utterances)});

// takes a sample from a (biased & discretized) dirichlet distribution for each word,
// representing the extent to which that word describes each object
var lexiconPrior = Infer({method: 'enumerate'}, function(){
  var meanings = map(function(utt) {
    var aBias = utt.split('_')[1] === 'a'
    var ps = aBias ? [.3,.25,.2,.15,.1] : [.1,.15,.2,.25,.3];
    return categorical({vs: [0.01, 0.25, .5, .75, .99], ps})
  }, unconstrainedUtterances);
  return _.zipObject(unconstrainedUtterances, meanings);
});

// length-based cost
var uttCost = function(utt) {
  return utt.split(' ').length;
};

// Looks up the meaning of an utterance in a lexicon object
var uttFitness = cache(function(utt, state, lexicon) {
  return Math.log(reduce(function(subUtt, memo) {
    var relevantProperty = (subUtt.split('_')[0] === 'type' ?
                            state.type : state.color);
    var lexiconProb = relevantProperty ? lexicon[subUtt] : 1- lexicon[subUtt]
    return lexiconProb * memo;
  }, 1, utt.split(' ')));
});


// literal listener
var L0 = cache(function(utt, lexicon) {
  return Infer({method:"enumerate"}, function(){
    var state = sample(statePrior);
    factor(uttFitness(utt, state, lexicon));
    return state;
  });
});

// pragmatic speaker
var S1 = cache(function(state, lexicon) {
  return Infer({method:"enumerate"}, function(){
    var utt = sample(utterancePrior);
    factor(params.alpha * L0(utt, lexicon).score(state)
           - params.beta * uttCost(utt));
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

var lexiconPosterior = cache(function(originAgent, data) {
  return Infer({method: 'enumerate'}, function() {
    var lexicon = sample(lexiconPrior);
    mapData({data: data}, function(datum){
      if(originAgent === 'L') {
        observe(S1(datum.response, lexicon), datum.utt);
      } else if(originAgent === 'S') {
        observe(L1(datum.utt, lexicon), datum.response);
      }
    });
    return lexicon;
  });
});

// conventional listener (L1, marginalizing over lexicons)
var L = cache(function(utt, data) {
  return Infer({method:"enumerate"}, function(){
    var lexicon = sample(lexiconPosterior('L', data));
    var state = sample(L1(utt, lexicon));
    return state;
  });
});

// conventional speaker (S1, reasoning about expected listener across lexicons)
var S = cache(function(state, data) {
  return Infer({method:"enumerate"}, function(){
    var utt = sample(utterancePrior);
    var listener = Infer({method: 'enumerate'}, function() {
      var lexicon = sample(lexiconPosterior('S', data));
      return sample(L1(utt, lexicon));
    });
    factor(params.alpha * listener.score(state)
           - params.beta * uttCost(utt));
    return utt;
  });
});

var model = function() {
  var step = function(data) {
    if(data.length > params.numSteps) return getTrajectories(data);
    var state = states[0]
    var utt = sample(S(state, data));
    var response = sample(L(utt, data));
    var newDatum = {utt, response, intended: state, acc: state == response};
    return step(data.concat(newDatum));
  };
  step([]);
};

model();
~~~~

We see that there's an initial preference for the longer conjunction of "type_a" and "color_a" despite the utterance cost because

(1) there's an initial bias in the lexicon prior for 'a' utterances to correspond to properties with the value 0 (thus why neither of the 'b' utterances are assigned any probability)

(2) the conjunction hedges against unlikely but possible lexicons where one or the other utterance actually corresponds to a property with value 1. 

After observing an example of this conjunction referring to the first state, the conjunction actually becomes *more* preferred by the speaker, as it increases the probability of utterances where both type_a and color_a mean the first state. The evidence is indeterminate about the separate meanings of type_a and color_a. Because of cost considerations, however, the shorter utterances are still assigned some probability. Once the speaker produces one or the other short utterances by chance, then, it breaks the symmetry and this shorter utterance becomes the most probable in future rounds.

<!-- 
### Part 3: Shortening arbitrary utterances

In Clark & Wilkes-Gibbs (1986), one signature phenomenon is the shortening of referring expressions over time. On the first round, a speaker may refer to a tangram as "the one with their arms out front, like an ice skater," which becomes "ice skater" by the final round. To account for this qualitative phenomenon, we add a noisy communication channel to the above lexical uncertainty model & show (very weakly) that under certain parameters, the shorter utterance becomes *more likely* relative to the longer utterance as the number of observations increases.

To demonstrate this most clearly, and to explore the effect of different choices in the noise model, we briefly turn to a simpler example, where the pair of possible labels have two morphemes: "kima" and "fuba". The noisy channel can *delete* a morpheme (e.g. "kima" -> "ki"), or replace a morpheme (e.g. "kima" -> "kiba").

~~~~
///fold:
var possibleWords = ['the', 'ki', 'ma'];

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
var grammaticalUtts = ['the ma', 'the ki'];
var intentionallyCorruptedUtts = ['the', 'ma', 'ki', 'ma ki', 'ki ma'];
var intendedUtts = grammaticalUtts.concat(intentionallyCorruptedUtts)
var utterancePrior = Categorical({vs: grammaticalUtts, ps: uniformPs(grammaticalUtts)});

// takes a sample from a (discretized) dirichlet distribution for each word,
// representing the extent to which that word describes each object
var lexiconPrior = Infer({method: 'enumerate'}, function(){
  var meanings = map(function(utt) {
    var t1Ps = (utt === 'the ki' ? [.1,.15,.2,.25,.3] : 
                utt === 'the ma' ? [.3,.25,.2,.15,.1] :
                [.2, .2, .2, .2, .2]);
    var t1Prob = categorical({vs: [0.01, 0.25, .5, .75, .99], ps: t1Ps})
    return {'t1' : t1Prob, 't2' : 1-t1Prob};
  }, grammaticalUtts);
  return _.object(grammaticalUtts, meanings);
});

// length-based cost 
var uttCost = function(utt) {
  return utt.split(' ').length > 1 ? 1.2 : 1;
};

var params = {
  alpha: 12,
  noiseRate: 0.01,
  maxDepth: 2
}

// Recursively edit string to maxDepth (log prob proportional to levenstein distance)
var transform = function(words, currDepth) {
  if(flip(1 - params.noiseRate) || currDepth > params.maxDepth) {
    return _.isEmpty(words) ? [''] : words;
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
// Note that the -100 is a hacks to make it well-formed after capping recursion:
// a corruption of a corruption may not be reachable from a corruption of a grammatical utt,
// and it's possible that none of the reachable meanings are true of any of the states
var L0 = cache(function(utt, lexicon) {
  return Infer({method:"enumerate"}, function(){
    var state = sample(statePrior);
    var intendedUtt = sample(utterancePrior)

    var noiseScore = (_.contains(noiseModel(intendedUtt).support(), utt) ?
                      noiseModel(intendedUtt).score(utt) :
                      -100)
    factor(Math.log(lexicon[intendedUtt][state]) + noiseScore);
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

    factor(params.alpha * listener.score(state) 
           - uttCost(intendedUtt));
    return sample(noiseModel(intendedUtt));
  });
});

// pragmatic listener (needed for S)
var L1 = cache(function(perceivedUtt, lexicon) {
  return Infer({method: 'enumerate'}, function() {
    var state = sample(statePrior);
    observe(S1(state, lexicon), perceivedUtt);
    return state;
  });
});

var lexiconPosterior = cache(function(originAgent, data) {
  return Infer({method: 'enumerate'}, function() {
    var lexicon = sample(lexiconPrior);
    mapData({data: data}, function(datum){
      if(originAgent === 'L') {
        observe(S1(datum.obj, lexicon), datum.utt);
      } else if(originAgent === 'S') {
        observe(L1(datum.utt, lexicon), datum.obj);
      }
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

// conventional speaker
var S = function(state, data) {
  return Infer({method:"enumerate"}, function(){
    var intendedUtt = uniformDraw(intendedUtts);
    var listener = Infer({method: 'enumerate'}, function(){
      var lexicon = sample(lexiconPosterior('S', data));
      var corruptedUtt = sample(noiseModel(intendedUtt))
      return sample(L1(corruptedUtt, lexicon))
    })
    
    factor(params.alpha * listener.score(state) - uttCost(intendedUtt));
    return intendedUtt;
  });
};

// viz.marginals(lexiconPosterior('S', []))
// viz.marginals(lexiconPosterior('S', [{obj: 't1', utt: 'the ki'},
//                        {obj: 't2', utt: 'the ma'}]))
viz(S('t1', []))
viz(S('t1', [{obj: 't1', utt: 'the ki'},
             {obj: 't2', utt: 'the ma'}]))

// console.log("respective ratios of 'ki' to 'kima' (increasing is good)")
// print(getRatio(S('t1', [{utt: 'ki ma', obj: 't1'}])))
// print(getRatio(S('t1', [{utt: 'ki ma', obj: 't1'},
//                         {utt: 'fu ba', obj: 't2'}])))
// print(getRatio(S('t1', [{utt: 'ki ma', obj: 't1'},
//                         {utt: 'fu ba', obj: 't2'},
//                         {utt: 'ki ma', obj: 't1'}])))
~~~~
-->