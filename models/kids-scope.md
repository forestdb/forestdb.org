---
layout: model
title: kids-scope
model-language: webppl
---

**A model of scope ambiguity resolution for *every-not* and *two-not* utterances.**

The basic *every-not* model:

~~~~
// helper function to tally up the state
var numTrue = function(state) {
  var fun = function(x) {
    x ? 1 : 0
  }
  return sum(map(fun,state))
}

// possible utterances
var utterances = ["null","every-not"];
var utterancePrior = function() {
  uniformDraw(utterances)
}
// uniform utterance cost
var cost = function(utterance) {
  return 1
}

// possible world states
var numHorses = 2
var states = [0,1,2]
var baserate = 0.5 // change to manipulate prior on world states
var stateMaker = function(numHorses,stateSoFar) {
  if (numHorses == 0) {
    return stateSoFar
  } else {
    var newHorse = flip(baserate)
    var newState = stateSoFar.concat([newHorse])
    return stateMaker(numHorses - 1, newState)
  }
}
var statePrior = function() {
  return numTrue(stateMaker(numHorses,[]))
}

// possible scope interpretations
var scopes = ["surface", "inverse"]
var scopePrior = function(){ 
  return categorical([.5,.5],scopes) // change to manipulate prior on scope interpretations
}


// meaning function
var meaning = function(utterance, state, scope) {
  return utterance == "every-not" ? 
    scope == "surface" ? state == 0 :
  state < numHorses : 
  true;
};

// possible QUDs
var QUDs = ["how many?","all red?","none red?"];
var QUDPrior = function() {
  uniformDraw(QUDs);
  //   categorical([.05,.05,.9],QUDs) // change to manipulate prior on QUDs
}
var QUDFun = function(QUD,state) {
  QUD == "all red?" ? state == numHorses :
  QUD == "none red?" ? state == 0 :
  state;
};

// Literal listener (L0)
var literalListener = cache(function(utterance,scope,QUD) {
  Infer({model: function(){
    var state = uniformDraw(states);
    var qState = QUDFun(QUD,state)
    condition(meaning(utterance,state,scope));
    return qState;
  }});
});

var alpha = 1

// Speaker (S1)
var speaker = cache(function(scope, state, QUD) {
  return Infer({model: function(){
    var utterance = utterancePrior()
    var qState = QUDFun(QUD, state)
    factor(alpha*(literalListener(utterance,scope,QUD).score(qState) 
                  - cost(utterance)))
    return utterance
  }})
})

// Pragmatic listener (L1)
var pragmaticListener = cache(function(utterance) {
  Infer({model: function(){
    var state = statePrior();
    var scope = scopePrior();
    var QUD = QUDPrior();
    observe(speaker(scope,state,QUD),utterance);
    return state
  }});
});

// Pragmatic speaker (S2)
var pragmaticSpeaker = cache(function(state) {
  Infer({model: function(){
    var utterance = utterancePrior();
    factor(pragmaticListener(utterance).score(state))
    return utterance
  }})
})

// A speaker decides whether to endorse the ambiguous utterance as a 
// description of the not-all world state
display(pragmaticSpeaker(1))
~~~~

The basic *two-not* model for 1-of-2 contexts:

~~~~
// tally up the state
var numTrue = function(state) {
  var fun = function(x) {
    x ? 1 : 0
  }
  return sum(map(fun,state))
}

// possible utterances
var utterances = ["null","two-not"];
var utterancePrior = function() {
  uniformDraw(utterances)
}
// uniform utterance cost
var cost = function(utterance) {
  return 1
}

// possible world states
var numHorses = 2
var states = [0,1,2]
var baserate = 0.5 // change to manipulate prior on world states
var stateMaker = function(numHorses,stateSoFar) {
  if (numHorses == 0) {
    return stateSoFar
  } else {
    var newHorse = flip(baserate)
    var newState = stateSoFar.concat([newHorse])
    return stateMaker(numHorses - 1, newState)
  }
}
var statePrior = function() {
  return numTrue(stateMaker(numHorses,[]))
}

// possible scope interpretations
var scopes = ["surface", "inverse"]
var scopePrior = function(){ 
  return categorical([.5,.5],scopes) // change to manipulate prior on scope interpretations
}

// meaning function
var meaning = function(utterance, state, scope) {
  if (utterance == "two-not") {
    if (scope == "surface") {
      return state == 0
    } else {
      return state < numHorses
    }
  } else {
    return true;
  }
};

// possible QUDs
var QUDs = ["many?","all?","none?"];
var QUDPrior = function() {
  uniformDraw(QUDs);
  //   categorical([.05,.9,.05],QUDs) // change to manipulate prior on QUDs
}
var QUDFun = function(QUD,state) {
  QUD == "all?" ? state == numHorses :
  QUD == "none?" ? state == 0 :
  state;
};

// Literal listener (L0)
var literalListener = cache(function(utterance,scope,QUD) {
  Infer({model: function(){
    var state = uniformDraw(states);
    var qState = QUDFun(QUD,state)
    condition(meaning(utterance,state,scope));
    return qState;
  }});
});

var alpha = 1

// Speaker (S1)
var speaker = cache(function(scope, state, QUD) {
  return Infer({model: function(){
    var utterance = utterancePrior()
    var qState = QUDFun(QUD, state)
    factor(alpha*(literalListener(utterance,scope,QUD).score(qState) 
                  - cost(utterance)))
    return utterance
  }})
})

// Pragmatic listener (L1)
var pragmaticListener = cache(function(utterance) {
  Infer({model: function(){
    var state = statePrior();
    var scope = scopePrior();
    var QUD = QUDPrior();
    observe(speaker(scope,state,QUD),utterance);
    return state
  }});
});

// Pragmatic speaker (S2)
var pragmaticSpeaker = cache(function(state) {
  Infer({model: function(){
    var utterance = utterancePrior();
    factor(pragmaticListener(utterance).score(state))
    return utterance
  }})
})

// A speaker decides whether to endorse the ambiguous utterance as a 
// description of the not-all world state
display(pragmaticSpeaker(1))
~~~~

The "exact" *two-not* model for 2-of-4 contexts:

~~~~
// tally up the state
var numTrue = function(state) {
  var fun = function(x) {
    x ? 1 : 0
  }
  return sum(map(fun,state))
}

// possible utterances
var utterances = ["null","two-not"];
var utterancePrior = function() {
  uniformDraw(utterances)
}
// uniform utterance cost
var cost = function(utterance) {
  return 1
}

// possible world states
var numHorses = 4
var states = [0,1,2,3,4]
var baserate = 0.5 // change to manipulate prior on world states
var stateMaker = function(numHorses,stateSoFar) {
  if (numHorses == 0) {
    return stateSoFar
  } else {
    var newHorse = flip(baserate)
    var newState = stateSoFar.concat([newHorse])
    return stateMaker(numHorses - 1, newState)
  }
}
var statePrior = function() {
  return numTrue(stateMaker(numHorses,[]))
}


// possible scope interpretations
var scopes = ["surface", "inverse"]
var scopePrior = function(){ 
  return categorical([.5,.5],scopes) // change to manipulate prior on scope interpretations
}



// meaning function (exact semantics)
var meaning = function(utterance, state, scope) {
  if (utterance == "two-not") {
    if (scope == "surface") {
      return state == 2
    } else {
      return state != 2
    }
  } else {
    return true;
  }
};

// possible QUDs
var QUDs = ["many?","all?","none?","two?"];
var QUDPrior = function() {
  uniformDraw(QUDs);
  //   categorical([.033,0.033,0.033,0.9],QUDs) // change to manipulate prior on QUDs
}
var QUDFun = function(QUD,state) {
  QUD == "all?" ? state == numHorses :
  QUD == "none?" ? state == 0 :
  QUD == "two?" ? state == 2 :
  state;
};

// Literal listener (L0)
var literalListener = cache(function(utterance,scope,QUD) {
  Infer({model: function(){
    var state = uniformDraw(states);
    var qState = QUDFun(QUD,state)
    condition(meaning(utterance,state,scope));
    return qState;
  }});
});

var alpha = 1

// Speaker (S1)
var speaker = cache(function(scope, state, QUD) {
  return Infer({model: function(){
    var utterance = utterancePrior()
    var qState = QUDFun(QUD, state)
    factor(alpha*(literalListener(utterance,scope,QUD).score(qState) 
                  - cost(utterance)))
    return utterance
  }})
})

// Pragmatic listener (L1)
var pragmaticListener = cache(function(utterance) {
  Infer({model: function(){
    var state = statePrior();
    var scope = scopePrior();
    var QUD = QUDPrior();
    observe(speaker(scope,state,QUD),utterance);
    return state
  }});
});

// Pragmatic speaker (S2)
var pragmaticSpeaker = cache(function(state) {
  Infer({model: function(){
    var utterance = utterancePrior();
    factor(pragmaticListener(utterance).score(state))
    return utterance
  }})
})

// A speaker decides whether to endorse the ambiguous utterance as a 
// description of the not-all world state
display(pragmaticSpeaker(2))
~~~~

The "at least" *two-not* model for 2-of-4 contexts:

~~~~
// tally up the state
var numTrue = function(state) {
  var fun = function(x) {
    x ? 1 : 0
  }
  return sum(map(fun,state))
}

// possible utterances
var utterances = ["null","two-not"];
var utterancePrior = function() {
  uniformDraw(utterances)
}
// uniform utterance costs
var cost = function(utterance) {
  return 1
}

// possible world states
var numHorses = 4
var states = [0,1,2,3,4]
var baserate = 0.5 // change to manipulate prior on world states
var stateMaker = function(numHorses,stateSoFar) {
  if (numHorses == 0) {
    return stateSoFar
  } else {
    var newHorse = flip(baserate)
    var newState = stateSoFar.concat([newHorse])
    return stateMaker(numHorses - 1, newState)
  }
}
var statePrior = function() {
  return numTrue(stateMaker(numHorses,[]))
}


// possible scopes
var scopes = ["surface", "inverse"]
var scopePrior = function(){ 
  return categorical([.5,.5],scopes) // change to manipulate prior on scope interpretations
}

// meaning function (at-least semantics)
var meaning = function(utterance, state, scope) {
  if (utterance == "two-not") {
    if (scope == "surface") {
      return state < 3
    } else {
      return state < 2
    }
  } else {
    return true;
  }
};

// QUDs
var QUDs = ["many?","all?","none?","two?"];
var QUDPrior = function() {
  uniformDraw(QUDs);
  //   categorical([.9,0.033,0.033,0.033],QUDs) // change to manipulate prior on QUDs
}
var QUDFun = function(QUD,state) {
  QUD == "all?" ? state == numHorses :
  QUD == "none?" ? state == 0 :
  QUD == "two?" ? state > 1 :
  state;
};

// Literal listener (L0)
var literalListener = cache(function(utterance,scope,QUD) {
  Infer({model: function(){
    var state = uniformDraw(states);
    var qState = QUDFun(QUD,state)
    condition(meaning(utterance,state,scope));
    return qState;
  }});
});

var alpha = 1

// Speaker (S)
var speaker = cache(function(scope, state, QUD) {
  return Infer({model: function(){
    var utterance = utterancePrior()
    var qState = QUDFun(QUD, state)
    factor(alpha*(literalListener(utterance,scope,QUD).score(qState) 
                  - cost(utterance)))
    return utterance
  }})
})

// Pragmatic listener (L1)
var pragmaticListener = cache(function(utterance) {
  Infer({model: function(){
    var state = statePrior();
    var scope = scopePrior();
    var QUD = QUDPrior();
    observe(speaker(scope,state,QUD),utterance);
    return state
  }});
});

// Pragmatic speaker (S2)
var pragmaticSpeaker = cache(function(state) {
  Infer({model: function(){
    var utterance = utterancePrior();
    factor(pragmaticListener(utterance).score(state))
    return utterance
  }})
})

// A speaker decides whether to endorse the ambiguous utterance as a 
// description of the not-all world state
display(pragmaticSpeaker(2))
~~~~