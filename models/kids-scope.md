---
layout: model
title: kids-scope
model-language: webppl
---

A model of scope ambiguity resolution by KJ Savinelli, Greg Scontras, and Lisa Pearl:

~~~~
////Most Recent Model (As of 6/1/2017)

// wrapper function so the global variables can be set up before each run
var runModel = function(utterances, numParticipants, baseRate, 
                         scopes, QUDs, QUDProbs, scopePriorDist,
                         pragSpeakerState){

  var utterancePrior = function() {
    uniformDraw(utterances)
  }

  /////// world state functions /////////////
  // recursive function that flips based on base rate to determine which
  //    individuals were successful
  var makeSuccesses = function(numParticipants, baseRate){
    // if numParticipants == 1, return the empty set or [numParticipants]
    // based on flip
    if(numParticipants == 1){
      if(flip(baseRate)){
        return [numParticipants]
      }else{
        return []
      }
    }// flip this one and concatenate it to results of doing so for one participant less
    else{
      if(flip(baseRate)){
        // success for this participant
        return sort([numParticipants].concat(makeSuccesses(numParticipants-1, baseRate)))
      }else{
        return makeSuccesses(numParticipants-1, baseRate)
      }
    }
  }

  // sanity check for making states
  //print(makeSuccesses(4,0.5))

  // getInverseHelper
  // input: non-empty list (ex: successes = [1,2])
  //        and non-empty list of individuals in world (ex: [1,2,3,4])
  // removes each success individual from individuals list until no more successes
  var getInverseHelper = function(toFind, individuals){
    // if toFind.length == 1, remove that element from individuals and then return
    if(toFind.length == 1){
      return remove(toFind[0], individuals);
    }
    // else remove first element of toFind from individuals and from toFind
    //   and then recurse on new smaller toFind and new smaller individuals
    else{
      return getInverseHelper(remove(toFind[0], toFind),  // toFind w/o first elem
                              remove(toFind[0], individuals)) // individuals w/o toFind[0]
    }
  }

  // main function for getting inverse of list, based on numParticipants in world
  // used for getting failures from list of successes
  var getInverse = function(successes, numParticipants){
    // explicitly list out each participant with a number (e.g., [1,2,3,4])
    var participants = mapN(function(x) { return x + 1; }, numParticipants);
    // check first to see if successes.length == 0; if so, inverse = whole list
    if(successes.length == 0){
      return participants;
    }
    // else check to see if successes.length == numParticipants; if so, inverse = empty
    else if(successes.length == numParticipants){
      return [];
    }else{
      // else call helper function to get inverse individuals
      return getInverseHelper(successes, participants);
    }
  }

  //// sanity check for getInverse
  // print("getInverse on successes [1] for numParticipants = " + numParticipants);
  // print(getInverse([1], numParticipants))


  // state builder: needs to know 
  //                (1) how many particants (numParticipants), 
  //                (2) base rate of success (baseRate)
  // returns: states = {successes: [list of success individuals], 
  //                    failures: [list of failure individuals]}
  var statePrior = function(numParticipants, baseRate) {
    // always return the collection -- can get length alone later on if needed
    //return makeSuccesses(numParticipants, baseRate);
    var successes = makeSuccesses(numParticipants, baseRate);
    return {
      successes: successes,
      failures: getInverse(successes, numParticipants)
    }
  }

  // sanity check on state prior
  //print(statePrior(4, 0.5))
  //////////// end world state generation

  /////////// scopePrior: possible scopes /////////
  var scopePrior = function(){ 
    // based on scopePriorDist
    return categorical({vs: scopes, 
                        ps: scopePriorDist}); 
  }
  ////////////////////////


  ///// helper function for unit-based meaning function calculation
  // takes two lists (unit and state) and determines if all the elements
  // of the first list (unit) are in the second list (state)
  // --> go through each element in first list and find it in second list
  // returns boolean

  /////////// meaning function used by literal listener ///////
  // meaning function: include numParticipants var, 
  // since this impacts the surface scope reading calculation
  // for units: need makeUnits boolean and unit to search for
  var meaning = function(utterance, state, scope, 
                          numParticipants) {
    if(utterance == "two-not"){
      if(scope == "surface"){ //there are two who didn't
        // Update so doing verification on failure state
        if(exactMeaning){
          // looking for exactly two failures
          return (state["failures"].length == 2);
        }else{
          //   Specifically, looking for failures >= 2 individuals
          return (state["failures"].length >= 2); 
        }
      }else{ // it's not true that two did
        // verification on successes
        if(exactMeaning){
          return (state["successes"].length != 2);
        }else{
          //  < 2 individuals
          return (state["successes"].length < 2)
        }
      }
    }else{
      // null utterance: fall back on prior 
      return true;
    }
  };

  // testing the meaning function
  var test_utt = "two-not";
  var test_state = {
    successes: [3,4], 
    failures: [1,2]
  };
  var test_scope = "surface";
  var test_NP = 4;
  var b = meaning(test_utt, test_state, test_scope, test_NP)
  print(b)
  //////// end meaning function used by literal listener //////////


  ///////////// QUDs ////////////////////
  // "how many?", "all?", "none?", "this unit?", "not this unit?"
  // TO DO: adjust QUD prior to be based on makeUnits value
  var QUDPrior = function() {
    return categorical({vs: QUDs, 
                        ps: QUDProbs});
  }


  // sanity check on the QUDprior
  // print("QUDs are: ")
  // print(QUDs);
  // print("Draw one when makeUnits = true");
  // print(QUDPrior(true));

  // "how many?", "all?", "none?", "this unit?", "not this unit?"
  var QUDFun = function(QUD,state,numParticipants) {
    if(QUD == "all?"){ // boolean
      return state["successes"].length == numParticipants;
    }else if(QUD == "none?"){ // boolean
      return state["successes"].length == 0;
    }else if(QUD == "how many?"){ // number successes
      return state["successes"].length;
    }
  };

  // QUD sanity check
  //print(QUDFun("this unit?", {"successes": [1,2,4], "failures": [3]}, 4, [1,2]))
  ///////// end QUD stuff /////////////

  // Literal listener (L0)
  var literalListener = cache(function(utterance,scope,QUD) {
    Infer({model: function(){
      var state = statePrior(numParticipants, baseRate);
      //print("literalListener, state = " + state)
      // if makeUnits, need to draw a unit and feed that to the qState
      var qState = QUDFun(QUD,state, numParticipants);
      //print("literalListener, qState = " + qState)
      //print("literalListener, meaning = " + meaning(utterance,state,scope,
      //                 numParticipants,makeUnits,[]))
      condition(meaning(utterance,state,scope,
                        numParticipants));
      return qState;
    }
          });
  });

  // sanity check for literal listener
  print("Literal Listener:")
  viz.auto(literalListener("two-not", "surface", "how many?"));

  var alpha_S1 = 2.5

  // Speaker (S)
  var speaker = cache(function(scope,state,QUD) {
    Infer({model: function(){
      var utterance = utterancePrior();
      var qState = QUDFun(QUD,state,numParticipants);
      factor(alpha_S1 * literalListener(utterance,scope,QUD).score(qState));
      return utterance;        
    }
          });
  });

  // sanity check for speaker S1
  print("Speaker:")
  viz.auto(speaker("surface", {"successes":[3,4],"failures":[1,2]}, "how many?"))


  // Pragmatic listener (L1)
  // get utterance, and infer everything else:
  //   scope, state, QUD, makeUnits, unit
  var pragmaticListener = cache(function(utterance) {
    Infer({model: function(){
      var scope = scopePrior();
      // numParticipants and baseRate of success globally set
      var state = statePrior(numParticipants, baseRate); 
      // get makeUnits
      var QUD = QUDPrior(); // QUDs depend on whether makeUnits
      // if makeUnits, unit sampled; 
      factor(speaker(scope,state,QUD).score(utterance));
      //return {state: state.length, makeUnits: makeUnits};
      return state;
    }
          });
  });

  // sanity check for pragmaticListener
  viz(pragmaticListener("two-not"))

  var alpha_S2 = 1 // optimality of pragmatic speaker

  // Pragmatic speaker (S2)
  // takes in state, and decides whether to endorse utterance,
  //   given all possibilities of scope, state, QUD, makeUnits, and unit
  var pragmaticSpeaker = cache(function(state) {
    Infer({model: function(){
      var utterance = utterancePrior();
      factor(alpha_S2 * pragmaticListener(utterance).score(state))
      return utterance
    }})
  })

  // now run pragmatic speaker
  print("Pragmatic Speaker:")
  print("num of participants: " + numParticipants)
  print("in world with successes = " + pragSpeakerState["successes"])
  print(" and failures = " + pragSpeakerState["failures"])
  pragmaticSpeaker(pragSpeakerState)

} // end runModel wrapper function

//// Global variables for runModel
var utterances = ["null","two-not"];
var exactMeaning = false;

//World Manip
var numParticipants = 4; // e.g., total frogs
var baseRate = 0.5; // success rate

//Scope Manip
var scopes = ["surface", "inverse"];
var scopePriorDist = [.5, .5]

//QUD Manip
var QUDs = ["how many?", "all?","none?"];
var QUDProbs = [1, 0, 0];


// now run the model with whatever state we want, given the variables above
var pragSpeaker2State = {
  successes: [1],
  failures: [2]
}

var pragSpeaker4State = {
  successes: [1,2],
  failures: [3,4]
}

////Change pragSpeakerState given 2 vs 4 frogs
// print("baseRate of success: " + baseRate);
// print("scope prior distribution [surface, inverse]: " + scopePriorDist)
// print("exact meaning? " + exactMeaning);
runModel(utterances, numParticipants, baseRate,
         scopes, QUDs, QUDProbs, scopePriorDist,
         pragSpeaker4State);
         
~~~~         
