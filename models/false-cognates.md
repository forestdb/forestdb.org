---
layout: model
title: McNabb, Tran, Vargas Fuentes, You False cognates
model-language: webppl
---

##Empirical Phenomenon of Interest 

        Being a speaker of more than one language gives rise to a unique set of problems. One of them being the confusion that comes with false cognates (Roca-Varela, 2015). We will explore how the competence of a speaker in a language impacts the pragmatic listener's interpretation of an incorrect utterance with a false cognate.

        Cognates are words of two different languages that are similar in spelling and meaning (Richards & Schmidt, 2013). Cognates can range from being spelled the exact same way to only using the same root word and usually, the similarity in spelling takes place in the beginning of the words. The word “doctor” is used in both English and Spanish to describe a medical physician. The Spanish word “actividad” and the English word “activity” are also cognates. The two words have the same first six letters and have the same meaning. When learning a new language, cognates can be very helpful. They help connect two languages together, making vocabulary acquisition simpler. However, every so often, a learner will come across a false cognate, also known as a false friend. 

        False cognates are words of two different languages that have similar spelling but do not hold the same meaning (Otwinowska & Szewczyk, 2017). When a non-fluent person looks at false cognates, they might immediately assume that they have the same meaning because of the similarities in their spelling. The English word “embarrassed” and the Spanish word “embarazada” are a false cognate pair. They have five consecutive letters that are the same but carry completely different definitions. When someone feels embarrassed, they feel self-conscious or ashamed. When someone is embarazada, they are pregnant. These false cognates often betray people that are learning a second language.
 
        The model that we wanted to see came from instances of confusion with the use of these false cognates. The scenario would arise in a highschool context. The speaker is feeling embarrassed because they tripped in front of their crush. They then exclaim to their Spanish speaking friend “I was so embarazada”. The speaker is either a speaker with a high or low competency in Spanish. Their utterance is a sentence with the incorrect use of a word that is a part of a false cognate pair. The pragmatic listener will then take the competence of the speaker and interpret whether the speaker intended the utterance or was tricked by a false friend. 

##Modeling Approach: Rational Speech Act (RSA) Model
        The Rational Speech Act Model (Frank & Goodman 2012) is a framework that simulates human behavior in making communicative inferences. The RSA model provides a measure of human behavior through probabilities and stays under the assumption that language understanding comes from cooperativity. How the model demonstrates cooperativity is how listeners and speakers in the model have prior beliefs and make inferences based on shared prior beliefs.  Although there appear to be two roles during communication with a speaker and a listener, the RSA model contains at least three layers to represent the layers of reasoning behind communication. By exploring these layers, the RSA model allows us to visualize in probability distributions how informativity affects human pragmatic inference. In this review, we will be going over only three layers of reasoning in the RSA model: the literal listener (L0), the pragmatic speaker (S1), and the pragmatic listener (L1). 

        The first layer of reasoning in the RSA model has the literal listener and the literal listener represents a listener who interprets an utterance based on its meaning. In the standard format for the literal listener function, the function takes in an utterance, makes a uniform draw over objects or states, and finds the meaning of the utterance to make an inference about the object or state the utterance is conveying. A uniform draw over objects or states picks a random object or state from the list of possible objects or states and reflects how this literal listener assigns equal probability to every possible object or state when considering the meaning of an utterance. From the literal listener function, we can visualize a probability distribution over objects or states the literal listener finds possible based on the meaning of the utterance.

        The second layer of reasoning in the RSA model has the pragmatic speaker and the pragmatic speaker is a speaker who chooses an utterance to best describe the object or state to a listener. The pragmatic speaker takes their knowledge of the state, reasons about what would be the most concise and informative utterance that they know, and makes an inference about an utterance that would best describe the state. In the original format for the pragmatic speaker function, the function takes in the actual state, chooses an utterance from the utterancePrior, finds the most informative and concise utterance by considering alpha and the utterance cost, and returns an utterance. The utterancePrior is a function that picks an utterance from a list of possible utterances generated from the pragmatic speaker’s prior beliefs and alpha is the optimality parameter that can maximize or decrease the utility of an utterance. From the pragmatic speaker function, we can visualize a probability distribution over utterances the pragmatic speaker finds possible to describe the actual object or state.

        The third layer of reasoning is the pragmatic listener and the pragmatic listener is a listener who reasons about the speaker’s choice of utterance to infer about the actual object or state. When making an inference about the actual object or state, the pragmatic listener takes into account the speaker’s utterance and reasons about what possible objects or states the speaker is trying to convey in their utterance. We can see the pragmatic listener’s reasoning in its original format in the RSA model. The pragmatic listener function takes in an utterance from the pragmatic speaker, chooses an object or state from their objectPrior or statePrior, observes what utterance the speaker is using to describe the object or state, and returns an inference about what the actual object or state is. The objectPrior or statePrior are functions that pick an object or state from a list of possible objects or states generated from the pragmatic listener’s prior beliefs. Hence, from the pragmatic listener function, we can visualize a probability distribution over the states the pragmatic listener finds possible that the speaker is describing with their utterance. 

        From developing an understanding behind how each of these layers of reasoning interact and overlap, we have constructed a new model based on our prior beliefs and the RSA model.

##Comparing our model with others

        Models are at the mercy of the linguistic scenario they are born from. Our model follows the basic structure of the original RSA framework with a literal listener, speaker, and pragmatic listener, but because our chosen scenario involves translation into another language, and inference on a unique competence variable, aspects of our model look similar to more advanced editions of the RSA framework or completely depart from what has been seen thus far. 

        Beginning with the setup of our model, our statePrior(), makes use of a weighted categorical draw in order to reflect a real world scenario where certain states of being are more likely than others. The details of our states will be explained later. See our statePrior() below:

~~~~
var states = 
   [
  {pregnant : 1, embarrassed : 0},
  {pregnant : 0, embarrassed : 1 }
] 

var statePrior = function() {
  return categorical ({ps: [40,60], vs: states})
}
~~~~

This weighted draw method is similar to the Non-literal language model (Kao et al., 2014):

~~~~
var states = ['terrible', 'ok', 'amazing']

var statePrior = function() {
  categorical([1, 50, 50], states)
}
~~~~

And the Vagueness model (Lassiter & Goodman, 2013):

~~~~
var book = {
  "prices": [2, 6, 10, 14, 18, 22, 26, 30],
  "probabilities": [1, 2, 3, 4, 4, 3, 2, 1]
};

var statePrior = function() {
  return categorical(book.probabilities, book.prices);
};
~~~~

Continuing on, because our model deals with two languages, we have an additional translate() function that translates English utterances into Spanish. This function is entirely unique and incomparable to any previous models we have seen. The details of how it operates will be explained in detail later, but view the function below:

~~~~
var translate = function(utterance, competence){
     if (utterance == "embarrassed" & competence == "high") 
   {return "avergonzada"; }
  if (utterance == "embarrassed" & competence == "low") 
    {return similarity("embarazada", "embarrassed") > similarity("avergonzada", "embarrassed")
    ? "embarazada" : "avergonzada";}
  if (utterance == "pregnant" & competence == "high") 
   {return "embarazada"; }
  if (utterance == "pregnant" & competence == "low") 
   {return similarity("embarazada", "embarrassed") > similarity("avergonzada", "embarrassed")
    ? "embarazada" : "avergonzada"; }
}
~~~~

translate() is called at the level of the speaker, changing how the speaker would ordinarily function in the original RSA framework. Usually, the RSA speaker reasons about the literal listener's interpretations of utterances and then generates a best-fit utterance for a given state.Comparatively, our speaker does this same thing, but then goes on to translate that best-fit utterance into Spanish. See how our speaker calls the translate() function below:

~~~~
var speaker = function(state, competence) {
  Infer({model: function() {
    var utterance = utterancePrior()
       factor(alpha*(literalListener(utterance).score(state) 
                  - cost(utterance)))
    return translate(utterance,competence)
  }})
}
~~~~

Additionally, our translate() function works by calling other unique function we have implemented such as the chunkMaker() function and the similarity() function:

~~~~
var chunkMaker = function(nChunksLeft,word,chunksSoFar) {
  var chunksSoFar = chunksSoFar == undefined ? [] : chunksSoFar
  if (nChunksLeft == 0) {
    return chunksSoFar
  } else {
    var newChunk = word[nChunksLeft-1][0]+word[nChunksLeft-1][1]
    var newChunksSoFar = chunksSoFar.concat([newChunk])
    return chunkMaker(nChunksLeft - 1,word,newChunksSoFar)
  }
}
var chunker = function(utterance) {
  var splitUtterance = utterance.split("")
  var chunk_org = _.chunk(utterance, 2)
  var chunk_1 = _.chunk(_.toArray(utterance).slice(1), 2)
  var chunks = _.union(chunk_org, chunk_1)
  var length = chunks.length
  return chunkMaker(length,chunks)
  }

var similarity = function(word1, word2) {
  return ((_.intersection(chunker(word1),chunker(word2))).length*2) /
    (chunker(word1).length + chunker(word2).length)
}
~~~~

We will go into the details of the chunkMaker() function seen above later, but it should be noted that its structure is based off of  Chapter six's plural prediction model (Scontras & Goodman 2017) in the way that it calls itself inside its own function. The code from Chapter six we modeled this off of can be seen here:

~~~~
var statePrior = function(nObjLeft,stateSoFar) {
  var stateSoFar = stateSoFar == undefined ? [] : stateSoFar
  if (nObjLeft == 0) {
    return stateSoFar
  } else {
    var newObj = objectPrior()
    var newState = stateSoFar.concat([newObj])
    return statePrior(nObjLeft - 1,newState)
  }
}
~~~~

Our model has another unique variable called competence. Again, the details of competence will be explained later, however as a general overview, it is an additional variable that actors within our RSA framework must reason over. There have been previous models where the speaker considers more than one variable such as in the Jointly inferring parameters and interpretations model (Savinelli et al. 2017):

~~~~
var literalListener = cache(function(utterance,scope) {
  return Infer({model: function(){
    var state = uniformDraw(states)
    condition(meaning(utterance,state,scope))
    return state
  }})
})

var alpha = 1

// Speaker (S)
var speaker = cache(function(scope,state) {
  return Infer({model: function(){
    var utterance = utterancePrior()
    factor(alpha*(literalListener(utterance,scope).score(state) 
                  - cost(utterance)))
    return utterance
  }})
})
~~~~

The above model reasons over the additional variable scope however our model is still unique because the effects of this competence variable is not seen in our literal listener or meaning function because, like stated before, the competence variable is not introduced to our model until the level of the speaker. See our meaning and literal listener functions compared to the Jointly inferring parameters and interpretations model (Savinelli et al. 2017) meaning and literal listener functions below:

~~~~
// Our model:

//meaning 
var meaning = function(state, utterance){
   return utterance == "pregnant" ? state.pregnant == 1 : state.embarrassed == 1
};

// literal listener
var literalListener = cache(function(utterance) {
  return Infer({model: function(){
    var state = statePrior()
    condition(meaning(state, utterance))
    return state
  }})
})

Savinelli et al.:

// meaning function
var meaning = function(utterance, state, scope) {
  return utterance == "every-not" ? 
    scope == "surface" ? state == 0 :
  state < 3 : 
  true
}

// Literal listener (L0)
var literalListener = cache(function(utterance, scope) {
  Infer({model: function(){
    var state = uniformDraw(states)
    condition(meaning(utterance,state,scope))
    return state
  }})
})
~~~~

Similar to the Jointly inferring parameters and interpretations model (Savinelli et al. 2017) though, our additional competence variable is  'lifted' so that it will be reasoned on by the pragmatic listener:

~~~~
//Pragmatic Listener inferring about competence
var pragmaticListener = cache(function(utterance) {
  return Infer({model: function(){
    var state = statePrior()
    var competence = uniformDraw(["high", "low"])
    observe(speaker(state, competence), utterance)
    return {state: state, competence: competence}
  }})
})
~~~~

Compare this to the pragmatic listener from the Jointly inferring parameters and interpretations model (Savinelli et al. 2017) who reasons about the lifted scope variable:

~~~~
// Pragmatic listener (L1)
var pragmaticListener = cache(function(utterance) {
  Infer({model: function(){
    var state = statePrior();
    var scope = scopePrior();
    observe(speaker(scope,state),utterance);
    return {state: state,
            scope: scope}
  }});
});
~~~~

##The current model: False Friends Model
        	Our model deals with the following scenario. One of our friends is telling us a story in Spanish, their second language, about the time they fell down the stairs in front of their crush. The friend ends the story saying “Estaba bien embarazada” (I was pregnant). Given the story, it is very unlikely that our friend intended to say they were pregnant. Since our friend is not a native speaker of Spanish, it is very likely they made a mistake. The Spanish word ‘embarazada’ looks very similar to the English word ‘embarrassed’. These words are an example of false cognates, and second language learners tend to misuse them in a sentence (Roca-Varela, 2015). Thus, once we hear our friend use the word ‘embarazada’ in an unlikely context, we must infer that they fell into the trap of false friends and interpret the sentence given the English word.

        	To construct our model, we first established the necessary components to build our RSA model. Our model works with two possible states: either the state of the world is that our friend is pregnant or they are embarrassed. The states are written as structured objects that have two properties (pregnant or embarrassed) and a numeric value of 1 or 0 to signify whether the state is pregnant ({pregnant: 1, embarrassed: 0}) or embarrassed ({pregnant: 0, embarrassed: 1}). To implement our crush story context, we used categorical in the statePrior function and made the prior likelihood of the embarrassed state higher than the pregnant state.

        	For our possible set of utterances, the speaker can intend to mean either ‘pregnant’ or ‘embarrassed’. Both utterances are as likely to be uttered, which we implemented by using uniformDraw in the utterancePrior, and they are equally as costly (see cost function). Finally, we created a meaning function which takes in an utterance and a state. If the utterance is ‘pregnant’ it will only return true when the property of pregnant is 1 and if the utterance is not ‘pregnant’ (meaning ‘embarrassed’) it will only return true if the property of embarrassed is 1. With these components in place, we can create the first layer of our RSA model: the literal listener.

        	The literal listener function takes in an utterance and it will return a probability distribution over states. As other RSA models, the literal listener samples a state from the statePrior and runs it through the meaning function with the utterance given in the argument. It is important to note that the literal listener is reasoning over the literal interpretation of English utterances. Below is the code for the previous components up to the literal listener.

~~~~
var states =
   [
  {pregnant : 1, embarrassed : 0},
  {pregnant : 0, embarrassed : 1 }
]
 
// context high school friend
var statePrior = function() {
  return categorical ({ps: [40,60], vs: states})
}
 
//possible English utterances
var utterances = ["pregnant", "embarrassed"]
 
// utterancePrior
var utterancePrior = function (){
   return uniformDraw(utterances)
}
 
// cost function for utterances
var cost = function(utterance) {
  return 0;
};
 
//meaning
var meaning = function(state, utterance){
   return utterance == "pregnant" ? state.pregnant == 1 : state.embarrassed == 1
};
 
// literal listener
var literalListener = cache(function(utterance) {
  return Infer({model: function(){
	var state = statePrior()
	condition(meaning(state, utterance))
	return state
  }})
})
print("Pregnant")
viz(literalListener("pregnant"))

print("Embarrassed")
viz(literalListener("embarrassed"))
~~~~

        	Now that we have our literal listener layer, we move on to our speaker. At this layer, the speaker will return a probability distribution over English utterances reasoning of whether the literal listener will arrive at that state given an utterance while minimizing the cost. Ultimately, those utterances will be translated to Spanish. Our speaker takes in two arguments. First, it knows the state of the world which they wish to communicate. Second, they know whether they have high or low competence in Spanish. This ‘competence’ variable will be resolved at the layer of the pragmatic listener, but it is an important component when the speaker translates the utterance to Spanish.

        	The translate function takes in two arguments: the English utterance (‘pregnant’ or ‘embarrassed’) and the competence of the speaker (‘high’ or ‘low’). If the competence is high, the translate function will always return the correct Spanish translation (‘pregnant’ = ‘embarazada’; ‘embarrassed’ = ‘avergonzada’). However, if the speaker’s competence is low, the translation function will return the Spanish word whose orthography is more similar with the English utterance. This means that they are likely to fall into the trap of false friends. The way that we measure orthographic similarity is using the DICE index (Inkpen, Frunza, & Kondrak, 2005). DICE bases similarity by looking at the sequence of bigrams shared between two words. The number of intersected bigrams is multiplied by 2 and divided by the total number of bigrams in the two words.

                            DICE (x,y) = 2|bigrams(x)bigrams(y)||bigrams(x)|+|bigrams(y)|

        	To implement this measure, we created three functions (chunkmaker, chunker and similarity). The first two functions create the bigrams needed to use the similarity measure. The chunker takes in a word, splits it into individual letters and then creates bigrams. However, the bigrams created by the chunker function have the following format ([“e”, “m”]) which is not ideal to run the intersection tool in the similarity function. To fix this problem, those bigrams are run through the chunkmaker function which ultimately returns all the bigrams in the following format ([“em”]). Finally, the similarity function takes in two words and runs them through the chuker function. Then it follows the DICE equation described above. The similarity function returns a numeric value between 0 (no similarity) and 1 (identical).

~~~~
var states =
   [
  {pregnant : 1, embarrassed : 0},
  {pregnant : 0, embarrassed : 1 }
]
 
// context high school friend
var statePrior = function() {
  return categorical ({ps: [40,60], vs: states})
}
 
//possible English utterances
var utterances = ["pregnant", "embarrassed"]
 
// utterancePrior
var utterancePrior = function (){
   return uniformDraw(utterances)
}
 
// cost function for utterances
var cost = function(utterance) {
  return 0;
};
 
//meaning
var meaning = function(state, utterance){
   return utterance == "pregnant" ? state.pregnant == 1 : state.embarrassed == 1
};
 
//competence
var competence  = ["high", "low"]
             	
// set speaker optimality
var alpha = 1
 
var chunkMaker = function(nChunksLeft,word,chunksSoFar) {
  var chunksSoFar = chunksSoFar == undefined ? [] : chunksSoFar
  if (nChunksLeft == 0) {
	return chunksSoFar
  } else {
	var newChunk = word[nChunksLeft-1][0]+word[nChunksLeft-1][1]
	var newChunksSoFar = chunksSoFar.concat([newChunk])
	return chunkMaker(nChunksLeft - 1,word,newChunksSoFar)
  }
}
var chunker = function(utterance) {
  var splitUtterance = utterance.split("")
  var chunk_org = _.chunk(utterance, 2)
  var chunk_1 = _.chunk(_.toArray(utterance).slice(1), 2)
  var chunks = _.union(chunk_org, chunk_1)
  var length = chunks.length
  return chunkMaker(length,chunks)
  }
 
var similarity = function(word1, word2) {
  return ((_.intersection(chunker(word1),chunker(word2))).length*2) /
	(chunker(word1).length + chunker(word2).length)
}
 
//translate
var translate = function(utterance, competence){
 	if (utterance == "embarrassed" & competence == "high")
   {return "avergonzada"; }
  if (utterance == "embarrassed" & competence == "low")
	{return similarity("embarazada", "embarrassed") > similarity("avergonzada", "embarrassed")
	? "embarazada" : "avergonzada";}
  if (utterance == "pregnant" & competence == "high")
   {return "embarazada"; }
  if (utterance == "pregnant" & competence == "low")
   {return similarity("avergonzada", "pregnant") > similarity( "embarazada", "pregnant")
	? "avergonzada" : "embarazada"; }
}
 
// literal listener
var literalListener = cache(function(utterance) {
  return Infer({model: function(){
    var state = statePrior()
    condition(meaning(state, utterance))
    return state
  }})
})

// Speaker model
var speaker = function(state, competence) {
  Infer({model: function() {
    var utterance = utterancePrior()
       factor(alpha*(literalListener(utterance).score(state) 
                  - cost(utterance)))
    return translate(utterance,competence)
  }})
}

print("speaker wants to say embarrassed and they have high competence in Spanish")
viz(speaker( {pregnant : 0, embarrassed : 1}, "high"))
print("speaker wants to say embarrassed and they have low competence in Spanish")
viz(speaker( {pregnant : 0, embarrassed : 1}, "low"))
~~~~

        	Having constructed our literal listener and speaker, we can build the final layer of our RSA model: our pragmatic listener. The pragmatic listener takes in the translation of the English utterance and returns a probability distribution of the state and the competence of the speaker. To do this, the speaker samples a state from the statePrior and a competence from a uniformDraw over the list [“high”, “low”]. They reason about how likely the speaker would have said that Spanish translation given the sampled state and competence. Below is the complete model with all the components previously described.

~~~~
var states =
   [
  {pregnant : 1, embarrassed : 0},
  {pregnant : 0, embarrassed : 1 }
]
 
// context high school friend
var statePrior = function() {
  return categorical ({ps: [40,60], vs: states})
}
 
//possible English utterances
var utterances = ["pregnant", "embarrassed"]
 
// utterancePrior
var utterancePrior = function (){
   return uniformDraw(utterances)
}
 
// cost function for utterances
var cost = function(utterance) {
  return 0;
};
 
//meaning
var meaning = function(state, utterance){
   return utterance == "pregnant" ? state.pregnant == 1 : state.embarrassed == 1
};
 
//competence
var competence  = ["high", "low"]
             	
// set speaker optimality
var alpha = 1
 
var chunkMaker = function(nChunksLeft,word,chunksSoFar) {
  var chunksSoFar = chunksSoFar == undefined ? [] : chunksSoFar
  if (nChunksLeft == 0) {
	return chunksSoFar
  } else {
	var newChunk = word[nChunksLeft-1][0]+word[nChunksLeft-1][1]
	var newChunksSoFar = chunksSoFar.concat([newChunk])
	return chunkMaker(nChunksLeft - 1,word,newChunksSoFar)
  }
}
var chunker = function(utterance) {
  var splitUtterance = utterance.split("")
  var chunk_org = _.chunk(utterance, 2)
  var chunk_1 = _.chunk(_.toArray(utterance).slice(1), 2)
  var chunks = _.union(chunk_org, chunk_1)
  var length = chunks.length
  return chunkMaker(length,chunks)
  }
 
var similarity = function(word1, word2) {
  return ((_.intersection(chunker(word1),chunker(word2))).length*2) /
	(chunker(word1).length + chunker(word2).length)
}
 
//translate
var translate = function(utterance, competence){
 	if (utterance == "embarrassed" & competence == "high")
   {return "avergonzada"; }
  if (utterance == "embarrassed" & competence == "low")
	{return similarity("embarazada", "embarrassed") > similarity("avergonzada", "embarrassed")
	? "embarazada" : "avergonzada";}
  if (utterance == "pregnant" & competence == "high")
   {return "embarazada"; }
  if (utterance == "pregnant" & competence == "low")
   {return similarity("avergonzada", "pregnant") > similarity( "embarazada", "pregnant")
	? "avergonzada" : "embarazada"; }
}
 
// literal listener
var literalListener = cache(function(utterance) {
  return Infer({model: function(){
	var state = statePrior()
	condition(meaning(state, utterance))
	return state
  }})
})
 
// Speaker model
var speaker = function(state, competence) {
  Infer({model: function() {
	var utterance = utterancePrior()
       factor(alpha*(literalListener(utterance).score(state)
              	- cost(utterance)))
	return translate(utterance,competence)
  }})
}
 
//Pragmatic Listener inferring about competence
var pragmaticListener = cache(function(translation) {
  return Infer({model: function(){
	var state = statePrior()
	var competence = uniformDraw(["high", "low"])
	observe(speaker(state, competence), translation)
	return {state: state, competence: competence}
  }})
})
 
 viz(pragmaticListener("embarazada"))
~~~~

##Results

        	When we run the pragmaticListener over the Spanish word “embarazada”, the listener reasons two different scenarios depending on the speaker’s competence in Spanish. If the competence is ‘high’, the listener believes the speaker meant to indicate they were actually pregnant. In this case, the intuition is that a highly competent Spanish speaker is unlikely to fall into the false friend’s trap. Meanwhile, if the speaker has low competence, the pragmatic listener believes it is more likely the speaker meant to say they are embarrassed (which is congruent with the false friend’s mistake). However, they still assign some probability to the literal interpretation of the Spanish translation (pregnant).

        	When we call the function of pragmaticListener on the Spanish word ‘avergonzada’, the listener only believes the speaker intends the literal interpretation of ‘embarrassed’(code box below). There is also no consideration of the scenario where the speaker has low competence. Given that ‘avergonzada’ and ‘pregnant’ are not a pair of false cognates, there is not a scenario where the two will be confused in the translation. Thus, based on the way the model is built, the pragmatic listener has no reason to believe the speaker has low competence and will mean the non-literal interpretation (pregnant).

~~~~
///fold:
var states =
   [
  {pregnant : 1, embarrassed : 0},
  {pregnant : 0, embarrassed : 1 }
]
 
// context high school friend
var statePrior = function() {
  return categorical ({ps: [40,60], vs: states})
}
 
//possible English utterances
var utterances = ["pregnant", "embarrassed"]
 
// utterancePrior
var utterancePrior = function (){
   return uniformDraw(utterances)
}
 
// cost function for utterances
var cost = function(utterance) {
  return 0;
};
 
//meaning
var meaning = function(state, utterance){
   return utterance == "pregnant" ? state.pregnant == 1 : state.embarrassed == 1
};
 
//competence
var competence  = ["high", "low"]
             	
// set speaker optimality
var alpha = 1
 
var chunkMaker = function(nChunksLeft,word,chunksSoFar) {
  var chunksSoFar = chunksSoFar == undefined ? [] : chunksSoFar
  if (nChunksLeft == 0) {
	return chunksSoFar
  } else {
	var newChunk = word[nChunksLeft-1][0]+word[nChunksLeft-1][1]
	var newChunksSoFar = chunksSoFar.concat([newChunk])
	return chunkMaker(nChunksLeft - 1,word,newChunksSoFar)
  }
}
var chunker = function(utterance) {
  var splitUtterance = utterance.split("")
  var chunk_org = _.chunk(utterance, 2)
  var chunk_1 = _.chunk(_.toArray(utterance).slice(1), 2)
  var chunks = _.union(chunk_org, chunk_1)
  var length = chunks.length
  return chunkMaker(length,chunks)
  }
 
var similarity = function(word1, word2) {
  return ((_.intersection(chunker(word1),chunker(word2))).length*2) /
	(chunker(word1).length + chunker(word2).length)
}
 
//translate
var translate = function(utterance, competence){
 	if (utterance == "embarrassed" & competence == "high")
   {return "avergonzada"; }
  if (utterance == "embarrassed" & competence == "low")
	{return similarity("embarazada", "embarrassed") > similarity("avergonzada", "embarrassed")
	? "embarazada" : "avergonzada";}
  if (utterance == "pregnant" & competence == "high")
   {return "embarazada"; }
  if (utterance == "pregnant" & competence == "low")
   {return similarity("avergonzada", "pregnant") > similarity( "embarazada", "pregnant")
	? "avergonzada" : "embarazada"; }
}
 
// literal listener
var literalListener = cache(function(utterance) {
  return Infer({model: function(){
	var state = statePrior()
	condition(meaning(state, utterance))
	return state
  }})
})
 
// Speaker model
var speaker = function(state, competence) {
  Infer({model: function() {
	var utterance = utterancePrior()
       factor(alpha*(literalListener(utterance).score(state)
              	- cost(utterance)))
	return translate(utterance,competence)
  }})
}
 
//Pragmatic Listener inferring about competence
var pragmaticListener = cache(function(translation) {
  return Infer({model: function(){
	var state = statePrior()
	var competence = uniformDraw(["high", "low"])
	observe(speaker(state, competence), translation)
	return {state: state, competence: competence}
  }})
})
///
 viz(pragmaticListener("avergonzada"))
~~~~

        	The current model permits us to see what variables are weighting on the pragmatic listener’s beliefs about the world after hearing the utterance. For example, the prior beliefs about the state play a substantial role on the posterior beliefs. In the original model, the embarrassed state is slightly likelier than the pregnant state (60 > 40). Making the former state even more likelier makes the listener give even more probability to the embarrassed interpretation given the ‘embarazada’ translation as is shown in the following code box.

~~~~
///fold: 
//possible English utterances
var utterances = ["pregnant", "embarrassed"]
 
// utterancePrior
var utterancePrior = function (){
   return uniformDraw(utterances)
}
 
// cost function for utterances
var cost = function(utterance) {
  return 0;
};
 
//meaning
var meaning = function(state, utterance){
   return utterance == "pregnant" ? state.pregnant == 1 : state.embarrassed == 1
};
 
//competence
var competence  = ["high", "low"]
             	
// set speaker optimality
var alpha = 1
 
var chunkMaker = function(nChunksLeft,word,chunksSoFar) {
  var chunksSoFar = chunksSoFar == undefined ? [] : chunksSoFar
  if (nChunksLeft == 0) {
	return chunksSoFar
  } else {
	var newChunk = word[nChunksLeft-1][0]+word[nChunksLeft-1][1]
	var newChunksSoFar = chunksSoFar.concat([newChunk])
	return chunkMaker(nChunksLeft - 1,word,newChunksSoFar)
  }
}
var chunker = function(utterance) {
  var splitUtterance = utterance.split("")
  var chunk_org = _.chunk(utterance, 2)
  var chunk_1 = _.chunk(_.toArray(utterance).slice(1), 2)
  var chunks = _.union(chunk_org, chunk_1)
  var length = chunks.length
  return chunkMaker(length,chunks)
  }
 
var similarity = function(word1, word2) {
  return ((_.intersection(chunker(word1),chunker(word2))).length*2) /
	(chunker(word1).length + chunker(word2).length)
}
 
//translate
var translate = function(utterance, competence){
 	if (utterance == "embarrassed" & competence == "high")
   {return "avergonzada"; }
  if (utterance == "embarrassed" & competence == "low")
	{return similarity("embarazada", "embarrassed") > similarity("avergonzada", "embarrassed")
	? "embarazada" : "avergonzada";}
  if (utterance == "pregnant" & competence == "high")
   {return "embarazada"; }
  if (utterance == "pregnant" & competence == "low")
   {return similarity("avergonzada", "pregnant") > similarity( "embarazada", "pregnant")
	? "avergonzada" : "embarazada"; }
}
/// 
var states =
   [
  {pregnant : 1, embarrassed : 0},
  {pregnant : 0, embarrassed : 1 }
]
 
// context high school friend
var statePrior = function() {
  return categorical ({ps: [10,90], vs: states})
}
// literal listener
var literalListener = cache(function(utterance) {
  return Infer({model: function(){
	var state = statePrior()
	condition(meaning(state, utterance))
	return state
  }})
})
 
// Speaker model
var speaker = function(state, competence) {
  Infer({model: function() {
	var utterance = utterancePrior()
       factor(alpha*(literalListener(utterance).score(state)
              	- cost(utterance)))
	return translate(utterance,competence)
  }})
}
 
//Pragmatic Listener inferring about competence
var pragmaticListener = cache(function(translation) {
  return Infer({model: function(){
	var state = statePrior()
	var competence = uniformDraw(["high", "low"])
	observe(speaker(state, competence), translation)
	return {state: state, competence: competence}
  }})
})
 
 viz(pragmaticListener("embarazada"))
~~~~

        On the other hand, if both states are as likely, then there is no longer a difference between the probability of the two interpretations. The listener still believes the low competence speaker can either mean the literal interpretation (pregnant) or the non-literal interpretation where the speaker fell into the false friend trap (embarrassed). These results suggest that context, as shown in statePrior, is an important variable in this communication scenario. While the listener might consider that a low competence speaker might make a mistake, especially when the word is part of a false cognate pair, context helps disambiguate the intended interpretation.

~~~~
///fold: 
//possible English utterances
var utterances = ["pregnant", "embarrassed"]
 
// utterancePrior
var utterancePrior = function (){
   return uniformDraw(utterances)
}
 
// cost function for utterances
var cost = function(utterance) {
  return 0;
};
 
//meaning
var meaning = function(state, utterance){
   return utterance == "pregnant" ? state.pregnant == 1 : state.embarrassed == 1
};
 
//competence
var competence  = ["high", "low"]
             	
// set speaker optimality
var alpha = 1
 
var chunkMaker = function(nChunksLeft,word,chunksSoFar) {
  var chunksSoFar = chunksSoFar == undefined ? [] : chunksSoFar
  if (nChunksLeft == 0) {
	return chunksSoFar
  } else {
	var newChunk = word[nChunksLeft-1][0]+word[nChunksLeft-1][1]
	var newChunksSoFar = chunksSoFar.concat([newChunk])
	return chunkMaker(nChunksLeft - 1,word,newChunksSoFar)
  }
}
var chunker = function(utterance) {
  var splitUtterance = utterance.split("")
  var chunk_org = _.chunk(utterance, 2)
  var chunk_1 = _.chunk(_.toArray(utterance).slice(1), 2)
  var chunks = _.union(chunk_org, chunk_1)
  var length = chunks.length
  return chunkMaker(length,chunks)
  }
 
var similarity = function(word1, word2) {
  return ((_.intersection(chunker(word1),chunker(word2))).length*2) /
	(chunker(word1).length + chunker(word2).length)
}
 
//translate
var translate = function(utterance, competence){
 	if (utterance == "embarrassed" & competence == "high")
   {return "avergonzada"; }
  if (utterance == "embarrassed" & competence == "low")
	{return similarity("embarazada", "embarrassed") > similarity("avergonzada", "embarrassed")
	? "embarazada" : "avergonzada";}
  if (utterance == "pregnant" & competence == "high")
   {return "embarazada"; }
  if (utterance == "pregnant" & competence == "low")
   {return similarity("avergonzada", "pregnant") > similarity( "embarazada", "pregnant")
	? "avergonzada" : "embarazada"; }
}
/// 
var states =
   [
  {pregnant : 1, embarrassed : 0},
  {pregnant : 0, embarrassed : 1 }
]
 
// no context
var statePrior = function() {
  return uniformDraw (states)
}
// literal listener
var literalListener = cache(function(utterance) {
  return Infer({model: function(){
	var state = statePrior()
	condition(meaning(state, utterance))
	return state
  }})
})
 
// Speaker model
var speaker = function(state, competence) {
  Infer({model: function() {
	var utterance = utterancePrior()
       factor(alpha*(literalListener(utterance).score(state)
              	- cost(utterance)))
	return translate(utterance,competence)
  }})
}
 
//Pragmatic Listener inferring about competence
var pragmaticListener = cache(function(translation) {
  return Infer({model: function(){
	var state = statePrior()
	var competence = uniformDraw(["high", "low"])
	observe(speaker(state, competence), translation)
	return {state: state, competence: competence}
  }})
})
 
 viz(pragmaticListener("embarazada"))
~~~~

        	Another modification we can make to the model is to add a third state where the speaker is both pregnant and embarrassed ({pregnant : 1, embarrassed : 1}). The code box below shows the results of the pragmatic listener reasoning about ‘embarazada’ with the new addition. The pragmatic listener believes that if the speaker has low competence, they are more likely embarrassed but they put as much probability to the states where the speaker is only pregnant or both pregnant and embarrassed. If the listener infers the speaker has high competence in Spanish, they believe the speaker is more likely pregnant but there is a probability assigned to the state where they are both pregnant and embarrassed. This is more likely due to how the meaning function was implemented. The meaning function looks into the individual properties of the structured object states.

~~~~
///fold: 
//possible English utterances
var utterances = ["pregnant", "embarrassed"]
 
// utterancePrior
var utterancePrior = function (){
   return uniformDraw(utterances)
}
 
// cost function for utterances
var cost = function(utterance) {
  return 0;
};
 
//meaning
var meaning = function(state, utterance){
   return utterance == "pregnant" ? state.pregnant == 1 : state.embarrassed == 1
};
 
//competence
var competence  = ["high", "low"]
             	
// set speaker optimality
var alpha = 1
 
var chunkMaker = function(nChunksLeft,word,chunksSoFar) {
  var chunksSoFar = chunksSoFar == undefined ? [] : chunksSoFar
  if (nChunksLeft == 0) {
	return chunksSoFar
  } else {
	var newChunk = word[nChunksLeft-1][0]+word[nChunksLeft-1][1]
	var newChunksSoFar = chunksSoFar.concat([newChunk])
	return chunkMaker(nChunksLeft - 1,word,newChunksSoFar)
  }
}
var chunker = function(utterance) {
  var splitUtterance = utterance.split("")
  var chunk_org = _.chunk(utterance, 2)
  var chunk_1 = _.chunk(_.toArray(utterance).slice(1), 2)
  var chunks = _.union(chunk_org, chunk_1)
  var length = chunks.length
  return chunkMaker(length,chunks)
  }
 
var similarity = function(word1, word2) {
  return ((_.intersection(chunker(word1),chunker(word2))).length*2) /
	(chunker(word1).length + chunker(word2).length)
}
 
//translate
var translate = function(utterance, competence){
 	if (utterance == "embarrassed" & competence == "high")
   {return "avergonzada"; }
  if (utterance == "embarrassed" & competence == "low")
	{return similarity("embarazada", "embarrassed") > similarity("avergonzada", "embarrassed")
	? "embarazada" : "avergonzada";}
  if (utterance == "pregnant" & competence == "high")
   {return "embarazada"; }
  if (utterance == "pregnant" & competence == "low")
   {return similarity("avergonzada", "pregnant") > similarity( "embarazada", "pregnant")
	? "avergonzada" : "embarazada"; }
}
/// 
var states =
   [
  {pregnant : 1, embarrassed : 0},
  {pregnant : 0, embarrassed : 1 },
{pregnant : 1, embarrassed : 1}
]
 
// context high school friend
var statePrior = function() {
  return categorical ({ps: [40,60,40], vs: states})
}
// literal listener
var literalListener = cache(function(utterance) {
  return Infer({model: function(){
	var state = statePrior()
	condition(meaning(state, utterance))
	return state
  }})
})
 
// Speaker model
var speaker = function(state, competence) {
  Infer({model: function() {
	var utterance = utterancePrior()
       factor(alpha*(literalListener(utterance).score(state)
              	- cost(utterance)))
	return translate(utterance,competence)
  }})
}
 
//Pragmatic Listener inferring about competence
var pragmaticListener = cache(function(translation) {
  return Infer({model: function(){
	var state = statePrior()
	var competence = uniformDraw(["high", "low"])
	observe(speaker(state, competence), translation)
	return {state: state, competence: competence}
  }})
})
 
 viz(pragmaticListener("embarazada"))
~~~~

##Conclusion

        	Speaking in a second language presents various problems that make communication difficult. One of those problems occurs with the case of false cognates. Less proficient speakers might misuse these words given the meaning of their false friend pair. Our current model simulates this communicative scenario. The listener has different posterior beliefs about the state depending on the inferred competence of the speaker. The speaker in turns uses two different mechanisms to translate the utterances in English to Spanish depending on their competence in the latter. While a speaker with high competence will correctly translate the English utterance, a low competence speaker chooses a translation based on the similarity to the English utterance. We recognize our translate function makes binary choices instead of assigning different probabilities to the possible Spanish translations. It is possible, high competence second language speakers might also misuse false cognate pairs. However, given the objective is to model comprehension, we believe this is a promising model for this scenario. 

Note: Nicole A. Vargas Fuentes, Stephanie Joah You, Mulan McNabb, and Jessica Tran worked on the code for this false cognates model. Stephanie wrote about the empirical phenomenon of interest, Jessica wrote about the modeling approach taken, Nicole wrote about how the model was built and the results of the model, and Mulan wrote about the similarities and differences between the RSA model and our false cognates model. Outside of our group meetings, Nicole added more to the code and Jessica coordinated the times for group meetings.

##References

Inkpen, D., Frunza, O., & Kondrak, G. (2005, September). Automatic identification of cognates and false friends in French and English. In Proceedings of the International Conference Recent Advances in Natural Language Processing (Vol. 9, pp. 251-257).

Frank, Michael C., and Noah D. Goodman. "Predicting pragmatic reasoning in language games." Science 336.6084 (2012): 998-998.

Richards, J. C., & Schmidt, R. W. (2013). Longman dictionary of language teaching and applied linguistics. Routledge.

Roca-Varela, M.L.. (2015). False friends in learner Corpora: A corpus-based study of English false friends in the written and spoken production of Spanish learners. 10.3726/978-3-0351-0841-5.

Scontras, G., Tessler, M. H., & Franke, M. (2017). Probabilistic language understanding: An introduction to the Rational Speech Act framework.

Otwinowska, A., & Szewczyk, J. M. (2017). The more similar the better? Factors in learning cognates, false cognates and non-cognate words. International Journal of Bilingual Education and Bilingualism.