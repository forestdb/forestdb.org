---
layout: model
title: Daniel, Hondel & Ramirez Adjectives QUD
model-language: webppl
---

Extending the Vagueness resolution with a QUD

Authors: Aaliyah Daniel, Sylvia Ramirez, and Nic Hondel

Vagueness~
Lassiter & Goodman (2013)

Sometimes words without context can be vague, take the word "expensive" into different context it can mean many different things. When we use the word "expensive" for buying an ice cream it is much different than what someone would mean when they say "expensive" for a purchasing a bike or a car. Then how do we conclude what the meaning of "expensive" really is?

"Semanticists settle on the least common denominator: a threshold semantics by which the adjective asserts that holders of the relevant property surpass some point on the relevant scale (i.e., expensive means more expensive than d for some contextually-determined degree of price d). Whereas semanticists punt on the mechanism by which context fixes these aspects of meaning, the RSA framework is well-suited to meet the challenge". 


Lassiter and Goodman suggest gradable adjectives...

(1) [Almost all ice cream in the OC cost $2-38.]

Someone says, "That ice cream is expensive"

a. A $38 ice cream is expensive

b. A $1 ice cream is not expensive

c. A $6 ice cream is neither expensive nor not expensive- rather its roughly average. 

Dependent on what question you ask .. can have an affect on what the listener believes to be expensive. 


Savinelli et. al 2017

What someone interprets as the QUD may impact their interpretation.



~~~~
var marginalize = function(dist, key){
  return Infer( {model: function(){
    return sample(dist)[key];
  }})
};
var icecream = {
   "prices":  [1, 4, 6, 10, 14, 18, 22, 30, 34, 38],
"probabilities": [0.01, 0.50, 0.85, 0.63, 0.35, 0.10, 0.04, 0.02, 0.02, 0.01]
};
~~~~

This model depends on our prior knowledge of the world state. 

Next, we create a prior for the degree threshold θ. Since we’re talking about expensive ice cream, θ will be the price cutoff to count as expensive. But we want to be able to use expensive to describe anything with a price, so we’ll set the thetaPrior to be uniform over the possible prices in our world.

We then introduce three new utterances, "expensive", "cheap", or "null". The semantics of the expensive utterance checks the relevant item’s price against the price cutoff. The "cheap utterance" is true only when it checks the relevant item's price against expensive and if it is not true that it is expensive, than it must be cheap. The “null utterance” is true everywhere and it is assumed to be less likely than uttering expensive.  

~~~~
var icecream = {
  "prices":  [1, 4, 6, 10, 14, 18, 22, 30, 34, 38],
"probabilities": [0.01, 0.50, 0.85, 0.63, 0.35, 0.10, 0.04, 0.02, 0.02, 0.01]
};
 
var statePrior = function() {
  return categorical(icecream.probabilities, icecream.prices);
};
 
var thetaPrior = function() {
  return uniformDraw(icecream.prices);
};
 
var alpha = 1; // optimality parameter
 
var utterances = ["expensive", "null", "cheap"];
var cost = {
  "expensive": 1,
  "cheap": 2,
  "null": 0
};
var utterancePrior = function() {
  var uttProbs = map(function(u) {return Math.exp(-cost[u]) }, utterances);
  return categorical(uttProbs, utterances);
};
 
var meaning = function(utterance, price, theta) {
  utterance == "expensive" ? price >= theta :
  utterance == "cheap" ? price <= theta :
  true
};
~~~~

The Question-under-Discussion (QUD) determines what topic of information is being conveyed, was well as what a speaker will seek to address. Together these form communicative goals that result in a model that can predict how likely is an utterance to be spoken in order to address the QUD. In our model, we feature both a baseline model (that only returns the price) and a QUD manipulation to show the changes when a speaker utters that an item is “expensive” or cheap (“less than 10”). Our probabilities reflect the prior elicitation of the interlocutor, or what a listener can infer about the speaker’s knowledge of the world. Thus the uniform draw shows when there is no extra information about the world and thus no one choice seems preferable over any other. Adding a QUD manipulation allows us to add probabilities about certain states of the world, and then to use this information to predict which utterance would be most likely for a given value (in this case, price). 


We predict that based upon which QUD is given a higher probability will result in a higher probability that someone will believe a certain price is expensive. For example prices $10,14, 18 have the highest probability that the listener will believe its expensive. For the adj model we know that using the utterance exp or cheap is more costly therefore the speaker would only use it if they thought the item was literally expensive or cheap. 


~~~~
// QUDs
var QUDs = ["expensive?","less than 15?","what is the price?"]
var QUDPrior = function() {
  //uniformDraw(QUDs)
// categorical([1,10,1],QUDs)// this is equivalent to uniformDraw
categorical([0,0,1],QUDs)// this is your baseline version
}
var QUDFun = function(QUD,state) {
  QUD == "expensive?" ? state >= 15 :
  QUD == "less than 15?" ? state <= 15 :
  state;
};
~~~~

By adding an S1 and L1 layer we create a full RSA model. The L1 hears the ambiguous utterance “expensive” or  “cheap”  and proceeds to determine what the speaker meant and the price cutoff to count as “expensive” and “cheap” while inferring the state of the world. 

The speaker observes a state and has a specific threshold in mind that it gets from the pragmatic listener and it samples an utterance and a QUD from the utterance prior and QUD prior and returns the utterance in proportion of the probability that the listener would arrive at the intended state. The pragmatic listener then hears that the ice cream  was either “expensive” or “cheap” samples a price and a threshold and checks to see the probability that the speaker would have chosen that specific utterance to communicate that price with that threshold and returns the price and threshold in proportion to that probability. Predictions were then generated from the pragmatic listener having heard that the ice cream was either “expensive” or “cheap” and we looked at the marginal distributions for price and theta. 

~~~~
var literalListener = cache(function(utterance, theta, QUD) {
  return Infer({model:function() {
  var price = statePrior()
  var qPrice = QUDFun(QUD,price)
    condition(meaning(utterance, price, theta))
  return qPrice;
}})
});
 
var speaker = cache(function(price, theta,QUD) {
  return Infer( {model: function() {
  var utterance = utterancePrior()
  var qPrice= QUDFun(QUD, price)
  factor( alpha * literalListener(utterance, theta, QUD).score(qPrice) );
  return utterance;
  }});
});
 
var pragmaticListener = function(utterance) {
  return Infer({model: function() {
  var price = statePrior()
  var theta = thetaPrior()
  var QUD =QUDPrior()
    factor(speaker(price, theta, QUD).score(utterance));
  return { price: price, theta: theta, qud: QUD};
  }})
}
~~~~

Before trying out our model, there are several possible permutations and predictions possible. Based upon the working parts our model features, we predict that, depending on which QUD is being discussed, we can have a fairly good idea of what utterances will be stated. Let’s take a look at each one to clarify.
First, let’s say that my friends and I are strolling downtown at the beach. We happen upon an ice cream shop and, in the heat of the midday, such a treat sounds like a great idea to all of us. I want to buy my own ice cream, but I only have $15 in my wallet. By weighing the QUD “less than $15?” higher than the others, we see that if someone utters that the ice cream is “cheap”, then I as the pragmatic listener assume that the ice cream is somewhere beneath the threshold of $15. If, however, I hear my friend say that the ice cream is “expensive”, the model accurately predicts that I will assume that the ice cream is close to the threshold, but not over (since the QUD is concerned with if the treat fits in my budget).
Now, let’s say that the QUD is actually “is it expensive?” With our previously established threshold of $15, we see a similar distribution of prices, although the probably “expensive” price is slightly lower. The point, of course, is that for me and my tight budget, “expensive” is tantamount to over $15, and thus this QUD is similar to the former one asking if the ice cream is “less than $15”. So, let’s say I just got paid and now I have $150 in my wallet! Sweet. By altering the QUD to reflect that “expensive” is over $150, we see that the prices still maintain a likely distribution (based on their probabilities), but then the likelihood that someone says that $20 or $30 is expensive is low, since it is way within my budget. Similarly, “cheap” also tends to gather most of the likely prices toward the lower end, as we have assumed most ice cream prices are not going to be in the double-digits.


Here is the full model. 

~~~~
var marginalize = function(dist, key){
  return Infer( {model: function(){
  return sample(dist)[key];
  }})
};
///
 
var icecream = {
  "prices":  [1, 4, 6, 10, 14, 18, 22, 30, 34, 38],
"probabilities": [0.01, 0.50, 0.85, 0.63, 0.35, 0.10, 0.04, 0.02, 0.02, 0.01]
};
 
var statePrior = function() {
  return categorical(icecream.probabilities, icecream.prices);
};
 
var thetaPrior = function() {
  return uniformDraw(icecream.prices);
};
 
var alpha = 1; // optimality parameter
 
var utterances = ["expensive", "null", "cheap"];
var cost = {
  "expensive": 1,
  "cheap": 2,
  "null": 0
};
var utterancePrior = function() {
  var uttProbs = map(function(u) {return Math.exp(-cost[u]) }, utterances);
  return categorical(uttProbs, utterances);
};
 
var meaning = function(utterance, price, theta) {
  utterance == "expensive" ? price >= theta :
  utterance == "cheap" ? price <= theta :
  true
};
// QUDs
var QUDs = ["expensive?","less than 15?","what is the price?"]
var QUDPrior = function() {
  //uniformDraw(QUDs)
// categorical([1,10,1],QUDs)// this is equivalent to uniformDraw
categorical([0,0,1],QUDs)// this is your baseline version
}
var QUDFun = function(QUD,state) {
  QUD == "expensive?" ? state >= 15 :
  QUD == "less than 15?" ? state <= 15 :
  state;
};
 
var literalListener = cache(function(utterance, theta, QUD) {
  return Infer({model:function() {
  var price = statePrior()
  var qPrice = QUDFun(QUD,price)
    condition(meaning(utterance, price, theta))
  return qPrice;
}})
});
 
var speaker = cache(function(price, theta,QUD) {
  return Infer( {model: function() {
  var utterance = utterancePrior()
  var qPrice= QUDFun(QUD, price)
  factor( alpha * literalListener(utterance, theta, QUD).score(qPrice) );
  return utterance;
  }});
});
 
var pragmaticListener = function(utterance) {
  return Infer({model: function() {
  var price = statePrior()
  var theta = thetaPrior()
  var QUD =QUDPrior()
    factor(speaker(price, theta, QUD).score(utterance));
  return { price: price, theta: theta, qud: QUD};
  }})
}
 
 
 
print ('cheap icecream')
var expensiveIcecream = pragmaticListener('expensive');
print(expectation(marginalize(expensiveIcecream, "price")))
viz.hist(marginalize(expensiveIcecream, "price"));
viz.hist(marginalize(expensiveIcecream, "theta"));
viz.hist(marginalize(expensiveIcecream, "qud"));
~~~~

As you can see, our model is similar to the adjectives model we learned about in class because we look at the utterances describing the cost of an object to determine if the speaker is being informative about the price. We also use the QUD model that we learned about in class, the QUD adds uncertainty about what the price is for an item and how expensive or cheap an item is. L1 decides which QUD is most likely given the speaker's utterance.

It is different in that we incorporated the QUD function into the adjectives model in order to help us understand the actual feelings of the speaker toward the price of an object. Another difference in our code would be that we do not include the scope aspect of QUD model (chapter four). Our meaning function is also different compared to meaning function in the QUD model, in that we specify it more specifically toward our utterances of "expensive" or "cheap" whereas the original model you can see relies on the scope to return either the "surface" or "inverse". 

Relevant code from Ch 4:

~~~~
// possible world states: how many apples are red
var states = [0,1,2,3]
var statePrior = function() {
  uniformDraw(states)
}

// possible utterances: saying nothing or asserting the ambiguous utterance
var utterances = ["null","every-not"]

// possible scopes
var scopePrior = function(){ 
  return uniformDraw(["surface", "inverse"])
}

// meaning function
var meaning = function(utterance, state, scope) {
  return utterance == "every-not" ? 
    scope == "surface" ? state == 0 :
  state < 3 : 
  true
}

meaning("every-not", 1, "surface")
~~~~

We also did not include the string enumerate in our adjectives model, since we are incorporating the QUD model with infererence based on probabilities, rather than a discrete sum. We used the simple adjective model instead of the better model presented to us in class where the better model, rather than assuming prior knowledge we can measure, then feed the measurements into the model as facts about the world allowing the model to make actual predictions about the behavior we expect from the listener. Our model does not allow this and also we are only able to infer about one variable at a time whereas the better model can infer about multiple variables. 

Example of the better model from Ch 5: 

~~~~
///fold:
var marginalize = function(dist, key){
  return Infer({method: "enumerate"}, function(){
    return sample(dist)[key];
  })
}

// "price" refers to the midpoint of the bin that participants marked a slider for
// "probability" refers to the average of participants' responses, after normalizing responses for each person for each item
var coffee = {
  "prices": [2, 6, 10, 14, 18, 22, 26, 30, 34, 38, 42, 46, 50, 54, 58, 62, 66, 70, 74, 78, 82, 86, 90, 94, 98, 102, 106, 110, 114, 118, 122, 126, 130, 134, 138, 142, 146, 150, 154, 158, 162, 166, 170, 174, 178, 182, 186, 190, 194, 198, 202, 206, 210, 214, 218, 222, 226, 230, 234, 238, 242, 246, 250, 254, 258, 262, 266, 270],
  "probabilities": [0.00394425443541116, 0.00566290387321734, 0.00825676850536135, 0.0128170636252316, 0.016825062585583, 0.0252551834871954, 0.028468923991368, 0.0287662881619652, 0.0282906782393822, 0.0298574213919862, 0.0289804109562303, 0.0294934077950995, 0.0282267499972963, 0.027150810280172, 0.0263590035097067, 0.0281826763029633, 0.0279055635543465, 0.0278411275661919, 0.0278729305091271, 0.0260779123427457, 0.025504052957576, 0.0244726498427457, 0.0245886104865853, 0.0244622094919612, 0.0245026711395991, 0.0215246465554349, 0.018590665797836, 0.0178546907780941, 0.0171993717256383, 0.0162494284404094, 0.0160224797974072, 0.0153063570140879, 0.0146233597944038, 0.0137848051705444, 0.0143326737593802, 0.0134490255028661, 0.0135790032064503, 0.0138226943563312, 0.0131925119561275, 0.0126370445876965, 0.0106890591752964, 0.0106020628239155, 0.0101185949533398, 0.00911819843666944, 0.0106104999317876, 0.0109322919246626, 0.0107129834142186, 0.00837168284710854, 0.0080750721559225, 0.00804082264332751, 0.0067313619453682, 0.00627460791368519, 0.00589113687110393, 0.00540523790626922, 0.00552727879179031, 0.00550309276614945, 0.00523220007943591, 0.00511881990396121, 0.00512531318796172, 0.00518250532794681, 0.00363644079549071, 0.00358121922955672, 0.00340485508432679, 0.00300204070158435, 0.00285900767263514, 0.00307193123371249, 0.00265238954722026, 0.00259519926379722]
};
var headphones = {
  "prices": [3, 9, 15, 21, 27, 33, 39, 45, 51, 57, 63, 69, 75, 81, 87, 93, 99, 105, 111, 117, 123, 129, 135, 141, 147, 153, 159, 165, 171, 177, 183, 189, 195, 201, 207, 213, 219, 225, 231, 237, 243, 249, 255, 261, 267, 273, 279, 285, 291, 297, 303, 309, 315, 321, 327],
  "probabilities": [0.0100932350462178, 0.0170242011533158, 0.0239368832461044, 0.0287274552724317, 0.0335581038692007, 0.0340978261675896, 0.0343161232972866, 0.0360390185911639, 0.036657423324831, 0.0363628055611882, 0.0363696076759004, 0.0349562074307856, 0.034074002420704, 0.0319526478383908, 0.0310094760253021, 0.0310633591664631, 0.0306213012447122, 0.0267011137913525, 0.0258670033914898, 0.0258502350122376, 0.025023519418377, 0.023483875551852, 0.0220028560428636, 0.0214970268323533, 0.0208370797134518, 0.0179036695749784, 0.0171528323730662, 0.0146789727474624, 0.0151970966046565, 0.0144002087359724, 0.0133747582315602, 0.0123806405169515, 0.0120269613954518, 0.0114214380586013, 0.0119920388378585, 0.011383038780713, 0.0113361342839428, 0.0109751373856222, 0.00862772988748631, 0.00849834370871022, 0.00929436665963529, 0.00977137935316548, 0.00855300304060609, 0.00704341088484788, 0.00760242506144268, 0.0066676293070587, 0.00664151547908964, 0.00631842654192989, 0.00633866335810551, 0.00617281009506307, 0.00588389192288987, 0.00480853638779448, 0.00438645078845164, 0.00381072308793509, 0.00323537982338538]
};
var laptop = {
  "prices": [25, 75, 125, 175, 225, 275, 325, 375, 425, 475, 525, 575, 625, 675, 725, 775, 825, 875, 925, 975, 1025, 1075, 1125, 1175, 1225, 1275, 1325, 1375, 1425, 1475, 1525, 1575, 1625, 1675, 1725, 1775, 1825, 1875, 1925, 1975, 2025, 2075, 2125, 2175, 2225, 2275, 2325, 2375, 2425, 2475],
  "probabilities": [0.00143805356403561, 0.00271785465081783, 0.00606278580557322, 0.00969025393738191, 0.0148967448541453, 0.0198125393878057, 0.0241329166751748, 0.0295996079491818, 0.0330742235328304, 0.0361264464785417, 0.0401533300800443, 0.0409948060761354, 0.0413284234227614, 0.0403647740154393, 0.0401794950623575, 0.0389208352425967, 0.0381211282626673, 0.0380271843819898, 0.0337130710333348, 0.0320869893997556, 0.0303954589742173, 0.0290371975286599, 0.026737673556754, 0.0259385656770147, 0.0246868317621353, 0.0229019589247208, 0.0214921049215865, 0.0206506505418053, 0.0194727089077923, 0.0192606778139999, 0.016502461425033, 0.015779852769427, 0.014573698680051, 0.0141934767007475, 0.0128416097234241, 0.0124983882714321, 0.011580082968956, 0.0111057181921517, 0.0107390517842162, 0.0097877804629936, 0.00869628232285698, 0.00796157687645315, 0.00781439122831209, 0.0074127159955819, 0.00686298004874477, 0.00667898701991275, 0.00628316286073362, 0.00606433685126466, 0.00552579230999036, 0.00508236108646155]
};
var sweater = {
  "prices": [1.5, 4.5, 7.5, 10.5, 13.5, 16.5, 19.5, 22.5, 25.5, 28.5, 31.5, 34.5, 37.5, 40.5, 43.5, 46.5, 49.5, 52.5, 55.5, 58.5, 61.5, 64.5, 67.5, 70.5, 73.5, 76.5, 79.5, 82.5, 85.5, 88.5, 91.5, 94.5, 97.5, 100.5, 103.5, 106.5, 109.5, 112.5, 115.5, 118.5, 121.5, 124.5, 127.5, 130.5, 133.5, 136.5, 139.5, 142.5, 145.5, 148.5, 151.5, 154.5, 157.5, 160.5, 163.5, 166.5, 169.5, 172.5, 175.5, 178.5, 181.5, 184.5, 187.5, 190.5, 193.5, 196.5, 199.5, 202.5, 205.5, 208.5, 211.5, 214.5, 217.5, 220.5, 223.5, 226.5, 229.5, 232.5, 235.5, 238.5],
  "probabilities": [0.00482838499944466, 0.00832934578733181, 0.0112952500492109, 0.0173774790108894, 0.0232006658974883, 0.0258422772579257, 0.0278986695293033, 0.0295289411585088, 0.0306833716679902, 0.0318318597751272, 0.0337834568516467, 0.0339921053872795, 0.0344439315108449, 0.033934432265521, 0.032462878943956, 0.031189466255733, 0.0308771801297135, 0.028870440122745, 0.0268081450723193, 0.0255858806436071, 0.0251329374896422, 0.0228478370318916, 0.0204321414255458, 0.0198885290723185, 0.018227461808914, 0.0175834655975716, 0.0162564776853142, 0.0157731201439098, 0.0152929182194243, 0.0150019091787104, 0.0149289099733278, 0.0141896849640091, 0.0139276040018639, 0.0134566451556076, 0.0121362548659773, 0.0108558348756676, 0.010613387912742, 0.010091436302254, 0.0098485635160309, 0.00934441306894098, 0.0085581357559821, 0.00679765980394746, 0.00693786098620408, 0.00696322949112243, 0.0070065216834756, 0.00642780104088309, 0.0064321559380608, 0.00656481702666307, 0.0062031359060876, 0.00687959185242292, 0.00510183619524203, 0.00519749642756681, 0.00498195289503402, 0.00463848039694862, 0.00446827225938831, 0.00434243838506282, 0.00454705086043589, 0.0045914966088052, 0.00451510979961212, 0.00443992782954235, 0.00329590488378491, 0.00349426470729333, 0.00329078051712042, 0.00323849270094039, 0.00302968185434419, 0.00294213735024183, 0.00335510797297302, 0.00328341117067163, 0.00329874147186505, 0.00306305447627786, 0.00262071902879654, 0.00274925007756808, 0.00246374710845232, 0.00262910011008071, 0.00248819809733968, 0.00211124548886266, 0.00204178897873852, 0.00208550762922333, 0.00204890779502054, 0.00228129283166782]
};
var watch = {
  "prices": [25, 75, 125, 175, 225, 275, 325, 375, 425, 475, 525, 575, 625, 675, 725, 775, 825, 875, 925, 975, 1025, 1075, 1125, 1175, 1225, 1275, 1325, 1375, 1425, 1475, 1525, 1575, 1625, 1675, 1725, 1775, 1825, 1875, 1925, 1975, 2025, 2075, 2125, 2175, 2225, 2275, 2325, 2375, 2425, 2475, 2525, 2575, 2625, 2675, 2725, 2775, 2825, 2875, 2925, 2975],
  "probabilities": [0.040844560268751, 0.0587099798246933, 0.0656194599591356, 0.0667642412698035, 0.0615953803048016, 0.0510809063784378, 0.0467203673419258, 0.0446735950187136, 0.040047421916613, 0.0350583957334483, 0.0297508215717606, 0.0256829651118227, 0.024135920250668, 0.0228891907259206, 0.021706684520276, 0.0186449440066946, 0.0187249266247728, 0.0179250744798993, 0.0173698811746238, 0.0165581725818319, 0.0160745066032247, 0.0127927305129066, 0.0113730680265067, 0.0109485307623827, 0.00923468422650943, 0.00899007751887508, 0.00880520147998275, 0.00838023585866885, 0.00841052411004918, 0.00828830635037619, 0.00834008093757411, 0.00750681534099784, 0.00724072133740109, 0.00717291664158004, 0.00682823777708754, 0.00646995193940331, 0.00697139732982518, 0.00711846547272734, 0.00698781312802354, 0.00732316558583701, 0.00594973158122097, 0.00557461443747403, 0.00541637601910211, 0.00518850469148531, 0.00572025848989677, 0.0051443557601358, 0.00510282169734075, 0.00493720252580643, 0.00560198932991028, 0.00519158715054485, 0.00473398797752786, 0.00540907722833213, 0.00494653421540979, 0.00495500420164643, 0.00494083025189895, 0.00481566268206312, 0.00442965937328148, 0.00441189688100535, 0.00415116538135834, 0.00361842012002631]
};
var data = {
  "coffee maker": coffee,
  "headphones": headphones,
  "laptop": laptop,
  "sweater": sweater,
  "watch": watch
};

var prior = function(item) {
  // midpoint of bin shown to participants
  var prices = data[item].prices;
  // average responses from participants, normalizing by item
  var probabilities = data[item].probabilities;
  return function() {
    return categorical(probabilities, prices);
  };
};

var theta_prior = function(item) {
  // midpoint of bin shown to participants
  var prices = data[item].prices;
  var bin_width = prices[1] - prices[0];
  var thetas = map(function(x) {return x - bin_width/2;}, prices);
  return function() {
    return uniformDraw(thetas);
  };
};
///

var alpha = 1; // optimality parameter

var utterances = ["expensive", ""];
var cost = {
  "expensive": 1,
  "": 0
};
var utterancePrior = function() {
  var uttProbs = map(function(u) {return Math.exp(-cost[u]) }, utterances);
  return categorical(uttProbs, utterances);
};

var meaning = function(utterance, price, theta) {
  utterance == "expensive" ? price >= theta : true;
};

var literalListener = cache(function(utterance, theta, item) {
  return Infer({method: "enumerate"}, function() {
    var pricePrior = prior(item);
    var price = pricePrior()
    condition(meaning(utterance, price, theta))
    return price;
  });
});

var speaker = cache(function(price, theta, item) {
  return Infer({method: "enumerate"}, function() {
    var utterance = utterancePrior();
    factor( alpha * literalListener(utterance, theta, item).score(price) );
    return utterance;
  });
});

var pragmaticListener = function(utterance, item) {
  var pricePrior = prior(item);
  var thetaPrior = theta_prior(item);
  return Infer({method: "enumerate"}, 
  function() {
    var price = pricePrior();
    var theta = thetaPrior();
    factor( speaker(price, theta, item).score(utterance) );
    return { price: price, theta: theta };
  });
};

var expensiveWatch = pragmaticListener("expensive", "watch");
print("the listener's posterior over watch prices:")
viz.density(marginalize(expensiveWatch, "price"));
print("the listener's posterior over watch price thresholds:")
viz.density(marginalize(expensiveWatch, "theta"));

var expensiveSweater= pragmaticListener("expensive", "sweater");
print("the listener's posterior over sweater prices:")
viz.density(marginalize(expensiveSweater, "price"));
print("the listener's posterior over sweater price thresholds:")
viz.density(marginalize(expensiveSweater, "theta"));
~~~~