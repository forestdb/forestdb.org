---
layout: model
title: Adjectives
model-status: code
model-category: Reasoning about Reasoning
model-tags: linguistics, pragmatics
---

A model for the adjective "expensive" for watches, laptops, and coffee makers.


	(define n 3000) ;mh-query iterations (fairly stable at around 10000)

	(define items '(watch laptop coffee-maker))

	(define adj-model
	  (mem
	   (lambda (item)
	     
	     ;histogram of prior beliefs
	     (define bins
	       (case item
	             (('watch) '((25 75 125 175 225 275 325 375 425 475 525 575 625 675 725 775 825 875 925 975 1025 1075 1125 1175 1225 1275 1325 1375 1425 1475 1525 1575 1625 1675 1725 1775 1825 1875 1925 1975 2025 2075 2125 2175 2225 2275 2325 2375 2425 2475 2525 2575 2625 2675 2725 2775 2825 2875 2925)
	                         (0.0947481085 0.0829602394 0.0691282303 0.0687806274 0.0462393725 0.0428531349 0.0332680084 0.0294560525 0.0264614283 0.025520169 0.0297938706 0.0275830374 0.0190878511 0.0183019386 0.0182072429 0.0169720206 0.0161306217 0.0157244369 0.0148730719 0.0139127459 0.0136646609 0.0127724373 0.0119222969 0.0112743386 0.0108796281 0.0103466111 0.0100150567 0.0093383238 0.0083403212 0.0092740744 0.0091558948 0.0091686955 0.0080846405 0.0075100722 0.0071655694 0.0070702371 0.0069937157 0.007429641 0.0077048872 0.0078570612 0.0070956281 0.0062619279 0.0054001727 0.005248231 0.0053726753 0.0054029555 0.0053054692 0.0050706849 0.0051703982 0.0062352357 0.00681543 0.0059259639 0.0050772414 0.0053275407 0.0051893349 0.0049916149 0.0046942968 0.0045705879 0.0048742385)))
	             (('laptop) '((25 75 125 175 225 275 325 375 425 475 525 575 625 675 725 775 825 875 925 975 1025 1075 1125 1175 1225 1275 1325 1375 1425 1475 1525 1575 1625 1675 1725 1775 1825 1875 1925 1975 2025 2075 2125 2175 2225 2275 2325 2375 2425)
	                          (0.0038596662 0.0052780179 0.0092376321 0.0143777605 0.0210857798 0.0272565005 0.0320084075 0.0337319854 0.0384519275 0.0401601095 0.0416361285 0.042194139 0.0407069779 0.0408063651 0.0401091934 0.0369512404 0.0363900423 0.033940785 0.0340980982 0.0329104253 0.0301255078 0.028410492 0.0282468529 0.0282460624 0.0259678899 0.0319276873 0.0210910449 0.0185873628 0.0145558778 0.0132796219 0.0119084774 0.0108438537 0.010075972 0.0097575347 0.0088184418 0.0089546311 0.0086063933 0.0083546482 0.00748755 0.0072936005 0.0164569574 0.0159067467 0.0058115565 0.0053726878 0.0049363997 0.0045310616 0.0032232202 0.0031481672 0.0028825182)))
	             (('coffee-maker) '((2 6 10 14 18 22 26 30 34 38 42 46 50 54 58 62 66 70 74 78 82 86 90 94 98 102 106 110 114 118 122 126 130 134 138 142 146 150 154 158 162 166 170 174 178 182 186 190 194 198 202 206 210 214 218 222 226 230 234 238 242 246 250 254 258 262 266)
	                                (0.0014135189 0.0030187806 0.009097032 0.0205499168 0.0274455246 0.0340911375 0.0327946317 0.0369745497 0.0369889823 0.039968206 0.04292171 0.041644476 0.0400096933 0.0377216743 0.0367039012 0.0317264412 0.0309438117 0.0295106872 0.0250885374 0.022625047 0.0200664278 0.0182912726 0.0180617542 0.0170574618 0.020399705 0.0192680305 0.0175360134 0.0155388325 0.0136614887 0.0132008226 0.0104037678 0.0099879301 0.0098778971 0.0096614263 0.0095359236 0.0091090479 0.0098468805 0.0100181172 0.0086961533 0.0081772916 0.0103206941 0.0100159824 0.0095329683 0.0087439465 0.0081538896 0.0076233779 0.0070864724 0.0062226981 0.0058026201 0.0050817722 0.0060832585 0.0053105535 0.0049923216 0.0046792577 0.0046408922 0.0048380171 0.0042201977 0.0046082499 0.0046038231 0.0046815223 0.0040714362 0.0036098126 0.0033469522 0.003258214 0.003323007 0.0028773768 0.0026361817)))
	             (else '(()()))))
	     
	     ;get prior distribution
	     (define (prior) (apply multinomial bins))
	     
	     ;prior distribution for theta is uniform over possible values
	     (define (theta-prior) (uniform-draw (first bins)))
	     
	     """
	     MATH FOR UTTERANCE PRIOR
	     
	     P(utt) = sum_deg P(utt|deg)
	     \propto sum_deg( exp( alpha * (log(P_literal(deg|utt)) - cost(utt))) )
	     
	     assume alpha=1
	     
	     P(utt) \propto sum_deg( exp(log(P_literal(deg|utt))) * exp(-cost(utt)) )
	     \propto sum_deg( P_literal(deg|utt) * exp(-cost(utt)) )
	     \propto exp(-cost(utt)) * sum_deg( P_literal(deg|utt) )
	     \propto exp(-cost(utt))
	     
	     assume cost(expensive)=1 and cost(no-utt)=0
	     
	     
	     P(expensive) \propto exp(-1) = 0.3678794
	     P(no-utt) \propto exp(0) = 1
	     """
	     
	     (define (utterance-prior) (multinomial '(adjective no-utt) '(0.3678794 1)))
	     
	     """top level: L1"""
	     ;pragmatic listener jointly infers theta (cutoff for adjective to be true)
	     ;and degree (value of object being described)
	     (define pragmatic-listener
	       (mem (lambda (utterance)
	              ;query price and theta given speaker would have said that
	              (mh-query n 5
	                        (define price (prior ));item))
	                        (define theta (theta-prior ));item))
	                        (list price theta)
	                        (eq? (apply multinomial (speaker price theta)) utterance)))))
	     
	     """mid level: S1"""
	     ;speaker chooses and utterance so the literal listener will guess the correct degree
	     (define speaker
	       (mem (lambda (price theta);item)
	              ;query utterance given literal listener would guess price given theta
	              (enumeration-query
	               (define u (utterance-prior))
	               u
	               (eq? (apply multinomial (literal-listener u theta ));item))
	                    price)))))
	     
	     """base level: L0"""
	     ;literal listener conditions prior distribution on utterance being true
	     (define literal-listener
	       (mem (lambda (u theta );item)
	              (define prior-dist bins);(bins item))
	              (define (posterior-given-adjective)
	                
	                ;prior distribution is originally in the form:
	                ;(list list-of-values list-of-probabilities).
	                ;want to change this to:
	                ;(list (list val1 prob1) (list val2 prob2) ... )
	                (define bin-pairs (map list
	                                       (first prior-dist)
	                                       (second prior-dist)))
	                
	                ;only keep pairs where the value is above threshold theta
	                (define filtered-bin-pairs (fold
	                                            (lambda (a lst) (if (>= (first a) theta)
	                                                                (pair a lst)
	                                                                lst))
	                                            '()
	                                            bin-pairs))
	                
	                ;reformat into multinomial form (list list-of-values list-of-probabilities)
	                ;church will renormalize
	                (list (map first filtered-bin-pairs) (map second filtered-bin-pairs)))
	              
	              ;if utterance is expensive, return posterior given expensive
	              ;otherwise (if no utterance) return prior
	              (if (eq? u 'adjective)
	                  (posterior-given-adjective)
	                  prior-dist))))
	     
	     (define samples (pragmatic-listener 'adjective))
	     (define prices (map first samples))
	     (define thetas (map second samples))
	     
	     (list prices thetas))))

	(define human-posterior
	  (mem
	   (lambda (item)
	     (define posterior-bins
	       (case item
	             (('watch) '((25 75 125 175 225 275 325 375 425 475 525 575 625 675 725 775 825 875 925 975 1025 1075 1125 1175 1225 1275 1325 1375 1425 1475 1525 1575 1625 1675 1725 1775 1825 1875 1925 1975 2025 2075 2125 2175 2225 2275 2325 2375 2425 2475 2525 2575 2625 2675 2725 2775 2825 2875 2925)
	                         (0.0094918676 0.0160250432 0.0220625116 0.0134282261 0.019440928 0.0227043616 0.0247748416 0.0263280433 0.0290211242 0.0281442435 0.0279293432 0.0291842912 0.0269263378 0.0258990441 0.0213844552 0.0226149004 0.0221720191 0.0226790749 0.0235018613 0.0240290062 0.0192637981 0.0195461549 0.0196076874 0.0194833361 0.0185093215 0.0175102908 0.0170210292 0.0162911172 0.0154411573 0.0148709111 0.0146761868 0.0148805472 0.0145587656 0.0143880126 0.0140760482 0.0136062441 0.0135940465 0.0133021563 0.0131153693 0.0133371032 0.0136722016 0.0139438587 0.0138114842 0.0135037004 0.0129322781 0.012256317 0.011739368 0.0114562312 0.0114545158 0.0113672773 0.0115022525 0.0114139332 0.0112462475 0.0110998635 0.010800291 0.0105849676 0.010503554 0.0103342407 0.0111091463)))
	             (('laptop) '((25 75 125 175 225 275 325 375 425 475 525 575 625 675 725 775 825 875 925 975 1025 1075 1125 1175 1225 1275 1325 1375 1425 1475 1525 1575 1625 1675 1725 1775 1825 1875 1925 1975 2025 2075 2125 2175 2225 2275 2325 2375 2425)
	                          (0.001209868 0.0016045715 0.002160416 0.0025724297 0.0037721363 0.0047613159 0.0064009801 0.0091254082 0.0132499403 0.0174944397 0.0197162409 0.0215838634 0.0227900375 0.0238630533 0.0268718853 0.0289719214 0.0305785103 0.0316325296 0.0327045237 0.0335830781 0.0287254778 0.0255009292 0.0244541238 0.0228734305 0.0230647343 0.0225073048 0.0225287081 0.0229841407 0.0231955298 0.0236818859 0.0231217209 0.0239631075 0.0242442794 0.0252963589 0.0261277001 0.0265922965 0.0265587752 0.0258151067 0.025256619 0.0243642201 0.0237226927 0.0214555842 0.0204470999 0.0195469915 0.0185217273 0.0186142313 0.0177192002 0.0174146461 0.017054228)))
	             (('coffee-maker) '((2 6 10 14 18 22 26 30 34 38 42 46 50 54 58 62 66 70 74 78 82 86 90 94 98 102 106 110 114 118 122 126 130 134 138 142 146 150 154 158 162 166 170 174 178 182 186 190 194 198 202 206 210 214 218 222 226 230 234 238 242 246 250 254 258 262 266)
	                                (0.0012006208 0.0017111714 0.0018599742 0.0024493708 0.003138257 0.0038484742 0.0044852641 0.0058977602 0.0071471283 0.0100450115 0.0107615539 0.0125201669 0.0150877109 0.0156579629 0.0162734621 0.0157309712 0.0166362817 0.0179815088 0.0192937029 0.0176959421 0.0178707626 0.0183286766 0.0183543053 0.0182825945 0.0183399492 0.0187933138 0.0162637251 0.0168989578 0.0172222305 0.0177550251 0.0167531768 0.0167945563 0.0163602449 0.0167818815 0.0172534975 0.017167389 0.0172229436 0.0185321853 0.019085594 0.0189585256 0.017759584 0.0173952903 0.0179253179 0.0176629419 0.0178727503 0.0176793736 0.0178764628 0.0175494855 0.0173945457 0.0169409939 0.0174650443 0.0170007005 0.0159381736 0.0149266869 0.0147892517 0.0145716585 0.0142780804 0.0146967067 0.0155588011 0.0155191586 0.0162360889 0.0164509276 0.016656641 0.0172812493 0.0169653883 0.0167345738 0.0164322921)))
	             (else '(()()))))
	     (map pair (first posterior-bins) (second posterior-bins)))))

	;density function doens't like discretizations right now...
	(define (smooth-density lst name width)
	  (density (map (lambda (x) (uniform (- x width) (+ x width))) lst) name))

	(define item 'watch)
	(multiviz
	 (smooth-density (first (adj-model item)) "Prices" 25)
	 (scatter (human-posterior item) "People")
	 (smooth-density (second (adj-model item)) "Thetas" 25))



References:

- Cite:Lassiter2013adj