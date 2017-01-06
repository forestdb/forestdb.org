---
layout: model
title: Projective Content
model-status: code
model-language: church
---

~~~~
; Church code accompanying the paper "A rational speech-act model of projective content"
; by Ciyang Qing, Noah D. Goodman, and Daniel Lassiter 
; submitted to Cogsci 2016


; Components of the model

; names are used for visualization
; list of utterances 
(define utterances 
  (list "nothing"
        "smokes" "not_smoke" "smoked" "not_smoked" 
        "stop_smoke" "not_stop" "start_smoke" "not_start" 
        "always_smoke" "not_always" "never_smoke" "not_never" ))  ;  ;

; utterance priors
(define u-priors 
  (list 2 1 1 1 1 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5)  ;  ;
)

; list of Common Ground names
; these also correspond to all 15 context sets in the paper
(define CG-names 
  (list "{}" "+past" "-past" "+now" "-now" 
        "+past+now" "+past-now" "-past+now" "-past-now"
        "change" "no_change"
        "~+past+now" "~+past-now" "~-past+now" "~-past-now")
)


(define CG-priors
  (list 34.533                       ; 0.95* 0.6*0.6 + 0.05* 1/15
        11.733 11.733 11.733 11.733  ; 0.95* 2*0.4*0.6/4 + 0.05* 1/15
        4.133 4.133 4.133 4.133      ; 0.95* 0.4*0.4/4 + 0.05* 1/15
        0.333 0.333                  ; 0.95* 0 + 0.05* 1/15
        0.333 0.333 0.333 0.333 
  )
)


; always nothing in the common ground 
(define noCG-priors
  (list 1 
        0 0 0 0 
        0 0 0 0 
        0 0 
        0 0 0 0 
  )
)

; uniform priors over context sets
(define CG-uniformpriors
  (list 1 
        1 1 1 1
        1 1 1 1 
        1 1 
        1 1 1 1 
  )
)

; pragmatic listener's world prior
(define world-priors 
  (list 1 1 1 1)
)

; literal listener's world prior
(define literal-priors 
  (list 1 1 1 1)
)

; Auxiliary functions

(define (member? x ls) (if (member x ls) #t #f))
(define (normalize ls) (map (lambda (x) (/ x (sum ls))) ls))

; compare whether x and y have the same value (works only for basic types and pairs)
; because the behavior of eq? is not what we want when x and y are pairs
; there might be a built-in Church function for this, but I do not know
(define (eq-val? x y)
  (if (and (pair? x) (pair? y))
      (and (eq? (first x) (first y)) (eq? (second x) (second y)))
      (eq? x y)
  )
)


; utterance meanings

(define (stop-smoke? w) (and (first w) (not (second w)) ) )
(define (not-stop-smoke? w) (not (stop-smoke? w)) )
(define (smoke? w)  (second w)  )
(define (not-smoke? w) (not (smoke? w)) )
(define (smoked? w)  (first w)  )
(define (not-smoked? w) (not (smoked? w)) )
(define (say-nothing w) #t)
(define (start-smoke? w) (and (not (first w)) (second w)  ) )
(define (not-start-smoke? w) (not (start-smoke? w)) )
(define (always-smoke? w) (and (first w) (second w)  ) )
(define (not-always-smoke? w) (not (always-smoke? w)) )
(define (never-smoke? w) (and (not (first w)) (not (second w))  ) )
(define (not-never-smoke? w) (not (never-smoke? w)) )

; QUDs

(define (qud-past w) (first w) )
(define (qud-now w) (second w) )
(define (qud-max w) w) 
(define (qud-stop w) (and (first w) (not (second w))) )
(define (qud-change w) (not (eq? (first w) (second w))) )
(define (qud-always w) (and (first w) (second w)) )
(define (qud-never w) (and (not (first w)) (not (second w))) )
(define (qud-start w) (and (not (first w)) (second w)) )



; propositions in the common ground
(define (past-smoke w) (first w))
(define (past-not-smoke w) (not (first w)))
(define (now-smoke w) (second w))
(define (now-not-smoke w) (not (second w)))
(define (change w) (or (and (past-smoke w) (now-not-smoke w)) 
                       (and (past-not-smoke w) (now-smoke w))
                   ))
(define (no-change w) (not (change w)))

; the function that maps utterances to their meanings
(define (meaning utterance)
  (cond ((eq? utterance "nothing") say-nothing)        
        ((eq? utterance "smokes") smoke?)
        ((eq? utterance "not_smoke") not-smoke?)
        ((eq? utterance "smoked") smoked?)
        ((eq? utterance "not_smoked") not-smoked?)
        ((eq? utterance "stop_smoke") stop-smoke?)
        ((eq? utterance "not_stop") not-stop-smoke?)
        ((eq? utterance "start_smoke") start-smoke?)
        ((eq? utterance "not_start") not-start-smoke?)
        ((eq? utterance "always_smoke") always-smoke?)
        ((eq? utterance "not_always") not-always-smoke?)
        ((eq? utterance "never_smoke") never-smoke?)
        ((eq? utterance "not_never") not-never-smoke?)
  )
)

; function that maps Common Ground names to CGs
; CG is a list of propositions

(define (name2cg cg_name)
  (cond ((eq? cg_name "{}") '())
        ((eq? cg_name "+past") (list past-smoke))
        ((eq? cg_name "-past") (list past-not-smoke))
        ((eq? cg_name "+now") (list now-smoke))
        ((eq? cg_name "-now") (list now-not-smoke))
        ((eq? cg_name "+past+now") (list past-smoke now-smoke))
        ((eq? cg_name "+past-now") (list past-smoke now-not-smoke))
        ((eq? cg_name "-past+now") (list past-not-smoke now-smoke))
        ((eq? cg_name "-past-now") (list past-not-smoke now-not-smoke))
        ((eq? cg_name "change") (list change))
        ((eq? cg_name "no_change") (list no-change))
        ((eq? cg_name "~+past+now") (list (lambda (w) (or (past-not-smoke w) (now-not-smoke w))) ))
        ((eq? cg_name "~+past-now") (list (lambda (w) (or (past-not-smoke w) (now-smoke w))) ))
        ((eq? cg_name "~-past+now") (list (lambda (w) (or (past-smoke w) (now-not-smoke w))) ))
        ((eq? cg_name "~-past-now") (list (lambda (w) (or (past-smoke w) (now-smoke w))) ))
  )
)

; the literal listener 
(define (literal-listener utterance qud common-ground)
  ; the utterance should be such that the common-ground is satisfiable
  ; needs to be enforced by the speaker

  (enumeration-query
   (define w (multinomial (list '(#t #t) '(#t #f) '(#f #t) '(#f #f) )
                          literal-priors))
   (qud w)

   ; the world must satisfy the utterance and every proposition in the
   ; common ground
   (and ((meaning utterance) w)
        (all (map (lambda (prop) (prop w)) common-ground))
   )
  )

)

; enumeration-query returns an enumlist, whose first element is a list of outcomes
; and second element is a list of the corresponding probabilities

(define (softmax alpha enumlist) 
  ; raise all the probabilities to the power of alpha
  (define alpha-probs (map (lambda (x) (expt x alpha)) (second enumlist)))
  ; renormalize and return the new enumlist
  (list (first enumlist) (normalize alpha-probs))
)


(define (speaker w qud common-ground)
  ; the world w must be consistent with the common-ground
  ; this is enforced by the pragmatic listener

  (enumeration-query
    (define utterance 
      (multinomial utterances 
                   u-priors) )
    utterance

    ; the speaker is always truthful 
    (if ((meaning utterance) w)
         (eq-val? (qud w) (apply multinomial (literal-listener utterance qud common-ground) ) )
        #f
    )
  )
)

(define (pragmatic-listener alpha utterance qud CS-priors)
  (enumeration-query
    (define w (multinomial (list '(#t #t) '(#t #f) '(#f #t) '(#f #f) )
                           world-priors))
    (define cg-name (multinomial CG-names CS-priors))

    ; returns joint distribution over worlds and context sets
    (list w cg-name)

    ; enforcing that the speaker's world is consistent with the CG
    (if (all (map (lambda (prop) (prop w)) (name2cg cg-name)))
        (eq? utterance (apply multinomial (softmax alpha (speaker w qud (name2cg cg-name)))))
        #f
    )
  )
)

(define (pragmatic-listener-world alpha utterance qud CS-priors)
  (enumeration-query
    (define w (multinomial (list '(#t #t) '(#t #f) '(#f #t) '(#f #f) )
                           world-priors))
    (define cg-name (multinomial CG-names CS-priors))

    ; return marginal distribution over worlds
    w

    ; enforcing that the speaker's world is consistent with the CG
    (if (all (map (lambda (prop) (prop w)) (name2cg cg-name)))
        (eq? utterance (apply multinomial (softmax alpha (speaker w qud (name2cg cg-name)))))
        #f
    )
  )
)


; parameters to manipulate
(define alpha 6)

; the Church built-in plotting functions below are for quick demo
; the plots in the paper were generated in R using ggplot2



; nothing in CG, QUD max
; (barplot (pragmatic-listener alpha "not_stop" qud-max noCG-priors))
; (barplot (pragmatic-listener-world alpha "not_stop" qud-max noCG-priors))


; add CG, uniform prior, QUD max
; (barplot (pragmatic-listener alpha "not_stop" qud-max CG-uniformpriors))
; (barplot (pragmatic-listener-world alpha "not_stop" qud-max CG-uniformpriors))

; CG prior, QUD max
; (barplot (pragmatic-listener alpha "not_stop" qud-max CG-priors))
; (barplot (pragmatic-listener-world alpha "not_stop" qud-max CG-priors))

; CG prior, QUD now
 (barplot (pragmatic-listener alpha "not_stop" qud-now CG-priors))
; (barplot (pragmatic-listener-world alpha "not_stop" qud-now CG-priors))

; CG prior, QUD past
; (barplot (pragmatic-listener alpha "not_stop" qud-past CG-priors))
; (barplot (pragmatic-listener-world alpha "not_stop" qud-past CG-priors))

; CG prior, QUD change
; (barplot (pragmatic-listener alpha "not_stop" qud-change CG-priors))
; (barplot (pragmatic-listener-world alpha "not_stop" qud-change CG-priors))

; CG prior, QUD always
; (barplot (pragmatic-listener alpha "not_stop" qud-always CG-priors))
; (barplot (pragmatic-listener-world alpha "not_stop" qud-always CG-priors))

; CG prior, QUD stop
; (barplot (pragmatic-listener alpha "not_stop" qud-stop CG-priors))
; (barplot (pragmatic-listener-world alpha "not_stop" qud-stop CG-priors))

; CG prior, QUD start
; (barplot (pragmatic-listener alpha "not_stop" qud-start CG-priors))
; (barplot (pragmatic-listener-world alpha "not_stop" qud-start CG-priors))

; CG prior, QUD never
; (barplot (pragmatic-listener alpha "not_stop" qud-never CG-priors))
; (barplot (pragmatic-listener-world alpha "not_stop" qud-never CG-priors))

; CG uniform prior, QUD now
; (barplot (pragmatic-listener alpha "not_stop" qud-now CG-uniformpriors))
; (barplot (pragmatic-listener-world alpha "not_stop" qud-now CG-uniformpriors))
~~~~
