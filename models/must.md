---
layout: model
title: Must
model-status: code
model-language: church
---
        
        ; Rationality parameter. Higher rationality resut in larger differences
        (define alpha 5)
        
        ;; Helper functions
        (define (power lst alpha) (map (lambda (x) (pow x alpha)) lst))
        
        (define (seq start distance end)
          (if (> (+ start distance) end) (list start)
              (append (list start) (seq (+ start distance) distance end))))
        
        ; Prior on whether or not it is raining in the world. Set to be 0.5
        ; because the listener has maximal uncertainty.
        (define (rain-prior) (flip 0.5))
        
        
        ; The probabilities of raining that we consider in the speaker's
        ; belief distribution (0 means speaker believes there's a 0% of rain; 0.4 means
        ; speaker believes there's a 40% of rain). 
        (define probrains '(0.2 0.4 0.6 0.8 1))
        
        ; Given the evidence that the speaker has, the speaker's beliefs
        ; about different chances of rain. P(beliefs about chance of rain | evidence) is empirically measured.
        (define (evidence-strength evidence)
          (case evidence
                
                (('pos-strong) (multinomial probrains '(0.001 0.001 0.03225806 0.09677419 0.87096774)))
                (('pos-medium) (multinomial probrains '(0.01492537 0.04477612 0.23880597 0.40298507 0.29850746)))
                (('pos-weak) (multinomial probrains '(0.001 0.1612903 0.4838710 0.2258065 0.1290323)))
                (('neg-strong) (multinomial probrains '(0.87096774 0.09677419 0.03225806 0.001 0.001)))
                (('neg-medium) (multinomial probrains '(0.29850746 0.40298507 0.23880597 0.04477612 0.01492537)))
                (('neg-weak) (multinomial probrains '(0.1290323 0.2258065 0.4838710 0.1612903 0.001)))
                )
          
          )
        
        
        ; Evidences: set of candidate evidences for the rain. A speaker may have strong/medium/weak positive evidence
        ; or strong/medium/weak negative evidence that it's raining.
        (define evidences (list 'pos-strong 'pos-medium 'pos-weak
                                'neg-strong 'neg-medium 'neg-weak))
        
        
        ; Given whether it's raining in the world, how likely it is
        ; that the speaker sees strong/medium/weak positive evidence
        ; or strong/medium/weak negative utterance.
        ; These priors can also be empirically measured, althoough here we just made them up
        ; and assumed that the probability of seeing positive evidence of any kind 
        ; is uniformly high given rain, and the probability of seeing negative evidence
        ; of any kind is uniformly low given no rain.
        
        (define (evidence-prior rain?)
          (case rain?
                ((#t) (multinomial evidences '(0.3 0.3 0.3 0.01 0.01 0.01)))
                ((#f) (multinomial evidences '(0.01 0.01 0.01 0.3 0.3 0.3)))))
        
        
        ; The speaker can say "p" or "must p" 
        (define utterances '(bare must))
        
        ; It's less costly to say "p" than to say "must p"
        (define costs '(0.1 3))
        
        (define (utterance-prior)
          (multinomial utterances
                       (map (lambda (utterance-cost) (exp (- utterance-cost)))
                            costs)))
        
        ; The thetas for "p" and "must p" are drawn uniformly
        (define (theta-prior)
          (uniform-draw '(0 0.2 0.4 0.6 0.8 1)))
        
        
        
        ; Semantics of "p" and "must p"
        (define (meaning utterance)
          (case utterance
                (('bare) '(> probrain theta-bare))
                (('must) '(> probrain theta-must))
                ))
        
        
        (define literal-listener
          (mem
           (lambda (utterance theta-bare theta-must)
             (enumeration-query
              (define probrain (uniform-draw probrains))
              probrain
              (eval (meaning utterance))))))
        
        (define speaker
          (mem
           (lambda (evidence belief theta-bare theta-must)
             (enumeration-query
              (define utterance (utterance-prior))
              (define interpretation
                (apply multinomial
                       (list (first (literal-listener utterance 
                                                      theta-bare 
                                                      theta-must
                                    ))
                             (power (second (literal-listener utterance 
                                                              theta-bare 
                                                              theta-must 
                                            )) alpha))))
              utterance
              (equal? interpretation belief)))))
        
        (define pragmatic-listener
          (mem
           (lambda (utterance)
             (enumeration-query
              (define theta-bare (theta-prior))
              (define theta-must (theta-prior))
              (define rain? (rain-prior))
              (define evidence (evidence-prior rain?))
              (define probrain (evidence-strength evidence))
              ;;; All of the things the pragmatic listener can infer: probability that it's raining (rain?)
              ;;; The speaker's belief distribution about chances of rain (probrain)
              ;;; The speaker's evidence distribution (evidence)
              rain?
              ;probrain
              ;evidence
              (equal? (apply multinomial (speaker evidence probrain theta-bare  
                                                  theta-must
                                         )) utterance)))))
        
        (barplot (pragmatic-listener 'bare))
        (barplot (pragmatic-listener 'must))
        ;(list (pragmatic-listener 'rain) (pragmatic-listener 'must))
