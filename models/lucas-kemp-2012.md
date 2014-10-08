% Lucas & Kemp 2012

Using a counterfactual model from [here](http://forestdb.org/models/exogenous-counterfactuals.html) and the free parameter *s* and specifics of the model that [Lucas & Kemp (2012)](http://www.psy.cmu.edu/~ckemp/papers/lucask_aunifiedtheoryofcounterfactualreasoning.pdf) tested against people's responses in [Sloman & Lagnado's (2005)](http://onlinelibrary.wiley.com/doi/10.1207/s15516709cog2901_2/pdf) Experiments 2 and 6, we get the same results as they did for the observation-type counterfactuals (at least looking at their graph). This is unsurprising, since our implementations of counterfactuals are basically the same.

~~~~
;;;fold:
;;first we have a bunch of helper code to do meta-transforms.. converts name to 
;;shadow-name and wraps top-level defines
(define (names model)
  (map (lambda (def)
         (if (is-function-definition? def)
             (first (second def))
             (second def)))
       model))

(define (is-function-definition? def)
  (list? (second def)))

(define (shadow-symbol name)
  (string->symbol (string-append "shadow-" name)))

(define (rename expr from-name to-name)
  (cond [(list? expr) (map (lambda (x) (rename x from-name to-name)) expr)]
        [(eq? expr from-name) to-name]
        [else expr]))

(define (shadow-rename expr name)
  (rename expr name (shadow-symbol name)))

(define (shadow-rename-all expr names)
  (if (null? names)
      expr
      (shadow-rename-all (shadow-rename expr (first names))
                         (rest names))))

(define (make-shadow-defines model)
  (define ns (names model))
  (map (lambda (def)
         (if (is-function-definition? def)
             (shadow-rename-all def ns)
             (let ([name (second def)])
               '(define ,(shadow-symbol name) (if (flip eps) 
                                                  ,(shadow-rename-all (third def) ns) 
                                                  ,name)))))
       model))

;;the meaning function constructs a church expression from an utterance. 
;;for 'because it uses quasiquote mojo to dynamically construct the right expression.
;;(in principle this handles embedded "because", but currently expand-because doesn't do 
;;the right thing since the model is a fixed global.)
(define (meaning utt)
  (define (because? u) (if (list? u) (eq? (first u) 'because) false))
  (if (list? utt)
      (if (because? utt)
          (expand-because (map meaning utt))
          (map meaning utt))
      utt))

;;expand an expr with form '(because a b), ie "a because b", into the (hypothesized) 
;;counterfactual meaning:
(define (expand-because expr) 
  (define a (second expr))
  (define b (third expr))
  '(and ,a ,b
        (apply multinomial
               (enumeration-query
                (define eps 0.01)
                ,@(make-shadow-defines model) ;;the shadow model
                (not ,(shadow-rename-all a (names model)))
                (condition (not ,(shadow-rename-all b (names model))))))))

;;listener is standard RSA literal listener, except we dynamically construct the 
;;query to allow complex meanings that include because:
(define listener 
  (mem (lambda (utt qud)
         (eval
          '(enumeration-query
            ,@model
            ,qud
            (condition ,(meaning utt)))))))

;;the speaker is no different from ordinary RSA
(define (speaker val qud) ;;want to communicate val as value of qud
  (enumeration-query
   (define utt (utt-prior))
   utt
   (condition (equal? val (apply multinomial (listener utt qud))))))

;;;

;; make this lower resolution if you want it to run faster
(define (discrete-uniform) (uniform-draw '(0 .2 .4 .6 .8 1)))

(define model 
  '(
    ;; causal strengths
    (define a→b 0.8)
    (define b→c 0.8)

	;; probability goes outside of function
    (define B→a (flip))
    ;; all nodes are deterministic functions 
    (define (a) B→a)

    (define U (discrete-uniform))
    (define (b) (if (a) (<= U a→b) #f))

    (define U2 (discrete-uniform))
    (define (c) (if (b) (<= U2 b→c) #f))
    ))

;; Would Y have happened if we observed that X did not happen?
(define (counterfactual? X Y)
  (eval
 '(enumeration-query
 	;;the actual world
   ,@model
   (define eps 0.23)
   ,@(make-shadow-defines model)
   ;;the shadow model
   ,(shadow-rename-all Y (names model))
   ;; participants are told that A and C are true in the real world and asked to counterfacualize on not B
   (and (a) (c) 
        (not ,(shadow-rename-all X (names model)))))))

(barplot (counterfactual? '(b) '(a))
         "Imagine we observed that B didn't happen. Would A still have happened?")
(barplot (counterfactual? '(b) '(c))
         "Imagine we observed that B didn't happen. Would C still have happened?")

~~~~