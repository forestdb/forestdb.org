---
layout: model
title: Tic-Tac-Toe
model-status: code
model-category: Reasoning about Reasoning
model-tags: game theory, planning
---

Each player chooses actions by sampling an action, simulating the
game until the end, and choosing actions in proportion to how
likely they are to lead to a successful outcome at the end of the
game. This mental simulation includes reasoning about both players'
reasoning at all future game steps, including their reasoning about
the other player.

    (define (valid-move? move state)
      (equal? (list-ref (list-ref state (first move))
                        (second move))
              0))
    
    (define action-prior-dist
      (mem (lambda (state)
             (enumeration-query
              (define action (list (sample-integer 3) (sample-integer 3)))
              action
              (valid-move? action state)))))
    
    (define (action-prior state)
      (apply multinomial (action-prior-dist state)))
    
    (define (transition state action player)
      (map (lambda (row i)
             (map (lambda (s j)
                    (if (equal? (list i j) action)
                        player
                        s))
                  row
                  (iota (length row))))
           state
           (iota (length state))))
    
    (define (diag1 state)
      (map (lambda (x i) (list-ref x i))
           state (iota (length state))))
    
    (define (diag2 state)
      (let ([len (length state)])
        (map (lambda (x i) (list-ref x (- len (+ i 1))))
             state (iota len))))
    
    (define (win? player state)
      (let ([check (lambda (elts) (all (map (lambda (x) (eq? x player)) elts)))])
        (any (map check
                  (list (first state) (second state) (third state)
                        (map first state) (map second state) (map third state)
                        (diag1 state) (diag2 state))))))
    
    (define (flatten lst)
      (apply append lst))
    
    (define (terminal? state)
      (all (map (lambda (x) (not (equal? x 0)))
                (flatten state))))
    
    (define (other-player player)
      (if (eq? player 'x) 'o 'x))
    
    (define (draw? outcome)
      (and (not (win? 'x outcome))
           (not (win? 'o outcome))))
    
    (define (exp-utility outcome player)
      (cond [(win? player outcome) 1.0]
            [(draw? outcome) 0.1]
            [else 0.01]))
    
    (define (sample-action state player)
      (apply multinomial (sample-action-dist state player)))
    
    (define sample-action-dist
      (mem 
       (lambda (state player)
         (enumeration-query
          (define action (action-prior state))
          (define outcome (sample-outcome state action player))
          action
          (flip (exp-utility outcome player))))))
    
    (define (sample-outcome state action player)
      (let ([next-state (transition state action player)])
        (if (terminal? next-state)
            next-state
            (let ([next-player (other-player player)])
              (sample-outcome next-state
                              (sample-action next-state next-player)
                              next-player)))))
    
    (define start-state
      '((0 o 0)
        (o x x)
        (0 o 0)))
    
    (barplot (sample-action-dist start-state 'x))

References:

- Cite:Stuhlmueller2013aa
