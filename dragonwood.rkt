#lang racket
(require (for-syntax syntax/parse racket/syntax racket/list)
         rackunit)

(define DIE '(1 2 2 3 3 4)) ; sides of 1 die
(define POINTS '(1 2 3 4 5 6 7)) ; possible point values of cards
(define NUMSIDES (length DIE))

;; prints header of each table
(define (print-header numdice numpossible)
  (printf "\n**number of dice = ~a**, " numdice)
  (printf "total possibilities = ~a\n" numpossible)
  (display "| Want | Chances | % Prob | targeting: | 1 pt |")
  (for/list ([p (cdr POINTS)]) (printf " ~a pts |" p))
  (newline)
  (display "| :---: | ---: | ---: | --- |")
  (for ([p POINTS]) (display " --- |"))
  (newline))

;; converts exact pts value to 2 decimal str
(define (pts->str pts)
  (~r (exact->inexact pts) #:precision '(= 2)))

;; converts prob in the form of exact fraction to [0,100]%
;; with precision 2
(define (prob->str prob [width 1])
  (~r (exact->inexact (* 100 prob)) #:precision '(= 2) #:min-width width))

;; calculate expected pts per card when miss penalty is `cost`
;; expected pts per card = expected pts / expected cards
(define (pts/card prob pts cards [cost 1])
  (/ (* prob pts) ; no pts for miss
     (+ (* prob cards) (* (- 1 prob) cost))))

;; this must be macro to generate the `$n` # of for* args
(define-syntax generate-probabilities
  (syntax-parser
    [(_ #:num-dice $n:exact-nonnegative-integer)
     #:with (d ...) (generate-temporaries (make-list (syntax-e #'$n) 0))
     #'(begin
         (define NUMDICE $n)
         (define NUMPOSSIBLE (expt NUMSIDES NUMDICE))
         
         (define rolls (for*/list ([d DIE] ...) (+ d ...)))

         (check-equal? (length rolls) NUMPOSSIBLE)

         ;; possible sums: [NUMDICE*1,NUMDICE*4]
         (print-header NUMDICE NUMPOSSIBLE)
         (for ([i (in-range (* NUMDICE 1) (add1 (* NUMDICE 4)))])
           (define chances (count (curry <= i) rolls))
           (define prob (/ chances NUMPOSSIBLE))
           (printf "| >= ~a | ~a / ~a | ~a% | *pts / card*: |"
                   (~a i #:min-width 2)
                   (~r chances #:min-width $n)
                   NUMPOSSIBLE
                   (prob->str prob))
           (for ([pts POINTS])
             ;; num cards = NUMDICE; miss costs 1 card
             (printf " ~a |" (pts->str (pts/card prob pts NUMDICE))))
           (newline)))]))
                   

;; must be macro to generate literal args for generate-probabilities
(define-syntax gen-all
  (syntax-parser
    [(_ #:max-dice $maxn:exact-nonnegative-integer)
     #:with (n ...) (datum->syntax #'$maxn (build-list (syntax-e #'$maxn) add1))
     #'(begin (generate-probabilities #:num-dice n) ...)]))

(with-output-to-file "README.md" #:exists 'replace
  (λ ()
    (printf "# Dice probabilities for the game Dragonwood\n\n")
    (printf "Each die has sides: 1, 2, 2, 3, 3, 4\n\n")
    (let ([dragon-prob (/ 13 36)])
      (printf "Dragon Spell = ~a% to get 6 or 7 pts = ~a or ~a pts/card\n"
              (prob->str dragon-prob)
              (pts->str (pts/card dragon-prob 6 3 2))
              (pts->str (pts/card dragon-prob 7 3 2))))
    (gen-all #:max-dice 6)))
)
