#lang racket
(require (for-syntax syntax/parse racket/syntax racket/list)
         rackunit)

(define DIE '(1 2 2 3 3 4)) ; sides of 1 die
(define POINTS '(1 2 3 4 5 6 7)) ; possible point values of cards
(define NUMSIDES (length DIE))

(define (print-header numdice numpossible)
  (printf "\n**number of dice = ~a**, " numdice)
  (printf "total possibilities = ~a\n" numpossible)
  (display "| Want | Chances | % Prob | 1 pt |")
  (for/list ([p (cdr POINTS)]) (printf " ~a pts |" p))
  (newline)
  (display "| :---: | ---: | ---: |")
  (for ([p POINTS]) (display " --- |"))
  (newline))
  

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
           (define prob-no (- 1 prob))
           (printf "| >= ~a | ~a / ~a | ~a% |"
                   (~a i #:min-width 2)
                   (~r chances #:min-width $n)
                   NUMPOSSIBLE
                   (~r (exact->inexact (* 100 prob))
                       #:precision '(= 2) #:min-width 6))
           (for ([pts POINTS])
             (printf "~a |"
                     ; expected pts per card = expected pts / expected cards
                     (~r (exact->inexact
                          (/ (* prob pts)
                             ;; num cards = NUMDICE
                             (+ (* prob NUMDICE) prob-no)))
                         #:precision '(= 2) #:min-width 4)))
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
    (printf "Each die has sides: 1, 2, 2, 3, 3, 4\n")
    (gen-all #:max-dice 6)))
