#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/list ; imports list
         syntax/parse/define) ;imports define-simple-macro

;; for-syntax imports things for compile time (?)

(define (factorial n) 1)
(define (fibonacci n) 1)

;; Macros can:
;;  - help abstract patterns
;;  - create hidden communication channels that two places of the program can use

(define-syntax forty-three
  (lambda (stx)
    (syntax-parse stx
      #;[pattern
         answers]
      [(_ one-thing)
       #; one-thing ;; ---> illegal use of pattern outside of template
       (syntax one-thing)])))

;; syntax "unwraps" the pattern variable from the internal syntax-parse pattern language

(+ (forty-three 1)
   (forty-three 3))

;; define-simple-macro is a simple abstraction over the common use of
;; define-syntax and syntax-parse

(define-simple-macro (simpler-forty-three one-thing)
  one-thing)

(+ (simpler-forty-three 1)
   (simpler-forty-three 4))

;; LOOKUP begin0

(define (bad-time-it computation)
  (define before (current-inexact-milliseconds))
  (begin0 (computation)
          (displayln (- (current-inexact-milliseconds) before))))
(bad-time-it (lambda () (factorial (fibonacci 10))))

(define-simple-macro (time-it computation)
  (bad-time-it (lambda () computation)))
(time-it (factorial (fibonacci 10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (burn-them-all l)
  (cond [(empty? l)
         empty]
        [else
         (cons (string-append "Burnt " (first l))
               (burn-them-all (rest l)))]))
(burn-them-all (list "Rickard" "Brandon"))

(map (λ (s) (string-append "Burnt " s))
     (list "Rickard" "Brandon"))

(define-simple-macro (simple-for/list0 ([elem-name seq]) computation)
  (map (λ (elem-name) computation) seq))
 
(simple-for/list0
 ([s (list "Rickard" "Brandon")]) ; see note below
 (string-append "Burnt " s))

;; this syntax is common to associate a list of bindings and their value
;; [binding value]
;; ([binding value] ...) list of these

;; lookup: deforestation, fold fusion

;; inside define-simple-macro pattern, you can use ... to match 0 or more things to the left of it
;; inside the template, take the thing on the left and repeat

;; LOOKUP generate-temporaries, ..., with-syntax