#lang racket

(require syntax/parse/define) ; imports define-simple-macro

;(provide define-five) ; this would be if the use module called define-five

(provide (rename-out [define-five define]))

(define-simple-macro (define-five name:id body:expr)
  #:do [(displayln "This is compile time")] ; this #:do attribute executes its contents at compile time
  (begin (printf "Assuming ~s is five\n" 'body) ; 'body prints the expression as given, not its value
         (define name 5))) ; define name as 5 no matter what

;; define-simple-macro generates an expression at compile time that only gets evaluated
;; at run-time, so it's contents are executed at run-time.