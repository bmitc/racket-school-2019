#lang racket/base

(require "time.rkt"
         (for-syntax racket/base
                     syntax/parse)) ; imports syntax-parse

(provide time-it)

#;
(define-syntax (time-it stx)
  (println stx)) ;; unbound without for-syntax since define-syntax is a bridge to compile time, but
;; println is only available at run-time with racket/base

#;
(println 5) ;; this works without for-syntax

;; This works, but is painful
#;
(define-syntax (time-it stx)
  (define es (syntax->list stx))
  (define body (list-ref es 1))
  (datum->syntax #'here
                 (list #'thunk-time-it
                       (list #'lambda
                             #'()
                             body))))

;; This uses quasi-syntax just like quasi-quote
#;
(define-syntax (time-it stx)
  (define es (syntax->list stx))
  (define body (list-ref es 1))
  #`(thunk-time-it (lambda () #,body)))


(define-syntax (time-it stx)
  (syntax-parse stx
    [(_ body)
     #`(thunk-time-it (lambda () body))]))

(time-it (+ 4 1))
(thunk-time-it (lambda () (+ 4 1)))