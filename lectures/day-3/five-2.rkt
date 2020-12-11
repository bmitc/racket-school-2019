#lang racket/base

(require (for-syntax racket/base))

(provide (rename-out [define-five define]))

#;
(define-syntax (define-five stx)
  (define es (syntax->list stx))
  (define name (list-ref es 1))
  (define body (list-ref es 2))
  ;(println es)
  #`(define #,name 5))

;; Insdie define-syntax, we are at compile-time

(define-syntax (define-five stx)
  (define es (syntax->list stx))
  (define name (list-ref es 1))
  (define body (list-ref es 2))
  (printf "Assuming ~s produces 5\n" (syntax->datum body)) ; syntax->datum convers syntax object to list of datums
  #`(define #,name 5))

;; syntax-e is one step of conversion to datums
;; syntax->datum recurses through the entire syntax object and returns list of datums