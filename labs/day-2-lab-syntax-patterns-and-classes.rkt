#lang racket

(require (for-syntax syntax/parse)
         syntax/parse/define)

(define predicate? (-> symbol? boolean?))

;; ***************************
;; Exercise 10
;; ***************************

;; Add syntax class annotations to your define&provide macro. Write test cases to
;; demonstrate the bad behavior you are protecting against.

(define-simple-macro (define-and-provide (function-name variable -> input-guard output-guard)
                       body)
  #:declare input-guard (expr/c #'(-> symbol? boolean?) #:name "a predicate")
  #:declare output-guard (expr/c #'(-> symbol? boolean?) #:name "a predicate")
  (begin (define (function-name variable) body)
         (provide
          (contract-out
           [function-name (-> input-guard.c output-guard.c)]))))
#;
(define numbers? 3)
#;
(define-and-provide (bigger-string x -> numbers? string?)
  (number->string (add1 x)))
(define-and-provide (bigger-string x -> number? string?)
  (number->string (add1 x)))

;; convert-syntax-error (?)

;; LOOKUP: define/contract


;; ***************************
;; Exercise 11
;; ***************************
