#lang racket

(define-enum animal
  anteater
  dumbo
  snake)

(define (food-of a)
  (enum-case animal a
             [anteater 'ants]
             [dumbo 'peanuts]
             [snake 'rats]))

(define-simple-macro
  (enum-case a b cases ...)
  (case b cases ...))

(define-s)