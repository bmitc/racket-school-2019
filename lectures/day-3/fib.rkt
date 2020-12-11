#lang racket

(require "time.rkt")

(define (fib n)
  (if (or (= n 0) (= n 1))
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(thunk-time-it (lambda () (fib 30)))