#lang racket

(require rackunit
         (for-syntax syntax/parse))

(define-syntax (trace stx)
  (syntax-parse stx
    [(_ n:number)
     #'n]
    [(_ x:id)
     #'x]
    [(_ (f:id e:expr ...))
     #'(print-and-return '(f e ...)
                         (f (trace e) ...))]))

(define (print-and-return expr val)
  (printf "~s = ~s\n" expr val)
  val)

#;
(e ::= id
   | number
   | (id e ...))

(check-equal? (with-output-to-string
                (lambda () (trace 1)))
              "")
(check-equal? (with-output-to-string
                (lambda () (trace 1)))
              "")
(check-equal? (with-output-to-string
                (lambda ()
                  (let ([x 3])
                    (trace x))))
              "")
(check-equal? (with-output-to-string
                (lambda () (trace (+ 1 2))))
              "(+ 1 2) = 3\n")
(check-equal? (with-output-to-string
                (lambda () (trace (+ 2 1))))
              "(+ 2 1) = 3\n")
(check-equal? (with-output-to-string
                (lambda () (trace (+ (* 3 4)
                                     (/ 4 2)))))
              (string-append "(* 3 4) = 12\n"
                             "(/ 4 2) = 2\n"
                             "(+ 12 2) = 14\n"))