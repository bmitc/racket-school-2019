#lang racket

(require (for-syntax syntax/parse)
         syntax/parse/define
         racket/stxparam)


;; ***************************
;; Exercise 16
;; ***************************

(define-simple-macro (define-with-return (name args ...) body)
  #:with return (datum->syntax #'body 'return)
  (define (name args ...)
    (let/ec return
      body)))

(define-with-return (multiply a b)
  (for ([i (in-naturals)])
    (when (equal? i a)
      (return (* i b)))))

(define-syntax-parameter return
  (λ (stx) (raise-syntax-error 'return "Illegal use of return" stx)))

(define-simple-macro (define-with-return2 (name args ...) body)
  (define (name args ...)
    (let/ec return-hidden
      (syntax-parameterize ([return (make-rename-transformer #'return-hidden)])
        body))))

(define-with-return2 (multiply2 a b)
  (for ([i (in-naturals)])
    (when (equal? i a)
      (return (* i b)))))

;; LOOKUP: make-rename-transformer


  ;; ***************************
  ;; Exercise 16
  ;; ***************************

(define-simple-macro (while ([iterate start stop-expression]) body)
  (let/ec break-to
    (

#;
(while #t
  (define input (read-line))
  (unless (regexp-match #px"please" input)
    (printf "You didn't say please\n")
    (continue))
  (when (regexp-match #px"quit" input)
    (break)))