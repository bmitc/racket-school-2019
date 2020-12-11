#lang racket

(require (for-syntax syntax/parse)
         syntax/parse/define)


;; ***************************
;; Exercise 1
;; ***************************

(define-simple-macro (define-and-provide (function-name variable -> input-guard output-guard)
                       body)
  (begin (define (function-name variable) body)
         (provide
          (contract-out
           [function-name (-> input-guard output-guard)]))))

(define-and-provide (bigger-string x -> number? string?)
  (number->string (add1 x)))


;;(define-simple-macro (bigger-string2 x))


;; ***************************
;; Exercise 2
;; ***************************

;; Computes f2(f1(x)) or (f2 (f1 x))
(define-simple-macro (fseq2 x f1 f2)
  (f2 (f1 x)))


;; ***************************
;; Exercise 3
;; ***************************

;; (fseq x f1 f2 f3) -> (f3 (f2 (f1 x)))

(define-simple-macro (fseq x functions ...)
  (foldl (lambda (function value) (function value))
         x
         (list functions ...)))

;; One solution
(define-syntax (fseq3 stx)
  (syntax-parse stx
    [(_ val) #'val]
    [(_val f1 fN ...)
     #'(fseq2 (f1 val) fN ...)]))

;; Another solution
(define-simple-macro (fseq4 val f ...)
  (let* ([ans val]
         [ans (f ans)] ...)
    ans))

;; Tests from solution session:
(equal? (fseq 5 add1 number->string string-length)
        (string-length (number->string (add1 5))))
#;(equal? (fseq 2 (list 1) (map add1) (foldr + 0) number->string)
          (number->string (foldr + 0 (map add1 (list 1 2)))))


;; ***************************
;; Exercise 4
;; ***************************

(define-simple-macro (simple-for/and ([element-name seq]) procedure)
  (foldl (lambda (element previous-value) (and (procedure element)))
         #t
         seq))

(define-simple-macro (simple-for/and2 ([element-name seq]) procedure)
  (andmap procedure seq))


;; ***************************
;; Exercise 5
;; ***************************

(define-simple-macro (simple-for/fold ([acc-name init-value])
                                      ([element-name seq])
                                      computation)
  (foldl (lambda (element-name acc-name) computation)
         init-value
         seq))


;; ***************************
;; Exercise 6
;; ***************************

