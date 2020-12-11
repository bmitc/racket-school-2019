#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse)
         racket/stxparam
         racket/math
         syntax/parse/define)

(define-syntax-parameter this
  (λ (stx) (raise-syntax-error 'this "Illegal outside classy" stx)))
(define-syntax-parameter update
  (λ (stx) (raise-syntax-error 'update "Illegal outside classy" stx)))
 
(define-syntax field
  (λ (stx) (raise-syntax-error 'field "Illegal outside classy" stx)))
(define-syntax method
  (λ (stx) (raise-syntax-error 'method "Illegal outside classy" stx)))

(begin-for-syntax
  (define-splicing-syntax-class classy-clauses
    #:literals (field method)
    #:attributes ([fields 1]
                  [methods 1]
                  [methods-args 2]
                  [methods-body 2])
    (pattern (~seq)
             #:with (fields ...) #'()
             #:with (((methods methods-args ...) methods-body ...) ...)
             #'())
    (pattern (~seq (field f:id) more:classy-clauses)
             #:with (fields ...) #'(f more.fields ...)
             #:with (((methods methods-args ...) methods-body ...) ...)
             #'(((more.methods more.methods-args ...) more.methods-body ...) ...))
    (pattern (~seq (method (m:id ma:id ...) mb ...+) more:classy-clauses)
             #:with (fields ...) #'(more.fields ...)
             #:with (((methods methods-args ...) methods-body ...) ...)
             #'(((m ma ...) mb ...)
                ((more.methods more.methods-args ...) more.methods-body ...) ...))))

(define-simple-macro (classy name:id cs:classy-clauses)
  #:with ((field-kw (field-cons-arg ...)) ...)
  #;#'([#:x (#:x x)]
       [#:y (#:y y)])
  (for/list ([f (in-list (syntax->list #'(cs.fields ...)))])
    (define f-kw (string->keyword (symbol->string (syntax->datum f))))
    (list f-kw (list f-kw f)))
  (begin
    (struct rep (cs.fields ...))
    (define (name field-cons-arg ... ...)
      #;(posn #:x x #:y y)
      (rep cs.fields ...)
      #;(rep x y))))
  
(classy
 posn
 (field x)
 (field y)
 (method (magnitude)
         (+ x y))
 (method (distance-from p2)
         (sqrt (+ (sqr (- x (posn.x p2))) (sqr (- y (posn.y p2))))))
 (method (slide dx)
         (update #:x (+ x dx)))
 (method (adjust dy)
         (update #:y (+ y dy)))
 (method (from-origin)
         (define o (posn #:x 0 #:y 0))
         (this . distance-from . o)))