#lang racket

(require "run.rkt"
         (for-syntax syntax/parse))
 
(provide #%module-begin
         (rename-out [pfsh:run run])
         (rename-out [pfsh:define define]))

(begin-for-syntax ; forces body to be defined at compile-time
  (define-syntax-class run-arg
    #:attributes (as-string) ; defines attribute as-string. see usage later.
    (pattern arg:id
             #:with as-string #`(quote #,(symbol->string (syntax-e #'arg))))
    (pattern arg:string
             #:with as-string #'(quote arg))))
 
(define-syntax (pfsh:run stx)
  (syntax-parse stx
    [(_ prog:run-arg arg:run-arg ... (~datum <) defined:id)
     #`(void (with-input-from-string defined (lambda () (run prog.as-string
                                                             arg.as-string ...))))]
    [(_ prog:run-arg arg:run-arg ...)
     #`(void (run prog.as-string arg.as-string ...))]))

#;
(define-syntax (as-string stx)
  (syntax-parse stx
    [(_ sym:id)
     #`#,(symbol->string (syntax-e #'sym))]))

(define-syntax (pfsh:define stx)
  (syntax-parse stx
    [(_ name:id body:expr)
     #`(define name (string-trim (with-output-to-string (lambda () body))))]))