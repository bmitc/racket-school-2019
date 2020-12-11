#lang racket

(require "run.rkt"
         racket/port
         (for-syntax syntax/parse))
 
(provide #%module-begin
         (rename-out [pfsh:run #%app]
                     [pfsh:top #%top]
                     [pfsh:define define]
                     [pfsh:datum #%datum]
                     [pfsh:top-interaction #%top-interaction]
                     #;[pfsh:string-append string-append]))
 
(module reader syntax/module-reader
  pfsh)

(begin-for-syntax ; forces body to be defined at compile-time
  (define-syntax-class run-id
    #:datum-literals (< >)
    #:description "an identifier other than > or <"
    (pattern (~and run-id:id (~not >) (~not <)))))
 
(define-syntax (pfsh:run stx)
  (syntax-parse stx
    #:datum-literals (< >)
    [(_ prog arg1 ... > arg2 arg3 arg4 ...)
     (error "> must be followed by only a single identifier")]
    [(_ prog arg ... > ~! name:run-id)
     #'(define name (string-trim (with-output-to-string
                                   (lambda ()
                                     (pfsh:run prog arg ...)))))]
    [(_ prog arg ... < stream:expr)
     #'(with-input-from-string stream
         (lambda ()
           (pfsh:run prog arg ...)))]
    [(_ prog arg ...)
     #`(void (run prog arg ...))]))
 
(define-syntax (pfsh:top stx)
  (syntax-parse stx
    [(_ . sym:id)
     #`#,(symbol->string (syntax-e #'sym))]))

(define-syntax (pfsh:define stx)
  (syntax-parse stx
    [(_ name:id body:expr)
     #'(define name body)]))

#;
(pfsh:define (pfsh:string-append arg1 arg2)
             (string-append arg1 arg2))

(define-syntax (pfsh:datum stx)
  (syntax-parse stx
    [(_ . s:string) #'(#%datum . s)]
    [(_ . other)
     (raise-syntax-error 'pfsh
                         "only literal strings are allowed"
                         #'other)]))
 
(define-syntax (pfsh:top-interaction stx)
  (syntax-parse stx
    [(_ . form) #'form]))