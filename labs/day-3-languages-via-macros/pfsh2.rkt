#lang racket

(require "run.rkt"
         (for-syntax syntax/parse))
 
(provide #%module-begin
         (rename-out [pfsh:run run])
         (rename-out [pfsh:define define]))
 
(define-syntax (pfsh:run stx)
  (syntax-parse stx
    [(_ prog:id arg:id ...)
     #`(void (run (as-string prog) (as-string arg) ...))]))
 
(define-syntax (as-string stx)
  (syntax-parse stx
    [(_ sym:id)
     #`#,(symbol->string (syntax-e #'sym))]))

(define-syntax (pfsh:define stx)
  (syntax-parse stx
    [(_ name:id body:expr)
     #`(define name (string-trim (with-output-to-string (lambda () body))))]))

#;
(define-syntax (pfsh:define stx)
  (syntax-parse stx
    [(_ name:id body:expr)
      (datum->syntax (list #'define #'name #'body))]))