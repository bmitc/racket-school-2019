#lang racket

(require "run.rkt"
         (for-syntax syntax/parse))
 
(provide #%module-begin
         #%top-interaction ; provides some interaction, the REPL
         (rename-out [pfsh:top #%top])
         (rename-out [pfsh:datum #%datum]) ; overrides handling of literals
         ;(rename-out [pfsh:run #%app]) ; instead of (run ls -l) we use (ls -l) now
         (rename-out [pfsh:app #%app])
         (rename-out [pfsh:define define])
         string-append)

(define-syntax (pfsh:app stx)
  (syntax-parse stx
    [(_ name:id arg ...)
     #:when (identifier-binding #'name)
     #'(#%app name arg ...)]
    [(_ other arg ...)
     #'(pfsh:run other arg ...)]))
 
(define-syntax (pfsh:run stx)
  (syntax-parse stx
    #:datum-literals (<)
    [(_ prog:id arg:id ... < stream)
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
    [(_ stream:id expr)
     #'(define stream (with-output-to-string
                        (lambda () expr)))]
    [(_ (proc:id arg:id ...) body)
     #'(define (proc arg ...) body)]))

(define-syntax (pfsh:datum stx)
  (syntax-parse stx
    [(_ . s:str) #'(#%datum . s)]
    [(_ . other)
     (raise-syntax-error 'pfsh
                         "literals not allowed"
                         #'other)]))