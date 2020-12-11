#lang racket

(require "run.rkt"
         (for-syntax syntax/parse))
 
(provide #%module-begin
         (rename-out [pfsh:run run])
         (rename-out [pfsh:define define]))
 
(define-syntax (pfsh:run stx)
  (syntax-parse stx
    [(_ prog:id arg:id ... (~datum <) defined:id)
     #`(void (with-input-from-string defined (lambda ()
                                               (run (as-string prog)
                                                    (as-string arg) ...))))]
    [(_ prog:id arg:id ...)
     #`(void (run (as-string prog) (as-string arg) ...))]))

;; maybe edit this such that the first case calls pfsh:run instead of run
 
(define-syntax (as-string stx)
  (syntax-parse stx
    [(_ sym:id)
     #`#,(symbol->string (syntax-e #'sym))]))

(define-syntax (pfsh:define stx)
  (syntax-parse stx
    [(_ name:id body:expr)
     #`(define name (string-trim (with-output-to-string (lambda () body))))]))