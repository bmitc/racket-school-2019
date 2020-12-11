#lang racket/base

(require "run.rkt"
         (for-syntax racket/base
                     syntax/parse)) ; syntax-parse

(provide (rename-out [pfsh:run run])
         #%module-begin) ; re-provides #% from racket/base

;; This is working
(define-syntax (pfsh:run stx)
  (syntax-parse stx
    [(_ prog:id arg:id ...)
     (define prog-str (symbol->string (syntax-e #'prog)))
     (define args-str (map symbol->string (map syntax-e
                                               (syntax->list #'(arg ...)))))
     #`(run #,prog-str #,@args-str)]))