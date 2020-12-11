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
     

;; Multiple argument version (not working right now)
#;
(define-syntax (pfsh:run stx)
  (define es (syntax->list stx))
  (define prog (list-ref es 1))
  (define args (list-tail es 2))
  (define prog-str (symbol->string (syntax-e prog)))
  (define args-str (map symbol->string (map syntax-e args)))
  (println stx)
  (println es)
  (println prog)
  (println args)
  (println prog-str)
  (println args-str)
  #`(run #,prog-str #,args-str))

;; Single argument version
#;
(define-syntax (pfsh:run stx)
  (define es (syntax->list stx))
  (define prog (list-ref es 1))
  (define arg (list-ref es 2))
  (define prog-str (symbol->string (syntax-e prog)))
  (define arg-str (symbol->string (syntax-e arg)))
  (println stx)
  (println es)
  (println prog)
  (println arg)
  (println prog-str)
  (println arg-str)
  #`(run #,prog-str #,arg-str))

(pfsh:run wsl.exe ls -l)