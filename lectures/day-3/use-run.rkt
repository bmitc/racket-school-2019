#lang racket

(require "run.rkt"
         syntax/parse/define)

(provide (rename-out [pfsh:run run]))

(define-simple-macro (pfsh:run prog:id arg:id ...)
  (run (symbol->string 'prog) (symbol->string 'arg) ...))

(pfsh:run where /?)
;(run (symbol->string 'ls) (symbol->string '-l))
;(run "ls" "-l")