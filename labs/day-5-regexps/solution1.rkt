#lang racket

(require syntax/parse/define)

(provide (all-defined-out))

(define-simple-macro (r^define regexp-id r)
  #'(void))

(define-simple-macro (r^match r string-expr)
  #'#\a)

(define-simple-macro (r^range regexp-range-char1 regexp-range-char2)
  #'#\a)