#lang racket

(require syntax/parse/define) ; for define-simple-macro, slashes are for the install tree
                              ; not directories

(provide thunk-time-it)

(define (thunk-time-it thunk)
  (define start (current-inexact-milliseconds))
  (define answer (thunk))
  (printf "~s msec\n" (- (current-inexact-milliseconds)))
  answer)

#;
(define-simple-macro (time-it expr)
  (thunk-time-it (lambda () expr)))