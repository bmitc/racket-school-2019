#lang s-exp "pfsh-afternoon.rkt"

(wsl.exe ls -l)

;0 ; -> pfsh: literals not allowed in: 0

"-1"

(define (double x)
  (string-append x x))

(double "a ")

(wsl.exe wc -w < (double "a "))