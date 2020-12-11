#lang pfsh2

(wsl.exe ls -l)

;0 ; -> pfsh: literals not allowed in: 0

"-1"

(define (double x)
  (string-append x x))

(double "a ")

(wsl.exe wc -w < (double "a "))

(define (triple z)
  (string-append z (string-append z z)))

double