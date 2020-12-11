#lang s-exp "pfsh3.rkt"

(define l (run wsl.exe ls))

l

(run wsl.exe wc -l < l)