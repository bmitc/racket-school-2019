#lang s-exp "pfsh4.rkt"

(run wsl.exe ls "-1")

(define l (run wsl.exe ls "-1"))
(run wsl.exe wc "-l" < l)