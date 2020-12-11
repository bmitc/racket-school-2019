#lang racket/base

(require turnstile/base)

(define-base-types Bool Int)
; You get:
;  - Int
;  - Int? : syntax? -> boolean?    <--- contract
;  - Int-
;  - ~Int

(define-type-constructor -> #:arity >= 1)
; You get:
;  - ->
;  - ->?
;  - ->-
;  - ~->

; (begin-for-syntax (displayln (Int? #'Int))) ; -> #f     (because #'Int is just the thing you type
                                              ;            in your program right now)

