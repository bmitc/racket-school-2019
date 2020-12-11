#lang racket

;; https://school.racket-lang.org/2019/plan/mon-mor-lecture.html

;; LOP
;;  - glue together external languages
;;  - strings plus their interpreters inside a programming language
;;  - embedded, expression-level "small" languages

;; formatting strings to prepare for printing

"~a :: ~a\n" "hello" 'String

(printf "~a :: ~a\n" "hello" 'String)

(format "~a :: ~a\n" "hello" 'String)

;; printf and format are interpreters for the program
;;   "~a :: ~a\n" "hello" 'String

;; regexp-match is the interpreter

;; strings don't make a good base because they lack context, generate run-time errors,
;; are difficult to compose, etc.

;; match is a language inside Racket. It can escape to itself.
;; match's pattern matching language is EMBEDDED into the language

;; Lisp ('58) -> Scheme ('74) -> Racket ('95)
;; [------macros, s-exp-----]    [('85)-----]
;;                               brought about the ideas:
;; ideas:
;;  (1) language extension            define-syntax
;;  (2) restrictions, constraints     #lang + syntax-parse
;;  (3) fine grained embeddings

#| Examples:
(1) provide contract
(2) #lang typed/racket, #lang datalog
(3) match, define-match-expander
|#

;; syntax-parse can be used for error messages

#|
string of characters -----------> tree ()s are balanced -----------> tree (interpreter can understand)
                          ^                                  ^
                   discover the tree             syntax / eliminate extensions
|#

;; lookup: let-syntax, syntax, syntax-e