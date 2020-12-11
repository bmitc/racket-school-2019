#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse)
         racket/stxparam
         racket/math
         syntax/parse/define)

;; We are at the point such that are moving from simple language extensions to building languages.
;; But languages need features such that they "conspire" behind the user's back. The macros need to
;; talk and communicate to do this.

(define-simple-macro (define-a-name name:id)
  (define name 42))

(define-a-name x)
x
(define-simple-macro (define-some-names name:id ...)
  (begin (define name 1) ...))
(define-some-names y z)
y z

(let ([tmp 7])
  (+ tmp
     (let ([tmp 8])
       tmp)
     tmp))

(let ([tmp 7])
  (+ tmp
     (let ([let 8])
       let)
     tmp))

(let ([tmp 7])
  (+ tmp
     (let ([define-some-names 8])
       define-some-names) ; here, define-some-names is 8, elsewhere it is the macro definition
     tmp))

(define-simple-macro (weirdo body)
  (let ([tmp 8])
    (displayln "inside macro")
    ;(displayln let) ; <- this is the real let and not #f
    (displayln tmp)
    body))

(let ([tmp 7])
  (+ tmp
     #;(let ([tmp 8]) tmp)
     (weirdo tmp) ; this temp is being protected from the internal definition of weirdo (mouse over to see)
     tmp)) ; -> 21

(let ([tmp 7])
  (+ tmp
     #;(let ([tmp 8]) tmp)
     (let ([let #f])
       (displayln let)
       (weirdo tmp)) ; this temp is being protected from the internal definition of weirdo (mouse over to see)
     tmp)) ; -> 21

(define-simple-macro (with-eight name body)
  (let ([name 8]) body))
(let ([tmp 7])
  (+ tmp
     (with-eight tmp tmp)
     tmp)) ; -> 22

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These its don't know about each other.
#;
(define-simple-macro (define-it val:expr)
  (define it val))
#;
(define-it 5)
#;
(+ it it)

#;
(define-simple-macro (define-it val:expr)
  #:do ((displayln (syntax-column #'val)))
  #:with some-name-that-no-one-knows (datum->syntax #'val 'it) ;could use "this-syntax" instead of #'val to get where the macro is called
  (begin (define some-name-that-no-one-knows val)
         (displayln val)))
#;
(define-it 5)
#;
(+ it it)

#;
(define-simple-macro (define-like base:id val:expr)
  #:with some-name-that-no-one-knows
  #;(datum->syntax #'base
                   (string->symbol
                    (string-append (symbol->string
                                    (syntax->datum #'base))
                                   "-like")))
  (format-id #'base "~a-like" #'base)
  (define some-name-that-no-one-knows val))
#;
(define-like x 5)
#;
( + x-like x-like)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(displayln "syntax-parameter")

(define-syntax-parameter it
  (λ (stx) (raise-syntax-error 'it "Illegal outside with-it" stx)))
(define-simple-macro (with-it val:expr body:expr)
  (let ([this-it val])
    (syntax-parameterize ([it (λ (stx) #'this-it)])
      body)))

;; Actually returns 14
(let ([it 7])
  (with-it 5
    (+ it it))) ;; <--- bound inside to 5
;; it ;; <--- bound, but illegal

;; Returns 10
(let ([it-from-another-place 7])
  (with-it 5
    (+ it it))) ;; <--- bound inside to 5
;; it ;; <--- bound, but illegal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(displayln "datums")

(define-syntax (EOF stx)
  (raise-syntax-error 'EOF "Illegal outside of with-ignored"))

(define-simple-macro (with-ignored real ... (~literal EOF) ignored ...)
  (begin real ...))

;; ~datum treats EOF as a datum such that we look just for EOF in the pattern match

(let-syntax ([outer-EOF (make-rename-transformer #'EOF)])
  (let ([EOF 5])
    (with-ignored
        (define x 6)
      (+ x EOF)
      outer-EOF
      x y z
      (/ 1 0)
      (error 'xxx))))