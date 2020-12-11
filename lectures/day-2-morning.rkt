#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     syntax/parse)
         syntax/parse/define)

;; The contents of "begin" get flattened out in Racket.
;; This allows you to "begin" a bunch of things, like "define" inside a macro, which
;; are then returned and visible outside of the begin.

;; Doesn't work for (simple-for/list ([x 5]) (add1 x))
#;
(define-simple-macro (simple-for/list ([elem-name seq]) computation)
  (map (lambda (elem-name) computation) seq))

;; This doesn't work properly because seq gets evaluated multiple times, so it doesn't
;; protect against the expected type of seq.
#;
(define-simple-macro (simple-for/list ([elem-name seq]) computation)
  (begin (unless (list? seq)
           (error 'simple-for/list "Not given a list: ~e" seq))
         (map (lambda (elem-name) computation) seq)))

;; Better, since it evaluates seq just once.
#;
(define-simple-macro (simple-for/list ([elem-name seq]) computation)
  (let ([seq-name seq])
    (unless (symbol? 'elem-name)
      (error 'simple-for/list "Not given an identifier ~e" elem-name))
    (unless (list? seq-name)
      (error 'simple-for/list "Not given a list: ~e" seq-name))
    (map (lambda (elem-name) computation) seq)))

;; Better error generation and reporting in DrRacket
#;
(define-simple-macro (simple-for/list ([elem-name seq]) computation)
  #:do [(unless (identifier? #'elem-name)
          (raise-syntax-error
           'simple-for/list "Not given an identifier"
           this-syntax #'elem-name))]
  (let ([seq-name seq])
    (unless (list? seq-name)
      (error 'simple-for/list "Not given a list: ~e" seq-name))
    (map (lambda (elem-name) computation) seq)))

#;
(define-syntax (simple-for/list stx)
  (syntax-parse stx
    [(_ ([elem-name seq]) computation)
     #:do [(unless (identifier? #'elem-name)
             (raise-syntax-error
              'simple-for/list "Not given an identifier"
              stx #'elem-name))]
     #'(let ([seq-name seq])
         (unless (list? seq-name)
           (error 'simple-for/list "Not given a list: ~e" seq-name))
         (map (lambda (elem-name) computation) seq))]
    [(_ anything ...)
     #'42]))

#;
(define-syntax (simple-for/list stx)
  (syntax-parse stx
    [(_ ([elem-name seq]) computation)
     #:declare elem-name identifier ; identifier is the name of a syntax class
     ;; #:fail-unless (identifier? #'elem-name)
     ;; "Not given an identifier"
     #'(let ([seq-name seq])
         (unless (list? seq-name)
           (error 'simple-for/list "Not given a list: ~e" seq-name))
         (map (lambda (elem-name) computation) seq))]))

#;
(define-syntax (simple-for/list stx)
  (syntax-parse stx
    [(_ ([elem-name:id seq]) computation) ;; left of : is a name used later, right of : is a syntax class shortened name
     ;; #:declare elem-name identifier
     ;; #:fail-unless (identifier? #'elem-name)
     ;; "Not given an identifier"
     #'(let ([seq-name seq])
         (unless (list? seq-name)
           (error 'simple-for/list "Not given a list: ~e" seq-name))
         (map (lambda (elem-name) computation) seq))]))

#;
(simple-for/list ([x (list 1 2)]) (add1 x))

;; LOOKUP: ctrl-d, ctrl-e


;; ***************************
;; Round 2
;; ***************************

#;
(define-simple-macro (simple-for/list ([elem-name:id seq]) computation)
  #:declare seq (expr/c #'list?)
  (map (lambda (elem-name) computation) seq))

#;
(define-simple-macro (simple-for/list ([elem-name:id seq]) computation)
  #:declare seq (expr/c #'list?)
  (map (lambda (elem-name) computation)
       (contract list? seq)))

(define-simple-macro (simple-for/list ([elem-name:id seq]) computation)
  #:declare seq (expr/c #'list?)
  (map (lambda (elem-name) computation)
       seq.c)) ;; calls a field on the expr/c syntax class

#;
(simple-for/list
 ([x 5 #;(list 1 2)])
 (add1 x))

(simple-for/list ([x (list 1 2)]) (add1 x))

;; There is some ordering with macros. For example, put the test expression above and you'll
;; get an error: "simple-for/list: bad syntax in: simple-for/list"
;; If you can, sequentialize your macros to avoid such problems. But, using the higher level
;; tools and organization (like modules), this is apparently rarely a problem.


;; ***************************
;; Round 2
;; ***************************

#;
(begin-for-syntax
  (displayln "Hey mom, I'm compiling!"))
#;
(define-simple-macro (our-let ([x:id xe:expr] ...) body ...+)
  ((λ (x ...) body ...) xe ...))
#;
(begin-for-syntax
  (define-syntax-class binding
    #:attributes ([x 0] [xe 0])
    (pattern [x:id xe:expr]) ; "our-let: expected binding in: (x 3)" without this
    (pattern [xe:expr #:as x:id])))
#;
(define-simple-macro (our-let (b:binding ...) body ...+)
  ((λ (b.x ...) body ...) b.xe ...))

(begin-for-syntax
  (define-syntax-class binding
    #:attributes ([x 0] [xe 0])
    (pattern [x:id xe:expr])
    (pattern [xe:expr #:as x:id]))
  (define-splicing-syntax-class bindings
    #:attributes ([x 1] [xe 1])
    (pattern (~seq b:binding ...)
             #:with (x ...) #'(b.x ...)
             #:with (xe ...) #'(b.xe ...)
             #:fail-when (check-duplicates (syntax->list #'(x ...))
                                           free-identifier=?)
             "Duplicate binding")))

(define-simple-macro (our-let (bs:bindings) body ...+)
  ((λ (bs.x ...) body ...) bs.xe ...))

(our-let ([x 3] [y 4]) (+ x y))
(our-let ([x 3] [4 #:as y]) (+ x y)) ; uses the last pattern above


(define-simple-macro
  (where body b:binding ...)
  ((λ (b.x ...) body) b.xe ...))

((+ x y)
 . where .
 [x 3]
 [4 #:as y])

;; As you define macros, you'll often develop syntax classes that identify common
;; elements amongst them.

;; Ryan Culpepper - look for RacketCon syntax parse / class presentation
;; https://www.youtube.com/watch?v=yv_PcyQmrFs