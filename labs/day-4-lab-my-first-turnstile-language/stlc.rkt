#lang turnstile/quicklang

(provide ann λ def if rec; if let rec #%app #%datum
         (rename-out [λ lambda]))
(provide (type-out Bool Int ->))
(provide (typed-out [not    (-> Bool Bool)]
                    [+      (-> Int Int Int)]
                    [*      (-> Int Int Int)]
                    [/      (-> Int Int Int)]
                    [<=     (-> Int Int Bool)]
                    [zero?  (-> Int Bool)]))
(provide #%datum
         #%app)

(require rackunit/turnstile)
(provide (all-from-out rackunit/turnstile))

(define-base-types Bool Int)
(define-type-constructor -> #:arity >= 1)


;;****************************************
;; Template
;;****************************************
#;
(define-typed-syntax (form-name)
  [⟨typed-form-pattern⟩
   ⇐ ⟨expected-type-pattern⟩
   ≫
   ⟨premiss⟩ ...
   -------------------------
   [⊢ ⟨untyped-form-template⟩]]
  [⟨typed-form-pattern⟩
   ≫
   ⟨premiss⟩ ...
   --------------------------
   [⊢ ⟨untyped-form-template⟩ ⇒ ⟨computed-type-template⟩]])


;;****************************************
;; Expression Syntax
;;****************************************

#|
Δ ⊢ e ≫ e- ⇐ τ
_________________________
Δ ⊢ (ann e τ) ≫ e- ⇒ τ
|#

; (ann :expr :type)
;
; Type annotation. Infers the given type if the expression can be
; checked to have that type.
(define-typed-syntax ann
  ; to COMPUTE the type of ‘(ann e τ)’, . . .
  [(_ e:expr τ:type)    
   ≫
   ; CHECK that ‘e’ has type ‘τ’ (normalized), calling its
   ; expansion ‘e-’, . . .
   [⊢ e ≫ e- ⇐ τ.norm] 
   ----
   ; then expand to ‘e-’ and COMPUTE type ‘τ’ (normalized).
   [⊢ e- ⇒ τ.norm]])


;;****************************************
;; Lambda Syntax
;;****************************************

; (λ (:id ...) :expr)
; (λ ([:id :type] :expr)
;
; Function abstraction. The first form has the same syntax as
; Racket, but is checking-mode only.
(define-typed-syntax λ
  [(_ (x:id ...) e:expr)
   ⇐ (~-> s ... t)  ; pattern matches on an ->. also, the types coming in are already normalized
   ≫
   #:fail-when (check-duplicate-identifier (stx->list #'(x ...)))
   "repated formal parameter name"
   #:fail-unless (= (stx-length #'(x ...)) (stx-length #'(s ...)))
   "wrong number of formal parameters for expected arrow type"
   [[x ≫ x- : s] ... ⊢ e ≫ e- ⇐ t] ; x translated to x- having type s and so on proves that e translates to e- and is computed by t (checks to be t)
   --------------------------
   [⊢ (λ- (x- ...) e-)]]
  [(_ ([x:id σ:type] ...) e:expr) ; when this matches
   ≫                             ; translate like this
   #:fail-when (check-duplicate-identifier (stx->list #'(x ...)))
   "repated formal parameter name"
   #:with (s ...) #'(σ.norm ...)
   [[x ≫ x- : s] ... ⊢ e ≫ e- ⇒ t]
   --------------------------
   [⊢ (λ- (x- ...) e-) ⇒ (-> s ... t)]])


;;****************************************
;; Definition Syntax
;;****************************************

; (def x:id e:expr)
; (def x:id t:type e:expr)
;
; Module-level variable definition. Unlike Racket’s define, def doesn't
; do recursion; its scope is downward-only.
(define-typed-syntax def
  [(_ x:id e:expr)
   ≫
   ----
   [≻ (define-typed-variable x e)]]
  [(_ x:id τ:type e:expr)
   ≫
   [⊢ e ≫ e- ⇐ τ.norm]
   ----
   [≻ (define-typed-variable x e- ⇐ τ.norm)]])


;;****************************************
;; Literals
;;****************************************

(define-typed-syntax #%datum
  [(_ . literal:integer)
   ≫
   --------------------------
   [⊢ (#%datum- . literal) ⇒ Int]]
  [(_ . literal:boolean)
   ≫
   --------------------------
   [⊢ (#%datum- . literal) ⇒ Bool]]
  [(_ . nonliteral)
   ≫
   --------------------------
   [#:error "unsupported literal given"]])


;;****************************************
;; Application
;;****************************************

(define-typed-syntax #%app
  [(_ f:expr e:expr ...)
   ≫
   [⊢ f ≫ f- ⇒ (~-> s ... t)] ; asking the premise to compute types, so pattern match to capture the types
   #:fail-unless (= (stx-length #'(e ...)) (stx-length #'(s ...)))
   "wrong number of arguments for function application"
   [⊢ e ≫ e- ⇐ s] ...
   -------------------------
   [⊢ (#%app- f- e- ...) ⇒ t]])


;;****************************************
;; if
;;****************************************

(define-typed-syntax if
  [(_ e1:expr e2:expr e3:expr)
   ≫
   [⊢ e1 ≫ e1- ⇐ Bool]
   [⊢ e2 ≫ e2- ⇒ s]  ; the type is already normalized since we are computing it, so use t, not τ
   [⊢ e3 ≫ e3- ⇒ t]
   #:fail-unless (type=? #'s #'t)
   "both the consequent and subsequent expressions must return the same type,"
   -------------------------
   [⊢ (if- e1- e2- e3-) ⇒ t]]
  [(_ e1 e2 e3)
   ⇐ t
   ≫
   [⊢ e1 ≫ e1- ⇐ Bool]
   [⊢ e2 ≫ e2- ⇐ t]
   [⊢ e3 ≫ e3- ⇐ t]
   -------------------------
   [⊢ (if- e1- e2- e3-)]])


;;****************************************
;; rec
;;****************************************


(define-simple-macro (rec- x:id e:expr)
  (letrec- ([x e]) x))

#;
(define-syntax (rec- stx)
  (syntax-parse stx
    [(_ x:id e:expr)
     #'(letrec- ([x e]) x)]))

(define-typed-syntax rec
  [(_ x:id τ:type e:expr)
   ≫
   [[x ≫ x- : τ.norm] ⊢ e ≫ e- ⇐ τ.norm]
   -------------------------
   [⊢ (rec- x- e-) ⇒ τ]]
  [(_ x:id e:expr)
   ⇐ t
   ≫
   [[x ≫ x- : t] ⊢ e ≫ e- ⇐ t]
   -------------------------
   [⊢ (rec- x- e-) ⇒ t]])