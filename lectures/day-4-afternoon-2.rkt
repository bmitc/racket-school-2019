#lang turnstile/quicklang

(provide ann λ (rename-out [λ lambda]))
(provide (type-out Bool Int ->))
(provide (typed-out [not (-> Bool Bool)]
                    [+   (-> Int Int Int)]
                    [<=  (-> Int Int Bool)]))

(require rackunit/turnstile)
(provide (all-from-out rackunit/turnstile))

(define-base-types Bool Int)
(define-type-constructor -> #:arity >= 1)

#|
Δ ⊢ e ≫ e- ⇐ τ
_________________________
Δ ⊢ (ann e τ) ≫ e- ⇒ τ
|#

(define-typed-syntax ann
  [(_ e:expr τ:type)
   ≫
   [⊢ e ≫ e- ⇐ τ.norm]
   ----------------------
   [⊢ e- ⇒ τ.norm]])

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
   

















