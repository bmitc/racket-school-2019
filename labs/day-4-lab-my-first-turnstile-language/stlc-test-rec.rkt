#lang s-exp "stlc.rkt"
   
(check-type 5             : Int -> 5)
(check-type (+ 4 5)       : Int -> 9)
(check-type (+ 4 (+ 5 6)) : Int -> 15)
   
(typecheck-fail (+ 5 #t))
(typecheck-fail (+ 5))
   
(def - (-> Int Int Int)
  (λ (x y) (+ x (* y -1))))
   
(check-type -             : (-> Int Int Int))
(check-type (- 12 7)      : Int -> 5)
   
(typecheck-fail (- #t #f))

(check-type (if #t 1 2) : Int -> 1)
(check-type (if #f 1 2) : Int -> 2)

(typecheck-fail (#t 1 #f))
(typecheck-fail (1 1 2))

(check-type ((λ ([x Int] [y Int]) (+ x y)) 2 3) : Int -> 5) ; this tests application of an expression and not an identifier

; Compute type of annotated λ:
(def fst (λ ([x Int] [y Bool]) x))
(check-type fst : (-> Int Bool Int))
   
; Cannot compute type of unannotated λ:
(typecheck-fail (λ (x y) y))
   
; Check expected type of unannotated λ:
(def snd
  (ann (λ (x y) y)
       (-> Int Bool Bool)))
(check-type snd : (-> Int Bool Bool))
   
; Expected type doesn't match λ arity:
(typecheck-fail (ann (λ (x y z) z)
                     (-> Int Bool Int)))
   
; No literals yet:
;(typecheck-fail 5)
;(typecheck-fail #f)

(def fact
  (rec self (-> Int Int)
    (λ (n)
      (if (<= n 1)
          1
          (* n (self (- n 1)))))))
   
(check-type (fact 5) : Int -> 120)

(def fibonacci
  (rec self (-> Int Int)
    (λ (n)
      (if (<= n 0)
          0
          (if (<= n 1)
              1
              (+ (self (- n 1)) (self (- n 2))))))))

(check-type (fibonacci 10) : Int -> 55)
(check-type (fibonacci 21) : Int -> 10946)
