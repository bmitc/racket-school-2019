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

((λ ([x Int] [y Int]) (+ x y)) 2 3) ; this tests application of an expression and not an identifier