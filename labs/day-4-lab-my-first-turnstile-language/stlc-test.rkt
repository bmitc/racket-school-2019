#lang s-exp "stlc.rkt"

(def - (-> Int Int Int)
  (λ (x y) (+ x (* y -1))))

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