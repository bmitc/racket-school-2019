#lang racket

;; Can only have a single #lang per file. The #lang granularity is at the file level.

#|
What is the base of Racket? Where does it end?
#lang ____ only requires ____ to have alphanumeric/... but then it is up to the ____ to
provide the rest.
|#

;; LOOKUP: fully-expanded, phases

(cons 1 (cons (+ 2 5) (cons 3 empty)))

`(1 ,(+ 2 5) 3)

`(1 ,@(list (+ 4 5) (+ 25)) 3)

#'(lambda (x) (+ 4 1))
#`(lambda (x) #,(+ 4 1))
#`(lambda (x) #,@(list (+ 9 8) (+ 4 1)))

;; #% is a convention for things you don't normally see. Something has been exposed.