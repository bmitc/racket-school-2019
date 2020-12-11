#lang racket

;; LOOKUP: #%app, #%datum, #%top, #%top-interaction, #%expression
;;         these are called (or at least referred to as) "implicits"

;; Can rename pfsh:run to #%app instead of run, such that instead of (run ls -l) we can
;; run just (ls -l) such that pfsh:run is ran instead of #%app.

;; #%app, quote, define-values, and (almost) lambda are primitive things

;; #%module-begin provides just the ability to have a sequence of things at the top of a module

;; LOOKUP: all-from-out, except-out inside a provide

;; Macro Stepper colors things based on different scopes