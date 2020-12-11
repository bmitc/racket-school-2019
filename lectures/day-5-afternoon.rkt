#lang racket

#|
LOP ~ design of Racket languages

0) Language extension
1) Module languages
    - untyped
    - statically typed
2) Embedded (fine grained) languages
3) You want 2 with 0: extensible embedded languages

Monday morning - the model (substituting trees and returning trees/syntax)
Monday afternoon - define-syntax, syntax-parse
                 - define-simple-macro is built upon these two
                 - syntax-parse is based upon syntax-case
Tuesday morning - syntax classes
                - these generate the syntax for you
Tuesday afternoon - hygiene
                  - datum->syntax (poison, allows you to break hygiene)
                  - syntax parameters (bittersweet)
                     - something we can call inside a macro but generates an error
                       if used outside
Wednesday - module languages
          - (module <box> LangName
              (#%module-begin ______))
               #%app
               #%datum
          - module allows you to require and provide language extensions
          - new languages come from old languages
             - re-provide
             - re-define/interpret
             - restrict
Thursday - use Turnstile for statically typed languages
Friday morning - embedded languages
               - algebraic pattern matching
               - regular expression language



Syntax extensions should not expose their guts just like functions should not expose
their guts.
|#