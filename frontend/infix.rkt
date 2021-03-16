#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     syntax/stx
                     racket/pretty
                     "../misc/map.rkt"
                     "typecheck.rkt")
         (submod "literals.rkt"  infix-literals)
         (submod "literals.rkt"  prefix-literals)
         (only-in "prefix.rkt" #%module-begin)
         "../eval.rkt")

(provide (all-from-out (submod "literals.rkt" infix-literals))
         (all-from-out (submod "literals.rkt" prefix-literals))
         (all-from-out "../eval.rkt")
         (rename-out [infix-module-begin #%module-begin])
         #%top-interaction #%datum #%app)

(define-syntax (infix-module-begin stx)
  (syntax-parse stx
    [(_ directive-assoc class ...)
     (define directives (hash->map
                         (make-hasheq
                          (stx-map
                           (λ (p) (cons (syntax->datum (stx-car p))
                                         (stx-cdr p)))
                           #'directive-assoc))))
     (define typed (not (∈ 'untyped directives)))
     (define eval (if (∈ 'eval directives)
                      (list (list (directives 'eval)))
                      '()))
     ;; (displayln "[infix]")
     ;; (pretty-print (syntax->datum stx))
     ;; (newline)

     ;; static type checking
     (define prefix-code
       (quasisyntax/loc stx
        (#%module-begin #,@eval 
         #,@(typecheck-program #'(class ...) typed))))
     ;; (displayln "[prefix]")
     ;; (pretty-print (syntax->datum prefix-code))
     ;; (newline)

     ;; into prefix...
     prefix-code]))
