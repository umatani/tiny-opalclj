#lang racket/base

(require (for-syntax racket/base)
         (only-in "domain/concrete.rkt"
                  define-toplevel-method-funs)
         syntax/parse/define
         "mix.rkt"
         "eval.rkt")

(provide (rename-out [bytecode-module-begin #%module-begin])
         (except-out (all-from-out "mix.rkt") #%module-begin)
         (all-from-out "eval.rkt")
         #%top-interaction #%datum #%app #%top)

(define-syntax (bytecode-module-begin stx)
  (define-syntax-class cdef
    (pattern ((~literal def-class) name:id . _)))
  (define-values (eval-id vmcode-stx)
    (syntax-parse stx
      [(_ (~optional (ev:id)) . rest)
       (values (or (attribute ev) #'eval-conc)
               (collect-concs stx #'(#%module-begin . rest)))]))
  (syntax-parse vmcode-stx
    #:literals [#%module-begin]
    [(#%module-begin c:cdef ...)
     (quasisyntax/loc stx
       (#%module-begin
        c ...
        ; instantiate monadic-eval
        #,@(setup-monadic-eval eval-id)
        ; setup abstract-interpretrer
        ; (and load classes into abstract world)
        #,@(setup-abstract-interpreter vmcode-stx)
        ; create and regist adapters
        #,(setup-adapters (unbox all-concs))

        ; call Prelude$sinit statically
        (when (unbox sinit)
          ((unbox sinit) #f #f))
        #,(finalize-abstract-interpretation)
        
        ; create method interfaces
        ; e.g. (A$f 1) or (A$f (run (iconst 1)))
        (define-toplevel-method-funs c.name ...)))]))
