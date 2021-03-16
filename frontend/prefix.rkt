#lang racket/base

(require (for-syntax racket/base racket/pretty
                     syntax/parse
                     "compile.rkt")
         (prefix-in ra$ racket)
         (only-in "../bytecode.rkt" #%module-begin)
         (submod "literals.rkt"  prefix-literals)
         "../eval.rkt")

(provide (all-from-out (submod "literals.rkt" prefix-literals))
         (all-from-out "../eval.rkt")
         (rename-out [prefix-module-begin #%module-begin])
         (rename-out [prefix-top-interaction #%top-interaction])
         #%datum #%app #%top
         eval)

(define-syntax (prefix-module-begin stx)
  (syntax-parse stx
    #:literals [prefix-module-begin]
    [(prefix-module-begin (~optional (ev:id)) . body)
     #:with eval-id (or (attribute ev) #'eval-conc)
     ;; compile to bytecode
     (define bytecode
       #`(#%module-begin #,@(if (attribute ev)
                                (list #'(ev))
                                (list))
                         #,@(bcompile #'body)))
     ;; (displayln "[bytecode]")
     ;; (pretty-print (syntax->datum bytecode))
     ;; (newline)

     ;; into backend...
     bytecode]))

(define-syntax (prefix-top-interaction stx)
  (syntax-parse stx
    #:literals [eval]
    [(_ . (eval e))
     #'(#%top-interaction .
         (ra$begin
          (module DUMMY tiny-opalclj/frontend/prefix
            (prefix-module-begin
             (class $TopLevel
               (method dummymain ($dummyarg 0)
                 e))))
          (require 'DUMMY))
         )]
    [(_ . e)
     #'(#%top-interaction . e)]))
