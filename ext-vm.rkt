;;;; extension to vm for adding abstract interpretation
#lang racket/base
(require (for-syntax racket/base)
         racket/stxparam
         syntax/parse
         syntax/parse/define
         "misc/misc.rkt"
         "misc/map.rkt"
         "vm.rkt")
(provide (except-out (all-from-out "vm.rkt")
                     def-method)
         (rename-out [ext-def-method def-method])
         abstract-interpreter
         abs conc set-meta
         ;; hooks
         abs-impl
         ;; for experiment
         run
         )

(define-syntax-parameter all-bindings (syntax-rules ()))
(define-syntax-parameter set-all-bindings! (syntax-rules ()))

(define-simple-macro (run inst ...)
  (parameterize ([operand-stack (make-stack '())])
    (syntax-parameterize ([all-bindings (λ (stx) #'∅)]
                          [set-all-bindings!
                           (λ (stx) #'(void))])
      inst ...)
    (pop-stack)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `static' override of vm implementation

(define-syntax (ext-def-method stx)
  (syntax-parse stx
    #:literals [ext-def-method]
    [(ext-def-method name:id (p:id) (v:id ...) body:expr)
     #'(def-method name (p) (v ...)
         (syntax-parameterize
             ([all-bindings
               (λ (stx)
                 #'(assoc-to-map `([self ,self]
                                   [p ,p]
                                   [v ,v] ...)))]
              [set-all-bindings!
               (syntax-parser
                 [(_ binds)
                  #'(let ([b binds])
                      (set! self (b 'self))
                      (set! p (b 'p))
                      (set! v (b 'v)) ...)])])
           body))]))

;;;; extension to bytecode and vm implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define abstract-interpreter (box #f))

(define-syntax-parameter abs-impl
  (make-rename-transformer #'conc-abs))
(define-syntax-parser conc-abs
  [(_ inst)
   #'(let-values ([(v binds) ((unbox abstract-interpreter)
                              #'inst (all-bindings) (dump-stack))])
       (set-all-bindings! binds)
       v)])
(define-simple-macro (abs inst) (abs-impl inst))

(define-simple-macro (conc inst) inst)

(define-simple-macro (set-meta cname:id mname:id)
  (begin (display "set-meta@conc: ")
         (print-top)))
