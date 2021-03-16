#lang racket/base
(require (for-syntax (except-in racket/base log)
                     "misc/misc.rkt"
                     "misc/set.rkt"
                     (submod "eval.rkt" spec))
         "ext-vm.rkt"
         racket/dict
         racket/unit
         racket/stxparam
         syntax/parse/define
         "misc/fix.rkt"
         "misc/misc.rkt"
         "misc/map.rkt"
         "misc/transformers.rkt"
         "signatures.rkt"
         "monad/monad.rkt"
         "monad/monad-dead.rkt"
         "monad/monad-output.rkt"
         "monad/monad-nd.rkt"
         "monad/monad-pdcfa.rkt"
         "metafun/alloc.rkt"
         "metafun/class.rkt"
         "metafun/stack.rkt"
         "metafun/sigma.rkt"
         "metafun/rho-sigma.rkt"
         "metafun/prop.rkt"
         "domain/abs-conc.rkt"
         "domain/abs-abs.rkt"
         "ev/ev.rkt"
         "ev/ev-trace.rkt"
         "ev/ev-dead.rkt"
         "ev/ev-cache.rkt"
         (prefix-in conc$ "domain/concrete.rkt")
         "eval.rkt")
(provide (all-from-out "ext-vm.rkt")
         (for-syntax all-concs collect-concs
                     setup-monadic-eval
                     setup-abstract-interpreter
                     finalize-abstract-interpretation
                     setup-adapters))

(define-for-syntax all-concs (box (set)))

(define-for-syntax (collect-concs orig-stx stx)
  (define (collect-concs-cdef cdef)
    (syntax-parse cdef
      #:literals [def-class]
      [(def-class cname:id (~or sname:id #f)
         (v:id ...) mdef ...)
       #:with [mdef′ ...] (map collect-concs-mdef
                                (syntax->list #'(mdef ...)))
       #`(def-class cname #,(or (attribute sname) #'#f)
           (v ...) mdef′ ...)]))

  (define (collect-concs-mdef mdef)
    (syntax-parse mdef
      #:literals [def-method]
      [(def-method mname:id (p:id) (x:id ...) inst)
       #:with inst′ (collect-concs-inst #'inst)
       #'(def-method mname (p) (x ...) inst′)]))

  (define (collect-concs-inst inst)
    (syntax-parse inst
      #:literals [begin iconst load store iadd isub imul idiv lt eq
                  if while new invokevirtual getfield putfield
                  print-top dup abs conc set-meta]
      [(begin inst′ ...+)
       #:with [inst″ ...] (map collect-concs-inst
                                (syntax->list #'(inst′ ...)))
       #'(begin inst″ ...)]
      [(iconst i:integer) inst]
      [(load  v:id) inst]
      [(store v:id) inst]
      [(iadd) inst]
      [(isub) inst]
      [(imul) inst]
      [(idiv) inst]
      [(lt)   inst]
      [(eq)   inst]
      [(if cond:expr thn:expr els:expr)
       #:with cond′ (collect-concs-inst #'cond)
       #:with thn′  (collect-concs-inst #'thn)
       #:with els′  (collect-concs-inst #'els)
       #'(if cond′ thn′ els′)]
      [(while cond:expr body:expr)
       #:with cond′ (collect-concs-inst #'cond)
       #:with body′ (collect-concs-inst #'body)
       #'(while cond′ body′)]
      [(new lbl:id cname:id) inst]
      [(invokevirtual cname:id mname:id) inst]
      [(getfield cname:id fname:id) inst]
      [(putfield cname:id fname:id) inst]
      [(print-top) inst]
      [(dup) inst]
      [(abs inst′)
       #:with inst″ (collect-concs-inst #'inst′)
       #'(abs inst″)]
      [(conc inst′)
       #:with inst″ (collect-concs-inst #'inst′)
       (define new-inst (syntax-property
                         #'(conc inst″)
                         'adapter (gensym 'conc)))
       (set-box! all-concs (set-add (unbox all-concs)
                                    new-inst))
       new-inst]
      [(set-meta cname:id mname:id) inst]))

  (syntax-parse stx
    #:literals [#%module-begin]
    [(#%module-begin cdef ...)
     #:with [cdef′ ...] (map collect-concs-cdef
                              (syntax->list #'(cdef ...)))
     (syntax/loc orig-stx
       (#%module-begin cdef′ ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define monadic-eval (box #f))
(define mret→v×σ (box #f))

(define-for-syntax (setup-monadic-eval meval)
  (define recipe (syntax-local-value meval))
  #`((define-values/invoke-unit/infer
       (export #,@(meval-spec-export recipe))
       (link #,@(meval-spec-link recipe)))
     (set-box! monadic-eval #,(meval-spec-meval recipe))
     (set-box! mret→v×σ #,(meval-spec-mret→v×σ recipe))
     (define-monad M)))

(define-for-syntax (finalize-abstract-interpretation)
  #'(display-all-props))

(define-for-syntax (setup-abstract-interpreter bytecode)
  #`((define (invoke-monad m ρ σ)
       (call-with-values
        (λ () (mret (mrun m ρ σ)))
        (unbox mret→v×σ)))

     (define (extend-ρ×σ name val ρ σ [conv? #t])
       (invoke-monad
        (if conv?
            (do ν ← (val→ν val)
                _ ≔ (verbose extend-ρ×σ
                             "val→ν: ~a → ~a" val ν)
                (with name ν ask-env))
            (with name val ask-env))
        ρ σ))

     (define (ref-var name ρ σ)
       (let-values ([(v _) (invoke-monad (ref name) ρ σ)])
         v))

     ;; load classes
     (define-values (global-rho global-sigma)
       (call-with-values
        (λ () (extend-ρ×σ
                θ #f  ;; dummy
                ∅ ∅))
        (λ (ρ σ) (invoke-monad (load-class #'#,bytecode) ρ σ))))
     
     (set-box!
      abstract-interpreter
      (letrec
          ([interp
            (λ (inst ρ σ stk)
              (let-values
                  ([(v σ′)
                    (invoke-monad
                     (withrec-current-ρ global-ρ
                       (do (assign θ stk)
                           ((unbox monadic-eval) inst)))
                     ρ σ)])
                (operand-stack (make-stack (ref-var θ ρ σ′)))
                (values v σ′)))])
        (case-lambda
          ;; for vm's kickstart
          [(inst env stk)
           (verbose abs "env = ~a" env)
           (let*-values
               ([(ρ₀ σ₀) (values global-rho global-sigma)
                           #;
                           (extend-ρ×σ
                            θ #f ;; dummy
                            global-rho global-sigma)]
                [(ρ₁ σ₁) (extend-ρ×σ global-μ ∅-eq ρ₀ σ₀ #f)]
                [(ρ₂ σ₂) (extend-ρ×σ global-omap ∅-eq ρ₁ σ₁ #f)]
                [(ρ σ) (for/fold ([r ρ₂] [s σ₂])
                                   ([(k v) (∈ env)])
                           (extend-ρ×σ k v r s))]
                [(v σ′) (interp inst ρ σ stk)])
             (define env′ (for/map ([(k _) (∈ env)])
                             (values
                              k
                              (let*-values
                                  ([(ν) (ref-var k ρ σ′)]
                                   [(ν′ _)
                                    (invoke-monad (ν→val ν) ρ σ′)])
                                (verbose abs "ν→val: ~a → ~a" ν ν′)
                                ν′))))
             (values v env′))]
          ;; for adapter's callback
          [(inst ρ σ stk)
           (verbose abs "ρ = ~a, σ = ~a" ρ σ)
           (interp inst ρ σ stk)])))
     
     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; for adapter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define local-rho (make-parameter #f))
(define local-sigma (make-parameter #f))

(define-for-syntax (setup-adapters concs)
  #`(let ()
      (define-syntax-parser load-adapter
        [(_ var:id)
         #'(let-values
               ([(v sigma)
                 (invoke-monad
                  (ref 'var) (local-rho) (local-sigma))])
             (local-sigma sigma)
             v)])

      (define-syntax-parser store-adapter
        [(_ var:id val:expr)
         #'(let-values
               ([(v σ)
                 (invoke-monad
                  (do v ≔ val
                      v′ ← (value→ν v)
                      _ ≔ (verbose store-adapter
                                   "value→ν: ~a → ~a" v v′)
                      (assign 'var v′))
                  (local-rho) (local-sigma))])
             (local-sigma σ)
             v)])

      (define-syntax-parser delta-adapter
        #:literals [quote]
        [(_ 'op v₁ v₂)
         #'(let-values
               ([(v σ)
                 (invoke-monad
                  (do v1 ≔ v₁
                      v2 ≔ v₂
                      v1′ ← (value→val v1)
                      v2′ ← (value→val v2)
                      _ ≔ (verbose
                           delta-adapter
                           "value→νal of v₁: ~a → ~a" v1 v1′)
                      _ ≔ (verbose
                           delta-adapter
                           "value→val of v₂: ~a → ~a" v2 v2′)
                      (return (conc$delta 'op v1′ v2′)))
                  (local-rho) (local-sigma))])
             (local-sigma σ)
             v)])

      (define-syntax-parser invokevirtual-adapter
        [(_ cls:id tgt:expr mtd:id arg:expr)
         #'(let-values
               ([(v σ)
                 (invoke-monad
                  (do t ≔ tgt
                      a ≔ arg
                      t′ ← (value→val t)
                      a′ ← (value→val a)
                      _ ≔ (verbose
                           invokevirtual-adapter
                           "value→νal of tgt: ~a → ~a" t t′)
                      _ ≔ (verbose
                           invokevirtual-adapter
                           "value→val of arg: ~a → ~a" a a′)
                      (return (parameterize ([local-rho #f])
                                (conc$invokevirtual
                                 cls t′ mtd a′))))
                  (local-rho) (local-sigma))])
             (local-sigma σ)
             v)])

      (define-syntax-parser getfield-adapter
        [(_ cls:id tgt:expr fld:id)
         #'(let-values
               ([(v σ)
                 (invoke-monad
                  (do t ≔ tgt
                      t′ ← (value→val t)
                      _ ≔ (verbose
                           getfield-adapter
                           "value→νal of tgt: ~a → ~a" t t′)
                      (return (conc$getfield cls t′ fld)))
                  (local-rho) (local-sigma))])
             (local-sigma σ)
             v)])

      (define-syntax-parser putfield-adapter
        [(_ cls:id tgt:expr fld:id val:expr)
         #'(let-values
               ([(v σ)
                 (invoke-monad
                  (do t ≔ tgt
                      t′ ← (value→val t)
                      _ ≔ (verbose
                           putfield-adapter
                           "value→νal of tgt: ~a → ~a" t t′)
                      r ≔ (conc$putfield cls t′ fld val)
                      (value→ν t′) ;; for copy-back fields
                      (return r))
                  (local-rho) (local-sigma))])
             (local-sigma σ)
             v)])

      (define-syntax-parser abs-adapter
        [(_ inst)
         #'(let-values ([(v σ)
                         ((unbox abstract-interpreter)
                          #'inst
                          (local-rho) (local-sigma)
                          (dump-stack))])
             (local-sigma σ)
             v)])

      #,@(for/list ([conc concs])
           #`(dict-set!
              adapter-table '#,(syntax-property conc 'adapter)
              (λ (ρ σ stk)
                (parameterize ([local-rho ρ]
                               [local-sigma σ]
                               [operand-stack (make-stack stk)])
                  (syntax-parameterize
                      ([load-impl (make-rename-transformer
                                   #'load-adapter)]
                       [store-impl (make-rename-transformer
                                    #'store-adapter)]
                       [delta-impl (make-rename-transformer
                                    #'delta-adapter)]
                       [invokevirtual-impl (make-rename-transformer
                                            #'invokevirtual-adapter)]
                       [getfield-impl (make-rename-transformer
                                       #'getfield-adapter)]
                       [putfield-impl (make-rename-transformer
                                       #'putfield-adapter)]
                       [abs-impl (make-rename-transformer
                                  #'abs-adapter)])
                    #,(cadr (syntax->list conc)))
                  (cons (local-sigma) (dump-stack))))))
      (void)))
