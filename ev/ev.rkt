#lang racket/base
(require racket/unit
         racket/dict
         syntax/parse
         "../signatures.rkt"
         "../misc/transformers.rkt"
         "../misc/misc.rkt"
         "../misc/map.rkt"
         "../ext-vm.rkt")
(provide ev@ ext-ev@ adapter-table)

(define-unit ev@
  (import monad^ menv^ mstore^ σ^ δ^ alloc^
          stack^ class^ obj^ ρ×σ^)
  (export ev^)
  ;;(init-depend monad^) ;; necessary?

  (define-monad M)

  (define ((ev ev) inst)
    (syntax-parse inst
      #:literals [begin iconst load store
                  iadd isub imul idiv lt eq
                  if while new invokevirtual
                  getfield putfield print-top dup]

      [(begin inst) (ev #'inst)]
      [(begin inst insts′ ...)
       (do (ev #'inst)
           (ev #'(begin insts′ ...)))]

      [(iconst i:integer)
       (push (syntax->datum #'i))]

      [(load x:id)
       (do ν ← (ref (id->sym #'x))
           (push ν))]

      [(store x:id)
       (do ν ← pop
           (assign (id->sym #'x) ν))]

      [((~and op (~or iadd isub imul idiv)))
       (do ν₂ ← pop
           ν₁ ← pop
           ν ← (δ #'op ν₁ ν₂)
           (push ν))]
      
      [((~and op (~or lt eq)))
       (do ν₂ ← pop
           ν₁ ← pop
           (δ #'op ν₁ ν₂))]

      [(if cond thn els)
       (do ν ← (ev #'cond)
           b ← (truish? ν)
           (ev (if b #'thn #'els)))]

      [(~and w (while cond body))
       (do ν ← (ev #'cond)
           b ← (truish? ν)
           (if b
               (ev #'(begin body w))
               (return (void))))]

      [(new lbl:id cname:id)
       (do o ← (make-aobj (id->sym #'cname))
           a ← (alloc (id->sym #'lbl))
           (ext a o)
           (push a))]

      [(invokevirtual cname:id mname:id)
       (define cls (id->sym #'cname))
       (define mtd (id->sym #'mname))
       (do arg ← pop
           tgt-a ← pop
           tgt ← (find tgt-a)
           tgt-cls ← (class-of tgt)
           b ← (class<=? tgt-cls cls)
           (if b
               (do (list param locals body) ← (lookup-method
                                                tgt-cls mtd)
                   ρ ← (ref global-ρ)
                   ret ← (local-env ρ
                           (with 'self tgt-a
                             (with θ '()
                               (with param arg
                                 (foldr
                                  (λ (ν body) (with ν #f body))
                                  (do (ev body) pop)
                                  locals)))))
                   (push ret))
               fail))]

      [(getfield cname:id fname:id)
       (define cls (id->sym #'cname))
       (define fld (id->sym #'fname))
       (do tgt-a ← pop
           tgt ← (find tgt-a)
           tgt-cls ← (class-of tgt)
           b ← (class<=? tgt-cls cls)
           b′ ← (has-field? cls fld)
           (if (and b b′)
               (do ν ← (lookup-field tgt cls fld)
                   (push ν))
               fail))]

      [(putfield cname:id fname:id)
       (define cls (id->sym #'cname))
       (define fld (id->sym #'fname))
       (do ν ← pop
           tgt-a ← pop
           tgt ← (find tgt-a)
           tgt-cls ← (class-of tgt)
           b ← (class<=? tgt-cls cls)
           b′ ← (has-field? cls fld)
           (if (and b b′)
               (update-field tgt cls fld ν)
               fail))]

      [(print-top)
       (do ν ← pop
           (return (displayln ν)))]
       [(dup)
        (do ν ← peek
            (push ν))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; abstract interpretation extension

(define adapter-table (make-hash))

(define-unit ext-ev@
  (import monad^ menv^ mstore^ ρ×σ^ stack^
          class^ border^ prop^)
  (export ext-ev^)

  (define-monad M)

  (define (((ext-ev ev₀) ev) inst)
    (syntax-parse inst
      #:literals [abs conc set-meta
                  store iadd isub imul idiv lt eq
                  invokevirtual getfield putfield]

      ;; extension

      [(abs inst) ; TODO? (対称性から)何かを登録？
       ((ev₀ ev) #'inst)]
      
      [(conc inst′)
       (let* ([conc-id (syntax-property inst 'adapter)]
              [adapter (dict-ref adapter-table conc-id)])
         (do ρ ← ask-env
             σ ← get-store
             stk ← (ref θ)
             (cons σ′ stk′) ≔ (adapter ρ σ stk)
             (update-store (λ (σ) (⊔ σ′ σ)))
             (assign θ stk′)))]

      ;; override

      [(store x:id)
       (do ν ← pop
           ν′ ← (value→ν ν)
           _ ≔ (verbose store
                        "value→ν: ~a → ~a" ν ν′)
           (push ν′)
           ((ev₀ ev) inst))]

      [((~and op (~or iadd isub imul idiv)))
       (do ν₂ ← pop
           ν₁ ← pop
           ν₁′ ← (value→ν ν₁)
           ν₂′ ← (value→ν ν₂)
           _ ≔ (verbose binop
                        "value→ν ~a → ~a, ~a → ~a" ν₁ ν₁′ ν₂ ν₂′)
           (push ν₁′)
           (push ν₂′)
           ((ev₀ ev) inst))]

      [((~and op (~or lt eq)))
       (do ν₂ ← pop
           ν₁ ← pop
           ν₁′ ← (value→ν ν₁)
           ν₂′ ← (value→ν ν₂)
           _ ≔ (verbose relop
                        "value→ν ~a → ~a, ~a → ~a" ν₁ ν₁′ ν₂ ν₂′)
           (push ν₁′)
           (push ν₂′)
           ((ev₀ ev) inst))]

      [(invokevirtual cname:id mname:id)
       (do arg ← pop
           tgt-a ← pop
           arg′ ← (value→ν arg)
           tgt-a′ ← (value→ν tgt-a)
           _ ≔ (verbose invokevirtual
                        "value→ν of arg: ~a → ~a" arg arg′)
           _ ≔ (verbose invokevirtual
                        "value→ν of tgt: ~a → ~a" tgt-a tgt-a′)
           (push tgt-a′)
           (push arg′)
           ((ev₀ ev) inst))]

      [(getfield cname:id fname:id)
       (do tgt-a ← pop
           tgt-a′ ← (value→ν tgt-a)
           _ ≔ (verbose getfield
                        "value→ν of tgt: ~a → ~a" tgt-a tgt-a′)
           (push tgt-a′)
           ((ev₀ ev) inst))]

      [(putfield cname:id fname:id)
       (do ν ← pop
           tgt-a ← pop
           tgt-a′ ← (value→ν tgt-a)
           _ ≔ (verbose putfield
                        "value→ν of tgt: ~a → ~a" tgt-a tgt-a′)
           (push tgt-a′)
           (push ν)
           ((ev₀ ev) inst)
           (value→val tgt-a′))]

      [(set-meta cname:id mname:id)
       (do cls ≔ (id->sym #'cname)
           mtd ≔ (id->sym #'mname)
           ν ← pop
           ;;_ ≔ (log set-meta "~a$~a = ~a" cls mtd ν)
           _ ≔ (set-method-prop cls mtd ν)
           (return (void)))]

      [_ ((ev₀ ev) inst)])))
