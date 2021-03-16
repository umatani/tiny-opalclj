;; abstract domain in one-to-one correspondence with concrete domain
#lang racket/base
(require racket/unit racket/bool racket/vector racket/dict
         syntax/parse
         "../misc/transformers.rkt"
         "../misc/map.rkt"
         "../signatures.rkt"
         "../ext-vm.rkt"
         (prefix-in conc$ "concrete.rkt"))
(provide abs-conc@)

(define aobj-id-count (box 0))

;; object representation
;; Nat × Sym × (Sym ↦ Value)
(struct aobject (id cname [flds #:mutable]) #:transparent)

(define-unit abs-conc@
  (import monad^ σ^ ρ×σ^ alloc^ class^)
  (export δ^ obj^ border^)

  (define-monad M)

  (define (δ op v₁ v₂)
    ;(printf "δ: ~a ~a ~a\n" (syntax->datum op) v₁ v₂)
    (syntax-parse op
      #:literals [iadd isub imul idiv lt eq]
      [iadd
       (return (conc$delta 'iadd v₁ v₂))]
      [isub
       (return (conc$delta 'isub v₁ v₂))]
      [imul
       (return (conc$delta 'imul v₁ v₂))]
      [idiv
       (if (zero? v₂)
           fail
           (return (conc$delta 'idiv v₁ v₂)))]
      [lt
       (return (conc$delta 'lt v₁ v₂))]
      [eq
       (return (conc$delta 'eq v₁ v₂))]
      [_ fail]))

  (define (truish? v)
    (return (not (false? v))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (make-aobj cname)
    (do flds ← (all-fields-of cname)
        fmap ≔ (for/map ([fld flds])
                 (values fld 0))
        (return (begin0 (aobject (unbox aobj-id-count) cname fmap)
                  (set-box! aobj-id-count
                            (add1 (unbox aobj-id-count)))))))

  (define (class-of o)
    (return (aobject-cname o)))

  (define (lookup-field o cname fname)
    (return ((aobject-flds o) (cons cname fname))))

  (define (update-field o cname fname val)
    (return (set-aobject-flds! o ((aobject-flds o)
                                  (cons cname fname) val))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; ν : AVal = AInt + Addr + #f (undef) + AStk (for θ) + C↦A (for μ)
  ;; o : AObj = Id ↦ (AVal + CVal)
  ;; ρ : Ρ    = Id ↦ Addr
  ;; σ : Σ    = Addr ↦ (AVal + AObj)
  ;; θ : AStk = List(AVal + CVal)

  ;; val : CVal = CInt + CObj + #f
  ;; obj : CObj = Vec(AVal + CVal)
  ;; env : Env = Id ↦ CVal

  ;; μ : C↦A = CObj → Addr
  (define global-μ 'μ)
  ;; omap : A↦C = Addr → CObj
  (define global-omap 'omap)

  (define (auninit? x) (return (false? x)))
  (define (astk? x) (return (list? x)))
  (define (memo? x) (return (map? x)))

  ;; TODO: attach tag
  (define (aint? x) (return (integer? x)))

  (define (aobj? x) (return (aobject? x)))

  (define (aval? x)
    (do b ← (auninit? x)
        (if b
            (return #t)
            (do b′ ← (addr? x)
                (if b′
                    (return #t)
                    (do b″ ← (astk? x)
                        (if b″
                            (return #t)
                            (do b‴ ← (memo? x)
                                (return #t)
                                (return #f)))))))))

  (define (value→ν x)
    (do b ← (aval? x)
        (if b
            (return x)
            (do b′ ≔ (conc$cval? x)
                (if b′
                    (val→ν x)
                    fail)))))

  (define (value→val x)
    (do b ≔ (conc$cval? x)
        (if b
            (return x)
            (do b′ ← (aval? x)
                (if b′
                    (ν→val x)
                    fail)))))

  (define (int→ι i) (return i))
  (define (ι→int i) (return i))

  (define (val→ν val)
    (do b ≔ (false? val)
        (if b
            (return #f)
            (do b′ ≔ (conc$cint? val)
                (if b′
                    (int→ι val)
                    (do b″ ≔ (conc$cobj? val)
                        (if b″
                            (obj→addr val)
                            fail)))))))

  (define (ν→val ν)
    (do b ← (auninit? ν)
        (if b
            (return #f)
            (do b′ ← (astk? ν) ; for θ
                (if b′
                    (return ν)
                    (do b″ ← (memo? ν) ; for μ
                        (if b″
                            (return ν)
                            (do b‴ ← (aint? ν)
                                (if b‴
                                    (ι→int ν)
                                    (do b⁗ ← (addr? ν)
                                        (if b⁗
                                            (addr→obj ν)
                                            fail)))))))))))

  (define (fields→flds obj o)
    (do cls ≔ (aobject-cname o)
        fnames ← (all-fields-of cls)
        fmap ≔ (for/map ([fname fnames]
                         [fld (vector-drop obj 2)])
                 (values fname fld))
        (return (set-aobject-flds! o fmap))))

  (define (flds→fields o obj)
    (do cls ≔ (aobject-cname o)
        fmap ≔ (aobject-flds o)
        fnames ← (all-fields-of cls)
        (return (for ([fname fnames]
                      [idx (in-naturals 2)])
                  (vector-set! obj idx (fmap fname))))))

  (define (obj→addr obj)
    (do μ ← (ref global-μ)
        omap ← (ref global-omap)
        (if (∈ obj μ)
            (do a ≔ (μ obj)
                o ← (find a)
                (fields→flds obj o)
                (return o))
            (do o ← (obj→o obj)
                a ← (alloc 'μ)
                (ext a o)
                (assign global-μ (μ obj a))
                (assign global-omap (omap a obj))
                (return a)))))

  (define (addr→obj a)
    (do o ← (find a)
        b ← (aval? o)
        (if b
            fail
            (do b′ ← (aobj? o)
                (if b′
                    (do μ ← (ref global-μ)
                        omap ← (ref global-omap)
                        (if (∈ a omap)
                            (do obj ≔ (omap a)
                                (flds→fields o obj)
                                (return obj))
                            (do obj ← (o→obj o)
                                (assign global-μ (μ obj a))
                                (assign global-omap (omap a obj))
                                (return obj))))
                    fail)))))

  (define (obj→o obj)
    (do cls ≔ (vector-ref obj 0)
        o ← (make-aobj cls)
        (fields→flds obj o)
        (return o)))

  (define (o→obj o)
    (do cls ≔ (aobject-cname o)
        new-fn ≔ (dict-ref conc$cls↦new-fn cls)
        obj ≔ (new-fn)
        (flds→fields o obj)
        (return obj)))
  )
