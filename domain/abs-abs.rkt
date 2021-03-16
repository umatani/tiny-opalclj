;; abstract domain in one-to-one correspondence with concrete domain
#lang racket/base
(require racket/unit racket/bool racket/vector racket/dict
         racket/match
         syntax/parse
         "../misc/transformers.rkt"
         "../misc/map.rkt"
         "../signatures.rkt"
         "../ext-vm.rkt"
         (prefix-in conc$ "concrete.rkt"))
(provide abs-abs@)

(define aobj-id-count (box 0))

;; object representation
;; Nat × Sym × (Sym ↦ Value)
(struct aobject (id cname [flds #:mutable]) #:transparent)

(define-unit abs-abs@
  (import monad^ σ^ ρ×σ^ alloc^ class^)
  (export δ^ domain^ obj^ border^ )

  (define-monad M)

  (define (α n)
    (if (number? n)
        (cond [(positive? n) '+]
              [(zero?     n) 0]
              [(negative? n) '-])
        n))

  (define (δ op v₁ v₂)
    (syntax-parse op
      #:literals [iadd isub imul idiv lt eq]
      [iadd
       (match* ((α v₁) (α v₂))
         [( x  0) (return  x)]
         [( 0  x) (return  x)]
         [('+ '+) (return '+)]
         [('- '-) (return '-)]
         [( _  _) (return '⊤)])]
      [isub
       (match* ((α v₁) (α v₂))
         [( x  0) (return  x)]
         [( 0 '-) (return '+)]
         [( 0 '+) (return '-)]
         [( 0 '⊤) (return '⊤)]
         [('- '+) (return '-)]
         [('+ '-) (return '+)]
         [( _  _) (return '⊤)])]
      [imul
       (match* ((α v₁) (α v₂))
         [( _  0) (return  0)]
         [( 0  _) (return  0)]
         [('- '-) (return '+)]
         [('- '+) (return '-)]
         [('+ '-) (return '-)]
         [('+ '+) (return '+)]
         [( _  _) (return '⊤)])]
      [idiv
       (match* ((α v₁) (α v₂))
         [( _  0) fail]
         [( 0 '-) (return  0)]
         [( 0 '+) (return  0)]
         [( 0 '⊤) (return '⊤)]
         [('+ '+) (return '+)]
         [('+ '-) (return '-)]
         [('- '+) (return '-)]
         [('- '-) (return '+)]
         [( _  _) (return '⊤)])]
      [lt
       (match* ((α v₁) (α v₂))
         [( 0  0) (return #f)]
         [( 0 '-) (return #f)]
         [( 0 '+) (return #t)]
         [('-  0) (return #t)]
         [('+  0) (return #f)]
         [('- '+) (return #t)]
         [('+ '-) (return #f)]
         [( _  _) (mplus (return #t) (return #f))])]
      [eq
       (match* ((α v₁) (α v₂))
         [( 0  0) (return #t)]
         [( 0 '-) (return #f)]
         [( 0 '+) (return #f)]
         [('-  0) (return #f)]
         [('+  0) (return #f)]

         [('- '+) (return #f)]
         [('+ '-) (return #f)]
         [( _  _) (mplus (return #t) (return #f))])]
      [_ fail]))

  #;
  (define (δ op v₁ v₂)
    (syntax-parse op
      #:literals [iadd isub imul idiv lt eq]
      [iadd (return 'N)]
      [isub (return 'N)]
      [imul (return 'N)]
      [idiv
       (cond
         [(and (integer? v₂) (zero? v₂)) fail]
         [(integer? v₂) (return 'N)]
         [else (mplus (return 'N) fail)])]
      [lt
       (if (and (integer? v₁) (integer? v₂))
           (return (conc$delta 'lt v₁ v₂))
           (mplus (return #t) (return #f)))]
      [eq
       (if (and (integer? v₁) (integer? v₂))
           (return (conc$delta 'eq v₁ v₂))
           (mplus (return #t) (return #f)))]
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
  ;; o : AObj = Id ↦ Set(AVal + CVal)
  ;; ρ : Ρ    = Id ↦ Addr
  ;; σ : Σ    = Addr ↦ (AVal + AObj)
  ;; θ : AStk = List(AVal + CVal)

  ;; val : CVal = CInt + CObj + #f
  ;; obj : CObj = List(AVal + CVal)
  ;; env : Env = Id ↦ CVal

  ;; TODO: toriaezu
  (define (⊔ν ν₁ ν₂)
    (if (and (integer? ν₁) (integer? ν₂) (= ν₁ ν₂))
        ν₁
        'N))
  (define ⊔o #f)
  (define γ #f)
  ;; (define α #f)


  ;; μ : C↦A = CObj → Addr
  (define global-μ 'μ)
  ;; omap : A↦C = Addr → CObj
  (define global-omap 'omap)

  (define (aundef? x) (return (false? x)))
  (define (astk? x) (return (list? x)))
  (define (memo? x) (return (map? x)))

  ;; TODO: attach tag
  (define (aint? x) (return (or (integer? x) (eq? x 'N))))

  (define (aobj? x) (return (aobject? x)))

  (define (aval? x)
    (do b ← (aundef? x)
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
    (do b ← (aundef? ν)
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
