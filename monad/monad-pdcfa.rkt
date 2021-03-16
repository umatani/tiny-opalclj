#lang racket/base
(require racket/unit
         racket/match
         "../signatures.rkt"
         "../misc/transformers.rkt"
         (only-in "../misc/map.rkt" ∅)
         "../misc/set.rkt")
(provide monad-pdcfa@)

(define-unit monad-pdcfa@
  (import)
  (export monad^ menv^ mstore^ mcache^)

  (define (CacheT O M)
    (ReaderT   ; Σ⊥
     (StateT O ; Σ
             ID)))

  (define (run-CacheT Σ⊥₀ Σ₀ m)
    (run-StateT Σ₀ (run-ReaderT Σ⊥₀ m)))

  ;; disregard the cache on return
  (define (ret-CacheT x) (car x))

  ;; monad^ impl:

  ;; M ρ σ Σ⊥ Σ a := ρ → σ → Σ⊥ → Σ → ℘(((a ∪ (failure)) × σ)) × Σ
  (define M
    (ReaderT                  ; ρ
     (FailT
      (StateT #f               ; σ
              (NondetT
               (CacheT (FinMapO PowerO) ID))))))

  (define-monad M)

  ;; mrun : (M a) [→ ρ [→ θ [→ σ]]] → a × σ
  (define (mrun m [ρ₀ ∅] [σ₀ ∅] [Σ⊥₀ ∅] [Σ₀ ∅])
    (run-CacheT Σ⊥₀ Σ₀
                (run-StateT σ₀
                            (run-ReaderT ρ₀ m))))

  (define (mret x)
    (let-values
        ([(v σ) (for/fold ([v 'failure]
                            [σ ∅]
                            #:result (values v σ))
                           ([v×σ′ (in-set (ret-CacheT x))])
                   (match-define (cons v′ σ′) v×σ′)
                   (match* (v v′)
                     [('failure _) (values v′ σ′)]
                     [(_ 'failure) (values v σ)]
                     [(_ _)
                      #;(join-aval old new)
                      (values v′ σ′)]))])
      (if (eq? v 'failure)
          (error 'mret "running monad failed.")
          (values v σ))))

  ;; menv^ impl:
  (define ask-env (bind ask (compose1 return car)))

  (define (local-env ρ m)
    (do (cons _ Σ⊥) ← ask
        (local (cons ρ Σ⊥) m)))

  ;; mstore^ impl:
  (define get-store (bind get (compose1 return car)))

  (define (put-store σ)
    (do (cons _ Σ) ← get
        (put (cons σ Σ))))

  (define (update-store f)
    (do σ ← get-store
        (put-store (f σ))))

  ;; mcache^ impl:
  (define ask-⊥ (bind ask (compose1 return cdr)))
  (define (local-⊥ Σ⊥ m)
    (do (cons ρ _) ← ask
        (local (cons ρ Σ⊥) m)))

  (define get-$ (bind get (compose1 return cdr)))
  (define (put-$ Σ)
    (do (cons σ _) ← get
        (put (cons σ Σ))))
  (define (update-$ f)
    (do Σ ← get-$
        (put-$ (f Σ))))
  )

(module+ test
  (define-values/invoke-unit/infer monad-pdcfa@)
  (define-monad M)

  (mret (mrun (mplus (return 1)
                     (return 'foo)) 'ρ 'σ 'c ∅))
  )
