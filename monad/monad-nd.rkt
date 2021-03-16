#lang racket/base
(require racket/unit
         racket/match
         "../signatures.rkt"
         "../misc/transformers.rkt"
         (only-in "../misc/map.rkt" ∅ ⊔)
         "../misc/set.rkt")
(provide monad-nd@)

(define-unit monad-nd@
  (import)
  (export monad^ menv^ mstore^)

  ;; monad^ impl:

  ;; M := ρ → σ → ℘((a ∪ (failure)) × σ)
  (define M (ReaderT (FailT (StateT #f (NondetT ID)))))
  (define-monad M)

  ;; mrun : (M a) [→ ρ [→ σ]] → ℘((a ∪ (failure)) × σ)
  (define (mrun m [ρ₀ ∅] [σ₀ ∅])
    (run-StateT σ₀ (run-ReaderT ρ₀ m)))

  ;; TODO: value-join & store-join like monad-cache
  (define (mret rs)
    (let-values
        ([(v σ) (for/fold ([v 'failure]
                            [σ ∅]
                            #:result (values v σ))
                           ([v×σ′ (in-set rs)])
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
  (define ask-env ask)
  (define local-env local)

  ;; mstore^ impl:
  (define get-store get)
  (define put-store put)
  (define (update-store f)
    (do σ ← get-store
        (put-store (f σ)))))
