#lang racket/base
(require racket/unit
         racket/match
         "../signatures.rkt"
         "../misc/transformers.rkt"
         (only-in "../misc/map.rkt" ∅))
(provide monad@)

(define-unit monad@
  (import)
  (export monad^ menv^ mstore^)

  ;; monad^ impl:

  ;; M ρ σ a := ρ → σ → (a × σ)
  (define M (ReaderT (FailT (StateT #f ID))))
  (define-monad M)

  ;; mrun : (M a) [→ ρ [→ θ [→ σ]]] → a × σ
  (define (mrun m [ρ₀ ∅] [σ₀ ∅])
    (run-StateT σ₀ (run-ReaderT ρ₀ m)))

  (define mret (match-lambda
                 [(cons 'failure _)
                  (error 'mret "running monad failed.")]
                 [(cons v σ) (values v σ)]))

  ;; menv^ impl:
  (define ask-env ask)
  (define local-env local)

  ;; mstore^ impl:
  (define get-store get)
  (define put-store put)
  (define (update-store f)
    (do σ ← get-store
        (put-store (f σ)))))


;;;;;;;; Tests ;;;;;;;;
(module+ test
  (define-values/invoke-unit/infer monad@)
  (define-monad M)

  (define test-reader1
    (do ρ ← ask
        (return ρ)))
  (mret (mrun test-reader1))

  (define test-reader2
    (do ρ ← ask
        (local (ρ 'n1 'a1)
          ask)))
  (mret (mrun test-reader2))

  (define test-reader3
    (do ρ ← ask
        (local (ρ 'n1 'a1)
          (do ρ ← ask
              ;;(return ρ)
              (return (ρ 'n1))))))
  (mret (mrun test-reader3))

  (define test-state1
    (do σ ← get
        (return σ)))
  (mret (mrun test-state1))
  
  (define test-state2
    (do σ ← get
        (put (σ 'a1 'v1))
        σ ← get
        (put (σ 'a2 'v2))
        σ ← get
        (return (σ 'a1))))
  (mret (mrun test-state2))

  (define test-fail1
    fail)
  (mret (mrun test-fail1))

  (define test-fail2
    (try (return 'OK)
         (return 'HANDLE)))
  (mret (mrun test-fail2))

  (define test-fail3
    (try fail
         (return 'HANDLE)))
  (mret (mrun test-fail3))

  (define test-fail4
    (try (do ρ ← ask
             fail
             (return ρ))
         (return 'HANDLE)))
  (mret (mrun test-fail4)))
