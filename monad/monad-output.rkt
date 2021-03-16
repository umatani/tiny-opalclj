#lang racket/base
(require racket/unit
         racket/match
         "../signatures.rkt"
         "../misc/transformers.rkt"
         (only-in "../misc/map.rkt" ∅))
(provide monad-output@)

(define-unit monad-output@
  (import monoid^)
  (export monad^ menv^ mstore^)
  (init-depend monoid^)

  ;; monad^ impl:

  ;; M O ρ σ a := ρ → σ → (a × O a) × σ
  (define M (ReaderT (FailT (StateT #f (WriterT O ID)))))
  (define-monad M)

  ;; mrun : (M O ρ σ a) [→ ρ [→ σ]] → (a × O a) × σ
  (define (mrun m [ρ₀ ∅] [σ₀ ∅])
    (run-StateT σ₀ (run-ReaderT ρ₀ m)))

  (define mret (match-lambda
                 [(cons (cons 'failure _) _)
                  (error 'mret "running monad failed.")]
                 [(cons (cons v σ) τ)
                  (values v σ τ)]))

  ;; menv^ impl:
  (define ask-env   ask)
  (define local-env local)

  ;; mstore^ impl:
  (define get-store get)
  (define put-store put)
  (define (update-store f)
    (do σ ← get-store
        (put-store (f σ)))))
