#lang racket/base
(require racket/unit
         racket/match
         "../signatures.rkt"
         "../misc/transformers.rkt"
         (only-in "../misc/map.rkt" ∅)
         "../misc/set.rkt")
(provide monad-dead@)

(define-unit monad-dead@
  (import)
  (export monad^ menv^ mstore^ mdead^)

  ;; monad^ impl:

  ;; M ρ σ θ a := ρ → σ → θ → (a × σ) × θ
  (define M (ReaderT (FailT (StateT #f (StateT #f ID)))))
  (define-monad M)

  ;; mrun : (M ρ σ θ a) [→ ρ [→ σ [→ θ]]] → (a × σ) × θ
  (define (mrun m [ρ₀ ∅] [σ₀ ∅] [θ₀ (set)])
    (run-StateT θ₀ (run-StateT σ₀ (run-ReaderT ρ₀ m))))

  (define mret (match-lambda
                 [(cons (cons 'failure _) _)
                  (error 'mret "running monad failed.")]
                 [(cons (cons v σ) θ)
                  (values v σ θ)]))

  ;; menv^ impl:
  (define ask-env   ask)
  (define local-env local)

  ;; mstore^ impl:
  (define get-store (bind get (compose1 return car)))
  (define (put-store σ)
    (do (cons _ θ) ← get
        (put (cons σ θ))))
  (define (update-store f)
    (do σ ← get-store
        (put-store (f σ))))

  ;; dead^ impl:
  (define get-dead (bind get (compose1 return cdr)))
  (define (put-dead θ)
    (do (cons σ _) ← get
      (put (cons σ θ))))
  (define (update-dead f)
    (do θ ← get-dead
        (put-dead (f θ)))))
