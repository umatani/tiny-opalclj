#lang racket/base
(require racket/unit
         "../signatures.rkt"
         "../misc/transformers.rkt")
(provide ρ×σ@)

(define-unit ρ×σ@
  (import monad^ menv^ σ^ alloc^)
  (export ρ×σ^)

  (define-monad M)

  (define (ref name)
    (do ρ ← ask-env
        (find (ρ name))))
  (define (assign name val)
    (do ρ ← ask-env
        a ≔ (ρ name)
        (ext a val)))

  (define global-ρ 'ρ)

  (define (with name val body)
    (do ρ ← ask-env
        a ← (alloc name)
        (local-env (ρ name a)
          (do (ext a val)
              body))))


  (define (withrec-current-ρ nρ body)
    (do ρ ← ask-env
        a ← (alloc nρ)
        (local-env (ρ nρ a)
          (do ρ ← ask-env
              (ext a ρ)
              body))))  
  )
