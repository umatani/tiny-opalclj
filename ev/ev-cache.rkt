#lang racket/base
(require racket/unit
         "../misc/transformers.rkt"
         "../signatures.rkt"
         "../misc/map.rkt"
         "../misc/set.rkt")
(provide ev-cache@)

(define-unit ev-cache@
  (import monad^ menv^ mstore^ mcache^)
  (export ev-cache^)

  (define-monad M)

  (define (((ev-cache ev₀) ev) e)
    (do ρ ← ask-env
        σ ← get-store
        ς ≔ (list e ρ σ)
        Σ ← get-$
        (if (∈ ς Σ)
            (for/monad+ ([v.σ (Σ ς)])
              (do (put-store (cdr v.σ))
                  (return (car v.σ))))
            (do Σ⊥ ← ask-⊥
                (put-$ (Σ ς (if (∈ ς Σ⊥) (Σ⊥ ς) (set))))
                v  ← ((ev₀ ev) e)
                σ  ← get-store
                (update-$ (λ (Σ) (Σ ς (set-add (Σ ς) (cons v σ)))))
                (return v)))))

  ; eval-coind : (e → M v) → e → M v
  (define ((eval-coind eval) e)  
    (do ρ ← ask-env
        σ ← get-store
        ς ≔ (list e ρ σ)
        (mlfp (λ (Σ) (do (put-$ ∅)
                           (put-store σ)
                         (local-⊥ Σ (eval e))
                         get-$)))
        Σ ← get-$
        (for/monad+ ([v.σ (Σ ς)])
          (do (put-store (cdr v.σ))
              (return (car v.σ))))))

  ; mlfp : ((k → v) → M (k ↦ v)) → M unit
  (define (mlfp f)
    (let loop ([x ∅])
      ;(printf "mlfp ~a\n" (size x))
      (do x′ ← (f x)
          (if (equal? (size x′) (size x))
              (return (void))
              (loop x′)))))
  )
