#lang racket
(require "../misc/transformers.rkt"
         "../signatures.rkt")
(provide stack@)

(define-unit stack@
  (import monad^ menv^ σ^)
  (export stack^)

  (define-monad M)

  (define θ 'θ)

  (define peek
    (do ρ ← ask-env
        stk ← (find (ρ θ))
        (if (null? stk)
            fail
            (return (car stk)))))

  (define pop
    (do ρ ← ask-env
        a ≔ (ρ θ)
        stk ← (find a)
        (if (null? stk)
            fail
            (do (ext a (cdr stk))
                (return (car stk))))))

  (define (push v)
    (do ρ ← ask-env
        a ≔ (ρ θ)
        stk ← (find a)
        (ext a (cons v stk)))))
