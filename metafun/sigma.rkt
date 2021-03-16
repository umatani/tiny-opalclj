#lang racket
(require "../misc/transformers.rkt"
         "../signatures.rkt"
         "../misc/map.rkt"
         "../misc/set.rkt")
(provide σ@ σ-nd@)

(define-unit σ@
  (import monad^ mstore^)
  (export σ^)

  (define-monad M)

  (define (find a)
    (do σ ← get-store
        (return (σ a))))

  (define (ext a v) (update-store (λ (σ) (σ a v)))))

;; Special care must be taken to the following:
;;   * θ (abstract stack) must be single (why?)
;;   * #f (undef) may be included due to nondet
;;   * values stored in the concrete world
(define-unit σ-nd@
  (import monad^ mstore^ menv^ stack^)
  (export σ^)

  (define-monad M)

  (define (find a)
    (if a ;; a maybe #f (undef)
        (do ;_ ≔ (printf "find: ~a\n" a)
            ;ρ ← ask-env
            ;θ-addr ≔ (ρ θ)
            σ ← get-store
            x ≔ (σ a)
            ;_ ≔ (printf "  found: ~a\n" x)
            (cond
              ;[(equal? a θ-addr) (return x)] ;; abs stack
              [(set? x) (for/monad+ ([v x])
                          (return v))] ; set of vals into nondet
              [else (return x)]))
        fail))

  (define (ext a v)
    (do ;_ ≔ (printf "ext: ~a ~a\n" a v)
        ρ ← ask-env
        θ-addr ≔ (ρ θ)
        (update-store
         (λ (σ)
           (if (equal? a θ-addr)
               (σ a v)
               (if (∈ a σ)
                   (let ([x (σ a)])
                     ;(printf "  ext found: ~a\n" x)
                     (if (set? x)
                         (σ a (set-add x v))
                         (σ a (set x v))))
                   (σ a (set v)))))))))
