#lang racket
(require "../signatures.rkt"
         "../misc/transformers.rkt"
         "../misc/map.rkt"
         "../misc/set.rkt")
(provide alloc-size@ alloc-gensym@ alloc-0cfa@)

(define-unit alloc-size@
  (import monad^ mstore^)
  (export alloc^)

  (define-monad M)

  ;; Relies on invariant that memory is never freed.
  ;; Unsafe with GC.
  (define (alloc _)
    (do σ ← get-store
        (return (size σ))))

  ;; TODO: attach tag
  (define (addr? a) (return (integer? a))))


(define-unit alloc-gensym@
  (import monad^)
  (export alloc^)

  (define-monad M)

  (define (alloc x)
    (return (gensym x)))

  (define (addr? a) (return (symbol? a))))

(define-unit alloc-0cfa@
  (import monad^)
  (export alloc^)

  (define-monad M)

  (define (alloc x)
    ;; Why 'θ cannot be?
    (define roots (set 'θ))
    ;(printf "alloc: ~a ~a\n" x (set-member? roots x))
    (if (set-member? roots x)
        (return (gensym x))
        (return x)))

  (define (addr? a) (return (symbol? a))))
