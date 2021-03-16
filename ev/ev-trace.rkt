#lang racket/base
(require racket/unit
         "../misc/transformers.rkt"
         "../signatures.rkt")
(provide ev-trace@)

(define-unit ev-trace@
  (import monad^ menv^ mstore^)
  (export ev-trace^)

  (define-monad M)

  (define (((ev-trace ev₀) ev) inst)
    (do ρ ← ask-env
        σ ← get-store
        (tell `((,(syntax->datum inst) ,ρ ,σ)))
        ((ev₀ ev) inst))))
