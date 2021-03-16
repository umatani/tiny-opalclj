#lang racket/base
(require (for-syntax racket/base)
         racket/pretty
         "misc/set.rkt")
(provide (all-defined-out))

;;;;;; recipes for monadic abstract interpreters

(module spec racket/base
  (provide (struct-out meval-spec))
  (struct meval-spec (export link meval mret→v×σ)))

(begin-for-syntax
  (require (submod 'spec)))

;; concrete domain
(define-syntax eval-conc
  (meval-spec
   '(monad^ menv^ ρ×σ^ stack^ class^ border^ ev^ ext-ev^ prop^)
   '(monad@ σ@ stack@ ρ×σ@ class+alloc-size@
     alloc-gensym@ ev@ ext-ev@ abs-conc@ prop@)
   '(fix (ext-ev ev))
   (λ (v σ) (values v σ))))

;; reachable state collecting semantics
(define-syntax eval-trace
  (meval-spec
   '(monad^ menv^ ρ×σ^ stack^ class^ border^ ev-trace^
     ev^ ext-ev^ prop^)
   '(ListO@ monad-output@ σ@ stack@ ρ×σ@ class+alloc-size@
     alloc-gensym@ ev-trace@ ev@ ext-ev@ abs-conc@ prop@)
   '(fix (ev-trace (ext-ev ev)))
   #'(λ (v σ τ)
       (printf "trace: ~a\n" (map car τ))
       (values v σ))))

;; dead code collecting semantics
(define-syntax eval-dead
  (meval-spec
   '(monad^ menv^ ρ×σ^ stack^ class^ border^ ev-dead^
     ev^ ext-ev^ prop^)
   '(monad-dead@ σ@ stack@ ρ×σ@ class+alloc-size@
     alloc-gensym@ ev-dead@ ev@ ext-ev@ abs-conc@ prop@)
   '(eval-dead (fix (ev-dead (ext-ev ev))))
   #'(λ (v σ Δ)
       (printf "Δ: ~a " (set-count Δ))
       (pretty-print (set-map Δ syntax->datum))
       (values v σ))))

;; non-deterministic concrete domain
(define-syntax eval-nd-conc
  (meval-spec
   '(monad^ menv^ ρ×σ^ stack^ class^ border^ ev^ ext-ev^ prop^)
   '(monad-nd@ σ-nd@ stack@ ρ×σ@ class+alloc-size@
     alloc-0cfa@ ev@ ext-ev@ abs-conc@ prop-nd@)
   '(fix (ext-ev ev))
   (λ (v σ) (values v σ))))

;; non-deterministic abstract domain
(define-syntax eval-nd-abs
  (meval-spec
   '(monad^ menv^ ρ×σ^ stack^ class^ border^ ev^ ext-ev^ prop^)
   '(monad-nd@ σ-nd@ stack@ ρ×σ@ class+alloc-size@
     alloc-0cfa@ ev@ ext-ev@ abs-abs@ prop-nd@)
   '(fix (ext-ev ev))
   (λ (v σ) (values v σ))))

;; abstract interpretation
(define-syntax eval-abs
  (meval-spec
   '(monad^ menv^ ρ×σ^ stack^ class^ border^
            ev^ ext-ev^ ev-cache^ prop^)
   '(monad-pdcfa@ σ-nd@ stack@ ρ×σ@ class+alloc-size@
     alloc-0cfa@ ev@ ext-ev@ ev-cache@ abs-abs@ prop-nd@)
   '(eval-coind (fix (ev-cache (ext-ev ev))))
   (λ (v σ) (values v σ))))
