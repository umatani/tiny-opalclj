#lang racket
(require (for-syntax syntax/parse))
(provide (all-defined-out))

(define-syntax (define-signatures stx)
  (syntax-parse stx
    [(_ [sig (~literal :) name ...+] ...)
     #'(begin (define-signature sig (name ...)) ...)]))

(define-signatures
  [ev^       : ev]
  [ext-ev^   : ext-ev]
  [ev-trace^ : ev-trace]
  [ev-cache^ : ev-cache eval-coind]
  [ev-dead^ : ev-dead eval-dead]
  )


;; monad, monoid, and component-specific effects
(define-signatures
  [monad^   : M mrun mret]
  [monoid^  : O]

  ;; lifted effects for state-space components
  [mcache^    : ask-⊥ local-⊥ get-$ put-$ update-$]
  ;[mlive^     : get-live put-live update-live]
  [menv^      : ask-env local-env]
  [mstore^    : get-store put-store update-store]
  [mdead^     : get-dead put-dead update-dead]
  ;[mcycle^    : ask-cycle local-cycle]
  ;[msymbolic^ : refine get-path-cond]
  ;[mhistory^  : as1k-call-history local-call]
  ;[gc^        : ask-roots extra-roots]
  )

;; metafunctions
(define-signatures
  ;[force^ : force]

  [alloc^    : alloc addr?]
  ;; alloc : any → M addr
  ;;   allocate an address in the heap

  [σ^    : find ext]
  ;; find : addr → M value
  ;;   finds the value bound to addr in the heap
  ;; ext : ddr → value → M unit
  ;;   bind addr to value
  
  [stack^ : θ peek pop push]

  [δ^        : δ truish?]
  ;; δ : value ... → M value
  ;;   primitive operations on values
  ;; truish? : value → M bool
  ;;   does the value subsume 0?

  ;[history^ : H∅ H+ H⁻¹]
  
  [class^ : load-class all-classes
            class<=? has-field?
            all-fields-of methods-of
            lookup-method]

  [domain^ : ⊔ν ⊔o γ α]

  [obj^ : make-aobj class-of lookup-field update-field]

  [border^ : global-μ global-omap
             value→ν value→val val→ν ν→val obj→o o→obj]

  [ρ×σ^ : ref assign global-ρ with withrec-current-ρ]

  [prop^ : set-method-prop display-all-props]

  )
