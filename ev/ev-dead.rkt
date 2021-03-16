#lang racket/base
(require racket/unit
         syntax/parse
         "../misc/transformers.rkt"
         "../signatures.rkt"
         "../misc/set.rkt"
         "../ext-vm.rkt")
(provide ev-dead@)

(define-unit ev-dead@
  (import monad^ mdead^ class^)
  (export ev-dead^)

  (define-monad M)

  (define (((ev-dead ev₀) ev) inst)
    (do Δ ← get-dead
        Δ′ ≔ (set-remove Δ inst)
        (put-dead Δ′)
        ((ev₀ ev) inst)))

  (define ((eval-dead eval) inst)
    (do Δ ← all-insts
        _ ≔ (printf "init-Δ: ~a\n" (set-count Δ))
        (put-dead Δ)
        (eval inst)))

  (define all-insts
    (do clss ← all-classes
        (all-insts-of-classes clss)))

  (define (all-insts-of-classes clss)
    (if (null? clss)
        (return (set))
        (do (cons cls clss′) ≔ clss
            mtds ← (methods-of cls)
            insts ← (all-insts-of-methods cls mtds)
            insts′ ← (all-insts-of-classes clss′)
            (return (set-union insts insts′)))))

  (define (all-insts-of-methods cls mtds)
    (if (null? mtds)
        (return (set))
        (do (cons mtd mtds′) ≔ mtds
            (list _ _ body) ← (lookup-method cls mtd)
            insts ≔ (all-insts-of-inst body)
            insts′ ← (all-insts-of-methods cls mtds′)
            (return (set-union insts insts′))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (all-insts-of-inst inst)
  (syntax-parse inst
    #:literals [begin iconst load store iadd isub imul idiv lt eq
                      if while new invokevirtual getfield putfield
                      print-top dup abs conc set-meta]
    [(begin inst′ ...+)
     (apply set-union (set inst)
            (map all-insts-of-inst (attribute inst′)))]
    [(iconst i:integer) (set inst)]
    [(load  v:id) (set inst)]
    [(store v:id) (set inst)]
    [(iadd)       (set inst)]
    [(isub)       (set inst)]
    [(imul)       (set inst)]
    [(idiv)       (set inst)]
    [(lt)         (set inst)]
    [(eq)         (set inst)]
    [(if cond:expr thn:expr els:expr)
     (set-union (set inst)
                (all-insts-of-inst #'cond)
                (all-insts-of-inst #'thn)
                (all-insts-of-inst #'els))]
    [(while cond:expr body:expr)
     (set-union (set inst)
                (all-insts-of-inst #'cond)
                (all-insts-of-inst #'body))]
    [(new lbl:id cname:id) (set inst)]
    [(invokevirtual cname:id mname:id) (set inst)]
    [(getfield cname:id fname:id) (set inst)]
    [(putfield cname:id fname:id) (set inst)]
    [(print-top) (set inst)]
    [(dup)       (set inst)]
    [(abs inst′)
     (set-union (set inst) (all-insts-of-inst #'inst′))]
    [(conc inst′)
     (set-union (set inst) (all-insts-of-inst #'inst′))]
    [(set-meta cname:id mname:id) (set inst)]))
