#lang racket
(require "../signatures.rkt"
         "../misc/map.rkt"
         "../misc/set.rkt")
(provide prop@ prop-nd@)

(define-unit prop@
  (import)
  (export prop^)

  (define properties (box ∅))

  (define (set-method-prop cls mtd prop)
    (set-box! properties
              ((unbox properties) (cons cls mtd) prop)))

  (define (display-all-props)
    (printf "[Props]\n")
    (for ([(k p) (∈ (unbox properties))])
      (printf "~a$~a: ~a\n" (car k) (cdr k) p)))
  )


(define-unit prop-nd@
  (import)
  (export prop^)

  (define properties (box ∅))

  (define (set-method-prop cls mtd prop)
    (set-box! properties
              (let* ([pmap (unbox properties)])
                (pmap (cons cls mtd)
                      (if (∈ (cons cls mtd) pmap)
                          (set-add (pmap (cons cls mtd)) prop)
                          (set prop))))))

  (define (display-all-props)
    (printf "[Props]\n")
    (for ([(k p) (∈ (unbox properties))])
      (printf "~a$~a: ~a\n" (car k) (cdr k) p)))
  )
