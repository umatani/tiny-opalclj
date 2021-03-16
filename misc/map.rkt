;;;; code from https://github.com/plum-umd/abstracting-definitional-interpreters.git

#lang racket
;; Finite map data structure
(require racket/hash)
(provide map? ∅ ∅-eq ⊔ ∈ size hash->map for/map
         rng keys restrict map-rem
         (rename-out [match-map ↦]))

(define-match-expander match-map
  (lambda (stx)
    (syntax-case stx ()
      [(_ (x y) ...)
       #'(app map-to-hash (hash-table (x y) ...))])))
    

(define (map-print m port mode)
  (let ([recur (case mode
                 [(#t) write]
                 [(#f) display]
                 [else (λ (p port) (print p port mode))])])
    (recur (hash->list (map-to-hash m)) port)))

(struct map (to-hash)
  #:transparent
  #:property
  prop:procedure
  (case-lambda
    [(m x) 
     (hash-ref (map-to-hash m) x)]
    [(m x v)
     (map (hash-set (map-to-hash m) x v))])
  #:methods gen:custom-write
  [(define write-proc map-print)])

(define hash->map map)

(define (rng r)
  (for/set ([(_ v) (∈ r)]) v))

(define (restrict r xs)
  (for/map ([(x _) (∈ r)]
            #:when (set-member? xs x))
    (values x (r x))))

(define ∈
  (case-lambda
    [(k m) (hash-has-key? (map-to-hash m) k)]
    [(m)   (in-hash (map-to-hash m))]))

(define (size m) (hash-count (map-to-hash m)))

(define (keys m)
  (hash-keys (map-to-hash m)))

(define (map-rem m j)
  (hash->map (hash-remove (map-to-hash m) j)))

(define ∅ (map (hash)))
(define ∅-eq (map (hasheq)))

(define (⊔ m₁ #:combine [cod-⊔ (λ (x _) x)] . m)
  (map (apply hash-union
              (map-to-hash m₁)
              (foldl (λ (m a) (cons (map-to-hash m) a)) '() m)
              #:combine cod-⊔)))

(define-syntax (for/map stx)
  (syntax-case stx ()
    [(_ clauses defs+exprs ...)
     (with-syntax ([original stx])
       #'(for/fold/derived original ([m ∅]) clauses
           (let-values ([(k v) (let () defs+exprs ...)])
             (m k v))))]))

(module+ test
  (require rackunit)
  (define r ∅)
  (check-equal? ((r 'x 1) 'x) 1)
  (check-equal? ((r 'x 1) 'y 2) ((r 'y 2) 'x 1))
  (check-equal? (∈ 'x (r 'x 1)) #t)
  (check-equal? ('y . ∈ . (r 'x 1)) #f)
  (check-true
   (let ([l (for/list ([(k v) (∈ ((r 'x 1) 'y 2))])
              (cons k v))])
     (not (false? (and (member (cons 'y 2) l)
                       (member (cons 'x 1) l)))))))
