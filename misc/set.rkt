;;;; code from https://github.com/plum-umd/abstracting-definitional-interpreters.git

#lang racket
;; A set compatible with match
(provide (rename-out [_set set])
         (except-out (all-from-out racket/set) set))
(require racket/set)

(define-match-expander _set
  (syntax-rules (... ...)
    [(_set p ... q (... ...))
     (and (app set->list (list-no-order p ... qs (... ...)))
          (app (λ (_) (list->set qs)) q))]
     
    [(_set p ...)
     (app set->list (list-no-order p ...))])
  (syntax-id-rules ()
    [(_set e ...) (set e ...)]
    [_set set]))
