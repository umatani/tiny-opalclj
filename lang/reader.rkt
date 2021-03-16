#lang s-exp syntax/module-reader

tiny-opalclj/frontend/infix

#:read-syntax
read-syntax
#:read
(λ (in)
  (let ([asts (read-syntax 'prog in)])
    (map syntax->datum asts)))

#:whole-body-readers?
#t

(require "../frontend/parser.rkt")
(define (read-syntax name in) (parse name in))

;; for test
(define (read-from-string str)
  (map syntax->datum (parse 'prog (open-input-string str))))
