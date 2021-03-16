#lang racket/base
(require (for-syntax racket/base)
         "map.rkt"
         syntax/parse/define)

(provide (for-syntax log-level)
         id->sym assoc-to-map warning log verbose)

(define (id->sym id)
  (if (identifier? id)
      (syntax->datum id)
      (error 'id->sym "not identifier: ~a" id)))

(define (assoc-to-map as)
  (for/map ([a as]) (values (car a) (cadr a))))

(define-for-syntax log-level (make-parameter 2))
(define-syntax-parser log-print
  [(log level:integer who:id format:string v:expr ...)
   (define l (syntax->datum #'level))
   (if (<= l (log-level))
       (with-syntax ([lstr (case l
                             [(1) #'"warning"]
                             [(2) #'"log"]
                             [(3) #'"verbose"])])
         #'(printf (string-append
                       "[" lstr "] "
                       (symbol->string 'who)
                       ": " format "\n") v ...))
       #'(void))])

(define-simple-macro (warning . args) (log-print 1 . args))
(define-simple-macro (log . args)     (log-print 2 . args))
(define-simple-macro (verbose . args) (log-print 3 . args))
