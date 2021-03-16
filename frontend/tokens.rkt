#lang racket

(require parser-tools/lex)
(provide (all-defined-out))

(define-empty-tokens Operators
  (OR && == < + - * / ! =))

(define-empty-tokens Separators
  (O_PAREN C_PAREN O_BRACE C_BRACE
   SEMI_COLON PERIOD COMMA White-Space
   HASH))

(define-empty-tokens EmptyLiterals
  (TRUE_LIT FALSE_LIT EOF))

(define-empty-tokens Keywords 
  (if while else this new int boolean
   class extends return System.out.println
   abs conc
   untyped eval ;; for # directive
   ))

(define-tokens tiny-java-vals
  (INTEGER_LIT IDENTIFIER))

(define-tokens racket-vals
  (RIDENTIFIER))
