#lang racket

(define-syntax-rule (define-literals lit ...)
  (begin (define-syntax lit (syntax-rules ())) ...))

(module* infix-literals #f
  (provide (all-defined-out))
  (define-literals boolean int))

(module* prefix-literals #f
  (provide (all-defined-out))
  (define-literals
    + - * /
    < == = ! && ||
    class abs conc field method local begin
    false if new println fref send this true while))
