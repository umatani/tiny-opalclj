#lang racket/base
(require racket/unit
         syntax/parse
         "../signatures.rkt"
         "../misc/misc.rkt"
         "../misc/map.rkt"
         "../misc/transformers.rkt"
         "../ext-vm.rkt"
         (only-in "../metafun/alloc.rkt" alloc-size@))
(provide class+alloc-size@)

;; class description
;; Sym × Sym × (List Sym) × (§ym → Mdesc)
(struct class-desc (name super fields methods))

;; method description
;; Sym × Sym × (List Sym) × Inst × (AVal + AObj)
(struct method-desc (name param locals body))

(define-unit class@
  (import monad^ menv^ ρ×σ^)
  (export class^)

  (define-monad M)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define class-names 'class-names)

  (define (cdesc-of cls) (ref cls))

  (define (class<=? cls cls′)
    (if (eq? cls cls′)
        (return #t)
        (do cdesc ← (cdesc-of cls)
            sup ≔ (and cdesc (class-desc-super cdesc))
            (if sup
                (class<=? sup cls′)
                (return #f)))))

  (define (has-field? cls fld)
    (do cdesc ← (cdesc-of cls)
        (return (and cdesc
                     (member fld (class-desc-fields cdesc))))))

  (define (lookup-method cls mtd)
    (do (class-desc _ sup _ mtbl) ← (cdesc-of cls)
        (with-handlers
          ([exn:fail? (λ (exn)
                        (if sup
                            (lookup-method sup mtd)
                            (return #f)))])
          (do (method-desc _ param locals body) ≔ (mtbl mtd)
              (return (list param locals body))))))

  (define (all-fields-of cls)
    (do (class-desc _ sup flds _) ← (cdesc-of cls)
        flds′ ≔ (map (λ (fld) (cons cls fld)) flds)
        (if sup
            (do all-flds ← (all-fields-of sup)
                (return (append all-flds flds′)))
            (return flds′))))

  (define (load-class bytecode)
    (define (l-c bytecode)
      (syntax-parse bytecode
        #:literals [#%module-begin def-class]
        [(#%module-begin
          (def-class cname:id . _) form ...)
         #:when (eq? (id->sym #'cname) '$Prelude)
         (l-c #'(#%module-begin form ...))]
        [(#%module-begin
          (def-class cname:id (~or sname:id #f) (v:id ...) m ...)
          form ...)
         (define cls-name (id->sym #'cname))
         (define super-name (and (attribute sname)
                                 (syntax->datum #'sname)))
         (define fields (syntax->datum #'(v ...)))
         (define methods
           (foldl
            (λ (m tbl)
              (syntax-parse m
                #:literals [def-method]
                [(def-method mname:id (pname:id) (v:id ...) inst)
                 (define meth-name (id->sym #'mname))
                 (define param-name (id->sym #'pname))
                 (tbl meth-name
                      (method-desc meth-name param-name
                                   (syntax->datum #'(v ...))
                                   #'inst))]))
            ∅
            (syntax->list #'(m ...))))
         (do cs ← (ref class-names)
             (assign class-names (cons cls-name cs))
             (with cls-name
                   (class-desc cls-name super-name fields methods)
                   (l-c #'(#%module-begin form ...))))]
        [(#%module-begin)
         ask-env]))
    (do (with class-names '()
              (l-c bytecode))))

  (define all-classes (ref class-names))

  (define (methods-of cls)
    (do (class-desc _ _ _ mtbl) ← (cdesc-of cls)
        (return (keys mtbl))))
  )

(define-compound-unit/infer class+alloc-size@
  (import monad^ menv^ mstore^ σ^ ρ×σ^)
  (export class^)
  (link alloc-size@ class@))
