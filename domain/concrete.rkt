#lang racket/base
;; concrete domain
;;   realize real JVM execution

(require (prefix-in re$ racket)
         racket/dict
         syntax/parse/define
         (for-syntax racket/base
                     racket/dict
                     racket/list
                     racket/match
                     racket/syntax
                     syntax/id-table
                     syntax/stx))
(provide define-toplevel-method-funs
         def-class
         const displayln delta
         load store getfield putfield new invokevirtual
         cint? cobj? cval?

         ;; only for domain/abs-conc.rkt
         cls↦new-fn
         )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-syntax
  (struct class-info (field-descs
                      method-names
                      method-table-id
                      new-fn-id) #:transparent)

  ;; field descripiton = #'(<class name> <field name>)
  (define (field-desc=? fdesc-stx1 fdesc-stx2)
    (and (free-identifier=? (stx-car fdesc-stx1)
                            (stx-car fdesc-stx2))
         (free-identifier=? (stx-car (stx-cdr fdesc-stx1))
                            (stx-car (stx-cdr fdesc-stx2)))))

  (define (build-mtable-info super-mnames super-mtable-id mdefs-stx)
    (syntax-parse mdefs-stx
      [((~and mdef (def-method mname:id
                     (_ (~optional _)) _ _ ...+)) ...)
       (define defined-mnames (syntax->list #'(mname ...)))
       (define defined-meth-mapping
         (make-immutable-free-id-table
          (map cons defined-mnames (syntax->list #'(mdef ...)))))
       ;; update super method infos
       (define-values (mnames mdefs new-mnames)
         (for/fold ([mnames '()]
                    [mdefs '()]
                    [new-mnames defined-mnames])
                   ([m   (in-list super-mnames)]
                    [idx (in-naturals)])
           (values (cons m mnames)
                   (cons (if (dict-has-key? defined-meth-mapping m)
                             (dict-ref defined-meth-mapping m)
                             #`(vector-ref #,super-mtable-id #,idx))
                         mdefs)
                   (filter-not (λ (n) (free-identifier=? n m))
                               new-mnames))))
       ;; add new method infos
       (for/fold ([mnames mnames]
                  [mdefs   mdefs]
                  #:result (values (reverse mnames) (reverse mdefs)))
                 ([m (in-list new-mnames)])
         (values (cons m mnames)
                 (cons (dict-ref defined-meth-mapping m) mdefs)))])))

;; added for experimentation in REPL
;; e.g. (A$f 1) (A$f (run (iconst 1)))
(define-syntax (define-toplevel-method-funs stx)
  (define (define-for-class cname)
    (define cinfo (syntax-local-value cname))
    (define mtable (class-info-method-table-id cinfo))
    (define mnames (class-info-method-names cinfo))
    (define (rhs-for-method mname)
      (define idx (index-of mnames mname free-identifier=?))
      #`(λ (arg)
          ((vector-ref #,mtable #,idx) (new #,cname) arg)))
    (with-syntax ([(f ...) (map (λ (mname)
                                  (format-id cname "~a$~a"
                                             cname mname))
                                mnames)]
                  [(rhs ...)
                   (map rhs-for-method mnames)])
      #'((define f rhs) ...)))
  (syntax-parse stx
    #:literals [define-toplevel-method-funs
                 ]
    [(define-toplevel-method-funs _Object cname:id ... _$Prelude)
     #:with ((d ...) ...) (map
                           define-for-class
                           (syntax->list #'(cname ...)))
     #'(begin d ... ...)]))

;; added for domain/abs-conc.rkt
(define cls↦new-fn (make-hash))

(define-syntax-parser def-class
  [(_ cname:id (~or sname:id #f) (v:id ...) m ...)
   #:with mtable (generate-temporary 'mtable)
   (match-define
     (class-info super-field-descs super-method-names
                 super-mtable-id _)
     (if (and (attribute sname)
              (not (free-identifier=? #'Object #'sname)))
         (syntax-local-value #'sname)
         (class-info '() '() #'dummy #'dummy)))
   (define field-descs (syntax->list
                        #`(#,@super-field-descs [cname v] ...)))
   (define-values (mnames mdefs)
     (build-mtable-info super-method-names super-mtable-id
                        #'(m ...)))
   #`(begin
       (define mtable (vector #,@mdefs))

       (define (new-fn)
         (vector 'cname mtable
                 #,@(make-list (length field-descs) 0)))

       ;; added for domain/abs-conc.rkt
       (dict-set! cls↦new-fn 'cname new-fn)

       (define-syntax cname
         (class-info
          (list
           #,@(map (λ (fdesc)
                     #`(list #'#,(stx-car fdesc)
                             #'#,(stx-car (stx-cdr fdesc))))
                   field-descs))
          (list #,@(map (λ (mname) #`#'#,mname) mnames))
          #'mtable #'new-fn)))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (const v) v)
(define (displayln v) (re$displayln v))

(define-syntax-parser delta
  #:datum-literals [quote iadd isub imul idiv lt eq]
  [(_ 'iadd v₁ v₂) #'(re$+ v₁ v₂)]
  [(_ 'isub v₁ v₂) #'(re$- v₁ v₂)]
  [(_ 'imul v₁ v₂) #'(re$* v₁ v₂)]
  [(_ 'idiv v₁ v₂) #'(re$quotient v₁ v₂)]
  [(_ 'lt   v₁ v₂) #'(re$< v₁ v₂)]
  [(_ 'eq   v₁ v₂) #'(re$= v₁ v₂)]
  [_   (error 'delta "unknow op: ~a" (syntax->datum this-syntax))])

(define-syntax-parser load
  [(_ var:id) #'var] )

(define-syntax-parser store
  [(_ var:id val:expr) #'(set! var val)])

(define-syntax-parser new
  [(_ cname:id)
   #:with new-fn (class-info-new-fn-id (syntax-local-value #'cname))
   #'(new-fn)])

(define-syntax-parser invokevirtual
  [(_ cls:id tgt:expr mtd:id arg:expr)
   (define methods (class-info-method-names
                    (syntax-local-value #'cls)))
   (define idx (index-of methods #'mtd free-identifier=?))
   #`(let ([meth (vector-ref (vector-ref tgt 1) #,idx)])
       (meth tgt arg))])

(define-syntax-parser getfield
  [(_ cls:id tgt:expr fld:id)
   (define idx (index-where
                (class-info-field-descs (syntax-local-value #'cls))
                (λ (fdesc) (field-desc=? fdesc #'(cls fld)))))
   #`(vector-ref tgt #,(+ 2 idx))])

(define-syntax-parser putfield
  [(_ cls:id tgt:expr fld:id val:expr)
   (define idx (index-where
                (class-info-field-descs (syntax-local-value #'cls))
                (λ (fdesc) (field-desc=? fdesc #'(cls fld)))))
   #`(vector-set! tgt #,(+ 2 idx) val)])


(define cint? integer?)

;; val : CVal = CInt + CObj + #f
(define (cobj? val) (vector? val))

;; obj : CObj = Set( ==AVal== + CVal )
(define (cval? x) (or (re$integer? x) (cobj? x) (re$false? x)))
