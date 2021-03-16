#lang racket/base
(require racket/list racket/match racket/dict
         syntax/parse syntax/id-table
         (for-template
          (prefix-in pre$ (submod "literals.rkt" prefix-literals))
          (except-in "../mix.rkt" #%module-begin)
          ))
(provide bcompile)

;;
;; prefix-to-bytecode compiler
;;

(define-syntax-class class-def
    #:literals (pre$class)
    (pattern (pre$class name:id (~optional (~seq #:extends super:id))
              fdef:field-def ... mdef:method-def ...)))

(define-syntax-class field-def
  #:literals (pre$field)
  (pattern (pre$field name:id)))

(define-syntax-class method-def
  #:literals (pre$method)
  (pattern (pre$method name:id (param:id (~optional init-arg:expr))
              var:lvar-def ...
              (~and stmt (~not _:lvar-def)) ... ret)
           #:with (var-name ...) #'(var.name ...)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-env (make-parameter (make-immutable-free-id-table)))

(define (extend-env env vars specs)
  (for/fold ([env env])
            ([var (in-list vars)]
             [spec (in-list specs)])
    (dict-set env var spec)))

(define-syntax-rule (with-extended-env vars specs . body)
  (parameterize ([current-env (extend-env (current-env) vars specs)])
    . body))

(define (bcompile pgm-stx)
  (syntax-parse pgm-stx
    [(cdef:class-def ...)
     (let* ([cdefs (syntax->list #'(cdef ...))]
            [class-table (build-class-table cdefs)])
       `(,#`(def-class Object #f ())
         ,@(map (λ (cdef) (bcompile-class cdef class-table))
                cdefs)
         ,(bcompile-static cdefs)))]))

(define (build-class-table cdefs)
  (for/fold ([cinfo (make-immutable-free-id-table)])
            ([cdef (in-list cdefs)])
    (syntax-parse cdef
      [cdef:class-def
       (dict-set cinfo
                 #'cdef.name
                 (cons (attribute cdef.super)
                       (map (syntax-parser [f:field-def #'f.name])
                            (syntax->list #'(cdef.fdef ...)))))])))

(define (bcompile-class cdef-stx class-table)
  (define (collect-fields cname)
    (if cname
        (match (dict-ref class-table cname)
          [`(,sname . ,fnames)
           `(,@(collect-fields sname)
             ,@(map (λ (fname) (cons fname cname)) fnames))])
        '()))

  (syntax-parse cdef-stx
    [(_ cname:id (~optional (~seq #:extends super:id))
        fdef:field-def ... mdef:method-def ...)
     #:with sname (or (attribute super) #'Object)
     (let ([fld-specs (collect-fields #'cname)])
       (with-extended-env (map car fld-specs) (map cdr fld-specs)
         #`(def-class cname sname
             #,(syntax->list #'(fdef.name ...))
             #,@(map bcompile-method
                     (syntax->list #'(mdef ...))))))]))
  
(define-syntax-class lvar-def
  #:literals (pre$local)
  (pattern (pre$local name:id)))

(define (bcompile-static cdefs)
  (syntax-parse cdefs
    [(cdef:class-def ...)
     #:with ([cn mn init ...] ...)
     (append-map
      (syntax-parser
        #:literals [pre$class]
        [(pre$class cn:id (~optional (~seq #:extends _:id))
                    _:field-def ...
                    mdef:method-def ...)
         #:with (mdef′ ...) (filter
                              (syntax-parser
                                [mdef:method-def
                                 (attribute mdef.init-arg)])
                              (syntax->list
                               #'(mdef ...)))
         #:with ([mn init ...] ...) (map
                                     (syntax-parser
                                       [mdef:method-def
                                        #`(mdef.name
                                           #,@(bcompile-exp
                                               #'mdef.init-arg))])
                                     (syntax->list #'(mdef′ ...)))
         (syntax->list #'((cn mn init ...) ...))])
      (syntax->list #'(cdef ...)))
     #`(def-class $Prelude Object ()
         (def-method #,sinit-id ($dummy) ()
           (abs (begin (begin (new #,(gensym 'nw) cn)
                              init ...
                              (invokevirtual cn mn)
                              (set-meta cn mn))
                       ...
                       (iconst 0)))))]))

(define (enclose-begin insts)
  (if (= (length insts) 1)
      (car insts)
      (cons #'begin insts)))

(define (bcompile-method mdef-stx)
  (syntax-parse mdef-stx
    [md:method-def
     (define vars (syntax->list #'(md.param md.var-name ...)))
     (with-extended-env vars (make-list (length vars) 'local)
       #`(def-method md.name (md.param)
           (md.var-name ...)
           #,(enclose-begin
              (append (append-map bcompile-stmt
                                  (syntax->list #'(md.stmt ...)))
                      (bcompile-exp #'md.ret)))))]))

(define (bcompile-stmt stmt-stx)
  (syntax-parse stmt-stx
    #:literals (pre$= pre$println pre$if pre$while pre$begin
                pre$abs pre$conc pre$fref)

    [(pre$begin stmt ...)
     (append-map bcompile-stmt (syntax->list #'(stmt ...)))]

    [(pre$abs stmt ...)
     (list #`(abs
              #,(enclose-begin
                 (append-map bcompile-stmt
                             (syntax->list #'(stmt ...))))))]
    [(pre$conc stmt ...)
     (list #`(conc
              #,(enclose-begin
                 (append-map bcompile-stmt
                             (syntax->list #'(stmt ...))))))]

    [(pre$if cond thn els)
     (list #`(if (begin #,@(bcompile-exp #'cond)
                        (iconst 1)
                        (eq))
                 #,(enclose-begin (bcompile-stmt #'thn))
                 #,(enclose-begin (bcompile-stmt #'els))))]

    [(pre$while cond body)
     (list #`(while (begin #,@(bcompile-exp #'cond)
                           (iconst 1)
                           (eq))
               #,(enclose-begin (bcompile-stmt #'body))))]

    [(pre$println exp)
     (append
      (bcompile-exp #'exp)
      (list #'(print-top)))]

    [(pre$= v:id exp)
     (let ([ecode (bcompile-exp #'exp)])
       (match (dict-ref (current-env) #'v)
         ['local
          `(,@ecode ,#'(store v))]
         [(? identifier? cname)
          `(,#'(load self) ,@ecode ,#`(putfield #,cname v))]))]
    [(pre$= (pre$fref cname:id exp₀ fname:id) exp)
     (let ([lcode (bcompile-exp #'exp₀)]
           [rcode (bcompile-exp #'exp)])
       `(,@lcode ,@rcode ,#`(putfield cname fname)))]))

(define (bcompile-exp exp-stx)
  (syntax-parse exp-stx
    #:literals (pre$true pre$false pre$+ pre$- pre$* pre$/
                pre$< pre$== pre$! pre$&& pre$||
                pre$new pre$fref pre$send pre$this
                pre$abs pre$conc)
    [pre$true  (list #'(iconst 1))]
    [pre$false (list #'(iconst 0))]
    [i:integer (list #'(iconst i))]
    [pre$this  (list #'(load self))]
    [v:id
     (match (dict-ref (current-env) #'v)
       ['local
        (list #'(load v))]
       [(? identifier? cname)
        (list #'(load self) #`(getfield #,cname v))])]
    [(pre$+ e1 e2)
     (append (bcompile-exp #'e1)
             (bcompile-exp #'e2)
             (list #'(iadd)))]
    [(pre$- e1 e2)
     (append (bcompile-exp #'e1)
             (bcompile-exp #'e2)
             (list #'(isub)))]
    [(pre$* e1 e2)
     (append (bcompile-exp #'e1)
             (bcompile-exp #'e2)
             (list #'(imul)))]
    [(pre$/ e1 e2)
     (append (bcompile-exp #'e1)
             (bcompile-exp #'e2)
             (list #'(idiv)))]
    [(pre$< e1 e2)
     (append (bcompile-exp #'e1)
             (bcompile-exp #'e2)
             (list #'(if (lt)
                         (iconst 1)
                         (iconst 0))))]
    [(pre$== e1 e2)
     (append (bcompile-exp #'e1)
             (bcompile-exp #'e2)
             (list #'(if (eq)
                         (iconst 1)
                         (iconst 0))))]
    [(pre$! e)
     (append (bcompile-exp #'e)
             (list #'(iconst 0)
                   #'(if (eq)
                         (iconst 1)
                         (iconst 0))))]
    [(pre$&& e1 e2)
     (append (bcompile-exp #'e1)
             (list #'(iconst 0)
                   #`(if (eq)
                         (iconst 0)
                         (begin
                           #,@(bcompile-exp #'e2)
                           (iconst 0)
                           (if (eq)
                               (iconst 0)
                               (iconst 1))))))]
    [(pre$|| e1 e2)
     (append (bcompile-exp #'e1)
             (list #'(iconst 0)
                   #`(if (eq)
                         (begin
                           #,@(bcompile-exp #'e2)
                           (iconst 0)
                           (if (eq)
                               (iconst 0)
                               (iconst 1)))
                         (iconst 1))))]
    [(pre$new cname:id)
     (list #`(new #,(gensym (syntax->datum #'cname)) cname))]
    [(pre$fref cname:id tgt fname:id)
     (append (bcompile-exp #'tgt)
             (list #'(getfield cname fname)))]
    [(pre$send cname:id tgt mname:id arg)
     (append (bcompile-exp #'tgt)
             (bcompile-exp #'arg)
             (list #'(invokevirtual cname mname)))]
    [(pre$abs e)
     (list #`(abs #,(enclose-begin (bcompile-exp #'e))))]
    [(pre$conc e)
     (list #`(conc #,(enclose-begin (bcompile-exp #'e))))]))
