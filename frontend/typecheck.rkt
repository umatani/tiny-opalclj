;; based on the typechecker from https://github.com/dfeltey/lwc2016.git
#lang racket/base

;; typecheck, and turn the old, paren-but-infix syntax to the new,
;; fully-prefix one, with explicit types at `send`s

(require (except-in racket/match ==)
         racket/format racket/dict racket/sequence
         (except-in syntax/parse boolean static)
         syntax/id-table
         syntax/id-set
         "../misc/error.rkt"
         (for-template (submod "literals.rkt" infix-literals))
         (for-template (submod "literals.rkt" prefix-literals)))

(provide typecheck-program)

;; Preserve syntax properties on the result of typechecking
(define typecheck #'typecheck)
(define-syntax-rule (syntax-parse/track-origin stx body ...)
  (apply
   values
   (map
    (λ (new-stx)
      (if (syntax? new-stx)
          (syntax-track-origin new-stx stx typecheck)
          new-stx))
    (call-with-values
     (λ () (syntax-parse stx body ...))
     list))))

;; Env
(define current-env (make-parameter (make-immutable-free-id-table)))

;; Types
(struct ty ())
(struct top-ty ty ())
(struct class-ty ty (name super fields methods) #:transparent)
(struct object-ty ty (class-ty) #:transparent)
(struct field-ty ty (name ty) #:transparent)
(struct method-ty ty (name arg-ty res-ty) #:transparent)
(struct base-ty ty (key) #:transparent)
(define int-ty (base-ty 'int))
(define bool-ty (base-ty 'boolean))
(struct binop-ty (left right res) #:transparent)

(define (make-class-ty name super fields methods)
  (class-ty name
            super
            (make-immutable-free-id-table
             (for/list ([field (in-list fields)])
               (match-define (field-ty name _) field)
               (cons name field)))
            (make-immutable-free-id-table
             (for/list ([method (in-list methods)])
               (match-define (method-ty name _ _) method)
               (cons name method)))))

;; (or/c type? identifier?) -> type?
(define (resolve-type t [env (current-env)])
  (cond
    [(ty? t) t]
    [(identifier? t)
     (free-id-table-ref
      env t
      (λ () (raise-syntax-error
             'resolve-type
             (~a "No type for identifier: " (syntax-e t)) t)))]
    [else (raise-syntax-error 'resolve-type
                              "This shouldn't happen!")]))

(define (resolve-object-type t [env (current-env)])
  (let ([rt (resolve-type t env)])
    (cond [(class-ty? rt) (object-ty rt)]
          [else rt])))

(define (subtype? t1 t2 [env (current-env)])
  ;;(displayln (~a "subtype? " t1 t2))
  (let ([rty1 (resolve-type t1 env)]
        [rty2 (resolve-type t2 env)])
    (match* (rty1 rty2)
      [(_ (top-ty)) #t]
      [((base-ty k1) (base-ty k2))
       (eq? k1 k2)]
      [((class-ty _ _ _ _) (class-ty n2 _ _ _))
       (free-id-set-member? (ancestors rty1 env) n2)]
      [((object-ty cty1) (object-ty cty2))
       (subtype? cty1 cty2 env)]
      [((method-ty _n1 arg-ty1 res-ty1)
        (method-ty _n2 arg-ty2 res-ty2))
       (and (subtype? arg-ty2 arg-ty1 env)
            (subtype? res-ty1 res-ty2 env))]
      [(_ _) #f])))

;; Returns a free-id-set of all ancestors of the given class-ty
(define (ancestors cls [env (current-env)])
  (let loop ([ancestors (immutable-free-id-set)]
             [current cls])
    (match (and current (resolve-type current env))
      [(class-ty name super _ _)
       (loop (free-id-set-add ancestors name)
             super)]
      [_ ancestors])))

(define (find-field-type fld-name class-id [env (current-env)])
  (if class-id
      (match (resolve-type class-id env)
        [(class-ty cname super fld-table _)
         (let ([fld-ty (dict-ref fld-table fld-name #f)])
           (if fld-ty
               (values cname fld-ty)
               (find-field-type fld-name super env)))]
        [_ (raise-syntax-error
            'find-field-type
            (~a "not a class type :" class-id))])
      (values 'dummy #f)))

(define (find-method-type meth-name class-id [env (current-env)])
  (and class-id
       (match (resolve-type class-id env)
         [(class-ty _name super _ meth-table)
          (or (dict-ref meth-table meth-name #f)
              (find-method-type meth-name super env))]
         [_ (raise-syntax-error
             'find-method-type
             (~a "not a class type :" class-id))])))


(define-splicing-syntax-class ty-sc
  #:datum-literals (int boolean)
  (pattern int
           #:attr ty int-ty)
  (pattern boolean
           #:attr ty bool-ty)
  (pattern class-name:id
           #:attr ty #'class-name))

(define-syntax-class var-decl
  (pattern (t:ty-sc name:id)
           #:attr ty (attribute t.ty)))

(define-syntax-class var-decl-with-default
  (pattern (t:ty-sc name:id (~optional default))
           #:attr ty (attribute t.ty)))

(define-syntax-class meth-decl
  #:datum-literals (method)
  (pattern (method ret-ty:ty-sc name:id param:var-decl-with-default
                   (local:var-decl ... body ... ret))
           #:with param-name #'param.name
           #:attr param-ty (attribute param.ty)
           #:attr param-default (attribute param.default)
           #:with (local-name ...) #'(local.name ...)
           #:attr meth-ty (method-ty #'name
                                     (attribute param.ty)
                                     (attribute ret-ty.ty))
           #:attr extend-names (cons (attribute param-name)
                                     (attribute local.name))
           #:attr extend-tys (cons (attribute param-ty)
                                   (attribute local.ty))))

(define-syntax-class binop
  #:datum-literals (&& < + - * / || ==)
  (pattern ==
           #:with ret-ty #'boolean
           #:attr ty (binop-ty (top-ty) (top-ty) bool-ty))
  (pattern &&
           #:with ret-ty #'boolean
           #:attr ty (binop-ty bool-ty bool-ty bool-ty))
  (pattern ||
           #:with ret-ty #'boolean
           #:attr ty (binop-ty bool-ty bool-ty bool-ty))
  (pattern <
           #:with ret-ty #'boolean
           #:attr ty (binop-ty int-ty int-ty bool-ty))
  (pattern +
           #:with ret-ty #'int
           #:attr ty (binop-ty int-ty int-ty int-ty))
  (pattern -
           #:with ret-ty #'int
           #:attr ty (binop-ty int-ty int-ty int-ty))
  (pattern *
           #:with ret-ty #'int
           #:attr ty (binop-ty int-ty int-ty int-ty))
  (pattern /
           #:with ret-ty #'int
           #:attr ty (binop-ty int-ty int-ty int-ty)))

(define-syntax-class regular-class
  #:datum-literals (class)
  ;; TODO actually implement inheritance and super
  (pattern (class name:id (~optional (~seq #:extends parent:id)
                                     #:defaults ([parent #f]))
             (var:var-decl ...) (meth:meth-decl ...))
           #:with (field-name ...)    #'(var.name ...)
           #:with (method-name ...)   #'(meth.name ...)
           #:with (method-ret-ty ...) #'(meth.ret-ty ...)
           #:attr field-names (attribute var.name)
           #:attr field-tys (attribute var.ty)
           #:attr extends-stx (if (attribute parent)
                                  #'(#:extends parent)
                                  #'())
           #:attr cls-ty (make-class-ty #'name (attribute parent)
                                        (map field-ty
                                             (attribute field-names)
                                             (attribute field-tys))
                                        (attribute meth.meth-ty))))


(define (empty-env) (make-immutable-free-id-table))

(define (extend-env env vars tys)
  (for/fold ([env env])
      ([v  (in-list vars)]
       [ty (in-list tys)])
    (dict-set env v (resolve-object-type ty))))

(define-syntax-rule (with-extended-env vars tys . body)
  (parameterize ([current-env (extend-env (current-env) vars tys)])
    . body))

(define stop-at-error? (make-parameter #f))

(define (raise-syntax-error+ . args)
  (if (stop-at-error?)
      (apply raise-syntax-error* args)
      (displayln (format "type error: ~a" (car args)))))

;; a program is a list of class declarations
;; (which includes main class)
(define (typecheck-program stx [stop-err? #t])
  (let ([toplevel-env (build-toplevel-env (syntax->list stx))])
    #`(#,@(parameterize ([current-env toplevel-env]
                         [stop-at-error? stop-err?])
            (for/list ([cls (in-syntax stx)])
              (typecheck-class cls))))))

(define (build-toplevel-env clss)
  (for/fold ([env (make-immutable-free-id-table)])
            ([cls (in-list clss)])
    (syntax-parse/track-origin
     cls
     [c:regular-class
      (dict-set env #'c.name (attribute c.cls-ty))])))

(define (typecheck-class stx [toplevel-env (current-env)])
  (define (collect-fields cname)
    (if cname
        (match (dict-ref toplevel-env cname)
           [(class-ty _ s fs _)
            `(,@(collect-fields s)
              ,@(dict-map fs (lambda (k v) v)))])
        '()))

  (syntax-parse/track-origin
   stx
   [c:regular-class
    (quasisyntax/loc stx
      (class c.name #,@(attribute c.extends-stx)
        (field c.field-name) ...
        #,@(let* ([flds (collect-fields (attribute c.name))]
                  [fld-names (map field-ty-name flds)]
                  [fld-tys   (map field-ty-ty   flds)])
             (with-extended-env fld-names fld-tys
               (for/list ([meth (in-syntax #'(c.meth ...))])
                 (typecheck-method #'c.name meth))))))]))

(define (typecheck-method cur-cls-name method [env (current-env)])
  (syntax-parse/track-origin method
    [meth:meth-decl
     (define cur-cls-ty (resolve-type cur-cls-name))
     ;; This might need better error handling ...
     (match-define (class-ty name super fld-dict meth-dict)
       cur-cls-ty)
     (define inherited-meth-ty
       (find-method-type #'meth.name super env))
     (define meth-ty (attribute meth.meth-ty))
     (when (and inherited-meth-ty
                (not (subtype? meth-ty inherited-meth-ty env)))
       (raise-syntax-error+
        "Method type not a subtype of inherited method"
        #'meth.name #f
        "in class" (syntax-e cur-cls-name)))
     (define expected-param-ty
       (resolve-object-type (method-ty-arg-ty meth-ty)))
     (define expected-ret-ty
       (resolve-object-type (method-ty-res-ty meth-ty)))
     (define-values (pdef-ty pdef-stx)
       (if (attribute meth.param-default)
           (typecheck-expression cur-cls-name #'meth.param-default)
           (values #f #f)))
     (when (and pdef-ty
                (not (subtype? pdef-ty expected-param-ty)))
       (raise-syntax-error+
        "Method's default value not a subtype of parameter type"
        #'meth.name #f
        "in class" (syntax-e cur-cls-name)))
     (with-extended-env
       (attribute meth.extend-names)
       (attribute meth.extend-tys)
       (define-values (ret-ty ret-stx)
         (typecheck-expression cur-cls-name #'meth.ret))
       (unless (subtype? ret-ty expected-ret-ty)
         (raise-syntax-error+ "return type mismatch" method #f))
       (quasisyntax/loc method
         (method meth.name (meth.param-name
                            #,@(if pdef-stx
                                   (list pdef-stx)
                                   '()))
           (local meth.local-name) ...
           #,@(for/list ([stat (in-syntax #'(meth.body ...))])
                (typecheck-statement cur-cls-name stat))
           #,ret-stx)))]))

(define (typecheck-statement cur-cls-name stat [env (current-env)])
  (define (t-s s) (typecheck-statement  cur-cls-name s env))
  (define (t-e e) (typecheck-expression cur-cls-name e env))
  (syntax-parse/track-origin stat
    #:datum-literals (if while println = abs conc fref)
    [(lhs:id = rhs)
     (define lhs-ty (resolve-type #'lhs))
     (define-values (rhs-ty rhs-stx) (t-e #'rhs))
     (unless (subtype? rhs-ty lhs-ty)
       (raise-syntax-error+ "Type mismatch in assignment" stat #f))
     (quasisyntax/loc stat (= lhs #,rhs-stx))]
    [((fref receiver fld:id) = rhs)
     (define-values (recvr-ty recvr-stx) (t-e #'receiver))
     (match recvr-ty
       [(object-ty cls-ty)
        (define-values (cname fld-ty)
          (find-field-type #'fld cls-ty env))
        (unless fld-ty
          (raise-syntax-error+ "Field not found" #'fld #f))
        (match-define (field-ty _ lhs-ty) fld-ty)
        (define-values (rhs-ty rhs-stx) (t-e #'rhs))
        (unless (subtype? rhs-ty lhs-ty)
          (raise-syntax-error+ "Type mismatch in assignment" stat #f))
        (quasisyntax/loc stat
          (= (fref #,cname #,recvr-stx fld) #,rhs-stx))]
       [_
        (raise-syntax-error+ "Type mismatch" stat #f
                             "expected" "object type")])]

    [(if (tst) thn els)
     (define-values (tst-ty tst-stx) (t-e #'tst))
     (unless (subtype? tst-ty bool-ty)
       (raise-syntax-error+ "Type mismatch in if condition" #'if #f))
     (quasisyntax/loc stat (if #,tst-stx #,(t-s #'thn)
                               #,(t-s #'els)))]
    [(while (tst) body)
     (define-values (tst-ty tst-stx) (t-e #'tst))
     (unless (subtype? tst-ty bool-ty)
       (raise-syntax-error+
        "Type mismatch in while condition" #'while #f))
     (quasisyntax/loc stat
       (#,(syntax-property (car (syntax-e stat))
                           'original-for-check-syntax
                           #t)
        #,tst-stx #,(t-s #'body)))]
    [(println arg)
     (define-values (arg-ty arg-stx) (t-e #'arg))
     (unless (subtype? arg-ty int-ty)
       (raise-syntax-error+ "Type mismatch" #'println #f))
     (quasisyntax/loc stat (println #,arg-stx))]
    [(abs s ...)
     (quasisyntax/loc stat
       (abs #,@(map t-s (syntax->list #'(s ...)))))]
    [(conc s ...)
     (quasisyntax/loc stat
       (conc #,@(map t-s (syntax->list #'(s ...)))))]
    [(s ...)
     (quasisyntax/loc stat
       (begin #,@(map t-s (syntax->list #'(s ...)))))]))

;; TODO as with typecheck-statement, could do actual checking.
;; for now just makes type info explicit
(define (typecheck-expression cur-cls expr [env (current-env)])
  (define (t-e e) (typecheck-expression cur-cls e env))
  (syntax-parse/track-origin expr
    #:datum-literals (new int true false ! this abs conc fref)
    [(new cname:id)
     (define cls-ty (resolve-type #'cname env))
     (match cls-ty
       [(class-ty n s fs ms)
        (values (object-ty cls-ty)
                (quasisyntax/loc expr (new cname)))]
       [_ (raise-syntax-error+ "Type mismatch" #'cname #f
                               "expected" "a class name")])]
    ;; TODO add `super` (and add as literal)
    [(lhs op:binop rhs)
     (match-define (binop-ty left-ty right-ty res-ty)
       (attribute op.ty))
     (define-values (lhs-ty lhs-stx) (t-e #'lhs))
     (define-values (rhs-ty rhs-stx) (t-e #'rhs))
     (unless (and (subtype? lhs-ty left-ty env)
                  (subtype? rhs-ty right-ty env))
       (raise-syntax-error+ "Type mismatch" #'op #f))
     (values res-ty
             (quasisyntax/loc expr (op #,lhs-stx #,rhs-stx)))]
    [(fref receiver fld:id)
     (define-values (recvr-ty recvr-stx) (t-e #'receiver))
     (match recvr-ty
       [(object-ty cls-ty)
        (define-values (cname fld-ty)
          (find-field-type #'fld cls-ty env))
        (unless fld-ty
          (raise-syntax-error+ "Field not found" #'fld #f))
        (match-define (field-ty _ ty) fld-ty)
        (values (resolve-object-type ty)
                (quasisyntax/loc expr
                  (fref #,cname #,recvr-stx fld)))]
       [_
        (raise-syntax-error+ "Type mismatch" expr #f
                             "expected" "object type")])]
    [(receiver meth:id arg)
     ; needs to be after the binop and new object cases
     (define-values (recvr-ty recvr-stx) (t-e #'receiver))
     (match recvr-ty
       [(object-ty cls-ty)
        (match-define (class-ty cname _ _ _) cls-ty)
        (define meth-ty (find-method-type #'meth cls-ty env))
        (unless meth-ty
          (raise-syntax-error+ "Method not found" #'meth #f))
        (match-define (method-ty _ marg-ty mres-ty) meth-ty)
        (define-values (garg-ty garg-stx) (t-e #'arg))
        (unless (subtype? garg-ty marg-ty env)
          (raise-syntax-error+ "Type mismatch" expr #f))
        (values (resolve-object-type mres-ty)
                (quasisyntax/loc expr
                  (send #,cname #,recvr-stx meth #,garg-stx)))]
       [_
        (raise-syntax-error+ "Type mismatch" expr #f
                             "expected" "object type")])]
    [(! arg)
     (define-values (arg-ty arg-stx) (t-e #'arg))
     (unless (subtype? arg-ty bool-ty env)
       (raise-syntax-error+ "Type mismatch" #'arg #f
                            "expected" "boolean"))
     (values bool-ty
             (quasisyntax/loc expr (! #,arg-stx)))]
    [(~or true false)
     (values bool-ty expr)]
    [this
     (define cls-ty (resolve-type cur-cls env))
     (values (object-ty cls-ty) expr)]
    [var:id
     (define var-ty (resolve-type #'var env))
     (unless var-ty
       (raise-syntax-error+ "No type for identifier" #'var #f))
     (values var-ty expr)]
    [n:exact-integer
     (values int-ty expr)]
    [(abs e)
     (define-values (e-ty e-stx) (t-e #'e))
     (values e-ty (quasisyntax/loc expr (abs #,e-stx)))]
    [(conc e)
     (define-values (e-ty e-stx) (t-e #'e))
     (values e-ty (quasisyntax/loc expr (conc #,e-stx)))]))
