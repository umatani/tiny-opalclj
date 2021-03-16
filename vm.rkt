#lang racket/base
(require (for-syntax racket/base)
         syntax/parse/define
         racket/stxparam
         (prefix-in conc$ "domain/concrete.rkt"))

(provide (rename-out [vm-module-begin #%module-begin])
         ;; from racket/base
         #%app #%datum #%top begin let if
         ;; from racket for debug
         displayln quote
         ;; from this module
         operand-stack make-stack dump-stack pop-stack
         (for-syntax sinit-id) sinit

         def-class self def-method
         store load while print-top dup
         iconst iadd isub imul idiv lt eq
         new invokevirtual getfield putfield

         ;; hooks
         load-impl store-impl delta-impl
         invokevirtual-impl getfield-impl putfield-impl
         )

;;;;;;;;;;;;;;;;;;;;;;;;;
;; bytecode execution
;;;;;;;;;;;;;;;;;;;;;;;;;

;; operand stack
(define operand-stack (make-parameter #f))
(define (make-stack l) (box l))

(define (dump-stack) (unbox (operand-stack)))
(define (push-stack v)
  (set-box! (operand-stack) (cons v (unbox (operand-stack)))))
(define (pop-stack)
  (let ([s (unbox (operand-stack))])
    (begin0 (car s)
      (set-box! (operand-stack) (cdr s)))))
(define (peek-stack) (car (unbox (operand-stack))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (vm-module-begin stx)
  (syntax-parse stx
    #:literals [vm-module-begin]
    [(vm-module-begin b ...)
     #'(#%module-begin b ...)]))

(define-simple-macro (def-class cname:id sname (v:id ...) m ...)
  (conc$def-class cname sname (v ...) m ...))

(define-syntax-parameter self (syntax-rules ()))

(define-for-syntax sinit-id (car (generate-temporaries '(sinit))))
(define sinit (box #f))

(define-syntax (def-method stx)
  (syntax-parse stx
    #:literals [def-method]
    [(def-method mname:id (param:id) (var:id ...) body:expr)
     #`(let ([f (λ (obj param [var #f] ...)
                  (parameterize ([operand-stack (make-stack '())])
                    (syntax-parameterize
                        ([self (make-rename-transformer #'obj)])
                      body)
                    (pop-stack)))])
         #,(if (free-identifier=? #'mname sinit-id)
               #'(set-box! sinit f)
               #'(void))
         f)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-simple-macro (iconst i:integer)
  (push-stack (conc$const i)))

(define-syntax-parameter load-impl
  (make-rename-transformer #'conc$load))
(define-simple-macro (load v:id)
  (push-stack (load-impl v)))

(define-syntax-parameter store-impl
  (make-rename-transformer #'conc$store))
(define-simple-macro (store v:id)
  (store-impl v (pop-stack)))

(define-syntax-parameter delta-impl
  (make-rename-transformer #'conc$delta))

(define-simple-macro (iadd)
  (let ([v2 (pop-stack)]
        [v1 (pop-stack)])
    (push-stack (delta-impl 'iadd v1 v2))))

(define-simple-macro (isub)
  (let ([v2 (pop-stack)]
        [v1 (pop-stack)])
    (push-stack (delta-impl 'isub v1 v2))))

(define-simple-macro (imul)
  (let ([v2 (pop-stack)]
        [v1 (pop-stack)])
    (push-stack (delta-impl 'imul v1 v2))))

(define-simple-macro (idiv)
  (let ([v2 (pop-stack)]
        [v1 (pop-stack)])
    (push-stack (delta-impl 'idiv v1 v2))))

(define-simple-macro (lt)
  (let ([v2 (pop-stack)]
        [v1 (pop-stack)])
    (delta-impl 'lt v1 v2)))

(define-simple-macro (eq)
  (let ([v2 (pop-stack)]
        [v1 (pop-stack)])
    (delta-impl 'eq v1 v2)))

(define-simple-macro (while cond:expr body:expr)
  (letrec ([loop (λ ()
                   (if cond
                       (begin body (loop))
                       (void)))])
    (loop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-simple-macro (new lbl:id cname:id)
  (push-stack (conc$new cname)))

(define-syntax-parameter invokevirtual-impl
  (make-rename-transformer #'conc$invokevirtual))
(define-simple-macro (invokevirtual cname:id mname:id)
  (let ([arg (pop-stack)]
        [tgt (pop-stack)])
    (push-stack (invokevirtual-impl cname tgt mname arg))))

(define-syntax-parameter getfield-impl
  (make-rename-transformer #'conc$getfield))
(define-simple-macro (getfield cname:id fname:id)
  (let ([tgt (pop-stack)])
    (push-stack (getfield-impl cname tgt fname))))

(define-syntax-parameter putfield-impl
  (make-rename-transformer #'conc$putfield))
(define-simple-macro (putfield cname:id fname:id)
  (let ([v (pop-stack)]
        [tgt (pop-stack)])
    (putfield-impl cname tgt fname v)))

(define-simple-macro (print-top)
  (conc$displayln (pop-stack)))

;; for debug (not used in compiled code)
(define-simple-macro (dup)
  (push-stack (peek-stack)))
