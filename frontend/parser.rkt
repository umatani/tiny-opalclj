#lang racket

(require parser-tools/yacc
         (only-in parser-tools/lex
                  file-path position-token-token position-line
                  position-col position-offset)
         syntax/readerr "tokens.rkt" "lexer.rkt")

(provide parse)

(define (get-all-tokens filename port)
  (port-count-lines! port)
  (file-path filename)
  (letrec ((loop
            (lambda (acc)
              (let ((cur-tok (get-token port)))
                (if (eq? 'EOF (position-token-token cur-tok))
                    (cons cur-tok acc)
                    (loop (cons cur-tok acc)))))))
    (reverse (loop '()))))

(define (parse filename port)
  (let ((tokens (get-all-tokens filename port)))
    (parse-tiny-java (lambda ()
                       (begin0 (car tokens)
                         (unless (null? (cdr tokens))
                           (set! tokens (cdr tokens))))))))

(struct srcloc (file line col pos span))
(define-syntax (build-srcloc stx)
  (syntax-case stx ()
    ((_ end)
     #'(build-srcloc 1 end))
    ((_ start end)
     (with-syntax ((start-pos (datum->syntax 
                               #'end
                               (string->symbol 
                                (format "$~a-start-pos"
                                        (syntax->datum #'start)))))
                   (end-pos   (datum->syntax 
                               #'end
                               (string->symbol 
                                (format "$~a-end-pos"
                                        (syntax->datum #'end))))))
       #'(srcloc (file-path)
                 (position-line start-pos)
                 (position-col start-pos)
                 (position-offset start-pos)
                 (- (position-offset end-pos)
                    (position-offset start-pos)))))))

(define (to-syntax s-expr srcloc)
  (define orig-prop (read-syntax 'src (open-input-bytes #"x")))
  (define (srcloc->list src)
    `(,(srcloc-file src)
      ,(srcloc-line src)
      ,(srcloc-col  src)
      ,(srcloc-pos  src)
      ,(srcloc-span src)))
  (datum->syntax #f s-expr (srcloc->list srcloc) orig-prop))

(define parse-tiny-java
  (parser
   (start CompUnit)
   (tokens  Operators Separators EmptyLiterals Keywords
            tiny-java-vals
            racket-vals)
   (error (lambda (_tok-ok? tok-name tok-val start-pos end-pos)
            (raise-read-error
             (format "Parse error near <~a:~a>" tok-name tok-val)
             (file-path)
             (position-line start-pos)
             (position-col start-pos)
             (position-offset start-pos)
             (- (position-offset end-pos) (position-offset start-pos)))))
   (end EOF)
   (src-pos)
   
   (grammar
    (CompUnit
     [(Program) `(() ,@$1)]
     [(Directives Program) `(,(reverse $1) ,@$2)])

    (Directives
     [(Directive) (list $1)]
     [(Directives Directive) (cons $2 $1)])

    (Directive
     [(HASH untyped) (cons 'untyped #t)]
     [(HASH eval RIdentifier) (cons 'eval $3)])

    (Program
     [(ClassDeclarations) (reverse $1)])
    
    (ClassDeclarations
     [(ClassDeclaration) (list $1)]
     [(ClassDeclarations ClassDeclaration) (cons $2 $1)])

    (ClassDeclaration
     [(class Identifier ClassBody)
      (to-syntax `(class ,$2 ,@$3)
                 (build-srcloc 3))]
     [(class Identifier extends Identifier ClassBody)
      (to-syntax `(class ,$2 #:extends ,$4 ,@$5)
                 (build-srcloc 5))])
    
    (ClassBody
     [(O_BRACE VariableDeclarations MethodDeclarations C_BRACE)
      (to-syntax `(,(reverse $2) ,(reverse $3))
                 (build-srcloc 4))]
     [(O_BRACE VariableDeclarations C_BRACE)
      (to-syntax `(,(reverse $2) ())
                 (build-srcloc 3))]
     [(O_BRACE MethodDeclarations C_BRACE)
      (to-syntax `(() ,(reverse $2))
                 (build-srcloc 3))]
     [(O_BRACE C_BRACE) 
      (to-syntax '(() ())
                 (build-srcloc 2))])
    
    (MethodDeclarations
     [(MethodDeclaration) (list $1)]
     [(MethodDeclarations MethodDeclaration) (cons $2 $1)])

    (MethodDeclaration
     [(MethodHeader MethodBody) (to-syntax `(,@$1 ,$2)
                                           (build-srcloc 2))])

    (MethodHeader
     [(Type Identifier O_PAREN FormalParameter C_PAREN)
      `(method ,$1 ,$2 ,$4)])

    (MethodBody
     [(O_BRACE VariableDeclarations BlockStatements
               return Expression SEMI_COLON C_BRACE)
      (to-syntax `(,@(reverse $2) ,@(reverse $3) ,$5)
                 (build-srcloc 7))]
     [(O_BRACE VariableDeclarations return Expression SEMI_COLON C_BRACE)
      (to-syntax `(,@(reverse $2) ,$4)
                 (build-srcloc 6))]
     [(O_BRACE BlockStatements return Expression SEMI_COLON C_BRACE)
      (to-syntax `(,@(reverse $2) ,$4)
                 (build-srcloc 6))]
     [(O_BRACE return Expression SEMI_COLON C_BRACE)
      (to-syntax `(,$3)
                 (build-srcloc 5))])
    
    (FormalParameter
     [(Type Identifier)
      (to-syntax `(,$1 ,$2)
                 (build-srcloc 2))]
     [(Type Identifier = Literal)
      (to-syntax `(,$1 ,$2 ,$4)
                 (build-srcloc 4))])
    
    (VariableDeclarations
     [(VariableDeclaration) (list $1)]
     [(VariableDeclarations VariableDeclaration) (cons $2 $1)])
    
    (VariableDeclaration
     [(Type Identifier SEMI_COLON)
      (to-syntax `(,$1 ,$2)
                 (build-srcloc 2))])
    
    (Type
     [(boolean) 'boolean]
     [(int) 'int]
     [(Identifier) $1])
    
    (Statement
     [(Block) $1]
     [(abs Block) (to-syntax `(abs ,@$2) (build-srcloc 2))]
     [(conc Block) (to-syntax `(conc ,@$2) (build-srcloc 2))]
     [(IfThenElseStatement) $1]
     [(WhileStatement) $1]
     [(Println) $1]
     [(Assignment) $1])
    
    (BlockStatements 
     [(Statement) (list $1)]
     [(BlockStatements Statement) (cons $2 $1)])
    
    (Block
     [(O_BRACE BlockStatements C_BRACE)
      (to-syntax `(,@(reverse $2))
                 (build-srcloc 3))]
     [(O_BRACE C_BRACE)
      (to-syntax '() (build-srcloc 2))])
    
    (IfThenElseStatement
     [(if O_PAREN Expression C_PAREN Statement else Statement)
      (to-syntax `(if (,$3) ,$5 ,$7)
                 (build-srcloc 7))])
    
    (WhileStatement
     [(while O_PAREN Expression C_PAREN Statement)
      (to-syntax `(while (,$3) ,$5)
                 (build-srcloc 5))])

    (Println
     [(System.out.println O_PAREN Expression C_PAREN SEMI_COLON)
      (to-syntax `(println ,$3)
                 (build-srcloc 5))])
    
    (Assignment
     [(Lhs = Expression SEMI_COLON)
      (to-syntax `(,$1 = ,$3)
                 (build-srcloc 4))])

    (Lhs
     [(Identifier) $1]
     [(Primary PERIOD Identifier)
      (to-syntax `(fref ,$1 ,$3)
                 (build-srcloc 3))])
    
    (Expression
     [(ConditionalOrExpression) $1])

    (ConditionalOrExpression
     [(ConditionalAndExpression) $1]
     [(ConditionalOrExpression OR ConditionalAndExpression)
      (to-syntax `(,$1 || ,$3)
                 (build-srcloc 3))])
    
    (ConditionalAndExpression
     [(EqualityExpression) $1]
     [(ConditionalAndExpression && EqualityExpression)
      (to-syntax `(,$1 && ,$3)
                 (build-srcloc 3))])

    (EqualityExpression
     [(RelationalExpression) $1]
     [(EqualityExpression == RelationalExpression)
      (to-syntax `(,$1 == ,$3)
                 (build-srcloc 3))])
    
    (RelationalExpression
     [(AdditiveExpression) $1]
     [(RelationalExpression < AdditiveExpression)
      (to-syntax `(,$1 < ,$3)
                 (build-srcloc 3))])
    
    (AdditiveExpression
     [(MultiplicativeExpression) $1]
     [(AdditiveExpression + MultiplicativeExpression)
      (to-syntax `(,$1 + ,$3)
                 (build-srcloc 3))]
     [(AdditiveExpression - MultiplicativeExpression)
      (to-syntax `(,$1 - ,$3)
                 (build-srcloc 3))])
    
    (MultiplicativeExpression
     [(UnaryExpression) $1]
     [(MultiplicativeExpression * UnaryExpression)
      (to-syntax `(,$1 * ,$3)
                  (build-srcloc 3))]
     [(MultiplicativeExpression / UnaryExpression)
      (to-syntax `(,$1 / ,$3)
                  (build-srcloc 3))])
         
    (UnaryExpression
     [(Primary) $1]
     [(! UnaryExpression) (to-syntax `(! ,$2)
                                      (build-srcloc 2))])
    
    (Primary
     [(Literal) $1]
     [(Identifier) $1]
     [(this) (to-syntax 'this
                         (build-srcloc 1))]
     [(O_PAREN Expression C_PAREN) $2]
     [(abs O_PAREN Expression C_PAREN)
      (to-syntax `(abs ,$3) (build-srcloc 4))]
     [(conc O_PAREN Expression C_PAREN)
      (to-syntax `(conc ,$3) (build-srcloc 4))]
     [(FieldAccess) $1]
     [(MethodInvocation) $1])
    
    (Literal
     [(INTEGER_LIT) (to-syntax $1
                               (build-srcloc 1))]
     [(TRUE_LIT) (to-syntax 'true
                            (build-srcloc 1))]
     [(FALSE_LIT) (to-syntax 'false
                             (build-srcloc 1))]
     [(ClassInstanceCreationExpression) $1])

    (Identifier
     [(IDENTIFIER) (to-syntax (string->symbol $1)
                              (build-srcloc 1))])
    (RIdentifier
     [(RIDENTIFIER) (to-syntax (string->symbol $1)
                               (build-srcloc 1))])
    
    (ClassInstanceCreationExpression
     [(new Identifier O_PAREN C_PAREN)
      (to-syntax `(new ,$2)
                 (build-srcloc 4))])
    
    (FieldAccess
     [(Primary PERIOD Identifier)
      (to-syntax `(fref ,$1 ,$3)
                 (build-srcloc 3))])
    (MethodInvocation
     [(Primary PERIOD Identifier O_PAREN Expression C_PAREN)
      (to-syntax `(,$1 ,$3 ,$5)
                 (build-srcloc 6))])
    )))
