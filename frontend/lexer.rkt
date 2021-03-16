#lang racket

(require parser-tools/lex
         syntax/readerr
         (prefix-in re: parser-tools/lex-sre)
         "tokens.rkt")
(provide get-token)


(define-lex-abbrevs
  (CR #\015)
  (LF #\012)
  (LineTerminator (re:or CR 
                         LF 
                         (re:: CR LF)))
  (InputCharacter (re:~ CR LF))
  
  (FF #\014)
  (TAB #\011)
  (NBSP #\uA0)
  (WhiteSpace (re:or #\space 
                     TAB
                     FF
                     LineTerminator
                     NBSP))

  (Comment (re:or TraditionalComment 
                  EndOfLineComment
                  DocumentationComment))
  (TraditionalComment (re:: "/*" NotStar CommentTail))
  (EndOfLineComment (re:: "//" (re:* InputCharacter)))
  (DocumentationComment (re:: "/**" CommentTailStar))
  (CommentTail (re:: (re:* (re:: (re:* NotStar) (re:+ "*")
                                 NotStarNotSlash))
                     (re:* NotStar)
                     (re:+ "*")
                     "/"))
  (CommentTailStar (re:: (re:* (re:: (re:* "*")
                                     NotStarNotSlash
                                     (re:* NotStar) "*"))
                         (re:* "*")
                         "/"))
  (NotStar (re:~ "*"))
  (NotStarNotSlash (re:~ "*" "/"))
  
  (SyntaxComment (re:or TraditionalCommentEOF
                        EndOfLineComment))
  (TraditionalCommentEOF (re:: "/*" CommentTailEOF))
  (CommentTailEOF (re:or (re:: (re:* (re:: (re:* NotStar)
                                           (re:+ "*")
                                           NotStarNotSlash))
                               (re:* NotStar)
                               (re:+ "*")
                               "/")
                         (re:: (re:* (re:: (re:* NotStar)
                                           (re:+ "*")
                                           NotStarNotSlash))
                               (re:* NotStar)
                               (re:* "*"))))
  
  (Identifier (re:: JavaLetter (re:* JavaLetterOrDigit)))
  (JavaLetter (re:or (re:/ "AZ" "az") "_" "$"))
  (JavaLetterOrDigit (re:or JavaLetter (re:/ "09")))
  
  (RIdentifier (re:: RacketLetter (re:* RacketLetterOrDigit)))
  (RacketLetter (re:or JavaLetter "-"))
  (RacketLetterOrDigit (re:or RacketLetter (re:/ "09")))

  (Keyword (re:or "if"          "this"         "boolean" 
                  "else"        "return"       "extends"
                  "static"      "class"        "int"
                  "while"       "new"          "System.out.println"
                  "abs"         "conc"
                  "untyped"     "eval" ;; for # directive
                  ))
  
  (DecimalNumeral (re:or #\0
                         (re:: (re:/ "19") (re:* (re:/ "09")))))
  
  (Operator (re:or "&&" "==" "<" "+" "-" "*" "/" "!" "=" "||")))

;; Handle Comments
(define read-line-comment
  (lexer
   [(re:~ #\newline) (read-line-comment input-port)]
   [#\newline end-pos]
   [(eof) end-pos]
   [(special) (read-line-comment input-port)]
   [(special-comment) (read-line-comment input-port)]
   ))

(define read-block-comment
  (lexer
   ["*/" end-pos]
   [(eof) end-pos]
   [(re:or "*" "/" (complement (re:: any-string (re:or "*" "/") any-string)))
    (read-block-comment input-port)]
   [(special) (read-block-comment input-port)]
   [(special-comment) (read-block-comment input-port)]))

(define get-token
  (lexer-src-pos
   (Operator (let ([l lexeme])
               (cond
                 [(string=? l "||") (token-OR)]
                 [else (string->symbol l)])))
   
   ("(" (token-O_PAREN))
   (")" (token-C_PAREN))
   ("{" (token-O_BRACE))
   ("}" (token-C_BRACE))
   (";" (token-SEMI_COLON))
   ("," (token-COMMA))
   ("." (token-PERIOD))
   ("#" (token-HASH))
   
   ("true" (token-TRUE_LIT))
   ("false" (token-FALSE_LIT))
   
   (DecimalNumeral
    (token-INTEGER_LIT (string->number lexeme 10)))
   
   (Keyword (string->symbol lexeme))
   
   (Identifier (token-IDENTIFIER lexeme))
   (RIdentifier (token-RIDENTIFIER lexeme))
   
   ("//" (begin (read-line-comment input-port)
                (return-without-pos (get-token input-port))))
   ("/*" (begin (read-block-comment input-port)
                (return-without-pos (get-token input-port))))

   ((re:+ WhiteSpace) (return-without-pos (get-token input-port)))
   
   (#\032 'EOF)
   ((eof) 'EOF)
   [any-char
    (raise-read-error
        (format "lexer: No match found in input starting with: ~a"
                (string-ref lexeme 0))
        (file-path)
        (position-line start-pos)
        (position-col start-pos)
        (position-offset start-pos)
        (- (position-offset end-pos) (position-offset start-pos)))]))
