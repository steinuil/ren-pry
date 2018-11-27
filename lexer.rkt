#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide make-renpy-lexer in-lexer)

(module+ test
  (require rackunit)

  (define (consume-token str)
    (position-token-token ((make-renpy-lexer) (open-input-string str)))))

(define (make-renpy-lexer (indent-stack '(0)))
  (lexer-src-pos
   [(eof) (token-EOF)]
   [(:or whitespace comment)
    (return-without-pos ((make-renpy-lexer indent-stack) input-port))]

   ;; Keywords not allowed in simple expressions
   ["as" (token-AS)]
   ["at" (token-AT)]
   ["behind" (token-BEHIND)]
   ["call" (token-CALL)]
   ["expression" (token-EXPRESSION)]
   ["hide" (token-HIDE)]
   ["if" (token-IF)]
   ["in" (token-IN)]
   ["image" (token-IMAGE)]
   ["init" (token-INIT)]
   ["jump" (token-JUMP)]
   ["menu" (token-MENU)]
   ["onlayer" (token-ONLAYER)]
   ["python" (token-PYTHON)]
   ["return" (token-RETURN)]
   ["scene" (token-SCENE)]
   ["set" (token-SET)]
   ["show" (token-SHOW)]
   ["with" (token-WITH)]
   ["while" (token-WHILE)]
   ["zorder" (token-ZORDER)]
   ["transform" (token-TRANSFORM)]

   [#\$ (token-SYM-DOLLAR)]
   [#\: (token-SYM-COLON)]
   [#\= (token-SYM-EQUALS)]

   [(:: #\\ #\newline) (token-BACKSLASH)]

   [#\( (token-L-PAREN)]
   [#\) (token-R-PAREN)]
   [#\[ (token-L-BRACKET)]
   [#\] (token-R-BRACKET)]
   [#\{ (token-L-BRACE)]
   [#\} (token-R-BRACE)]

   [(:: (:? #\return) #\newline (:or (:* #\space) (:* #\tab)))
    (begin
      (set! indent-stack (cons 1 indent-stack))
      (token-INDENT indent-stack))]

   [string (token-STRING lexeme)]
   [(:: #\r string) (token-RAW-STRING (substring lexeme 1))]
   [word (token-WORD lexeme)]
   [number-literal (token-NUMBER lexeme)]
   ))

(define-tokens tokens
  (STRING
   RAW-STRING
   WORD
   NUMBER
   INDENT))

(define-empty-tokens keywords
  (SYM-DOLLAR
   AS
   AT
   BEHIND
   CALL
   EXPRESSION
   HIDE
   IF
   IN
   IMAGE
   INIT
   JUMP
   MENU
   ONLAYER
   PYTHON
   RETURN
   SCENE
   SET
   SHOW
   WITH
   WHILE
   ZORDER
   TRANSFORM))

(define-empty-tokens other-keywords
  (EOF
   SYM-COLON
   SYM-EQUALS
   BACKSLASH
   L-BRACKET R-BRACKET
   L-PAREN R-PAREN
   L-BRACE R-BRACE))

(define-lex-trans rpy-string
  (syntax-rules ()
    [(_ n q)
     (:: (:= n q)
         (complement (:: (:* (:~ #\\ q)) (:= n q) any-string))
         (:= n q))]))

(define-lex-abbrevs
  [string (:or (rpy-string 3 #\')
               (rpy-string 3 #\")
               (rpy-string 3 #\`)
               (rpy-string 1 #\')
               (rpy-string 1 #\")
               (rpy-string 1 #\`))]

  [whitespace (:or #\newline #\return #\tab #\space)]
  [comment (:: #\# (:* (:~ #\newline)) #\newline)]

  [letter (:or #\_ (:/ #\a #\z
                       #\A #\Z
                       #\u00A0 #\uFFFD))]
  [number (:/ #\0 #\9)]
  [word (:: letter (:* (:or letter number)))]
  [image-word (:+ (:or #\- number letter))]
  [number-literal (:: (:? #\-) (:+ number))])

;; Sequence generator for lexers
(define (in-lexer lexer port)
  (define (pred tok)
    (eq? (token-name (position-token-token tok))
         'EOF))
  (define (producer)
    (lexer port))
  (stop-after (in-producer producer) pred))

(module+ test
  (check-equal? (token-name (consume-token "as")) 'AS)

  (let ([token (consume-token "r\"a\"")])
    (check-equal? (token-name token) 'RAW-STRING)
    (check-equal? (token-value token) "\"a\""))

  (let ([token (consume-token "\"\"\"a\"\"\"")])
    (check-equal? (token-name token) 'STRING)
    (check-equal? (token-value token) "\"\"\"a\"\"\""))

  (check-equal? (token-value (consume-token "\"\"\"a\"\"")) "\"\"")
  (check-equal? (token-value (consume-token "\"\"\"a\"")) "\"\"")
  (check-equal? (token-value (consume-token "\"\"\"a")) "\"\"")

  (let ([tokens (for/list ([token (in-lexer (make-renpy-lexer)
                                            (open-input-string "as if"))])
                  (token-name (position-token-token token)))])
    (check-equal? tokens '(AS IF EOF))))
