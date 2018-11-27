#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "stack.rkt")

(provide make-renpy-lexer in-lexer)

(define (unget! port)
  (file-position port (- (file-position port) 1)))

(define (calculate-indent stack indent)
  (define level (string-length indent))
  (define (pop-until-eq)
    (define last (stack-pop! stack))
    (define top (stack-peek stack))
    (cond [(eq? top level) 'DEDENT]
          [(> top level) (pop-until-eq)]
          [else
           (error 'calculate-indent
                  "invalid indentation: expected ~a or ~a, got ~a"
                  top last level)]))
  (define top (stack-peek stack))
  (cond [(eq? top level) 'NODENT]
        [(< top level)
         (stack-push! stack level)
         'INDENT]
        [else (pop-until-eq)]))

(define (make-renpy-lexer (indent-stack (make-stack 0)))
  (lexer-src-pos
   [(eof) (token-EOF)]
   [whitespace
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

   [(:: newline (:or (:* #\space) (:* #\tab)))
    (calculate-indent indent-stack (substring lexeme 1))]

   [comment
    (begin
      (unget! input-port)
      (return-without-pos ((make-renpy-lexer indent-stack) input-port)))]

   [string (token-STRING lexeme)]
   [(:: #\r string) (token-RAW-STRING (substring lexeme 1))]
   [word (token-WORD lexeme)]
   [number-literal (token-NUMBER lexeme)]
   ))

(define-tokens tokens
  (STRING
   RAW-STRING
   WORD
   NUMBER))

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
   L-BRACE R-BRACE
   INDENT DEDENT NODENT))

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

  [newline (:: (:? #\return) #\newline)]

  [whitespace (:or #\tab #\space)]
  [comment (:: #\# (:* (:~ #\newline)) newline)]

  [letter (:or #\_ (:/ #\a #\z
                       #\A #\Z
                       #\u00A0 #\uFFFD))]
  [number (:/ #\0 #\9)]
  [word (:: letter (:* (:or letter number)))]
  [image-word (:+ (:or #\- number letter))]
  [number-literal (:: (:? #\-) (:+ number))])

;; Sequence generator for lexers
(define (in-lexer lexer port)
  (define (eof? tok)
    (eq? (token-name (position-token-token tok))
         'EOF))
  (define (producer)
    (lexer port))
  (stop-after (in-producer producer) eof?))

;;; Test
(module+ test
  (require rackunit)

  (define (consume-token str)
    (position-token-token ((make-renpy-lexer) (open-input-string str))))

  (define (token-list str)
    (for/list ([token (in-lexer (make-renpy-lexer)
                                (open-input-string str))])
      (position-token-token token)))

  (define (token-name-list str)
    (map token-name (token-list str)))

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

  (check-equal? (token-name-list "# tfw no gf\n") '(NODENT EOF))

  (check-equal? (token-name-list "as if") '(AS IF EOF)))
