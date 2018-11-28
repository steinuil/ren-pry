#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "stack.rkt")

(provide make-renpy-lexer in-lexer)

(define (unget! port (n 1))
  (file-position port (- (file-position port) n)))

(define (calculate-indent stack indent)
  (define level (string-length indent))
  (define (pop-until-eq n)
    (define last (stack-pop! stack))
    (define top (stack-peek stack))
    (cond [(eq? top level) (token-DEDENT n)]
          [(> top level) (pop-until-eq (+ n 1))]
          [else
           (error 'calculate-indent
                  "invalid indentation: expected ~a or ~a, got ~a"
                  top last level)]))
  (define top (stack-peek stack))
  (cond [(eq? top level) (token-NODENT)]
        [(< top level)
         (stack-push! stack level)
         (token-INDENT)]
        [else (pop-until-eq 1)]))

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
   [#\, (token-SYM-COMMA)]

   [(:: #\\ newline) (token-BACKSLASH)]

   [#\( (token-L-PAREN)]
   [#\) (token-R-PAREN)]
   [#\[ (token-L-BRACKET)]
   [#\] (token-R-BRACKET)]
   [#\{ (token-L-BRACE)]
   [#\} (token-R-BRACE)]

   [(:: newline (:* whitespace) (:? comment) newline)
    (begin
      (unget! input-port)
      (return-without-pos ((make-renpy-lexer indent-stack) input-port)))]

   [(:: newline (:or (:* #\space) (:* #\tab)))
    (calculate-indent indent-stack (substring lexeme 1))]

   [(:: comment newline)
    (begin
      (unget! input-port)
      (return-without-pos ((make-renpy-lexer indent-stack) input-port)))]

   [(:or #\" #\' #\`)
    (token-STRING (string-lexer input-port (string-ref lexeme 0) 1))]

   [(:: #\r (:or #\" #\' #\`))
    (token-RAW-STRING (string-lexer input-port (string-ref lexeme 1) 1))]

   [(:or (:= 3 #\")
         (:= 3 #\')
         (:= 3 #\`))
    (token-STRING (string-lexer input-port (string-ref lexeme 0) 3))]

   [(:: #\r
        (:or (:= 3 #\")
             (:= 3 #\')
             (:= 3 #\`)))
    (token-RAW-STRING (string-lexer input-port (string-ref lexeme 1) 3))]

   ;[string-literal (token-STRING lexeme)]
   ;[(:: #\r string-literal) (token-RAW-STRING (substring lexeme 1))]
   [word (token-WORD lexeme)]
   [number-literal (token-NUMBER lexeme)]
   ))

;; FIXME return #f and backtrack when the number of quotes is > than 1 and
;; an EOF object is returned
;; Keep track of the number of chars for the position-token
(define (string-lexer port quote n)
  (define out "")
  (let loop ()
    (define chr (read-char port))
    (cond [(eof-object? chr)
           (error "unterminated string")]
          [(and (eq? chr quote)
                (= n 1))
           out]
          [(eq? chr #\\)
           (define escaped (read-char port))
           (unless (eq? escaped quote)
             (set! out (string-append out (string chr))))
           (set! out (string-append out (string escaped)))
           (loop)]
          [(eq? chr quote)
           (if (string=? (read-string n port) (make-string (- n 1) quote))
               out
               (begin
                 (unget! port (- n 2))
                 (loop)))]
          [else
           (set! out (string-append out (string chr)))
           (loop)])))

(define-tokens tokens
  (STRING
   RAW-STRING
   WORD
   NUMBER
   DEDENT))

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
   SYM-COMMA
   BACKSLASH
   L-BRACKET R-BRACKET
   L-PAREN R-PAREN
   L-BRACE R-BRACE
   INDENT NODENT))

(define-lex-abbrevs
  [newline (:: (:? #\return) #\newline)]

  [whitespace (:or #\tab #\space)]
  [comment (:: #\# (:* (:~ #\newline)))]

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
    (check-equal? (token-value token) "a"))

  (let ([token (consume-token "\"\"\"a\"\"\"")])
    (check-equal? (token-name token) 'STRING)
    (check-equal? (token-value token) "a"))

  ;(check-equal? (token-value (consume-token "\"\"\"a\"\"")) "\"\"")
  ;(check-equal? (token-value (consume-token "\"\"\"a\"")) "\"\"")
  ;(check-equal? (token-value (consume-token "\"\"\"a")) "\"\"")

  (check-equal? (token-value (consume-token "\"\\\"\"")) "\"")

  (check-equal? (token-name-list "# tfw no gf\n") '(NODENT EOF))

  (check-equal? (token-name-list "as if") '(AS IF EOF)))
