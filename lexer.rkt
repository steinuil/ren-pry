#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "stack.rkt"
         "sub-lexers.rkt")

(provide make-renpy-lexer in-lexer)

(define (make-renpy-lexer (indent-stack (make-stack 0)))
  (lexer-src-pos
   [(eof) (token-EOF)]

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
   [#\. (token-SYM-PERIOD)]
   [#\* (token-SYM-ASTERISK)]
   [#\+ (token-SYM-PLUS)]
   [#\- (token-SYM-MINUS)]

   [#\( (token-L-PAREN)]
   [#\) (token-R-PAREN)]
   [#\[ (token-L-BRACKET)]
   [#\] (token-R-BRACKET)]
   [#\{ (token-L-BRACE)]
   [#\} (token-R-BRACE)]

   [(:: #\\ linebreak) (token-BACKSLASH)]


   [white-space
    (return-without-pos ((make-renpy-lexer indent-stack) input-port))]

   ;; Empty line
   #;[(:: newline (complement newline) (:? comment))
    (begin
      (return-without-pos ((make-renpy-lexer indent-stack) input-port)))]

   [(:: linebreak (:or (:* #\space) (:* #\tab)))
    (calculate-indent indent-stack (substring lexeme 1))]

   [comment
    (begin
      (return-without-pos ((make-renpy-lexer indent-stack) input-port)))]

   [string-delim
    (let-values ([(str end-line end-col end-offset)
                  (string-literal input-port (string-ref lexeme 0)
                                  (string-length lexeme))])
      (return-without-pos
       (position-token (token-STRING str)
                       (position-offset start-pos)
                       (position-offset (position end-offset end-line end-col)))))]

   [raw-string-delim
    (let-values ([(str end-line end-col end-offset)
                  (string-literal input-port (string-ref lexeme 1)
                                  (- (string-length lexeme) 1))])
      (return-without-pos
       (position-token (token-RAW-STRING str)
                       (position-offset start-pos)
                       (position-offset (position end-offset end-line end-col)))))]

   [word (token-WORD lexeme)]
   [number-literal (token-NUMBER lexeme)]
   ))


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


;; Sequence generator for lexers
(define (in-lexer lexer port)
  (port-count-lines! port)
  (define (eof? tok)
    (eq? (token-name (position-token-token tok))
         'EOF))
  (define (producer)
    (lexer port))
  (stop-after (in-producer producer) eof?))



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
   SYM-PERIOD
   SYM-ASTERISK
   SYM-PLUS
   SYM-MINUS
   BACKSLASH
   L-BRACKET R-BRACKET
   L-PAREN R-PAREN
   L-BRACE R-BRACE
   INDENT NODENT))



;;; Test
(module+ test
  (require rackunit)

  (define (consume-token str)
    (define input (open-input-string str))
    (port-count-lines! input)
    (position-token-token ((make-renpy-lexer) input)))

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

  (check-equal? (token-value (consume-token "r`abcd`")) "abcd")

  (check-equal? (token-value (consume-token "```1`2``3`3```"))
                "1`2``3`3")

  (check-equal? (token-value (consume-token "```1```123"))
                "1")

  (check-equal? (consume-token "1") (token-NUMBER "1"))
  (check-equal? (consume-token "12") (token-NUMBER "12"))
  (check-equal? (consume-token "-123") (token-NUMBER "-123"))
  (check-equal? (consume-token "1.0") (token-NUMBER "1.0"))
  (check-equal? (consume-token "1.") (token-NUMBER "1."))
  (check-equal? (consume-token "1.23") (token-NUMBER "1.23"))
  (check-equal? (consume-token ".23") (token-NUMBER ".23"))
  (check-equal? (consume-token "-.12") (token-NUMBER "-.12"))

  ;(check-equal? (token-value (consume-token "\"\"\"a\"\"")) "\"\"")
  ;(check-equal? (token-value (consume-token "\"\"\"a\"")) "\"\"")
  ;(check-equal? (token-value (consume-token "\"\"\"a")) "\"\"")

  (check-equal? (token-value (consume-token "\"\\\"\"")) "\"")

  (check-equal? (token-name-list "# tfw no gf\n") '(EOF))

  (check-equal? (token-name-list "as if") '(AS IF EOF)))
