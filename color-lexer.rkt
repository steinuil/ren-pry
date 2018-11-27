#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide renpy-color-lexer)

(define renpy-color-lexer
  (lexer
   [(eof)
    (values lexeme 'eof #f #f #f)]
   [comment
    (values lexeme 'comment #f
            (position-offset start-pos)
            (position-offset end-pos))]
   [any-char
    (values lexeme 'no-color #f
            (position-offset start-pos)
            (position-offset end-pos))]))

(define-lex-abbrevs
  [newline (:: (:? #\return) #\newline)]
  [comment (:: #\# (:* (:~ #\newline)) newline)])