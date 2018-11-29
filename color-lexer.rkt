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
   [number-literal
    (values lexeme 'constant #f
            (position-offset start-pos)
            (position-offset end-pos))]
   [any-char
    (values lexeme 'no-color #f
            (position-offset start-pos)
            (position-offset end-pos))]))

(define-lex-abbrevs
  [newline (:: (:? #\return) #\newline)]

  [whitespace (:or #\tab #\space)]
  [comment (:: #\# (:* (:~ #\newline)))]

  [string-delimiter (:or (:= 3 #\")
                         (:= 3 #\')
                         (:= 3 #\`)
                         #\" #\' #\`)]

  [letter (:or #\_ (:/ #\a #\z
                       #\A #\Z
                       #\u00A0 #\uFFFD))]
  [number (:/ #\0 #\9)]
  [word (:: letter (:* (:or letter number)))]
  [image-word (:+ (:or #\- number letter))]
  [number-literal (:: (:? #\-) (:+ number))])