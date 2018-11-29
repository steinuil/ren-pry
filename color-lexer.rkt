#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         (only-in "lexer.rkt"
                  renpy-string-lexer))

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
   [string-delimiter
    (let-values ([(str end-line end-col end-offset)
                  (renpy-string-lexer input-port (string-ref lexeme 0)
                                      (string-length lexeme))])
      (values str 'string #f
              (position-offset start-pos)
              (position-offset
               (position end-offset end-line end-col))))]
   [(:+ whitespace)
    (values lexeme 'whitespace #f
            (position-offset start-pos)
            (position-offset end-pos))]
   [#\(
    (values lexeme 'parenthesis #\(
            (position-offset start-pos)
            (position-offset end-pos))]
   [#\)
    (values lexeme 'parenthesis #\)
            (position-offset start-pos)
            (position-offset end-pos))]
   [#\[
    (values lexeme 'parenthesis #\[
            (position-offset start-pos)
            (position-offset end-pos))]
   [#\]
    (values lexeme 'parenthesis #\]
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
  [number-literal (:: (:? #\-)
                      (:or (:: (:+ number)
                               (:? (:: #\. (:* number))))
                           (:: #\. (:+ number))))])