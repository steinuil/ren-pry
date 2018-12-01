#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         syntax-color/lexer-contract
         "sub-lexers.rkt")

(provide (contract-out [renpy-color-lexer lexer/c]))

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
   [string-delim
    (let-values
        ([(str end-line end-col end-offset)
          (string-literal input-port (string-ref lexeme 0)
                          (string-length lexeme))])
      (values str (if str 'string 'error) #f
              (position-offset start-pos)
              (position-offset (position end-offset end-line end-col))))]
   [raw-string-delim
    (let-values
        ([(str end-line end-col end-offset)
          (string-literal input-port (string-ref lexeme 1)
                          (- (string-length lexeme) 1))])
      (values str (if str 'string 'error) #f
              (position-offset start-pos)
              (position-offset (position end-offset end-line end-col))))]
   [(:or #\( #\)
         #\[ #\]
         #\{ #\})
    (values lexeme 'parenthesis
            (string->symbol (substring lexeme 0 1))
            (position-offset start-pos)
            (position-offset end-pos))]
   [(:+ white-space)
    (values lexeme 'whitespace #f
            (position-offset start-pos)
            (position-offset end-pos))]
   [any-char
    (values lexeme 'no-color #f
            (position-offset start-pos)
            (position-offset end-pos))]))
