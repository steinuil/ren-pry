#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide (except-out (all-defined-out)
                     string-literal)
         (contract-out [string-literal
                        (-> (and/c input-port? port-counts-lines?)
                            char?
                            exact-positive-integer?
                            (values (or/c string? #f)
                                    exact-positive-integer?
                                    exact-nonnegative-integer?
                                    exact-positive-integer?))]))


(define-lex-abbrevs
  [white-space (:or #\space #\tab)]
  [linebreak (:: (:? #\return) #\newline)]

  [comment (:: #\# (:* (:~ #\newline)))]

  [number (:/ #\0 #\9)]
  [letter (:or #\_ (:/ #\a #\z #\A #\Z
                       #\u00A0 #\uFFFD))]

  [word (:: letter (:* (:or letter number)))]
  [image-word (:+ (:or #\- number letter))]

  [int-literal (:: (:? #\-) (:+ number))]
  [float-literal (:: (:? #\-) (:or (:: (:+ number) #\. (:* number))
                                   (:: #\. (:+ number))))]
  [number-literal (:or int-literal float-literal)]

  [string-delim (:or (:= 3 #\") (:= 3 #\') (:= 3 #\`)
                     #\" #\' #\`)]
  [raw-string-delim (:: #\r string-delim)])


(define (string-literal port delim n)
  (define out "")
  (define end-delim (make-string (- n 1) delim))
  (define (append-chars! . chrs)
    (set! out (string-append out (apply string chrs))))

  (let keep-lexing ()
    (define-values (line col offset) (port-next-location port))
    (match (read-char port)
      [(? eof-object?)
       (values #f line col offset)]
      [(== delim)
       #:when (= n 1)
       (values out line (+ col 1) (+ offset 1))]
      [(== delim)
       (define maybe-end (peek-string (- n 1) 0 port))
       (cond [(eof-object? maybe-end)
              (values #f line col offset)]
             [(string=? maybe-end end-delim)
              (read-string (- n 1) port)
              (values out line (+ col n) (+ offset n))]
             [else
              (append-chars! delim (read-char port))
              (keep-lexing)])]
      [#\\
       (define escaped (read-char port))
       (cond [(eof-object? escaped)
              (values #f line col offset)]
             [(or (eq? escaped delim)
                  (eq? escaped #\\))
              (append-chars! escaped)
              (keep-lexing)]
             [else
              (append-chars! #\\ escaped)
              (keep-lexing)])]
      [other-char
       (append-chars! other-char)
       (keep-lexing)])))


(module+ test
  (require rackunit)

  (define-syntax-rule (check-lexes? sub-lexer str)
    (let ([lex (lexer [sub-lexer lexeme]
                      [any-char #f])])
      (check-equal? (lex (open-input-string str)) str)))

  (define (lex-string str (delim #\`) (n 1))
    (define port (open-input-string str))
    (port-count-lines! port)
    (define-values (lexed line col offset)
      (string-literal port delim n))
    lexed)

  (check-equal? (lex-string "a`") "a")
  (check-equal? (lex-string "a") #f)
  (check-equal? (lex-string "abcde`") "abcde")
  (check-equal? (lex-string "abc\\``") "abc`")
  (check-equal? (lex-string "abc\\`\\``") "abc``")
  (check-equal? (lex-string "abc\\`abc`") "abc`abc")
  (check-equal? (lex-string "abc\\abc`") "abc\\abc")
  (check-equal? (lex-string "a\"" #\") "a")
  (check-equal? (lex-string "abc\\\\`abc") "abc\\")

  (check-lexes? float-literal "-.1")
  (check-lexes? float-literal "1."))
