#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-lex-abbrevs
  [whitespace (:or #\tab #\space)]
  [newline (:: (:? #\return) #\newline)]

  [comment (:: #\# (:* (:~ #\newline)) newline)]

  [number (:/ #\0 #\9)]
  [letter (:or #\_ (:/ #\a #\z #\A #\Z
                       #\u00A0 #\uFFFD))]

  [word (:: letter (:* (:or letter number)))]
  [image-word (:+ (:or #\- number letter))]

  [int-literal (:: (:? #\-) (:+ number))]
  [float-literal (:: (:? #\-) (:or (:: (:+ number) #\. (:* number))
                                   (:: #\. (:+ number))))]

  [string-delimiter (:or (:= 3 #\") (:= 3 #\') (:= 3 #\`)
                         #\" #\' #\`)]
  [empty-line (:: newline (:* whitespace) (:? comment) newline)]
  [indentation (:: newline (:or (:* #\space) (:* #\tab)))])


;; TODO check if the ren'py parser allows an empty single-quote
;; string and then another string right after (I'm guessing not).
;; TODO check if the port has location enabled
(define (string-literal port delim n)
  (define out "")
  (define end-delim (make-string (- n 1) delim))
  (define (append-chars! . chrs)
    (set! out (string-append out (apply string chrs))))

  (let keep-lexing ()
    (define-values (line col offset) (port-next-location port))
    (match (read-char port)
      [eof
       (values #f line col offset)]
      [delim #:when (= n 1)
       (values out line (+ col 1) (+ offset 1))]
      [delim
       (define maybe-end (read-string (- n 1) port))
       (cond [(eof-object? maybe-end)
              (values #f line col offset)]
             [(string=? maybe-end end-delim)
              (values out line (+ col n) (+ offset n))]
             [else
              (unget port (- n 2))
              (append-chars! chr (string-ref maybe-end 0))
              (keep-lexing)])]
      [#\\
       (define escaped (read-char port))
       (unless (eq? escaped delim)
         (append-chars! #\\))
       (append-chars! escaped)
       (keep-lexing)]
      [other-char
       (append-chars! other-char)
       (keep-lexing)])))





       (if (string=? (read-string (- n 1) port)
                     (make-string (- n 1) delim))
         (values out 
