#lang racket
(require syntax/strip-context
         "lexer.rkt"
         "color-lexer.rkt")

(provide (rename-out
          ;[renpy-read read]
          [renpy-read-syntax read-syntax]
          [renpy-get-info get-info]))

(define (renpy-read-syntax path in)
  (with-syntax ([ls (for/list ([tok (in-lexer (make-renpy-lexer) in)]) tok)])
    (strip-context
     #'(module test racket
         (define data 'ls)
	 (display data)))))

(define (renpy-get-info in-port this-path
                        src-line src-column pos)
  (λ (key default)
    (case key
      [(drracket:default-filters)
       '(["Ren'Py Sources" "*.rpy"])]
      [(drracket:default-extension)
       "rpy"]
      [(color-lexer)
       renpy-color-lexer]
      
      [(drracket:indentation
        drracket:keystrokes
        drracket:show-big-defs/ints-labels
        drracket:opt-out-toolbar-buttons
        drracket:opt-in-toolbar-buttons
        drracket:submit-predicate
        drracket:toolbar-buttons
        definitions-text-surrogate)
       default]
      
      [else default])))
