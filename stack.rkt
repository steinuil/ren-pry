#lang racket/base

(provide make-stack
         stack-push!
         stack-pop!
         stack-peek)

(define (stack-push! stack item)
  (set-box! stack (cons item (unbox stack))))

(define (stack-pop! stack)
  (set-box! stack (cdr (unbox stack))))

(define (stack-peek stack)
  (cdr (unbox stack)))

(define (make-stack . items)
  (box (reverse items)))