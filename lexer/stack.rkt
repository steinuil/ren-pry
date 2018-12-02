#lang racket/base
(require racket/contract/base)

(provide make-stack stack-push!
         stack-pop! stack-peek
         stack/c)

(define stack/c
  (box/c (listof any/c)))

(module+ test
  (require rackunit))

(define (stack-push! stack item)
  (set-box! stack (cons item (unbox stack))))

(define (stack-pop! stack)
  (let ([unboxed-stack (unbox stack)])
    (set-box! stack (cdr unboxed-stack))
    (car unboxed-stack)))

(module+ test
  (check-equal? (stack-pop! (make-stack 0)) 0)
  (let ([stack (make-stack 0 1)])
    (check-equal? (stack-pop! stack) 1)
    (check-equal? (stack-peek stack) 0)))

(define (stack-peek stack (n 0))
  (list-ref (unbox stack) n))

(define (make-stack . items)
  (box (reverse items)))
