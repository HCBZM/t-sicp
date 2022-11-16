#lang sicp
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))
(define (square x) (* x x))
(define (square-list1 items)
  (map square items))

(define (square-list2 items)
  (define (iter res things)
    (if (null? things)
        res
        (iter (cons res (square (car things)))
              (cdr things))))
  (iter nil items))

(define (for-each proc se)
  (cond ((null? se) true)
        (else
         (proc (car se))
         (for-each proc (cdr se)))))