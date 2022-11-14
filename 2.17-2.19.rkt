#lang sicp
(define (last-pair list)
  (if (null? (cdr list))
      list
      (last-pair (cdr list))))
(define (reverse list)
  (define (iter res list)
    (if (null? list)
        res
        (iter (cons (car list) res) (cdr list))))
  (iter nil list))

(define (change-counting amount coins)
  (cond ((= amount 0) 1)
        ((or (no-more? coins)
             (< amount 0))
         0)
        (else
         (+ (change-counting amount (except-first-denomination coins))
            (change-counting (- amount (first-denomination coins)) coins)))))
(define (no-more? coins)
  (null? coins))
(define (first-denomination coins)
  (car coins))
(define (except-first-denomination coins)
  (cdr coins))


;; example
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
