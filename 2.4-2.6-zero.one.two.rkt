#lang sicp
(define (cons x y)
  (lambda (m) (m x y)))
(define (car x)
  (x (lambda (a b) a)))
(define (cdr x)
  (x (lambda (a b) b)))

(define (cons1 x y)
  (* (expt 2 x) (expt 3 y)))
(define (car1 x)
  (factor-amount x 2))
(define (cdr1 x)
  (factor-amount x 3))

(define (factor-amount x f)
  (if (divid? x f)
      (+ 1 (factor-amount (/ x f) f))
      0))
(define (divid? number f)
  (= 0 (remainder number f)))


; 2.6

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))
(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))