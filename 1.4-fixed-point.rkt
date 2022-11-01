#lang sicp
(define (fixed-point f guess)
  (define (close-enough? a b)
    (< (abs (- 1 (/ a b))) 0.0001))
  ((iterative-improve f close-enough?) guess))

(define (average-damp f)
  (lambda (x)
    (average x (f x))))
(define (average a b) (/ (+ a b) 2))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (square x) (* x x))



(define (deriv g)
  (let ((dx 0.00001))
    (lambda (x)
      (/ (- (g (+ x dx)) (g x))
         dx))))

(define (newtow g)
  (let ((dg (deriv g)))
    (fixed-point (lambda (x)
                   (- x
                      (/ (g x)
                         (dg x))))
                 1.0)))

(define (cuberoot x)
  (newtow (lambda (y) (- x (cube y)))))
(define (cube x) (* x x x))
(define (squareroot x)
  (newtow (lambda (y) (- (square y) x))))







;1.4
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(define (double proc)
  (lambda (x) (proc (proc x))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (let ((g (repeated f (- n 1))))
        (lambda (x)
          (f (g x))))))
(define (repeated1 f n)
  (if (= n 1)
      f
      (compose f (repeated1 f (- n 1)))))

(define (repeated-iter f n)
  (define (iter res n)
    (if (= n 1)
        res
        (iter (lambda (x) (f (res x))) (- n 1))))
  (iter f n))
(define (repeated-iter1 f n)
  (define (iter res n)
    (if (= n 1)
        res
        (iter (compose f res) (- n 1))))
  (iter f n))


(define (smooth f)
  (let ((dx 0.00001))
    (lambda (x)
      (/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3))))
(define (smooth-times f n)
  ((repeated smooth n) f))

;; 1.45
(define (root-n x n)
  (define (f n) n)
  (fixed-point ((repeated average-damp (f (- n 1) 2)) (lambda (y) (/ x (expt y (- n 1))))) 1.0))


;;1.46
(define (iterative-improve improve good-enough?)
  (define (iter guess)
    (let ((new-guess (improve guess)))
      (if (good-enough? guess new-guess)
          new-guess
          (iter new-guess))))
  iter)