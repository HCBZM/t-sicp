#lang sicp
(define (make-interval a b)
  (if (< a b)
      (cons a b)
      (cons b a)))
(define (upper-bound interval)
  (cdr interval))
(define (lower-bound interval)
  (car interval))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center-interval interval)
  (half (+ (upper-bound interval) (lower-bound interval))))
(define (width-interval interval)
  (half (- (upper-bound interval) (lower-bound interval))))

(define (make-center-percent c p)
  (make-center-width c (* c p)))
(define (percent-interval interval)
  (/ (width-interval interval) (center-interval interval)))

(define (add-interval x y)
  (make-interval (+ (upper-bound x) (upper-bound y))
                 (+ (lower-bound x) (lower-bound y))))
(define (sub-interval x y)
  (make-interval (- (upper-bound x) (lower-bound y))
                 (- (lower-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((x-l (lower-bound x))
        (x-u (upper-bound x))
        (y-l (lower-bound y))
        (y-u (upper-bound y)))
    (cond ((positive-interval? x)
           (cond ((positive-interval? y)
                  (make-interval (* x-l y-l) (* x-u y-u)))
                 ((negtive-interval? y)
                  (make-interval (* x-u y-l) (* x-l y-u)))
                 (else
                  (make-interval (* x-u y-u) (* x-u y-l)))))
          ((negtive-interval? x)
           (cond ((positive-interval? y)
                  (make-interval (* x-l y-u) (* x-u y-l)))
                 ((negtive-interval? y)
                  (make-interval (* x-l y-l) (* x-u y-u)))
                 (else
                  (make-interval (* x-l y-l) (* x-l y-u)))))
          (else
           (cond ((positive-interval? y)
                  (make-interval (* x-l y-u) (* x-u y-u)))
                 ((negtive-interval? y)
                  (make-interval (* x-l y-l) (* x-u y-l)))
                 (else
                  (make-interval (max (* x-l y-l) (* x-u y-u))
                                 (min (* x-u y-l) (* x-l y-u)))))))))
(define (div-interval x y)
  (let ((l (lower-bound y))
        (u (upper-bound y)))
    (if (and (> u 0) (> 0 l))
        (error "error")
        (mul-interval x
                      (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y)))))))

(define (positive-interval? interval)
  (> (lower-bound interval) 0))
(define (negtive-interval? interval)
  (< (upper-bound interval) 0))


(define (average a b) (/ (+ a b) 2))
(define (half n) (/ n 2))




(define a (make-interval -1 2))
(define a-p (make-interval 1 2))
(define a-n (make-interval -1 -2))
(define b (make-interval -3 4))
(define b-p (make-interval 3 4))
(define b-n (make-interval -3 -4))


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))