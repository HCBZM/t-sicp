#lang sicp
(define (make-segment start end)
  (cons start end))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))
(define (length-segment seg)
  (let ((start (start-segment seg))
        (end (end-segment seg)))
    (distance-two-point start end)))

(define (distance-two-point p1 p2)
  (let ((x (- (x-point p1) (x-point p2)))
        (y (- (y-point p1) (y-point p2))))
    (sqrt (+ (square x) (square y)))))

(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display " ,")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment seg)
  (let ((s-seg (start-segment seg))
        (e-seg (end-segment seg)))
    (make-point (average (x-point s-seg) (x-point e-seg))
                (average (y-point s-seg) (y-point e-seg)))))


(define (permeter-rectangle rectangle)
  (let ((length (length-segment (length-rectangle rectangle)))
        (width (length-segment (width-rectangle rectangle))))
    (* 2 (+ length width))))
(define (area-rectangle rectangle)
  (let ((length (length-segment (length-rectangle rectangle)))
        (width (length-segment (width-rectangle rectangle))))
    (* length width)))

(define (make-rectangle length width)
  (cons length width))
(define (length-rectangle rec) (car rec))
(define (width-rectangle rec) (cdr rec))

(define (make-rectangle-1 o p1 p2)
  (cons (make-segment o p1) (make-segment o p2)))


(define (average a b) (/ (+ a b) 2))
(define (square x) (* x x))




;; example
(define p1 (make-point 1 2))
(define p2 (make-point 5 2))
(define p3 (make-point 1 4))
(define seg1 (make-segment p1 p2))
(define seg2 (make-segment p1 p3))

(define rec1 (make-rectangle-1 p1 p2 p3))
(define rec (make-rectangle seg1 seg2))