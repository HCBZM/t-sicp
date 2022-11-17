#lang sicp
(define a (list 1 (list 2 (list 3 4))))
(define b (list 1 3 (list 5 7) 9))
(define c (list (list 7)))
(define d (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(define x (list 1 2 3))
(define y (list 4 5 6))

(define (append se1 se2)
  (cond ((null? se1) se2)
        (else
         (cons (car se1) (append (cdr se1) se2)))))

(define (reverse se)
  (define (iter r se)
    (if (null? se)
        r
        (iter (cons (car se) r) (cdr se))))
  (iter nil se))

(define (deep-reverse tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) tree)
        (else
         (reverse (map deep-reverse tree)))))
(define (deep-reverse2 tree)
  (define (dealing-tree tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) tree)
          (else
           (reverse-start tree))))
  (define (reverse-start tree)
    (define (iter res rest)
      (if (null? rest)
          res
          (iter (cons (dealing-tree (car rest)) res)
                (cdr rest))))
    (iter nil tree))
  (dealing-tree tree))

(define (deep-reverse3 se)
  (cond ((null? se) nil)
        ((not (pair? se)) se)
        (else
         (map-reverse deep-reverse3 se))))

(define (map-reverse proc se)
  (define (iter res rest)
    (if (null? rest)
        res
        (iter (cons (proc (car rest)) res)
              (cdr rest))))
  (iter nil se))




(define (fringe tree)
  (define (dealing-cell cell)
    (cond ((null? cell) nil)
          ((not (pair? cell)) (list cell))
          (else
           (dealing-tree cell))))
  (define (dealing-tree tree)
    (if (null? tree)
        nil
        (append (dealing-cell (car tree))
                (dealing-tree (cdr tree)))))
  (dealing-cell tree))


(define (fringe2 tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else
         (map-accumulate-end-begin fringe2 append nil tree))))

(define (map-accumulate-end-begin proc combine init se)
  (if (null? se)
      init
      (combine (proc (car se))
               (map-accumulate-end-begin proc combine init (cdr se)))))

;; exercise：
(display " 2.25:")
(newline)
(car (cdr (car (cdr (cdr b)))))
(car (car c))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr d))))))))))))

(display " 2.26:")
(newline)
(append x y)
(cons x y)
(list x y)

(display " 2.27:")
(newline)
(define dx (list (list 1 2) (list 3 4)))
dx
(reverse dx)
(deep-reverse dx)

(display " 2.28:")
(newline)
(fringe dx)
(fringe (list dx dx))
(fringe2 (list dx dx))