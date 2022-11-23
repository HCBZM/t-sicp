#lang sicp
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else
         (cons (square-tree (car tree))
               (square-tree (cdr tree))))))
(define (square x) (* x x))
(define (square-tree2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree2 sub-tree)
             (square sub-tree)))
       tree))

;; 2.31
(define (tree-map proc tree)
  (map (lambda (sub)
         (if (pair? sub)
             (tree-map proc sub)
             (proc sub)))
       tree))

(define (square-tree3 tree)
  (tree-map square tree))

;; 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((cur-element (car s))
            (rest-subsets (subsets (cdr s))))
        (append (map (lambda (set)
                       (cons cur-element set))
                     rest-subsets)
                rest-subsets))))
        


;; example
(define x (list (list 1 (list 2 3)) (list 4 5) (list 1 2 3)))