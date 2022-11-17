#lang sicp
;; the binary mobile  and its branch
(define (make-mobile left right) (cons left right))
(define (make-branch length structure) (cons length structure))
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cdr branch))

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (number? structure)
        structure
        (mobile-weight structure))))
(define (mobile-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (total-weight mobile)
  (mobile-weight mobile))


(define (torque-branch branch)
  (* (branch-length branch) (branch-weight branch)))

(define (balanced? mobile)
  (define (mobile-check? mobile)
    (let ((left (left-branch mobile))
          (right (right-branch mobile)))
      (and (branch-check? left)
           (branch-check? right)
           (eq? (torque-branch left) (torque-branch right)))))
  (define (branch-check? branch)
    (let ((structure (branch-structure branch)))
      (if (number? structure)
          #T
          (mobile-check? structure))))
  (mobile-check? mobile))

;; checks in balanced-mobile? return weight number or false;
(define (balanced-mobile? mobile)
  (define (mobile-check mobile)
    (let ((left (left-branch mobile))
          (right (right-branch mobile)))
      (let ((left-res (branch-check left)))
        (if (not left-res)
            #f
            (let ((right-res (branch-check right)))
              (if (not right-res)
                  #f
                  (let ((res (eq? (* (branch-length left)
                                     left-res)
                                  (* (branch-length right)
                                     right-res))))
                    (if res
                        (+ left-res right-res)
                        #f))))))))
  (define (branch-check branch)
    (let ((structure (branch-structure branch)))
      (if (number? structure)
          structure
          (mobile-check structure))))
  (let ((res (mobile-check mobile)))
    (if (not res)
        #f
        #t)))
      
;; example
(define b1 (make-branch 1 3))
(define b2 (make-branch 1 2))
(define b3 (make-branch 1 2))
(define b4 (make-branch 1 1))
(define b5 (make-branch 1 1))

(define m1 (make-mobile b1 b2))
(define m2 (make-mobile b4 b5))

(define b6 (make-branch 1 m1))
(define m3 (make-mobile b6 b3))
(define b7 (make-branch 1 m3))
(define b8 (make-branch 1 m2))
(define t-m (make-mobile b7 b8))


;; question 3, about balanced
(define bb1 (make-branch 1 4))
(define bb2 (make-branch 2 2))
(define bb3 (make-branch 1 1))
(define mm1 (make-mobile bb1 bb2))
(define mm2 (make-mobile bb1 bb3))

(define bb4 (make-branch 3 4))
(define bb5 (make-branch 2 mm1))
(define mm3 (make-mobile bb4 bb5))


(define bb6 (make-branch 2 mm3))
(define bb7 (make-branch 8 1))
(define bb8 (make-branch 2 4))
(define mm4 (make-mobile bb7 bb8))
(define bb9 (make-mobile 4 mm4))
(define mm5 (make-mobile bb6 bb9))



(balanced-mobile? mm1)
(balanced-mobile? mm2)
(balanced-mobile? mm5)

(total-weight mm5)
(total-weight mm4)
(total-weight mm3)
