#lang sicp
(define (unique-pairs n)
  (flat-map (lambda (i)
              (map (lambda (j)
                     (list i j))
                   (enumerate-interval 1 (- i 1))))
            (enumerate-interval 2 n)))
(define (flat-map proc seq)
  (accumulate append nil (map proc seq)))
(define (enumerate-interval lower upper)
  (if (> lower upper)
      nil
      (cons lower (enumerate-interval (+ 1 lower) upper))))
(define (accumulate combine init seq)
  (if (null? seq)
      init
      (combine (car seq)
               (accumulate combine init (cdr seq)))))

;; 2.41
(define (triple n)
  (flat-map (lambda (k)
             (map (lambda (res)
                    (cons k res))
                  (unique-pairs (- k 1))))
           (enumerate-interval 3 n)))
(define (triple-sum n s)
  (filter (lambda (seq)
            (= s (list-sum seq)))
          (triple n)))
(define (list-sum seq)
  (accumulate + 0 seq))

(define (filter predicate? seq)
  (cond ((null? seq) nil)
        ((predicate? (car seq))
         (cons (car seq) (filter predicate? (cdr seq))))
        (else
         (filter predicate? (cdr seq)))))

(define (filter-map predicate? proc seq)
  (cond ((null? seq) nil)
        ((predicate? (car seq))
         (cons (proc (car seq))
               (filter-map predicate? proc (cdr seq))))
        (else
         (filter-map predicate? proc (cdr seq)))))

(define (triple-sum2 n s)
  (flat-map (lambda (ke)
              (flat-map (lambda (ie)
                          (filter-map (lambda (je)
                                        (= s (+ je ie ke)))
                                      (lambda (je)
                                        (list je ie ke))
                                      (enumerate-interval 1 (- ie 1))))
                        (enumerate-interval 2 (- ke 1))))
            (enumerate-interval 3 n)))

;; 2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions)
                  (safe? k positions))
                (flat-map (lambda (rest-of-queens)
                            (map (lambda (new-row)
                                   (adjoin-position new-row k rest-of-queens))
                                 (enumerate-interval 1 board-size)))
                          (queen-cols (- k 1))))))
  (queen-cols board-size))
(define empty-board nil)
(define (make-position row col) (list row col))
(define (row-pos p) (car p))
(define (col-pos p) (cadr p))
(define (adjoin-position row col rest-postions)
  (cons (make-position row col) rest-postions))
(define (safe? col positions)
  (let ((cur-position (find (lambda (position)
                              (= col (col-pos position)))
                            positions))
        (rest-positions (remove (lambda (position)
                                  (= col (col-pos position)))
                                positions)))
    (let ((row (row-pos cur-position)))
      (not (some-test (lambda (position)
                        (let ((col2 (col-pos position))
                              (row2 (row-pos position)))
                          (or (= row2 row)
                              (and (= (abs (- row2 row)) (abs (- col2 col)))))))
                      rest-positions)))))

(define (find predicate? seq)
  (cond ((null? seq) #F)
        ((predicate? (car seq))
         (car seq))
        (else
         (find predicate? (cdr seq)))))
(define (remove predicate? seq)
  (filter (lambda (el)
            (not (predicate? el)))
          seq))
(define (some-test predicate? seq)
  (cond ((null? seq) #F)
        ((predicate? (car seq)) #T)
        (else
         (some-test predicate? (cdr seq)))))
(define (each-for-result proc seq)
  (if (null? seq)
      #F
      (let ((res (proc (car seq))))
        (if res
            res
            (each-for-result proc (cdr seq))))))


(define (queens-one-way board-size)
  (define rows (enumerate-interval 1 board-size))
  (define (place col positions)
    (if (> col board-size)
        positions
        (each-for-result
         (lambda (new-row)
           (let ((new-positions (adjoin-position new-row col positions)))
             (if (safe? col new-positions)
                 (place (+ col 1) new-positions)
                 #F)))
         rows)))
  (let ((one-way (place 1 empty-board)))
    (if one-way
        one-way
        "no any way")))
(define (queens2 board-size)
  (define rows (enumerate-interval 1 board-size))
  (define (try col positions)
    (if (> col board-size)
        positions
        (try (+ 1 col) (flat-map (lambda (one-positions)
                                   (map-filter (lambda (new-row)
                                                 (adjoin-position
                                                  new-row
                                                  col
                                                  one-positions))
                                               (lambda (test-positions)
                                                 (safe? col test-positions))
                                               rows))
                                 positions))))
  (try 1 (list empty-board)))
(define (map-filter transducer predicate? seq)
  (if (null? seq)
      nil
      (let ((res (transducer (car seq))))
        (if (predicate? res)
            (cons res (map-filter transducer predicate? (cdr seq)))
            (map-filter transducer predicate? (cdr seq))))))
;; 2.43
;; (1 + 8^1 + 8^2 + 8^3 + 8^4 + 8^5 + 8^6 + 8^7) / 8 * T   X
 
;; (f 1) = T
;; (f 2) = T + 8 (f 1) = T + 8 T
;; (f 3) = T + 8 (f 2) = T + 8 T + 8^2 T = (1 + 8^1 + 8^2) T
;; .
;; .
;; .
;; (f 8) = (1 + 8^1 + 8^2 + 8^3 + 8^4 + 8^5 + 8^6 + 8^7) T
  