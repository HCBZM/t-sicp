#lang sicp
(define (map1 p sequence)
  (accumulate (lambda (cur rest)
                (cons (p cur)
                      rest))
              nil
              sequence))
(define (append s1 s2)
  (accumulate cons s2 s1))
(define (length seq)
  (accumulate (lambda (cur rest) (+ 1 rest))
              0
              seq))

(define (accumulate combine initial seq)
  (if (null? seq)
      initial
      (combine (car seq)
               (accumulate combine initial (cdr seq)))))

;; 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (cur rest)
                (+ cur (* x rest)))
              0
              coefficient-sequence))

;; 2.35
(define (count-leaves t)
  (accumulate append nil (map (lambda (cur)
                                (if (not (pair? cur))
                                    (list cur)
                                    (count-leaves cur)))
                              t)))

;; 2.36
(define (accumulate-n combine init sequences)
  (if (null? (car sequences))
      nil
      (cons (accumulate combine init (map car sequences))
            (accumulate-n combine init (map cdr sequences)))))


;; 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))
(define (transpose matrix)
  (accumulate-n cons nil matrix))
(define (matrix-*-matrix m n)
  (let ((col (transpose n)))
    (map (lambda (m-r)
           (map (lambda (n-c)
                  (dot-product m-r n-c))
                col))
         m)))

;; 2.38
(define fold-right accumulate)
(define (fold-left combine init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (combine result (car rest))
              (cdr rest))))
  (iter init seq))

;; 2.39

(define (reverse1 seq)
  (fold-right (lambda (cur rest)
                (append rest (list cur)))
              nil
              seq))
(define (reverse2 seq)
  (fold-left (lambda (res next)
               (cons next res))
             nil
             seq))