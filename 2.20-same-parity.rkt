#lang sicp
(define (same-parity . paras)
  (define (recur predicate? se)
    (cond ((null? se) nil)
          ((predicate? (car se))
           (cons (car se) (recur predicate? (cdr se))))
          (else (recur predicate? (cdr se)))))
  (if (null? paras)
      nil
      (recur (if (even? (car paras)) even? odd?) paras)))
