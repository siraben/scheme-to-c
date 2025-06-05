(define (make-adder n)
  (lambda (x) (+ x n)))
(define add5 (make-adder 5))
(define (loop i acc)
  (if (= i 0)
      acc
      (loop (- i 1) (add5 acc))))
(display (loop 100000 0))
