(define (sum n acc)
  (if (= n 0)
      acc
      (sum (- n 1) (+ acc 1))))
(display (sum 100000 0))
