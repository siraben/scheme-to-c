(define (loop n)
  (if (= n 0)
      'done
      (loop (- n 1))))
(display (loop 100000))
