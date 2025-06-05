(define (range n)
  (if (= n 0) '() (cons n (range (- n 1)))))
(define (fold f acc lst)
  (if (null? lst)
      acc
      (fold f (f acc (car lst)) (cdr lst))))
(define (add a b) (+ a b))
(display (fold add 0 (range 1000)))
