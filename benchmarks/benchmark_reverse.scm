(define (range n)
  (if (= n 0) '() (cons n (range (- n 1)))))
(define (reverse lst acc)
  (if (null? lst)
      acc
      (reverse (cdr lst) (cons (car lst) acc))))
(display (car (reverse (range 1000) '())))
