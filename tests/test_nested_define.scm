(define (foo x)
  (define (double x) (+ x x))
  (double (double x)))

(display (foo 3))
