(define (foo x) (double (double x)))
(define (double x) (* x 2))
(display (foo 5))
