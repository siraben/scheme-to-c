(begin
  (define a 10)
  (define get-a (lambda () a))
  (set! a 20)
  (display (get-a))
  (display (let ((x 1)) (set! x 3) x))
)
