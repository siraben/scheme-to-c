(begin
  (define Z-impl ; Y-combinator
    (lambda (f)
      ((lambda (x) (f (lambda (v) ((x x) v))))
       (lambda (x) (f (lambda (v) ((x x) v)))))))

  (define make-map-impl
    (lambda (self)
      (lambda (args-pair)
        (let ((proc (car args-pair))
              (lst (cdr args-pair)))
          (if (null? lst)
              '()
              (cons (proc (car lst))
                    (self (cons proc (cdr lst)))))))))

  (define my-map-setup
    (lambda (z-combinator actual-maker)
      (lambda (proc lst)
        ((z-combinator actual-maker) (cons proc lst)))))

  (define my-map (my-map-setup Z-impl make-map-impl))

  (define add1 (lambda (n) (+ n 1)))

  (define listify (lambda (x) (cons x '())))

  (display (my-map add1 '(1 2 3 4)))
  (display (my-map listify '(a b c)))
  (display (my-map add1 '()))
  (display (my-map (lambda (x) (* x 10)) '(5 6 7)))
) 