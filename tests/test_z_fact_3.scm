(begin
  (define Z
    (lambda (f)
      ((lambda (x)
         (f (lambda (v) ((x x) v))))
       (lambda (x)
         (f (lambda (v) ((x x) v)))))))
  (define make-factorial
    (lambda (recursive-func)
      (lambda (n)
        (if (zero? n)
            1
            (* n (recursive-func (sub1 n)))))))
  (define factorial (Z make-factorial))
  (display (factorial 3))) 