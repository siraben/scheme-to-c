(begin
  (display
    (letrec ((f (lambda (n)
                   (if (zero? n)
                       1
                       (g (sub1 n)))))
             (g (lambda (n)
                   (if (zero? n)
                       1
                       (f (sub1 n))))))
      (f 4)))
)
