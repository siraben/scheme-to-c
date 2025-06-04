(begin
  (define Z-impl ; Y-combinator
    (lambda (f)
      ((lambda (x) (f (lambda (v) ((x x) v))))
       (lambda (x) (f (lambda (v) ((x x) v)))))))

  (define make-concat-impl
    (lambda (self) ; self takes two lists: lst1, lst2 as a pair (lst1 . lst2)
      (lambda (args-pair) 
        (let ((lst1 (car args-pair))
              (lst2 (cdr args-pair)))
          (if (null? lst1)
              lst2
              (cons (car lst1) (self (cons (cdr lst1) lst2))))))))

  ;; Initial function that takes two separate arguments
  (define my-concat-setup
    (lambda (z-combinator actual-maker)
      (lambda (lst1 lst2)
        ((z-combinator actual-maker) (cons lst1 lst2)))))

  (define my-concat (my-concat-setup Z-impl make-concat-impl))

  (display (my-concat '(1 2 3) '(4 5 6)))
  (display (my-concat '() '(4 5 6)))
  (display (my-concat '(1 2 3) '()))
  (display (my-concat '() '()))
  (display (my-concat '(a b) '(c d e f)))
) 