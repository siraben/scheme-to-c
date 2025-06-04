(begin
  (define Z-impl ; Y-combinator
    (lambda (f)
      ((lambda (x) (f (lambda (v) ((x x) v))))
       (lambda (x) (f (lambda (v) ((x x) v)))))))

  (define make-zip-impl
    (lambda (self) ; self takes (lst1 . lst2)
      (lambda (args-pair)
        (let ((lst1 (car args-pair))
              (lst2 (cdr args-pair)))
          (if (or (null? lst1) (null? lst2))
              '()
              (cons (cons (car lst1) (car lst2)) ; make a pair (car l1 . car l2)
                    (self (cons (cdr lst1) (cdr lst2)))))))))

  (define my-zip-setup
    (lambda (z-combinator actual-maker)
      (lambda (lst1 lst2)
        ((z-combinator actual-maker) (cons lst1 lst2)))))

  (define my-zip (my-zip-setup Z-impl make-zip-impl))

  (display (my-zip '(1 2 3) '(a b c)))
  (display (my-zip '(1 2) '(a b c d)))
  (display (my-zip '(1 2 3 4) '(a b)))
  (display (my-zip '() '(a b c)))
  (display (my-zip '(1 2 3) '()))
  (display (my-zip '() '()))
) 