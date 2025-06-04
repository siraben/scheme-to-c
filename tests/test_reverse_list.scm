(begin
  (define Z-impl
    (lambda (f)
      ((lambda (x) (f (lambda (v) ((x x) v))))
       (lambda (x) (f (lambda (v) ((x x) v)))))))

  (define make-reverser-impl
    (lambda (self) ; self takes one arg: (packed-lst . packed-acc)
      (lambda (packed-args) ; packed-args is (current-lst . current-acc)
        (if (null? (car packed-args)) ; if current-lst is null
            (cdr packed-args) ; return current-acc
            (self (cons (cdr (car packed-args)) ; next-lst
                        (cons (car (car packed-args)) (cdr packed-args)))))))) ; next-acc, prepending to old acc

  (define reverse-constructor
    (lambda (the-Z the-make-reverser)
      (lambda (lst) ; initial call takes one list
        ( (the-Z the-make-reverser) (cons lst '()) )))) ; initial packed args: (lst . '())

  (define reverse (reverse-constructor Z-impl make-reverser-impl))

  (display (reverse '(1 2 3 4)))) 