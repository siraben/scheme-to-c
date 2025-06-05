(begin
  (display (string-length "hello"))
  (display (string-ref "abc" 1))
  (display (list? '(1 2 3)))
  (display (list? (cons 1 2)))
  (display (char->integer (string-ref "A" 0)))
  (display (integer->char 66))
  ;; mutate a fresh copy instead of a literal so guile agrees
  (let ((s (string-copy "ab")))
    (string-set! s 1 (integer->char 120))
    (display s))
)
