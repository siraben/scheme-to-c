(begin
  (define lst '(2 3))
  (display `(1 ,@lst 4))
  (define x 5)
  (display `(a ,x c))
)
