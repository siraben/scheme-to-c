(define puzzle
  '((0 4 3 0 8 0 2 5 0)
    (6 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 1 0 9 4)
    (9 0 0 0 0 4 0 7 0)
    (0 0 0 6 0 8 0 0 0)
    (0 1 0 2 0 0 0 0 3)
    (8 2 0 5 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 5)
    (0 3 4 0 9 0 7 1 0)))

;; list utilities
(define (list-ref lst n)
  (if (= n 0)
      (car lst)
      (list-ref (cdr lst) (- n 1))))

(define (list-set lst n val)
  (if (= n 0)
      (cons val (cdr lst))
      (cons (car lst) (list-set (cdr lst) (- n 1) val))))

;; board utilities
(define (board-ref board r c)
  (list-ref (list-ref board r) c))

(define (board-set board r c val)
  (list-set board r (list-set (list-ref board r) c val)))

(define (contains? lst val)
  (if (null? lst)
      #f
      (if (= (car lst) val)
          #t
          (contains? (cdr lst) val))))

(define (column-contains? board c val)
  (if (null? board)
      #f
      (if (= (list-ref (car board) c) val)
          #t
          (column-contains? (cdr board) c val))))

(define (subgrid-row-check board r c val count)
  (if (= count 3)
      #f
      (if (= (board-ref board r c) val)
          #t
          (subgrid-row-check board r (+ c 1) val (+ count 1)))))

(define (subgrid-check board r c val count)
  (if (= count 3)
      #f
      (if (subgrid-row-check board r c val 0)
          #t
          (subgrid-check board (+ r 1) c val (+ count 1)))))

(define (subgrid-contains? board r c val)
  (let ((sr (- r (modulo r 3)))
        (sc (- c (modulo c 3))))
    (subgrid-check board sr sc val 0)))

(define (valid? board r c val)
  (if (contains? (list-ref board r) val)
      #f
      (if (column-contains? board c val)
          #f
          (if (subgrid-contains? board r c val)
              #f
              #t))))

(define (find-zero-in-row row col)
  (if (null? row)
      -1
      (if (= (car row) 0)
          col
          (find-zero-in-row (cdr row) (+ col 1)))))

(define (find-zero board row)
  (if (null? board)
      #f
      (let ((col (find-zero-in-row (car board) 0)))
        (if (= col -1)
            (find-zero (cdr board) (+ row 1))
            (cons row col)))))

(define (solve-num board r c num)
  (if (> num 9)
      #f
      (if (valid? board r c num)
          (let ((res (solve (board-set board r c num))))
            (if res res (solve-num board r c (+ num 1))))
          (solve-num board r c (+ num 1)))))

(define (solve board)
  (let ((pos (find-zero board 0)))
    (if pos
        (solve-num board (car pos) (cdr pos) 1)
        board)))

(display (solve puzzle))
