(define eval-test-list '())

(define (add-test exp res)
  (set! eval-test-list (cons (cons exp res) eval-test-list)))

(define (self-evaluating? x)
  (or (boolean? x) (number? x) (null? x) (string? x)))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

(define (evlist exp env)
  (cond
   ((null? exp) '())
   (else
    (cons (eval (car exp) env)
          (evlist (cdr exp) env)))))

(define (append l s)
  (cond ((null? l) s)
        (else
         (cons (car l)
               (append (cdr l) s)))))

(define (make-frame vars vals)
  (cons vars vals))

(define empty-environment '())

(define (lookup-in-frame var frame)
  (cond ((null? (car frame)) #f)
        ((eq? var (caar frame))
         (cadr frame))
        (else
         (lookup-in-frame var
                          (cons (cdar frame) (cddr frame))))))

(define (lookup-in-env var env)
  (cond ((null? env)
         (error 'lookup-in-env "Unbound symbol" var))
        (else
         (or (lookup-in-frame var (car env))
             (lookup-in-env var (cdr env))))))


(define (lookup sym env)
  (cond ((null? env) (error "Unbound symbol: " sym))
        ((eq? (caar env) sym) (cadar env))
        (else (lookup sym (cdr env)))))

(define (evcond ps env)
  (cond ((null? exp) '())
        ((eq? (caar ps) 'else)
         (eval (cadar ps) env))
        (else
         (if (eval (caar ps) env)
             (eval (cadar ps) env)
             (evcond (cdr ps) env)))))

(define (make-procedure args body env)
  (list 'procedure args body env))

(define (eval exp env)
  (cond
   ((self-evaluating? exp) exp)
   ((symbol? exp) (lookup-in-env exp env))
   ((tagged-list? exp 'quote) (cadr exp))
   ((tagged-list? exp 'car) (car (eval (cadr exp) env)))
   ((tagged-list? exp 'cdr) (cdr (eval (cadr exp) env)))
   ((tagged-list? exp 'cons) (cons (eval (cadr exp) env)
                                   (eval (caddr exp) env)))
   ((tagged-list? exp 'eq?) (eq? (eval (cadr exp) env)
                                 (eval (caddr exp) env)))
   ((tagged-list? exp 'if) (if (eval (cadr exp) env)
                               (eval (caddr exp) env)
                               (eval (cadddr exp) env)))
   ((tagged-list? exp 'cond) (evcond (cdr exp) env))
   ((tagged-list? exp 'null?) (null? (eval (cadr exp) env)))
   
   ;; Here's the big one, lambda! Extract the lambda body, and
   ;; make a closure with the extended environment
   ((tagged-list? exp 'lambda)
    (make-procedure (cadr exp) (cddr exp) env))
   ((pair? exp)
    (apply* (eval (car exp) env)
            (evlist (cdr exp) env)))))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

;; (begin <exp> ...) only returns the value of the last expression

;; It discards the values of the previous expressions as well, unless
;; there is mutation going on.
(define (ev-begin exps env)
  (cond ((null? (cdr exps)) (eval (car exps) env))
        (else
         (eval (car exps) env)
         (ev-begin (cdr exps) env))))

(define (extend-environment vars vals env)
  (cons (make-frame vars vals) env))

(define (apply* proc args)
  (cond ((procedure? proc)
         (apply proc args))
        ((compound-procedure? proc)
         (ev-begin (caddr proc)
                   (extend-environment
                    (cadr proc)
                    args
                    (cadddr proc))))
        (else (error 'apply* "Unknown application" proc))))

(add-test '(null? (cdr '(a))) #t)
(add-test '((lambda (x) x) 'a) 'a)
(add-test '((lambda (f x) (f x)) (lambda (x) (cons x 'b)) 'a) '(a . b))
(add-test '(((lambda (x) (lambda (y) (cons x y))) 'a) 'b) '(a . b))
(add-test '(quote a) 'a)
(add-test '(car (cons 'a 'b)) 'a)
(add-test '(cdr (cons 'a 'b)) 'b)
(add-test '(car (cdr (cons 'a (cons 'b '())))) 'b)
(add-test '() '())
(add-test '(car (cons '(a b c) '(d e))) '(a b c))
(add-test '(cdr (cons '(a b c) '(d e))) '(d e))
(add-test '(if (eq? 'a 'a) 'y 'n) 'y)
(add-test '(cond ((eq? 5 3) 'one)
                 ((eq? 'a (car (cons 'a 'b)))
                  'two)) 'two)

(add-test '(cond ((eq? 5 3) 'one)
                 ((eq? 'c (car (cons 'a 'b)))
                  'two)
                 (else 'three)) 'three)

;; Format of a test:

;; ((<exp> . <expected result>) ...)

(define (run-eval-tests . args)
  (let ((test-count (length eval-test-list))
        (success-count 0)
        (current-test 0)
        (verbose (or (null? args) (car args))))
    (printf "Starting ~a tests...\n\n" test-count)
    (for-each
     (lambda (test-pair)
       (set! current-test (+ 1 current-test))
       (let* ((exp (car test-pair))
              (res (cdr test-pair))
              (actual (eval exp '())))
         (if (equal? actual res)
             (begin
               (set! success-count (+ 1 success-count))
               (if verbose
                   (printf "Test ~a : ~a => ~a\n"
                           current-test
                           exp
                           res)))
             (begin
               (printf "\nTest ~a FAILED : ~a\n\nEXPECTED: ~a\nGOT: ~a\n\n"
                       current-test
                       exp
                       res
                       actual))
             )))
     eval-test-list)
    (printf "\nDone. ~a of ~a tests passed.\n" success-count test-count)))

