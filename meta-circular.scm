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


(define (set-var-in-frame! var val frame)
  (cond ((null? (car frame)) #f)
        ((eq? var (caar frame))
         (set-car! (cdr frame) val))
        (else
         (set-var-in-frame! var
                            val
                            (cons (cdar frame) (cddr frame))))))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))


(define (set-var! var val env)
  (define (set-var-search! var val frame)
    (cond ((or (null? frame) (null? (car frame))) #f)
          ((eq? var (caar frame))
           (set-car! (cdr frame) val) #t)
          (else (set-var-search! var val
                                 (cons (cdr (car frame))
                                       (cddr frame))))))
  (if (null? env)
      #f
      (or (set-var-search! var val (car env))
          (set-var! var val (cdr env)))))

(define (define-var! var val env)
  (or (set-var! var val env)
      (add-binding-to-frame! var val (car env))))

(define the-empty-environment '())

(define primitive-procedure-names
  '(cons car cdr list + - * /))

(define primitive-procedure-objects
  (list cons car cdr list + - * /))


(define (extend-environment vars vals env)
  (cons (make-frame vars vals) env))


(define (setup-environment)
  (let ((initial-env
         (extend-environment primitive-procedure-names
                             primitive-procedure-objects
                             the-empty-environment)))
    (define-var! 'true #t initial-env)
    (define-var! 'false #f initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define (evcond ps env)
  (cond ((null? exp) '())
        ((eq? (caar ps) 'else)
         (eval (cadar ps) env))
        (else
         (if (eval (caar ps) env)
             (ev-begin (cdar ps) env)
             (evcond (cdr ps) env)))))

(define (make-procedure args body env)
  (list 'procedure args body env))

;; Replace the definitions for cons car and cdr with the following
;; lines to replicate the "cons should not evaluate its arguments"
;; behavior.

;; ((tagged-list? exp 'car)
;;  (if (quoted? (cadr exp))
;;      (car (cadr exp))
;;      (apply* (car (eval (cadr exp) env)) '())))

;; ((tagged-list? exp 'cdr)
;;  (if (quoted? (cadr exp))
;;      (cdr (cadr exp))
;;      (apply* (cdr (eval (cadr exp) env)) '())))
;; ;; cons should not evaluate its arguments

;; ((tagged-list? exp 'cons)
;;  (cons (make-procedure '()
;;                        (cons (cadr exp) '())
;;                        env)
;;        (make-procedure '()
;;                        (cons (caddr exp) '())
;;                        env)))

(define (eval exp env)
  (cond
   ((self-evaluating? exp) exp)
   ((symbol? exp) (lookup-in-env exp env))
   ((tagged-list? exp 'quote) (cadr exp))
   ((tagged-list? exp 'eq?) (eq? (eval (cadr exp) env)
                                 (eval (caddr exp) env)))
   ((tagged-list? exp 'if) (if (eval (cadr exp) env)
                               (eval (caddr exp) env)
                               (eval (cadddr exp) env)))
   ((tagged-list? exp 'cond) (evcond (cdr exp) env))
   ((tagged-list? exp 'null?) (null? (eval (cadr exp) env)))

   ((tagged-list? exp 'define)
    (if (pair? (cadr exp))
        (define-var!
          (caadr exp)
          (make-procedure (cdadr exp) (cddr exp) env)
          env)
        (define-var!
          (cadr exp)
          (eval (caddr exp) env)
          env)))
   
   ((tagged-list? exp 'set!)
    (set-var!
     (cadr exp)
     (eval (caddr exp) env)
     env))
   ((tagged-list? exp 'begin)
    (ev-begin (cdr exp) env))

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

(define (apply* proc args)
  (cond
   ((procedure? proc)
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
(add-test '(cond ((eq? #t #t) 'a 'b)) 'b)

(add-test '((lambda (reverse)
              (reverse (quote (1 2 3 4 5 6 7 8 9))))

            (lambda (list)
              ((lambda (rev)
                 (rev rev '() list))
               (lambda (rev^ a l)
                 (if (null? l)
                     a
                     (rev^ rev^ (cons (car l) a) (cdr l)))))))
          '(9 8 7 6 5 4 3 2 1))

(add-test '((lambda (f g x) (f (g x)))
            (lambda (x) (cons 'a x))
            (lambda (x) (cons 'b x))
            '(c))
          '(a b c))
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
       (let ((exp (car test-pair))
             (res (cdr test-pair))
             (env (setup-environment)))
         (define actual (eval exp env))
         (if (equal? actual res)
             (begin
               (set! success-count (+ 1 success-count))
               (if verbose
                   (printf "Test ~a PASSED : ~a => ~a\n"
                           current-test
                           exp
                           res)))
             (begin
               (printf "\nTest ~a FAILED : ~a\n\nEXPECTED: ~a\nGOT: ~a\n\n"
                       current-test
                       exp
                       res
                       actual)))))
     eval-test-list)
    (printf "\nDone. ~a of ~a tests passed.\n" success-count test-count)))

;;(add-test '(append '(a b c) '(d e)) '())

(add-test '(begin
             (define append
               (lambda (l s)
                 (cond ((null? l) s)
                       (else (cons (car l) (append (cdr l) s))))))
             (append '(a b c) '(d e)))
          '(a b c d e))

(add-test '(begin
             (define (append l s)
               (cond ((null? l) s)
                     (else (cons (car l) (append (cdr l) s)))))
             (append '(a b c) '(d e)))
          '(a b c d e))

(add-test '(begin
             (define x 'a)
             (define (foo bar)
               (define x bar)
               (cons 'a x))
             (foo 'b))
          '(a . b))

(add-test '(begin
             (define x 'foo)
             (define x 'bar)
             x)
          'bar)

(add-test '(begin
             (define x 'foo)
             (define res '())
             (define (thing) (set! x 'bar))
             (set! res (cons x res))
             (thing)
             (set! res (cons x res))
             res)
          '(bar foo))


(define (fib n)
  (cond ((< n 2) n)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
(define (append* l s)
  (cond ((null? l) s)
        (else (cons (car l) (append* (cdr l) s)))))

(define (reverse* l s)
  (cond ((null? l) '())
        (else (reverse* (cdr l) (cons (car l) s)))))
