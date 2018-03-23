(define (self-evaluating? x)
  (or (boolean? x) (number? x) (null? x) (string? x)))

(define (evlist exp env)
  (cond
   ((null? exp) '())
   (else (cons (eval (car exp) env)
               (evlist (cdr exp) env)))))

(define (append l s)
  (cond ((null? l) s)
        (else
         (cons (car l)
               (append (cdr l) s)))))

(define (pair-up a b)
  (cond ((and (null? a) (null? b)) '())
        (else (cons (cons (car a) (car b))
                    (pair-up (cdr a) (cdr b))))))

(define (lookup sym env)
  (cond ((null? env) (error "Unbound symbol: " sym))
        ((eq? (caar env) sym) (cadar env))
        (else (lookup sym (cdr env)))))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((eq? (car exp) 'quote) (cadr exp))
        ((eq? (car exp) 'car) (car (eval (cadr exp) env)))
        ((eq? (car exp) 'cdr) (cdr (eval (cadr exp) env)))
        ((eq? (car exp) 'cons) (cons (eval (cadr exp) env)
                                     (eval (caddr exp) env)))))
