(define compile-port
  (make-parameter
   (current-output-port)
   (lambda (p)
     (unless (output-port? p)
       (error 'compile-port (format #t "Not an output port ~s." p)))
     p)))

(define (emit . args)
  (apply format (compile-port) args)
  (format (compile-port) ";")
  (newline (compile-port)))

(define (emit-no-colon . args)
  (apply format (compile-port) args)
  (format (compile-port) "")
  (newline (compile-port)))


(define (emit-no-newline . args)
  (apply format (compile-port) args)
  (format (compile-port) ""))


(define (atom? x)
  (or (symbol? x) (null? x)))

(define (immediate? x)
  (or (integer? x) (boolean? x) (null? x)))

(define (emit-immediate x)
  (cond ((integer? x)
         (begin
           (emit "eax.t = FIXNUM")
           (emit "eax.n = ~s" x)))
        ((boolean? x)
         (begin
           (emit "eax.t = BOOLEAN")
           (if x
               (emit "eax.b = 1")
               (emit "eax.b = 0"))))
        ((null? x)
         (begin
           (emit "eax.t = NIL")))))

(define (emit-program x)
  ;; TODO: Emit helper functions rather than just main.
  (emit-no-colon "int main(void)")
  (emit-no-colon "{")
  (emit "GC_INIT()")
  (emit-expr x)
  (emit "printf(\"Value of eax: \")")
  (emit "print(eax)")
  (emit "puts(\"\")")
  (emit-no-colon "}"))

(define (emit-display x)
  (emit-expr x)
  (emit "print(~s)" x)
  (emit "puts(\"\")"))

(define (emit-eq x)
  (let ((a (car x)) (b (cadr x)))
    (emit-expr a)
    (emit "push()")
    (emit-expr b)
    (emit "cmp()")))

(define (emit-fixnum? x)
  (let ((a (car x)))
    (emit-expr a)
    (emit "al = (eax.t == FIXNUM)")
    (emit "eax.t = BOOLEAN")
    (emit "eax.b = al")))

(define (emit-char? x)
  (let ((a (car x)))
    (emit-expr a)
    (emit "al = (eax.t == CHAR)")
    (emit "eax.t = BOOLEAN")
    (emit "eax.b = al")))

(define (emit-boolean? x)
  (let ((a (car x)))
    (emit-expr a)
    (emit "al = (eax.t == BOOLEAN)")
    (emit "eax.t = BOOLEAN")
    (emit "eax.b = al")))

(define (emit-null? x)
  (let ((a (car x)))
    (emit-expr a)
    (emit "al = (eax.t == NIL)")
    (emit "eax.t = BOOLEAN")
    (emit "eax.b = al")))

(define (emit-add1 x)
  (let ((a (car x)))
    (emit-expr a)
    (emit "eax.n++")))

(define (emit-sub1 x)
  (let ((a (car x)))
    (emit-expr a)
    (emit "eax.n--")))

(define (emit-zero? x)
  (let ((a (car x)))
    (emit-expr a)
    (emit "al = (eax.n == 0)")
    (emit "eax.t = BOOLEAN")
    (emit "eax.b = al")))

(define (emit-not x)
  (let ((a (car x)))
    (emit-expr a)
    (emit "eax.b = (eax.b != 1)")))

(define gensym-count 0)
(define gensym
  (lambda ()
    (set! gensym-count (+ 1 gensym-count))
    (string->symbol (string-append "label" (number->string gensym-count)))))

(define (emit-if x)
  (let ((alt-label (gensym))
        (end-label (gensym))
        (pred (car x))
        (conseq (cadr x))
        (alt (caddr x)))
    (emit-expr pred)
    (emit "if (!eax.b){goto ~s;}" alt-label)
    (emit-expr conseq)
    (emit "goto ~s" end-label)
    (emit-no-colon "~s:" alt-label)
    (emit-expr alt)
    (emit-no-colon "~s:" end-label)
    (emit "")))

(define (emit-add x)
  (let ((a (car x)) (n (cadr x)))
    (emit-expr a)
    (emit "push()")
    (emit-expr n)
    (emit "al = eax.n")
    (emit "pop()")
    (emit "eax.n += al")))

(define (emit-sub x)
  (let ((a (car x)) (n (cadr x)))
    (emit-expr a)
    (emit "push()")
    (emit-expr n)
    (emit "al = eax.n")
    (emit "pop()")
    (emit "eax.n -= al")))

(define (emit-mult x)
  (let ((a (car x)) (n (cadr x)))
    (emit-expr a)
    (emit "push()")
    (emit-expr n)
    (emit "al = eax.n")
    (emit "pop()")
    (emit "eax.n *= al")))

(define (emit-mod x)
  (let ((a (car x)) (n (cadr x)))
    (emit-expr a)
    (emit "push()")
    (emit-expr n)
    (emit "al = eax.n")
    (emit "pop()")
    (emit "eax.n %= al")))

(define (emit-div x)
  (let ((a (car x)) (n (cadr x)))
    (emit-expr a)
    (emit "push()")
    (emit-expr n)
    (emit "al = eax.n")
    (emit "pop()")
    (emit "eax.n /= al")))

(define (emit-exit x)
  (emit "exit(~a)" x))


(define (emit-expr x)
  (cond ((immediate? x)
         (emit-immediate x))
        ((atom? x)
         (emit "eax = ~s" x))
        ((eq? (car x) 'eq?)
         (emit-eq (cdr x)))
        ((eq? (car x) 'fixnum?)
         (emit-fixnum? (cdr x)))
        ((eq? (car x) 'boolean?)
         (emit-fixnum? (cdr x)))
        ((eq? (car x) 'char?)
         (emit-char? (cdr x)))
        ((eq? (car x) 'null?)
         (emit-null? (cdr x)))
        ((eq? (car x) 'add1)
         (emit-add1 (cdr x)))
        ((eq? (car x) 'sub1)
         (emit-sub1 (cdr x)))
        ((eq? (car x) 'not)
         (emit-not (cdr x)))
        ((eq? (car x) 'if)
         (emit-if (cdr x)))
        ((eq? (car x) 'zero?)
         (emit-zero? (cdr x)))
        ((eq? (car x) '+)
         (emit-add (cdr x)))
        ((eq? (car x) '-)
         (emit-sub (cdr x)))
        ((eq? (car x) '*)
         (emit-mult (cdr x)))
        ((eq? (car x) 'define)
         (emit-define (cdr x)))
        ((eq? (car x) 'begin)
         (emit-begin (cdr x)))
        ((eq? (car x) 'let)
         (emit-let (cdr x)))
        ((eq? (car x) 'cons)
         (emit-cons (cdr x)))
        ((eq? (car x) 'car)
         (emit-car (cdr x)))
        ((eq? (car x) 'cdr)
         (emit-cdr (cdr x)))
        ((eq? (car x) 'quote)
         (emit-quote (cadr x)))
        ((eq? (car x) 'cond)
         (emit-cond (cdr x)))
        ((eq? (car x) 'set!)
         (emit-set! (cdr x)))
        ((eq? (car x) 'display)
         (emit-display (cdr x)))
        ((eq? (car x) 'label)
         (emit-label (cadr x)))
        ((eq? (car x) 'goto)
         (emit-goto (cadr x)))
        ((eq? (car x) 'remainder)
         (emit-mod (cdr x)))
        ((eq? (car x) '/)
         (emit-div (cdr x)))
        ((eq? (car x) 'exit)
         (emit-exit (cadr x)))))

(define (emit-set! x)
  (let ((definition (car x))
        (body (cadr x)))
    (if (symbol? definition)
        (emit-set-var definition body)
        (emit-no-colon "// NOT IMPLEMENTED"))))

(define (emit-set-var var body)
  (emit-expr body)
  (emit "~s = eax" var))

(define (emit-define x)
  (let ((definition (car x))
        (body (cadr x)))
    (if (symbol? definition)
        (emit-define-var definition body)
        (emit-no-colon "// NOT IMPLEMENTED"))))

(define (emit-define-var var body)
  (emit-expr body)
  (emit "reg ~s = eax" var))

(define (emit-begin x)
  (for-each (lambda (x) (emit-expr x)) x))


;; Convert let
;; example: (let ((x 3)) (+ x 3))

;; {
;;    compile 3...
;;    reg x = eax;
;;    compile (+ x 3), replacing references to x accordingly
;;    store result in eax
;; }


(define (emit-quoted-list x)
  (emit-quote (car x))
  (emit "push()")
  (emit-quote (cdr x))
  (emit "ebx = eax")
  (emit "pop()")
  (emit "eax = *cons(&eax, &ebx)"))

(define (emit-quote x)
  (if (null? x)
      (emit "eax.t = NIL")
      (cond ((symbol? x)
             (emit "eax = *make_symbol(\"~s\")" x))
            ((pair? x)
             (emit-quoted-list x))
            ((immediate? x)
             (emit-immediate x)))))

(define (emit-let x)
  (let ((varlist (car x))
        (body (cadr x)))
    (emit-no-colon "{")
    (for-each (lambda (x) (emit-define-var (car x) (cadr x))) varlist)
    (emit-expr body)
    (emit-no-colon "}")))


(define (emit-cons x)
  (let ((a (car x)) (b (cadr x)))
    (emit-expr a)
    (emit "push()")
    (emit-expr b)
    (emit "ebx = eax")
    (emit "pop()")
    (emit "eax = *cons(&eax, &ebx)")))


(define (emit-car x)
  (let ((a (car x)))
    (emit-expr a)
    (emit "eax = *car(&eax)")))

(define (emit-cdr x)
  (let ((a (car x)))
    (emit-expr a)
    (emit "eax = *cdr(&eax)")))


;; (cond  ((a b) (c d) ..))
;; if a then b, else if c then d else if...

;; compile a
;; jump to next predicate if false
;; compile body
;; body:
;; compile body, then jump to end.

(define (emit-cond x)
  (let ((end-label (gensym)))
    (for-each
     (lambda (x)
       (let ((pred (car x))
             (body (cadr x))
             (alt-label (gensym))
             (next-label (gensym)))
         (emit-expr pred)
         (emit "if (!eax.b){goto ~s;}" alt-label)
         (emit-expr body)
         (emit "goto ~s" end-label)
         (emit "~s:" alt-label)))
     x)
    (emit "~s:" end-label)))

(define (emit-if x)
  (let ((alt-label (gensym))
        (end-label (gensym))
        (pred (car x))
        (conseq (cadr x))
        (alt (caddr x)))
    (emit-expr pred)
    (emit "if (!eax.b){goto ~s;}" alt-label)
    (emit-expr conseq)
    (emit "goto ~s" end-label)
    (emit "~s:" alt-label)
    (emit-expr alt)
    (emit "~s:" end-label)
    (emit "")))

;; (emit-program '(let ((a 3))
;;                  (if (eq? a 3)
;;                      (let ((b 10))
;;                        (* a b))
;;                      (- a 1000))))

(define (emit-symbol x)
  (emit "eax = *make_symbol(\"~s\")" (symbol->string x)))

;; (emit-program '(let ((a 100))
;;                  (if (eq? a 0)
;;                      100
;;                      (let ((a 10))
;;                        (if (eq? a 10)
;;                            0
;;                            -1)))))


;; How does one implement closures in C?
;; There's the issue of free variables
;; e.g. (let ((x 3)) ((lambda (a) (+ a x)) 5))
;; x is "free" in this closure

;; Therefore, a closure representation in C must have some sort of
;; environment structure keeping track of free variables and the
;; enclosing environment.

;; This is a bit of a problem with the stack/register model, so it'll
;; require some workarounds.

;; I'll opt for an inefficient way to store environments as cons
;; cells: ((a b c d...) 1 2 3 4 ...) this binds "a" with the value of
;; 1, "b" with 2 etc.



(define (emit-label x)
  (emit-no-colon "~s:" x))

(define (emit-goto x)
  (emit "goto ~s" x))


;; Some sample programs. The compiler works surprisingly well,
;; allowing for more or less natural scheme programs sans the
;; closures.

(define counter-prog
  '(begin 
     (define a 100)
     (label foo)
     (if (eq? a 0)
         (goto end)
         (begin (set! a (- a 1))
                (display a)
                (goto foo)))
     (label end)))

(define collatz-prog
  '(begin
     (define a 27)
     (label foo)
     (display a)
     (if (eq? 1 a)
         (goto end)
         (if (eq? 1 (remainder a 2))
             (begin
               (set! a (+ (* 3 a) 1))
               (goto foo))
             (begin
               (set! a (/ a 2))
               (goto foo))))
     (label end)))


(define reverse-prog
  '(begin
     (define a (quote (a b c d e)))
     (define b (quote ()))
     (label start)
     (if (null? a)
         (goto end)
         (begin (set! b (cons (car a) b))
                (display a)
                (set! a (cdr a))
                (goto start)))
     (label end)
     (display b)))

(define lookup-prog
  '(begin
     (define sym (quote foo))
     (define result (quote ()))
     (define e (quote ((bar baz) (x y) (foo 5))))
     (display sym)
     (display e)
     (label start)
     (if (null? e)
         (goto end)
         (begin
           (let ((frame (car e)))
             (if (eq? (car frame) sym)
                 (begin
                   (set! result (car (cdr frame)))
                   (goto end))
                 (begin
                   (set! e (cdr e))
                   (goto start))))))
     (goto end)
     (display result)
     (exit 0)))

;; Idea: Just compile to an intermediate language without closures
