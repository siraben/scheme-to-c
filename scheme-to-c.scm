(load "tests-driver.scm")

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

(emit-program
 '(cond ((eq? (cond ((eq? 8 (* 4 2))
                     (let ((a 3) (b 1000))
                       (* a b)))) 3000) 9999) ((eq? #t #t) -1)))

(define (emit-cmp x y)
  (emit-exp x)
  (emit "push(eax)")
  (emit-exp y)
  (emit "ebx = pop()")
  (emit "cmp = compare(pop(), eax)"))

(define (emit-program x)
  ;; Need to paste in everything before main() here
  ;; (display "#include <stdio.h>\n")
  ;; (display "#include <stdlib.h>\n")
  ;; ;; Global variables
  ;; (emit "typedef enum type {FIXNUM, CHAR, BOOLEAN} type")
  
  ;; (emit "typedef struct reg {type t;union {long long n; char c; int b;}")
  ;; (emit "} reg")
  ;; (emit "static int cmp")
  ;; (emit "static reg eax, ebx")
  (emit-no-colon "int main(int argc, char const *argv[])")
  (emit-no-colon "{")
  (emit-expr x)
  (emit "printf(\"Value of eax: \")")
  (emit "print(eax)")
  (emit "puts(\"\")")
  (emit-no-colon "}"))

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
         (emit-char? (cdr x)))
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
         (emit-cond (cdr x)))))


(define (emit-define x)
  (let ((definition (car x))
        (body (caddr x)))
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
      (if (symbol? x)
          (emit "eax = *make_symbol(\"~s\")" x)
          (emit-quoted-list x))))

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
