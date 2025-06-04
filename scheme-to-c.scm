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


(define (sanitize-c-identifier sym)
  (string-join (string-split (symbol->string sym) #\-) "_"))

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
  (emit-no-colon "// -- BEGIN GENERATED C PREAMBLE --")
  (emit-no-colon "#include <stdio.h>")
  (emit-no-colon "#include <stdlib.h>")
  (emit-no-colon "#include <string.h>")
  (emit-no-colon "#include <gc/gc.h>")
  (emit-no-colon "")
  (emit-no-colon "#define MAX_SYMBOL_LEN 32")
  (emit-no-colon "")
  (emit-no-colon "struct reg; // Forward declaration for function pointer in reg union")
  (emit-no-colon "")
  (emit-no-colon "typedef enum type {FIXNUM, CHAR, BOOLEAN, NIL, PAIR, SYMBOL, STRING, CLOSURE, PRIMITIVE_PROC} type;")
  (emit-no-colon "typedef struct reg {")
  (emit-no-colon "  type t;")
  (emit-no-colon "  union {")
  (emit-no-colon "    long long n;")
  (emit-no-colon "    char c;")
  (emit-no-colon "    char *s;")
  (emit-no-colon "    int b;")
  (emit-no-colon "    struct { struct reg *car; struct reg *cdr; }; // PAIR")
  (emit-no-colon "    struct { struct reg *vars; struct reg *body; struct reg *env; }; // CLOSURE")
  (emit-no-colon "    struct reg (*c_primitive_proc)(struct reg args_list); // PRIMITIVE_PROC")
  (emit-no-colon "  };")
  (emit-no-colon "} reg;")
  (emit-no-colon "")
  (emit-no-colon "typedef struct llist { reg curr; struct llist *next; } llist;")
  (emit-no-colon "")
  (emit-no-colon "extern reg eax, ebx;")
  (emit-no-colon "extern llist *stack;")
  (emit-no-colon "extern reg *env;")
  (emit-no-colon "extern int al;")
  (emit-no-colon "")
  (emit-no-colon "// Forward declarations for functions in vm.c")
  (emit-no-colon "void push();")
  (emit-no-colon "void pop();")
  (emit-no-colon "void cmp();")
  (emit-no-colon "reg *car(reg *head);")
  (emit-no-colon "reg *cdr(reg *head);")
  (emit-no-colon "void write_obj(reg r);")
  (emit-no-colon "void display_obj(reg r);")
  (emit-no-colon "reg *cons(reg *a, reg *b);")
  (emit-no-colon "reg *make_symbol(char *name);")
  (emit-no-colon "reg *make_number(long long value);")
  (emit-no-colon "reg *make_boolean(unsigned int value);")
  (emit-no-colon "reg *make_string(char *name);")
  (emit-no-colon "reg* make_closure(reg* params, reg* body_expr, reg* captured_env);")
  (emit-no-colon "reg apply_closure(reg closure, reg args);")
  (emit-no-colon "void initialize_global_env();")
  (emit-no-colon "reg *alloc_reg();")
  (emit-no-colon "void lookup_in_env(reg *env_ptr);")
  (emit-no-colon "// Primitives from vm.c")
  (emit-no-colon "reg primitive_plus(reg args_list_obj);")
  (emit-no-colon "reg primitive_zero_p(reg args_list_obj);")
  (emit-no-colon "reg primitive_multiply(reg args_list_obj);")
  (emit-no-colon "reg primitive_sub1(reg args_list_obj);")
  (emit-no-colon "// -- END GENERATED C PREAMBLE --")
  (emit-no-colon "")
  (emit-no-colon "int main(void)")
  (emit-no-colon "{")
  (emit "initialize_global_env()")
  (emit "GC_INIT()")
  (emit-expr x)
  (emit-no-colon "}"))

(define (emit-display x)
  (emit-expr (car x)) ; Evaluate the expression to be displayed
  (emit "display_obj(eax)") ; Display the value in eax
  (emit "printf(\"\\n\")")
  (emit "fflush(stdout)")) ; Ensure it's written out

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

(define (emit-lambda-expr expr)
  (emit-no-colon "{") ; Start a new scope
  (let ((params (car expr)) ; If expr is ((p1 p2) body), params is (p1 p2)
        (body (cadr expr)))  ; body is body
    (emit "// Compiling LAMBDA with params: ~s body: ~s" params body)

    ; Quote the parameters list and put it in eax
    (emit-expr (list 'quote params))
    (emit "reg lambda_params_val = eax")

    ; Quote the body expression and put it in eax
    (emit-expr (list 'quote body))
    (emit "reg lambda_body_val = eax")

    ; Call make_closure with the quoted params and body, and current 'env'
    ; make_closure expects reg* for params, body, and env.
    ; lambda_params_val and lambda_body_val are reg, so we take their address.
    ; 'env' is already a reg* (global variable in vm.c, or current eval env for interpreter)
    ; For compilation, we use the global 'env' from vm.c.
    (emit "reg* new_closure_ptr = make_closure(&lambda_params_val, &lambda_body_val, env)")
    (emit "eax = *new_closure_ptr") ; Dereference to get the reg value into eax
  )
  (emit-no-colon "}")) ; End the scope

(define (emit-apply expr)
  (emit-no-colon "{") ; Start a new scope
  (let ((proc (car expr))
        (args (cdr expr)))
    (emit-expr proc)          ; Evaluate the procedure, result in eax
    (emit "push()")             ; Save proc on stack

    ; Push an empty list onto the stack to accumulate arguments
    (emit "eax.t = NIL")
    (emit "push()")

    ; Evaluate arguments right-to-left and cons them onto the list
    (for-each
     (lambda (arg-expr)
       (emit-expr arg-expr)    ; Arg value in eax
       (emit "ebx = eax")      ; Move arg value to ebx
       (emit "pop()")          ; Pop current arg list into eax
       ; eax = current list, ebx = new arg value
       (emit "eax = *cons(&ebx, &eax)") ; Cons new arg to front of list (ebx is arg, eax is list)
       (emit "push()"))         ; Push updated arg list back
     (reverse args))         ; Process args right to left

    ; Now the top of the stack has the list of evaluated arguments.
    ; The item below it is the evaluated procedure.

    (emit "pop()") ; Pop evaluated arguments list into eax
    (emit "reg actual_args_val = eax")

    (emit "pop()") ; Pop evaluated procedure into eax
    (emit "reg proc_to_call_val = eax")
    
    ; Call apply_closure
    (emit "eax = apply_closure(proc_to_call_val, actual_args_val)"))
  (emit-no-colon "}")) ; End the scope

(define (emit-expr x)
  (cond ((immediate? x)
         (emit-immediate x))
        ((string? x)
         (emit "eax = *make_string(\"~a\");" x))
        ((atom? x)
         (if (null? x) ; nil is an atom and an immediate
             (emit-immediate x)
             (begin ; Otherwise, it's a variable to be looked up
               (emit "// Variable lookup for scheme symbol: ~a" x)
               (emit "eax = *make_symbol(\"~a\");" (symbol->string x))
               (emit "lookup_in_env(env); // Result of lookup will be in eax"))))
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
         (emit-exit (cadr x)))
        ((eq? (car x) 'lambda)
         (emit-lambda-expr (cdr x)))
        (else ; Default to procedure call if it's a list and not a special form
         (emit-apply x))))

(define (emit-set! x)
  (let ((definition (car x))
        (body (cadr x)))
    (if (symbol? definition)
        (emit-set-var definition body)
        (emit-no-colon "// NOT IMPLEMENTED"))))

(define (emit-set-var var body)
  (emit-expr body)
  (emit "~a = eax" (sanitize-c-identifier var)))

(define (emit-add-var-to-global-vm-env scheme-var-name c-src-reg-name)
  (emit "// Add simple global variable '~a' to VM's global 'env' from C reg ~a" scheme-var-name c-src-reg-name)
  (emit "{")
  (emit (string-append "  reg* var_sym_for_env = make_symbol(\"" scheme-var-name "\");"))
  (emit "  reg* var_val_ptr_for_env = alloc_reg();")
  (emit "  memcpy(var_val_ptr_for_env, &~a, sizeof(reg)); // Store copy of ~a's value" c-src-reg-name c-src-reg-name)
  (emit "  reg* env_current_global_frame = car(env);")
  (emit "  reg* env_old_symbols = car(env_current_global_frame);")
  (emit "  reg* env_old_values = cdr(env_current_global_frame);")
  (emit "  reg* env_new_symbols = cons(var_sym_for_env, env_old_symbols);")
  (emit "  reg* env_new_values = cons(var_val_ptr_for_env, env_old_values);")
  (emit "  reg* new_global_frame = cons(env_new_symbols, env_new_values);")
  (emit "  env = cons(new_global_frame, cdr(env));")
  (emit "}"))

(define (emit-define x)
  (let ((definition (car x))
        (body (cadr x)))
    (if (symbol? definition)
        (emit-define-var (list definition body))
        (if (pair? definition) ; Check if definition is like (f args...)
            (let ((func-name (car definition))
                  (params (cdr definition)))
              (emit-define-var (list func-name (list 'lambda params body))))
            (begin
              (emit-no-colon "// ERROR: Malformed define expression.")
              (emit "// Define expected (define var val) or (define (func params) body). Got:")
              (emit "// ~s" x))))))

(define (emit-define-var expr)
  (let* ((var (car expr))
        (val-expr (cadr expr))
        (c-var-name (sanitize-c-identifier var))
        (scheme-var-name (symbol->string var)))
    (if (and (pair? val-expr) (eq? (car val-expr) 'lambda))
        ;; Handle (define var (lambda (params...) body...)) for potential recursion
        (let* ((lambda-expr val-expr)
               (params (cadr lambda-expr))
               (body-expressions (cddr lambda-expr))
               (actual-body (if (null? (cdr body-expressions))
                                (car body-expressions)
                                (cons 'begin body-expressions))))
          (emit "// Defining potentially recursive function ~a as ~s" var actual-body)
          (emit "reg* ~a_storage = alloc_reg(); // Storage for the closure reg struct itself" c-var-name)
          (emit "reg ~a_c_var;" c-var-name)

          (emit "// Prepare lambda parts (params and body) by quoting them")
          ;; Use unique names for these temp C regs to avoid clashes if defines were ever nested
          (emit "reg quoted_params_val_for_~a;" c-var-name)
          (emit "reg quoted_body_val_for_~a;" c-var-name)
          
          (emit-expr (list 'quote params))
          (emit "quoted_params_val_for_~a = eax;" c-var-name)
          
          (emit-expr (list 'quote actual-body))
          (emit "quoted_body_val_for_~a = eax;" c-var-name)
          
          (emit "// 1. Create a temporary closure that captures the CURRENT global 'env'")
          (emit "//    (This env does not yet contain the self-reference for ~a)" scheme-var-name)
          (emit "reg* temp_closure_ptr_for_~a = make_closure(&quoted_params_val_for_~a, &quoted_body_val_for_~a, env);" 
                c-var-name c-var-name c-var-name)
          
          (emit "// 2. Copy this temporary closure into our dedicated storage '~a_storage'" c-var-name)
          (emit "memcpy(~a_storage, temp_closure_ptr_for_~a, sizeof(reg));" c-var-name c-var-name)
          
          (emit "// 3. Add this closure (now in *~a_storage) to the global environment under its name '~a'." c-var-name scheme-var-name)
          (emit "//    The helper emit-add-var-to-global-vm-env will update the global 'env' variable.")
          (emit "eax = *~a_storage; // Load the closure struct value into eax for the helper" c-var-name)
          (emit-add-var-to-global-vm-env scheme-var-name "eax") 
          
          (emit "// 4. CRITICAL STEP: Update the .env field of the closure in ~a_storage " c-var-name)
          (emit "//    to point to the NEW global 'env' (which now includes the self-reference for ~a)." scheme-var-name)
          (emit "~a_storage->env = env;" c-var-name)
          
          (emit "// Make the C host variable ~a_c_var hold the final closure value from storage (optional, for inspection)" c-var-name)
          (emit "~a_c_var = *~a_storage;" c-var-name c-var-name)
          (emit "// End defining ~a" var))
        ;; Not a lambda, simple variable definition
        (begin
          (emit "// Defining simple global variable ~a" var)
          (emit-expr val-expr) ; Value is now in eax
          (emit "reg ~a_c_var = eax; // C host variable (optional)" c-var-name)
          (emit-add-var-to-global-vm-env scheme-var-name "eax") ; Call the new helper
          (emit "// End defining simple global ~a" var))
        ) ; Closes 'begin' (false branch of if)
    ) ; Closes 'if'
  ) ; Closes 'let' (main body of emit-define-var)

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
  (cond ((null? x) (emit "eax.t = NIL"))
        ((string? x) (emit "eax = *make_string(\"~a\");" x))
        ((symbol? x) (emit "eax = *make_symbol(\"~a\");" x))
        ((pair? x) (emit-quoted-list x))
        ((immediate? x) (emit-immediate x))
        (else (error 'emit-quote "Attempted to quote an unsupported type" x))))

(define (emit-let x)
  (let ((varlist (car x))
        (body (cadr x)))
    (emit-no-colon "{")
    (for-each (lambda (x) (emit-define-var x)) varlist)
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
