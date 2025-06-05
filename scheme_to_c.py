import sys
import re
import io

class SchemeToC:
    def __init__(self):
        self.compile_port = sys.stdout
        self.gensym_count = 0
        self.anf_gensym_count = 0

    def set_compile_port(self, p):
        if not hasattr(p, 'write'):
            raise ValueError(f"Not an output port {p}")
        self.compile_port = p

    def emit(self, fmt_str, *f_args):
        s = fmt_str.format(*f_args)
        self.compile_port.write(s)
        self.compile_port.write(";")
        self.compile_port.write("\n")

    def emit_no_colon(self, fmt_str, *f_args):
        s = fmt_str.format(*f_args)
        self.compile_port.write(s)
        self.compile_port.write("\n")

    def emit_no_newline(self, fmt_str, *f_args):
        s = fmt_str.format(*f_args)
        self.compile_port.write(s)
        
    def sanitize_c_identifier(self, sym_name):
        return sym_name.replace('-', '_')

    def is_null(self, x):
        return x == []

    def is_immediate(self, x):
        return isinstance(x, int) or isinstance(x, bool) or self.is_null(x)

    def emit_immediate(self, x):
        if isinstance(x, int):
            self.emit("eax.t = FIXNUM")
            self.emit("eax.n = {}", x)
        elif isinstance(x, bool):
            self.emit("eax.t = BOOLEAN")
            if x:
                self.emit("eax.b = 1")
            else:
                self.emit("eax.b = 0")
        elif self.is_null(x):
            self.emit("eax.t = NIL")
            
    def gensym(self):
        self.gensym_count += 1
        return f"label{self.gensym_count}"

    def anf_gensym(self):
        self.anf_gensym_count += 1
        return f"anf_tmp_{self.anf_gensym_count}"

    def emit_program(self, x_expr):
        self.emit_no_colon("// -- BEGIN GENERATED C PREAMBLE --")
        self.emit_no_colon("#include <stdio.h>")
        self.emit_no_colon("#include <stdlib.h>")
        self.emit_no_colon("#include <string.h>")
        self.emit_no_colon("#include <gc/gc.h>")
        self.emit_no_colon("")
        self.emit_no_colon("#define MAX_SYMBOL_LEN 32")
        self.emit_no_colon("")
        self.emit_no_colon("struct reg; // Forward declaration for function pointer in reg union")
        self.emit_no_colon("")
        self.emit_no_colon("typedef enum type {{FIXNUM, CHAR, BOOLEAN, NIL, PAIR, SYMBOL, STRING, CLOSURE, PRIMITIVE_PROC}} type;")
        self.emit_no_colon("typedef struct reg {{")
        self.emit_no_colon("  type t;")
        self.emit_no_colon("  union {{")
        self.emit_no_colon("    long long n;")
        self.emit_no_colon("    char c;")
        self.emit_no_colon("    char *s;")
        self.emit_no_colon("    int b;")
        self.emit_no_colon("    struct {{ struct reg *car; struct reg *cdr; }}; // PAIR")
        self.emit_no_colon("    struct {{ struct reg *vars; struct reg *body; struct reg *env; }}; // CLOSURE")
        self.emit_no_colon("    struct reg (*c_primitive_proc)(struct reg args_list); // PRIMITIVE_PROC")
        self.emit_no_colon("  }};")
        self.emit_no_colon("}} reg;")
        self.emit_no_colon("")
        self.emit_no_colon("extern reg eax, ebx;")
        self.emit_no_colon("extern reg *env;")
        self.emit_no_colon("extern int al;")
        self.emit_no_colon("")
        self.emit_no_colon("// Forward declarations for functions in vm.c")
        self.emit_no_colon("int reg_equal(reg a, reg b);")
        self.emit_no_colon("reg *car(reg *head);")
        self.emit_no_colon("reg *cdr(reg *head);")
        self.emit_no_colon("void write_obj(reg r);")
        self.emit_no_colon("void display_obj(reg r);")
        self.emit_no_colon("reg *cons(reg *a, reg *b);")
        self.emit_no_colon("reg *make_symbol(char *name);")
        self.emit_no_colon("reg *make_number(long long value);")
        self.emit_no_colon("reg *make_boolean(unsigned int value);")
        self.emit_no_colon("reg *make_string(char *name);")
        self.emit_no_colon("reg* make_closure(reg* params, reg* body_expr, reg* captured_env);")
        self.emit_no_colon("reg apply_closure(reg closure, reg args);")
        self.emit_no_colon("void initialize_global_env();")
        self.emit_no_colon("reg *alloc_reg();")
        self.emit_no_colon("void lookup_in_env(reg *env_ptr);")
        self.emit_no_colon("// Primitives from vm.c")
        self.emit_no_colon("reg primitive_plus(reg args_list_obj);")
        self.emit_no_colon("reg primitive_zero_p(reg args_list_obj);")
        self.emit_no_colon("reg primitive_multiply(reg args_list_obj);")
        self.emit_no_colon("reg primitive_sub1(reg args_list_obj);")
        self.emit_no_colon("// -- END GENERATED C PREAMBLE --")
        self.emit_no_colon("")
        self.emit_no_colon("int main(void)")
        self.emit_no_colon("{{")
        self.emit("initialize_global_env()")
        self.emit("GC_INIT()")
        self.emit_expr(x_expr)
        self.emit_no_colon("}}")

    def emit_expr(self, x):
        if self.is_immediate(x):
            self.emit_immediate(x)
        elif isinstance(x, str): # This is a Scheme symbol/variable an Python string
            self.emit("// Variable lookup for scheme symbol: {}", x)
            self.emit("eax = *make_symbol(\"{}\");", x)
            self.emit("lookup_in_env(env); // Result of lookup will be in eax")
        elif isinstance(x, list) and x: 
            op = x[0]
            args = x[1:]
            if op == 'quote':
                self.emit_quote(args[0])
            elif op == 'if':
                self.emit_if(args)
            elif op == 'begin':
                self.emit_begin(args)
            elif op == 'define':
                self.emit_define(args)
            elif op == 'let':
                self.emit_let(args)
            elif op == 'lambda':
                self.emit_lambda_expr(args)
            elif op == 'set!':
                self.emit_set_bang(args)
            elif op == 'display':
                self.emit_display(args)
            elif op == 'cons':
                self.emit_cons(args)
            elif op == 'car':
                self.emit_car(args)
            elif op == 'cdr':
                self.emit_cdr(args)
            elif op == 'add1': self.emit_unary_op_generic(args[0], "eax.n++")
            elif op == 'sub1': self.emit_unary_op_generic(args[0], "eax.n--")
            elif op == 'not': self.emit_unary_op_generic(args[0], "eax.b = (eax.b != 1)")
            elif op == 'zero?': self.emit_unary_pred_generic(args[0], "eax.n == 0")
            elif op == 'fixnum?': self.emit_unary_pred_generic(args[0], "eax.t == FIXNUM")
            elif op == 'char?': self.emit_unary_pred_generic(args[0], "eax.t == CHAR")
            elif op == 'boolean?': self.emit_unary_pred_generic(args[0], "eax.t == BOOLEAN")
            elif op == 'null?': self.emit_unary_pred_generic(args[0], "eax.t == NIL")
            elif op == 'eq?': self.emit_eq(args[0], args[1])
            elif op == '+': self.emit_binary_op_direct(args[0], args[1], '+')
            elif op == '-': self.emit_binary_op_direct(args[0], args[1], '-')
            elif op == '*': self.emit_binary_op_direct(args[0], args[1], '*')
            elif op == '/': self.emit_binary_op_direct(args[0], args[1], '/')
            elif op == 'remainder': self.emit_binary_op_direct(args[0], args[1], '%')
            elif op == 'label': self.emit_label(args[0])
            elif op == 'goto': self.emit_goto(args[0])
            elif op == 'exit': self.emit_exit(args[0])
            elif op == 'cond': self.emit_cond(args)
            else: 
                self.emit_apply(x)
        elif self.is_null(x): 
            self.emit_immediate(x)
        else:
            self.emit("// UNHANDLED EXPRESSION TYPE in emit_expr: {}", repr(x))

    def emit_unary_op_generic(self, arg_expr, c_operation):
        self.emit_expr(arg_expr)
        self.emit(c_operation)

    def emit_unary_pred_generic(self, arg_expr, c_condition):
        self.emit_expr(arg_expr)
        self.emit("al = ({})", c_condition)
        self.emit("eax.t = BOOLEAN")
        self.emit("eax.b = al")

    def emit_binary_op_generic(self, arg1_expr, arg2_expr, c_operation_fmt):
        """Emit code for a binary operation using a temporary variable."""
        temp = f"tmp_{self.gensym()}"
        self.emit_expr(arg1_expr)
        self.emit(f"reg {temp} = eax")
        self.emit_expr(arg2_expr)
        self.emit(c_operation_fmt.format(temp=temp))

    def emit_binary_op_direct(self, arg1_expr, arg2_expr, op_symbol):
        """Emit code for a binary arithmetic operation without using the VM stack."""
        temp = f"tmp_{self.gensym()}"
        self.emit_expr(arg1_expr)
        self.emit(f"reg {temp} = eax")
        self.emit_expr(arg2_expr)
        self.emit(f"eax.n = {temp}.n {op_symbol} eax.n")
        self.emit("eax.t = FIXNUM")

    def emit_eq(self, arg1_expr, arg2_expr):
        temp = f"tmp_{self.gensym()}"
        self.emit_expr(arg1_expr)
        self.emit(f"reg {temp} = eax")
        self.emit_expr(arg2_expr)
        self.emit(f"al = reg_equal({temp}, eax)")
        self.emit("eax.t = BOOLEAN")
        self.emit("eax.b = al")

    def emit_display(self, args_list):
        expr_to_display = args_list[0]
        self.emit_expr(expr_to_display)
        self.emit("display_obj(eax)")
        self.emit("printf(\"{}\")", "\\n")
        self.emit("fflush(stdout)")
        
    def emit_if(self, args_list):
        pred, conseq, alt = args_list[0], args_list[1], args_list[2]
        alt_label = self.gensym()
        end_label = self.gensym()
        
        self.emit_expr(pred)
        self.emit("if (!eax.b){{goto {};}}", alt_label)
        self.emit_expr(conseq)
        self.emit("goto {}", end_label)
        self.emit_no_colon("{}:", alt_label)
        self.emit_expr(alt)
        self.emit_no_colon("{}:", end_label)
        self.emit("")

    def emit_begin(self, expressions_list):
        for expr in expressions_list:
            self.emit_expr(expr)
            
    def emit_add_var_to_global_vm_env(self, scheme_var_name, c_src_var_name_or_reg):
        self.emit("// Add simple global variable '{}' to VM's global 'env' from C var/reg {}", scheme_var_name, c_src_var_name_or_reg)
        self.emit_no_colon("{{")
        self.emit("  reg* var_sym_for_env = make_symbol(\"{}\");", scheme_var_name)
        self.emit("  reg* var_val_ptr_for_env = alloc_reg();")
        self.emit("  memcpy(var_val_ptr_for_env, &{}, sizeof(reg)); // Store copy of {}'s value", c_src_var_name_or_reg, c_src_var_name_or_reg)
        self.emit("  reg* env_current_global_frame = car(env);")
        self.emit("  reg* env_old_symbols = car(env_current_global_frame);")
        self.emit("  reg* env_old_values = cdr(env_current_global_frame);")
        self.emit("  reg* env_new_symbols = cons(var_sym_for_env, env_old_symbols);")
        self.emit("  reg* env_new_values = cons(var_val_ptr_for_env, env_old_values);")
        self.emit("  reg* new_global_frame = cons(env_new_symbols, env_new_values);")
        self.emit("  env = cons(new_global_frame, cdr(env));")
        self.emit_no_colon("}}")

    def emit_define(self, args_list):
        definition = args_list[0]
        body_expr = args_list[1]

        if isinstance(definition, str):
            self.emit_define_var([definition, body_expr])
        elif isinstance(definition, list) and definition:
            func_name = definition[0]
            params = definition[1:]
            lambda_expr = ['lambda', params, body_expr]
            self.emit_define_var([func_name, lambda_expr])
        else:
            self.emit_no_colon("// ERROR: Malformed define expression.")
            self.emit("// Define expected (define var val) or (define (func params) body). Got:")
            self.emit("// {}", repr(args_list))

    def emit_define_var(self, expr_parts):
        var_name_str = expr_parts[0]
        val_expr = expr_parts[1]
        
        c_var_name = self.sanitize_c_identifier(var_name_str)
        scheme_var_name = var_name_str

        is_lambda_def = isinstance(val_expr, list) and val_expr and val_expr[0] == 'lambda'

        if is_lambda_def:
            lambda_params = val_expr[1]
            body_parts = val_expr[2:]
            actual_body = body_parts[0] if len(body_parts) == 1 else ['begin'] + body_parts

            self.emit("// Defining potentially recursive function {} as {}", var_name_str, repr(actual_body))
            # Storage for the C variable that will hold the closure reg struct.
            # This is distinct from _storage which holds the reg struct itself if allocated separately.
            self.emit("reg {}_c_var; // C host variable for the closure", c_var_name)
            # Storage for the closure reg struct itself, to allow self-reference.
            self.emit("reg* {}_storage = alloc_reg();", c_var_name)


            self.emit("reg quoted_params_val_for_{};", c_var_name)
            self.emit("reg quoted_body_val_for_{};", c_var_name)
            
            self.emit_expr(['quote', lambda_params])
            self.emit("quoted_params_val_for_{} = eax;", c_var_name)
            
            self.emit_expr(['quote', actual_body])
            self.emit("quoted_body_val_for_{} = eax;", c_var_name)
            
            self.emit("// 1. Create a temporary closure that captures the CURRENT global 'env'")
            self.emit("reg* temp_closure_ptr_for_{} = make_closure(&quoted_params_val_for_{}, &quoted_body_val_for_{}, env);", 
                      c_var_name, c_var_name, c_var_name)
            
            self.emit("// 2. Copy this temporary closure into our dedicated storage '{}_storage'", c_var_name)
            self.emit("memcpy({}_storage, temp_closure_ptr_for_{}, sizeof(reg));", c_var_name, c_var_name)
            
            self.emit("// 3. Add this closure (now in *{}_storage) to the global environment under its name '{}'.", c_var_name, scheme_var_name)
            self.emit("{}_c_var = *{}_storage; // Load the closure struct value into the C host var", c_var_name, c_var_name)
            self.emit_add_var_to_global_vm_env(scheme_var_name, "{}_c_var".format(c_var_name)) # Add the C host var (by value) to env
            
            self.emit("// 4. CRITICAL STEP: Update the .env field of the closure in {}_storage ", c_var_name)
            self.emit("//    to point to the NEW global 'env' (which now includes the self-reference for {}).", scheme_var_name)
            self.emit("{}_storage->env = env;", c_var_name)

            self.emit("// Ensure the C host variable also reflects this updated env if its struct was copied earlier (optional but good practice).")
            self.emit("{}_c_var.env = env; // Update env in the C host var too, if it's a copy", c_var_name)

            self.emit("// End defining {}", var_name_str)
        else: 
            # This is the simple `(define var val)` case from the original scheme
            self.emit("// Defining simple global variable {}", var_name_str)
            self.emit_expr(val_expr) # Value is now in eax
            # Original scheme: (emit "reg ~a_c_var = eax;" c-var-name)
            #                  (emit-add-var-to-global-vm-env scheme-var-name "eax")
            # This means a C variable var_c_var is created and initialized from eax,
            # but "eax" (containing the same value) is passed to add_to_env.
            # So env gets a copy of the value, not a reference to var_c_var.
            # This means a C variable var_c_var is created and initialized from eax,
            # but "eax" (containing the same value) is passed to add_to_env.
            # So env gets a copy of the value, not a reference to var_c_var.
            self.emit("reg {}_c_var = eax; // C host variable (optional, for inspection)", c_var_name)
            self.emit_add_var_to_global_vm_env(scheme_var_name, "eax") 
            self.emit("// End defining simple global {}", var_name_str)

    def emit_let(self, args_list):
        var_bindings = args_list[0]
        body_expr = args_list[1]

        self.emit_no_colon("{{ // Start LET scope")
        # Original 'let' binds globally within a C block, not true lexical scope. Replicating.
        for var_name_str, val_expr in var_bindings:
            self.emit_define_var([var_name_str, val_expr])
        
        self.emit_expr(body_expr)
        self.emit_no_colon("}} // End LET scope")

    def emit_quote(self, x):
        if self.is_null(x):
            self.emit("eax.t = NIL")
        elif isinstance(x, str) and not any(c in x for c in ['(', ')', ' ', '\'', '#']): # Heuristic: is it a simple symbol name?
            # This path is for symbols:
            escaped_x_for_symbol = x.replace('\\', '\\\\').replace('"', '\\"') 
            self.emit("eax = *make_symbol(\"{}\");", escaped_x_for_symbol)
        elif isinstance(x, str): # Assumed to be a symbol name
            escaped_str = x.replace('\\', '\\\\').replace('"', '\\"') 
            self.emit("eax = *make_symbol(\"{}\");", escaped_str)
        elif isinstance(x, list) and x: 
            self.emit_quoted_list(x)
        elif self.is_immediate(x) and not self.is_null(x):
            self.emit_immediate(x)
        else:
            # This means (quote "string") is not handled correctly if string itself is passed as str.
            self.emit_no_colon("// ERROR: emit-quote cannot quote: {}", repr(x))


    def emit_quoted_list(self, lst):
        if not lst:
            self.emit("eax.t = NIL") # Should be caught by is_null in emit_quote
            return

        head_tmp = f"tmp_{self.gensym()}"
        self.emit_quote(lst[0])
        self.emit(f"reg {head_tmp} = eax")
        self.emit_quote(lst[1:])
        self.emit("ebx = eax")
        self.emit(f"eax = *cons(&{head_tmp}, &ebx)")


    def emit_lambda_expr(self, args_list):
        params_list = args_list[0]
        body_parts = args_list[1:]
        actual_body_expr = body_parts[0] if len(body_parts) == 1 else ['begin'] + body_parts

        self.emit_no_colon("{{ // Start LAMBDA scope")
        self.emit("// Compiling LAMBDA with params: {} body: {}", repr(params_list), repr(actual_body_expr))

        self.emit_expr(['quote', params_list])
        self.emit("reg lambda_params_val = eax")

        self.emit_expr(['quote', actual_body_expr])
        self.emit("reg lambda_body_val = eax")

        self.emit("reg* new_closure_ptr = make_closure(&lambda_params_val, &lambda_body_val, env)")
        self.emit("eax = *new_closure_ptr")
        self.emit_no_colon("}} // End LAMBDA scope")

    def emit_apply(self, expr_list): 
        self.emit_no_colon("{{ // Start APPLY scope")
        proc_expr = expr_list[0]
        arg_exprs = expr_list[1:]

        self.emit_expr(proc_expr)
        proc_tmp = f"tmp_{self.gensym()}"
        self.emit(f"reg {proc_tmp} = eax")

        args_tmp = f"tmp_{self.gensym()}"
        self.emit("eax.t = NIL")
        self.emit(f"reg {args_tmp} = eax")

        for arg_expr in reversed(arg_exprs):
            self.emit_expr(arg_expr)
            self.emit("ebx = eax")
            self.emit(f"{args_tmp} = *cons(&ebx, &{args_tmp})")

        self.emit(f"eax = apply_closure({proc_tmp}, {args_tmp})")
        self.emit_no_colon("}} // End APPLY scope")

    def emit_set_bang(self, args_list): 
        var_name_str = args_list[0]
        body_expr = args_list[1]

        if isinstance(var_name_str, str): 
            self.emit_expr(body_expr) 
            # Following original scheme's emit-set-var, which is likely problematic:
            # It assumes a C variable (var_name_str) exists and sets it.
            # This doesn't update the Scheme 'env' if 'env' holds copies.
            c_target_var = self.sanitize_c_identifier(var_name_str)
            self.emit("{} = eax", c_target_var)
            self.emit("// WARNING: set! implementation follows original, may not update Scheme environment correctly.")
        else:
            self.emit_no_colon("// SET! target is not a symbol: {}", var_name_str)

    def emit_cons(self, args_list):
        self.emit_expr(args_list[0])
        car_tmp = f"tmp_{self.gensym()}"
        self.emit(f"reg {car_tmp} = eax")
        self.emit_expr(args_list[1])
        self.emit("ebx = eax")
        self.emit(f"eax = *cons(&{car_tmp}, &ebx)")

    def emit_car(self, args_list): 
        self.emit_expr(args_list[0])
        self.emit("eax = *car(&eax)")

    def emit_cdr(self, args_list): 
        self.emit_expr(args_list[0])
        self.emit("eax = *cdr(&eax)")

    def emit_cond(self, clauses_list):
        end_label = self.gensym()
        
        for i, clause in enumerate(clauses_list):
            if len(clause) < 1: # Clause must have at least a predicate
                self.emit_no_colon("// ERROR: cond clause empty: {}", repr(clause))
                continue

            pred_expr = clause[0]
            
            if len(clause) == 1: # (pred) case, value of pred is result if true
                 # Scheme standard: if only (predicate), result of predicate is returned.
                 # This compiler's (emit-cond (lambda (x) (pred (car x)) (body (cadr x))))
                 # implies it expects (pred body ...).
                 # For (pred) only, (cadr x) would be error or nil.
                 # Let's assume clauses are (pred body_expr). If (pred) alone, body is pred.
                 # This is not standard. Standard is (pred => proc) or (pred exp1 exp2...)
                 # or (else exp1 exp2...).
                 # The original `(body (cadr x))` implies at least two elements.
                 self.emit_no_colon("// WARNING: cond clause with only predicate not fully standard: {}", repr(clause))
                 body_expr = pred_expr # Non-standard, but to avoid error with (cadr x) logic.
            else:
                body_expr = clause[1] # Takes only the first expression after predicate.


            next_clause_label = self.gensym()

            self.emit_expr(pred_expr)
            self.emit("if (!eax.b){{goto {};}}", next_clause_label)
            self.emit_expr(body_expr)
            self.emit("goto {}", end_label)
            self.emit_no_colon("{}:", next_clause_label)
        
        self.emit_no_colon("{}:", end_label)

    def emit_label(self, label_name_str):
        self.emit_no_colon("{}:", label_name_str)

    def emit_goto(self, label_name_str):
        self.emit("goto {}", label_name_str)

    def emit_exit(self, arg_val):
        if isinstance(arg_val, int):
            self.emit("exit({})", arg_val)
        else:
            # Original was (emit "exit(~a)" x). ~a on a symbol prints symbol name.
            # This means `exit(foo)` not `exit(valueOfFoo)`. For safety, only int.
            self.emit("// emit-exit expects an integer literal argument. Got: {}", repr(arg_val))
            self.emit_expr(arg_val) # Evaluate if it's an expr
            self.emit("exit(eax.n) // Attempting to exit with evaluated expr")

    # --- ANF Transformation ---
    def _anf_atomic(self, expr):
        return not isinstance(expr, list) or expr == [] or (isinstance(expr, list) and expr and expr[0] == 'quote')

    def anf_transform(self, expr):
        self.anf_gensym_count = 0
        return self._anf(expr)

    def _anf(self, expr):
        if self._anf_atomic(expr):
            return expr

        if not isinstance(expr, list):
            return expr

        op = expr[0]

        if op == 'begin':
            return ['begin'] + [self._anf(e) for e in expr[1:]]
        elif op == 'if':
            return ['if', self._anf(expr[1]), self._anf(expr[2]), self._anf(expr[3])]
        elif op == 'lambda':
            params = expr[1]
            body = [self._anf(e) for e in expr[2:]]
            return ['lambda', params] + body
        elif op == 'let':
            bindings = [[var, self._anf(val)] for var, val in expr[1]]
            return ['let', bindings, self._anf(expr[2])]
        elif op == 'define':
            definition = expr[1]
            body = self._anf(expr[2])
            return ['define', definition, body]
        elif op == 'set!':
            return ['set!', expr[1], self._anf(expr[2])]
        elif op == 'cond':
            clauses = []
            for clause in expr[1:]:
                if len(clause) == 1:
                    clauses.append([self._anf(clause[0])])
                else:
                    clauses.append([self._anf(clause[0]), self._anf(clause[1])])
            return ['cond'] + clauses
        else:
            args = expr[1:]
            new_args = []
            bindings = []
            for arg in args:
                arg_t = self._anf(arg)
                if self._anf_atomic(arg_t):
                    new_args.append(arg_t)
                else:
                    tmp = self.anf_gensym()
                    bindings.append([tmp, arg_t])
                    new_args.append(tmp)
            core = [op] + new_args
            if not bindings:
                return core
            else:
                return ['let', bindings, core]


    # --- Parser specific methods ---
    def _preprocess_scheme_remove_comments(self, text):
        lines = text.splitlines()
        processed_lines = []
        for line in lines:
            line = line.split(';', 1)[0] 
            processed_lines.append(line)
        return "\n".join(processed_lines)

    def _tokenize_sexp(self, s):
        s = s.replace('(', ' ( ').replace(')', ' ) ').replace("'", " ' ")
        return [token for token in s.split() if token] # Filter out empty strings

    def _parse_sexp_from_tokens(self, tokens): # tokens is a list
        if not tokens:
            raise SyntaxError('unexpected EOF in _parse_sexp_from_tokens')
        token = tokens.pop(0)
        if token == "'":
            if not tokens: raise SyntaxError("EOF after quote")
            return ['quote', self._parse_sexp_from_tokens(tokens)]
        if token == '(':
            L = []
            if not tokens: raise SyntaxError("EOF after open paren")
            while tokens[0] != ')':
                L.append(self._parse_sexp_from_tokens(tokens))
                if not tokens: raise SyntaxError("EOF in list, missing ')'")
            tokens.pop(0)  # pop off ')'
            return L
        elif token == ')':
            raise SyntaxError('unexpected )')
        else: # Atom
            try: return int(token)
            except ValueError:
                if token == '#t': return True
                if token == '#f': return False
                # String literals "foo" are returned as Python string "foo" (content)
                if token.startswith('"') and token.endswith('"') and len(token) >=2:
                    return token[1:-1].replace('\\"', '"').replace('\\n','\n').replace('\\t','\t') # Handle basic escapes
                # Handle '() as nil/empty list
                if token == 'nil': # Guile's `read` may not produce 'nil' often for '()
                    return []
                return token # Symbol as string

    def parse_scheme_file(self, file_path):
        try:
            with open(file_path, 'r') as f:
                raw_scheme_code = f.read()
        except FileNotFoundError:
            sys.stderr.write(f"Error: Input file '{file_path}' not found.\n")
            sys.exit(1)
            
        scheme_code_no_comments = self._preprocess_scheme_remove_comments(raw_scheme_code)
        
        # Handle empty or comment-only file
        if not scheme_code_no_comments.strip():
            # Interpret as '() or an empty program representation
            # Depending on how `emit_program` handles `[]` (e.g. `nil`)
            # For now, let's say it's an error or needs specific handling.
            # Original compiler reads one S-expression. If file is empty after comments, (read) might yield EOF.
            # Let's return something `emit_program` can handle, like `nil`.
            # However, (emit-expr '()) is fine.
            # The problem is if the token list is empty for _parse_sexp_from_tokens.
            sys.stderr.write(f"Warning: File '{file_path}' is empty or contains only comments.\n")
            return [] # Represent as nil for emit_program

        tokens = self._tokenize_sexp(scheme_code_no_comments)
        
        if not tokens: # Also handles case where file had only whitespace after comments
            sys.stderr.write(f"Warning: File '{file_path}' has no parsable tokens after preprocessing.\n")
            return []


        # Scheme's `(read)` typically reads one expression.
        # If multiple top-level expressions, they should be in a (begin ...)
        # For simplicity, our parser will try to parse one full S-expression.
        # If there's trailing stuff, it's an error.
        try:
            parsed_expr = self._parse_sexp_from_tokens(tokens)
            if tokens: # If tokens remain, it means there was more than one top-level S-expression without a `begin`
                sys.stderr.write(f"Warning: Extra tokens found after parsing main S-expression in '{file_path}': {tokens}\n")
                # Optionally, could wrap in a 'begin' or error out. For now, proceed with first parsed.
        except Exception as e:
            sys.stderr.write(f"Error parsing Scheme file '{file_path}': {e}\n")
            sys.stderr.write(f"Problematic tokens might be around: {tokens[:10]}\n") # Show some context
            sys.exit(1) # Critical error, stop compilation
            
        return parsed_expr

# Make class methods for parser helpers static if they don't use self, or keep as is.
# For now, they are instance methods.

if __name__ == '__main__':
    if len(sys.argv) < 2 : # Changed to allow just input, output to stdout
        print("Usage: python scheme_to_c.py <input_scheme_file> [output_c_file]", file=sys.stderr)
        sys.exit(1)

    input_scm_file = sys.argv[1]
    compiler = SchemeToC()
    parsed_program = compiler.parse_scheme_file(input_scm_file)
    anf_program = compiler.anf_transform(parsed_program)

    if len(sys.argv) >= 3:
        output_c_file = sys.argv[2]
        try:
            with open(output_c_file, 'w') as f_out:
                compiler.set_compile_port(f_out)
                compiler.emit_program(anf_program)
            print(f"Python compiler: Generated {output_c_file} from {input_scm_file}", file=sys.stderr)
        except IOError:
            sys.stderr.write(f"Error: Could not write to output file '{output_c_file}'.\n")
            sys.exit(1)
    else:  # Output to stdout if no output file specified
        compiler.set_compile_port(sys.stdout)
        compiler.emit_program(anf_program)
