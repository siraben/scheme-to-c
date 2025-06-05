import sys
from typing import Any, Dict, Iterable, List, Optional, TextIO

class SchemeToC:
    def __init__(self) -> None:
        self.compile_port: TextIO = sys.stdout
        self.gensym_count: int = 0
        self.nesting_level: int = 0
        self.name_map: Dict[str, str] = {}

    def set_compile_port(self, p: TextIO) -> None:
        if not hasattr(p, 'write'):
            raise ValueError(f"Not an output port {p}")
        self.compile_port = p

    def emit(self, fmt_str: str, *f_args: Any) -> None:
        s = fmt_str.format(*f_args)
        self.compile_port.write(s)
        self.compile_port.write(";")
        self.compile_port.write("\n")

    def emit_no_colon(self, fmt_str: str, *f_args: Any) -> None:
        s = fmt_str.format(*f_args)
        self.compile_port.write(s)
        self.compile_port.write("\n")

    def emit_no_newline(self, fmt_str: str, *f_args: Any) -> None:
        s = fmt_str.format(*f_args)
        self.compile_port.write(s)
        

    def get_safe_name(self, sym_name: str) -> str:
        if sym_name not in self.name_map:
            self.name_map[sym_name] = f"v_{len(self.name_map)}"
        return self.name_map[sym_name]

    def is_null(self, x: Any) -> bool:
        return x == []

    def is_immediate(self, x: Any) -> bool:
        return (
            isinstance(x, bool)
            or isinstance(x, int)
            or self.is_null(x)
            or (isinstance(x, tuple) and len(x) == 2 and x[0] == 'string')
        )

    def emit_immediate(self, x: Any) -> None:
        if isinstance(x, bool):
            self.emit("eax.t = BOOLEAN")
            if x:
                self.emit("eax.b = 1")
            else:
                self.emit("eax.b = 0")
        elif isinstance(x, int):
            self.emit("eax.t = FIXNUM")
            self.emit("eax.n = {}", x)
        elif self.is_null(x):
            self.emit("eax.t = NIL")
        elif isinstance(x, tuple) and len(x) == 2 and x[0] == 'string':
            escaped = x[1].replace('\\', '\\\\').replace('"', '\\"')
            self.emit("eax = *make_string(\"{}\");", escaped)
            
    def gensym(self) -> str:
        self.gensym_count += 1
        return f"label{self.gensym_count}"

    def _transform_internal_defines(self, body_parts: List[Any]) -> List[Any]:
        defines: List[Any] = []
        rest: List[Any] = []
        collecting_defines = True
        for expr in body_parts:
            if collecting_defines and isinstance(expr, list) and expr and expr[0] == 'define':
                defines.append(expr)
            else:
                collecting_defines = False
                rest.append(expr)

        if not defines:
            return body_parts

        bindings: List[List[Any]] = []
        for d in defines:
            if not isinstance(d, list) or len(d) < 2:
                continue
            definition = d[1]
            value_parts = d[2:]
            if isinstance(definition, list) and definition:
                name = definition[0]
                params = definition[1:]
                val_expr = ['lambda', params, *value_parts]
            else:
                name = definition
                val_expr = value_parts[0] if len(value_parts) == 1 else ['begin'] + value_parts
            bindings.append([name, val_expr])

        let_bindings = bindings
        expr = ['let', let_bindings, *rest]
        return [expr]




    def emit_program(self, x_expr: Any) -> None:
        self.nesting_level = 0
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
        self.emit_no_colon("    struct {{ struct reg *vars; struct reg *body; struct reg *env; int variadic; }}; // CLOSURE")
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
        self.emit_no_colon("reg *cons_ptr(reg *a, reg *b);")
        self.emit_no_colon("reg *make_symbol(char *name);")
        self.emit_no_colon("reg *make_number(long long value);")
        self.emit_no_colon("reg *make_boolean(unsigned int value);")
        self.emit_no_colon("reg *make_string(char *name);")
        self.emit_no_colon("reg* make_closure(reg* params, reg* body_expr, reg* captured_env, int variadic);")
        self.emit_no_colon("reg apply_closure(reg closure, reg args);")
        self.emit_no_colon("void initialize_global_env();")
        self.emit_no_colon("reg *alloc_reg();")
        self.emit_no_colon("void lookup_in_env(reg *env_ptr);")
        self.emit_no_colon("void set_var_in_env(reg *env_ptr);")
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

    def emit_expr(self, x: Any) -> None:
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
            elif op == 'let*':
                self.emit_let_star(args)
            elif op == 'letrec':
                self.emit_letrec(args)
            elif op == 'lambda':
                self.emit_lambda_expr(x)
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
            elif op == '=': self.emit_numeric_equal(args[0], args[1])
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

    def emit_unary_op_generic(self, arg_expr: Any, c_operation: str) -> None:
        self.emit_expr(arg_expr)
        self.emit(c_operation)

    def emit_unary_pred_generic(self, arg_expr: Any, c_condition: str) -> None:
        self.emit_expr(arg_expr)
        self.emit("al = ({})", c_condition)
        self.emit("eax.t = BOOLEAN")
        self.emit("eax.b = al")

    def emit_binary_op_generic(self, arg1_expr: Any, arg2_expr: Any, c_operation_fmt: str) -> None:
        """Emit code for a binary operation using a temporary variable."""
        temp = f"tmp_{self.gensym()}"
        self.emit_expr(arg1_expr)
        self.emit(f"reg {temp} = eax")
        self.emit_expr(arg2_expr)
        self.emit(c_operation_fmt.format(temp=temp))

    def emit_binary_op_direct(self, arg1_expr: Any, arg2_expr: Any, op_symbol: str) -> None:
        """Emit code for a binary arithmetic operation without using the VM stack."""
        temp = f"tmp_{self.gensym()}"
        self.emit_expr(arg1_expr)
        self.emit(f"reg {temp} = eax")
        self.emit_expr(arg2_expr)
        self.emit(f"eax.n = {temp}.n {op_symbol} eax.n")
        self.emit("eax.t = FIXNUM")

    def emit_eq(self, arg1_expr: Any, arg2_expr: Any) -> None:
        temp = f"tmp_{self.gensym()}"
        self.emit_expr(arg1_expr)
        self.emit(f"reg {temp} = eax")
        self.emit_expr(arg2_expr)
        self.emit(f"al = reg_equal({temp}, eax)")
        self.emit("eax.t = BOOLEAN")
        self.emit("eax.b = al")

    def emit_numeric_equal(self, arg1_expr: Any, arg2_expr: Any) -> None:
        temp = f"tmp_{self.gensym()}"
        self.emit_expr(arg1_expr)
        self.emit(f"reg {temp} = eax")
        self.emit_expr(arg2_expr)
        self.emit(f"al = ({temp}.n == eax.n)")
        self.emit("eax.t = BOOLEAN")
        self.emit("eax.b = al")

    def emit_display(self, args_list: List[Any]) -> None:
        expr_to_display = args_list[0]
        self.emit_expr(expr_to_display)
        self.emit("display_obj(eax)")
        self.emit("fflush(stdout)")
        
    def emit_if(self, args_list: List[Any]) -> None:
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

    def emit_begin(self, expressions_list: Iterable[Any]) -> None:
        for expr in expressions_list:
            self.emit_expr(expr)
            
    def emit_add_var_to_global_vm_env(self, scheme_var_name: str, c_src_var_name_or_reg: str) -> None:
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

    def emit_add_var_to_global_vm_env_ptr(self, scheme_var_name: str, c_src_reg_ptr: str) -> None:
        self.emit("// Add global variable '{}' to VM env by pointer".format(scheme_var_name))
        self.emit_no_colon("{{")
        self.emit("  reg* var_sym_for_env = make_symbol(\"{}\");", scheme_var_name)
        self.emit("  reg* env_current_global_frame = car(env);")
        self.emit("  reg* env_old_symbols = car(env_current_global_frame);")
        self.emit("  reg* env_old_values = cdr(env_current_global_frame);")
        self.emit("  reg* env_new_symbols = cons(var_sym_for_env, env_old_symbols);")
        self.emit("  reg* env_new_values = cons_ptr({}, env_old_values);", c_src_reg_ptr)
        self.emit("  reg* new_global_frame = cons(env_new_symbols, env_new_values);")
        self.emit("  env = cons(new_global_frame, cdr(env));")
        self.emit_no_colon("}}")

    def emit_define(self, args_list: List[Any]) -> None:
        definition = args_list[0]
        body_parts = args_list[1:]

        if isinstance(definition, str):
            body_expr = body_parts[0] if len(body_parts) == 1 else ['begin'] + body_parts
            self.emit_define_var([definition, body_expr])
        elif isinstance(definition, list) and definition:
            func_name = definition[0]
            params = definition[1:]
            lambda_expr = ['lambda', params, *body_parts]
            self.emit_define_var([func_name, lambda_expr])
        else:
            self.emit_no_colon("// ERROR: Malformed define expression.")
            self.emit("// Define expected (define var val) or (define (func params) body). Got:")
            self.emit("// {}", repr(args_list))

    def emit_define_var(self, expr_parts: List[Any]) -> None:
        var_name_str = expr_parts[0]
        val_expr = expr_parts[1]
        
        c_var_name = self.get_safe_name(var_name_str)
        scheme_var_name = var_name_str

        is_lambda_def = isinstance(val_expr, list) and val_expr and val_expr[0] == 'lambda'
        is_top_level = (self.nesting_level == 0)

        if is_lambda_def:
            lambda_params = val_expr[1]
            body_parts = val_expr[2:]
            body_parts = self._transform_internal_defines(body_parts)
            actual_body = body_parts[0] if len(body_parts) == 1 else ['begin'] + body_parts

            self.emit("// Defining potentially recursive function {} as {}", var_name_str, repr(actual_body))
            # Storage for the C variable that will hold the closure reg struct.
            # This is distinct from _storage which holds the reg struct itself if allocated separately.
            # Storage for the closure reg struct itself, to allow self-reference.
            self.emit("reg* {}_storage = alloc_reg();", c_var_name)


            self.emit("reg quoted_params_val_for_{};", c_var_name)
            self.emit("reg quoted_body_val_for_{};", c_var_name)
            
            self.emit_expr(['quote', lambda_params])
            self.emit("quoted_params_val_for_{} = eax;", c_var_name)
            variadic_flag = 1 if isinstance(lambda_params, str) else 0
            
            self.emit_expr(['quote', actual_body])
            self.emit("quoted_body_val_for_{} = eax;", c_var_name)
            
            self.emit("// 1. Create a temporary closure capturing {}", 'global env' if not is_top_level else 'no environment')
            captured_env_arg = 'env' if not is_top_level else 'NULL'
            self.emit("reg* temp_closure_ptr_for_{} = make_closure(&quoted_params_val_for_{}, &quoted_body_val_for_{}, {}, {});",
                      c_var_name, c_var_name, c_var_name, captured_env_arg, variadic_flag)
            
            self.emit("// 2. Copy this temporary closure into our dedicated storage '{}_storage'", c_var_name)
            self.emit("memcpy({}_storage, temp_closure_ptr_for_{}, sizeof(reg));", c_var_name, c_var_name)
            
            self.emit("// 3. Add this closure (now in *{}_storage) to the global environment under its name '{}'.", c_var_name, scheme_var_name)
            self.emit_add_var_to_global_vm_env_ptr(scheme_var_name, f"{c_var_name}_storage")
            
            if is_top_level:
                self.emit("{}_storage->env = NULL;", c_var_name)
                pass
            else:
                self.emit("{}_storage->env = env;", c_var_name)
                pass

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
            self.emit_add_var_to_global_vm_env(scheme_var_name, "eax")
            self.emit("// End defining simple global {}", var_name_str)

    def emit_let(self, args_list: List[Any]) -> None:
        var_bindings = args_list[0]
        body_parts = args_list[1:]
        body_expr = body_parts[0] if len(body_parts) == 1 else ['begin'] + body_parts

        self.emit_no_colon("{{ // Start LET scope")
        self.nesting_level += 1
        # Original 'let' binds globally within a C block, not true lexical scope. Replicating.
        for var_name_str, val_expr in var_bindings:
            self.emit_define_var([var_name_str, val_expr])

        self.emit_expr(body_expr)
        self.nesting_level -= 1
        self.emit_no_colon("}} // End LET scope")

    def emit_let_star(self, args_list: List[Any]) -> None:
        bindings = args_list[0]
        body_parts = args_list[1:]
        body_expr = body_parts[0] if len(body_parts) == 1 else ['begin'] + body_parts
        expr = body_expr
        for var, val in reversed(bindings):
            expr = ['let', [[var, val]], expr]
        self.emit_expr(expr)

    def emit_letrec(self, args_list: List[Any]) -> None:
        bindings = args_list[0]
        body_parts = args_list[1:]
        placeholder = ['quote', []]
        let_bindings = [[var, placeholder] for var, _ in bindings]
        set_forms = [['set!', var, val] for var, val in bindings]
        body_core_forms = set_forms + body_parts
        body_expr = body_core_forms[0] if len(body_core_forms) == 1 else ['begin'] + body_core_forms
        expr = ['let', let_bindings, body_expr]
        self.emit_expr(expr)

    def emit_quote(self, x: Any) -> None:
        if self.is_null(x):
            self.emit("eax.t = NIL")
        elif isinstance(x, str) and not any(c in x for c in ['(', ')', ' ', '\'', '#']):
            escaped_x_for_symbol = x.replace('\\', '\\\\').replace('"', '\\"')
            self.emit("eax = *make_symbol(\"{}\");", escaped_x_for_symbol)
        elif isinstance(x, str):
            escaped_str = x.replace('\\', '\\\\').replace('"', '\\"')
            self.emit("eax = *make_symbol(\"{}\");", escaped_str)
        elif isinstance(x, tuple) and len(x) == 2 and x[0] == 'string':
            escaped = x[1].replace('\\', '\\\\').replace('"', '\\"')
            self.emit("eax = *make_string(\"{}\");", escaped)
        elif isinstance(x, list) and x: 
            self.emit_quoted_list(x)
        elif self.is_immediate(x) and not self.is_null(x):
            self.emit_immediate(x)
        else:
            # This means (quote "string") is not handled correctly if string itself is passed as str.
            self.emit_no_colon("// ERROR: emit-quote cannot quote: {}", repr(x))


    def emit_quoted_list(self, lst: List[Any]) -> None:
        if not lst:
            self.emit("eax.t = NIL") # Should be caught by is_null in emit_quote
            return

        head_tmp = f"tmp_{self.gensym()}"
        self.emit_quote(lst[0])
        self.emit(f"reg {head_tmp} = eax")
        self.emit_quote(lst[1:])
        self.emit("ebx = eax")
        self.emit(f"eax = *cons(&{head_tmp}, &ebx)")


    def emit_lambda_expr(self, lambda_expr: List[Any]) -> None:
        params_list = lambda_expr[1]
        body_parts = lambda_expr[2:]
        body_parts = self._transform_internal_defines(body_parts)
        actual_body_expr = body_parts[0] if len(body_parts) == 1 else ['begin'] + body_parts

        self.emit_no_colon("{{ // Start LAMBDA scope")
        self.emit("// Compiling LAMBDA with params: {} body: {}", repr(params_list), repr(actual_body_expr))
        env_var = "env"

        self.emit_expr(['quote', params_list])
        self.emit("reg lambda_params_val = eax")
        variadic_flag = 1 if isinstance(params_list, str) else 0

        self.emit_expr(['quote', actual_body_expr])
        self.emit("reg lambda_body_val = eax")

        self.emit("reg* new_closure_ptr = make_closure(&lambda_params_val, &lambda_body_val, {}, {} )", env_var, variadic_flag)
        self.emit("eax = *new_closure_ptr")
        self.emit_no_colon("}} // End LAMBDA scope")

    def emit_apply(self, expr_list: List[Any]) -> None:
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

    def emit_set_bang(self, args_list: List[Any]) -> None:
        var_name_str = args_list[0]
        body_expr = args_list[1]

        if isinstance(var_name_str, str):
            # Evaluate new value
            self.emit_expr(body_expr)
            self.emit("reg tmp_set_val = eax")
            # Prepare parameters for runtime environment update
            self.emit("ebx = tmp_set_val")
            self.emit("eax = *make_symbol(\"{}\");", var_name_str)
            self.emit("set_var_in_env(env)")
            self.emit("eax = tmp_set_val")
        else:
            self.emit_no_colon("// SET! target is not a symbol: {}", var_name_str)

    def emit_cons(self, args_list: List[Any]) -> None:
        self.emit_expr(args_list[0])
        car_tmp = f"tmp_{self.gensym()}"
        self.emit(f"reg {car_tmp} = eax")
        self.emit_expr(args_list[1])
        self.emit("ebx = eax")
        self.emit(f"eax = *cons(&{car_tmp}, &ebx)")

    def emit_car(self, args_list: List[Any]) -> None:
        self.emit_expr(args_list[0])
        self.emit("eax = *car(&eax)")

    def emit_cdr(self, args_list: List[Any]) -> None:
        self.emit_expr(args_list[0])
        self.emit("eax = *cdr(&eax)")

    def emit_cond(self, clauses_list: List[Any]) -> None:
        end_label = self.gensym()

        for clause in clauses_list:
            if not clause:
                self.emit_no_colon("// ERROR: cond clause empty")
                continue

            pred_expr = clause[0]
            body_exprs = clause[1:]

            if isinstance(pred_expr, str) and pred_expr == 'else':
                for expr in body_exprs:
                    self.emit_expr(expr)
                self.emit("goto {}", end_label)
                break

            next_clause_label = self.gensym()

            self.emit_expr(pred_expr)
            self.emit("if (!eax.b){{goto {};}}", next_clause_label)

            if body_exprs:
                for expr in body_exprs:
                    self.emit_expr(expr)
            # When no body expressions, result of predicate remains in eax

            self.emit("goto {}", end_label)
            self.emit_no_colon("{}:", next_clause_label)

        self.emit_no_colon("{}:", end_label)

    def emit_label(self, label_name_str: str) -> None:
        self.emit_no_colon("{}:", label_name_str)

    def emit_goto(self, label_name_str: str) -> None:
        self.emit("goto {}", label_name_str)

    def emit_exit(self, arg_val: Any) -> None:
        if isinstance(arg_val, int):
            self.emit("exit({})", arg_val)
        else:
            # Original was (emit "exit(~a)" x). ~a on a symbol prints symbol name.
            # This means `exit(foo)` not `exit(valueOfFoo)`. For safety, only int.
            self.emit("// emit-exit expects an integer literal argument. Got: {}", repr(arg_val))
            self.emit_expr(arg_val) # Evaluate if it's an expr
            self.emit("exit(eax.n) // Attempting to exit with evaluated expr")



    # --- Parser specific methods ---
    def _preprocess_scheme_remove_comments(self, text: str) -> str:
        lines = text.splitlines()
        processed_lines = []
        for line in lines:
            line = line.split(';', 1)[0] 
            processed_lines.append(line)
        return "\n".join(processed_lines)

    def _tokenize_sexp(self, s: str) -> List[str]:
        s = s.replace('(', ' ( ').replace(')', ' ) ').replace("'", " ' ")
        return [token for token in s.split() if token]  # Filter out empty strings

    def _parse_sexp_from_tokens(self, tokens: List[str]) -> Any:
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
                # String literals "foo"
                if token.startswith('"') and token.endswith('"') and len(token) >=2:
                    content = token[1:-1].replace('\\"', '"').replace('\\n','\n').replace('\\t','\t')
                    return ('string', content)
                # Handle '() as nil/empty list
                if token == 'nil': # Guile's `read` may not produce 'nil' often for '()
                    return []
                return token # Symbol as string

    def parse_scheme_file(self, file_path: str) -> Any:
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


        # Parse one or more S-expressions. If more than one is present,
        # automatically wrap them in a (begin ...).
        try:
            exprs = []
            while tokens:
                exprs.append(self._parse_sexp_from_tokens(tokens))
            if len(exprs) == 1:
                parsed_expr = exprs[0]
            else:
                parsed_expr = ['begin'] + exprs
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

    if len(sys.argv) >= 3:
        output_c_file = sys.argv[2]
        try:
            with open(output_c_file, 'w') as f_out:
                compiler.set_compile_port(f_out)
                compiler.emit_program(parsed_program)
            print(f"Python compiler: Generated {output_c_file} from {input_scm_file}", file=sys.stderr)
        except IOError:
            sys.stderr.write(f"Error: Could not write to output file '{output_c_file}'.\n")
            sys.exit(1)
    else:  # Output to stdout if no output file specified
        compiler.set_compile_port(sys.stdout)
        compiler.emit_program(parsed_program)
