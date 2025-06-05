#include "vm.h"
#include "primitives.h"

int al;
reg eax, ebx;


int reg_equal(reg a, reg b) {
  if (a.t != b.t) return 0;
  switch (a.t) {
    case BOOLEAN: return a.b == b.b;
    case FIXNUM: return a.n == b.n;
    case CHAR: return a.c == b.c;
    case NIL: return 1;
    case SYMBOL: return strncmp(a.s, b.s, MAX_SYMBOL_LEN - 1) == 0;
    default: return 0;
  }
}

void write_obj(reg r);
void display_obj(reg r);
reg* make_closure(reg* params, reg* body_expr, reg* captured_env, int variadic);
reg apply_closure(reg closure_obj, reg args_list_obj);
reg eval_scheme_expr(reg expr, reg* current_eval_env);
int list_length(reg list_obj);


void set_var_in_env(reg *env_ptr);
reg *cons_ptr(reg *a, reg *b); // New helper
reg *alloc_reg();

reg *car(reg *head) {
  if(head->t == PAIR) {
    return head->car;
  }
  write_obj(*head);
  puts(" is not a pair!");
  exit(1);
}

reg *cdr(reg *head) {
  if(head->t == PAIR) {
    return head->cdr;
  }
  write_obj(*head);
  puts(" is not a pair!");
  exit(1);
}

void write_obj(reg r) {
  if (r.t == BOOLEAN) {
    printf("%s", r.b ? "#t" : "#f");
  } else if (r.t == FIXNUM) {
    printf("%lld", r.n);
  } else if (r.t == CHAR) {
    putchar(r.c);
  } else if (r.t == NIL) {
    printf("()");
  } else if ( r.t == SYMBOL) {
    printf("%s", r.s);
  } else if ( r.t == STRING) {
    printf("\"%s\"", r.s);
  } else if (r.t == CLOSURE) {
    printf("#<closure>");
  } else if (r.t == PRIMITIVE_PROC) {
    printf("#<primitive>");
  }

  if (r.t == PAIR) {
    reg *head = calloc(1, sizeof(reg));
    memcpy(head,&r,sizeof(reg));
    printf("(");
    print_pair:

    write_obj(*car(head));
    if (cdr(head)->t != NIL) {
      printf(" ");
      head = cdr(head);
      if (head->t != PAIR) {
        printf(" . ");
        write_obj(*head);
        printf(")");
        return;
      }
      goto print_pair;
    }
    printf(")");
  }
}

// Function to display Scheme objects (strings without quotes)
void display_obj(reg r) {
  if (r.t == BOOLEAN) {
    printf("%s", r.b ? "#t" : "#f");
  } else if (r.t == FIXNUM) {
    printf("%lld", r.n);
  } else if (r.t == CHAR) {
    putchar(r.c);
  } else if (r.t == NIL) {
    printf("()");
  } else if ( r.t == SYMBOL) {
    printf("%s", r.s);
  } else if ( r.t == STRING) {
    printf("%s", r.s); // Changed for display_obj: no quotes for strings
  } else if (r.t == CLOSURE) {
    printf("#<closure>");
  } else if (r.t == PRIMITIVE_PROC) {
    printf("#<primitive>");
  }

  if (r.t == PAIR) {
    reg *head = calloc(1, sizeof(reg));
    memcpy(head,&r,sizeof(reg));
    printf("(");
    print_pair_display: // Renamed label for clarity

    display_obj(*car(head)); // Recursive call to display_obj
    if (cdr(head)->t != NIL) {
      head = cdr(head);
      if (head->t != PAIR) {
        printf(" . ");
        display_obj(*head); // Recursive call to display_obj
        printf(")");
        return;
      }
      printf(" ");
      goto print_pair_display;
    }
    printf(")");
  }
}

reg *cons(reg *a, reg *b) {
  reg *res, *ac, *bc;
  res = calloc(1, sizeof(reg));
  ac = calloc(1, sizeof(reg));
  bc = calloc(1, sizeof(reg));
  res->t = PAIR;
  memcpy(ac, a, sizeof(reg));
  #ifdef DEBUG_VM
  if (a->t == PRIMITIVE_PROC) {
      printf("DEBUG: cons (memcpy ac from a) - a->c_primitive_proc = %p, ac->c_primitive_proc = %p\n", (void*)a->c_primitive_proc, (void*)ac->c_primitive_proc);
  }
  #endif
  memcpy(bc, b, sizeof(reg));
  #ifdef DEBUG_VM
  if (b->t == PRIMITIVE_PROC) {
      printf("DEBUG: cons (memcpy bc from b) - b->c_primitive_proc = %p, bc->c_primitive_proc = %p\n", (void*)b->c_primitive_proc, (void*)bc->c_primitive_proc);
  }
  #endif
  res->car = ac;
  res->cdr = bc;
  return res;
}

// Helper like cons but does not copy the car pointer (used for env to hold exact closure)
reg *cons_ptr(reg *a, reg *b) {
  reg *res = alloc_reg();
  res->t = PAIR;
  res->car = a;
  res->cdr = b;
  return res;
}

reg *alloc_object() {
  reg *res = 0;
  res = calloc(1, sizeof(reg));
  return res;
}

reg *alloc_reg() {
  reg *res = 0;
  res = calloc(1, sizeof(reg));
  return res;
}

reg *make_symbol(char *name) {
  reg *res;
  res = alloc_reg();
  res->t = SYMBOL;
  res->s = strndup(name, MAX_SYMBOL_LEN);
  return res;
}

reg *make_number(long long value) {
  reg *res;
  res = alloc_reg();
  res->t = FIXNUM;
  res->n = value;
  return res;
}

reg *make_boolean(unsigned int value) {
  reg *res;
  res = alloc_reg();
  res->t = BOOLEAN;
  res->b = value;
  return res;
}

reg *make_string(char *name) {
  reg *res;
  res = alloc_reg();
  res->t = STRING;
  res->s = strndup(name, MAX_SYMBOL_LEN);
  return res;
}

reg *make_char(char ch) {
  reg *res = alloc_reg();
  res->t = CHAR;
  res->c = ch;
  return res;
}

void lookup_in_frame(reg *frame)
{
  // assume that eax contains the symbol to look for
  reg *var, *binding, symbol;
  var = car(frame);
  binding = cdr(frame);
  symbol = eax;
  for(; var->t != NIL || binding->t != NIL;
      var = cdr(var), binding = cdr(binding))
  {
    if (reg_equal(*car(var), symbol)) {
      eax = *car(binding);
      al = 1;
      return;
    }
  }
  al = 0;
}

void lookup_in_env(reg *env)
{
  // assume that eax contains the symbol to look for
  reg symbol, *frame;
  frame = car(env);
  symbol = eax;
  for(; env->t != NIL; env = cdr(env))
  {
    frame = car(env);
    eax = symbol;
    lookup_in_frame(frame);
    if (al == 1) {
      return;
    }
  }
  printf("Unbound symbol: ");
  write_obj(symbol);
  puts("");
  exit(1);
}

void set_var_in_env(reg *env_ptr)
{
    reg symbol = eax; // symbol to update
    reg new_val = ebx; // new value in ebx
    for (; env_ptr->t != NIL; env_ptr = env_ptr->cdr) {
        reg *frame = env_ptr->car;
        reg *vars = car(frame);
        reg *vals = cdr(frame);
        for (; vars->t != NIL && vals->t != NIL; vars = cdr(vars), vals = cdr(vals)) {
            if (reg_equal(*car(vars), symbol)) {
                memcpy(car(vals), &new_val, sizeof(reg));
                return;
            }
        }
    }
    printf("Unbound variable in set!: ");
    write_obj(symbol);
    puts("");
    exit(1);
}

// Initialize global env to point to a NIL object rather than being NULL itself.
reg actual_nil_for_global_env; // Statically/globally allocated reg struct
reg *env = NULL; // Will be initialized in main

void initialize_global_env() {
    if (env == NULL) { // Initialize only once
        #ifdef DEBUG_VM
        printf("DEBUG: Initializing global environment...\n");
        #endif

        reg* nil_ptr = alloc_reg();
        nil_ptr->t = NIL;

        reg* prim_symbols = nil_ptr;
        reg* prim_values = nil_ptr;
        for (size_t i = 0; i < primitive_table_count; ++i) {
            reg* sym = make_symbol((char*)primitive_table[i].name);
            reg* obj = alloc_reg();
            obj->t = PRIMITIVE_PROC;
            obj->c_primitive_proc = primitive_table[i].func;
            prim_symbols = cons(sym, prim_symbols);
            prim_values = cons(obj, prim_values);
        }

        reg* global_frame = cons(prim_symbols, prim_values);
        env = cons(global_frame, nil_ptr);

        #ifdef DEBUG_VM
        printf("DEBUG: Global env initialized. Env = "); write_obj(*env); puts("");
        #endif
    } else {
        #ifdef DEBUG_VM
        printf("DEBUG: Global environment already initialized.\n");
        #endif
    }
}


reg* make_closure(reg* params, reg* body_expr, reg* captured_env, int variadic) {
    reg* closure_obj = alloc_reg(); // Use existing helper for allocation
    closure_obj->t = CLOSURE;

    // The params and body_expr are pointers to 'reg' structs.
    // The closure needs to own its copies of these, as the originals might be temporary.
    closure_obj->vars = alloc_reg(); // Allocate space for the 'vars' reg struct
    memcpy(closure_obj->vars, params, sizeof(reg)); // Copy the content

    closure_obj->body = alloc_reg(); // Allocate space for the 'body' reg struct
    memcpy(closure_obj->body, body_expr, sizeof(reg)); // Copy the content

    // captured_env is a pointer to an existing environment structure.
    closure_obj->env = captured_env;
    closure_obj->variadic = variadic;

    return closure_obj;
}

reg apply_closure(reg closure_obj, reg args_list_obj) {
    #ifdef DEBUG_VM
    printf("DEBUG: apply_closure called.\n");
    printf("DEBUG: Closure object: "); write_obj(closure_obj); puts("");
    printf("DEBUG: Arguments list: "); write_obj(args_list_obj); puts("");
    #endif

    // Allow calling primitive procedures directly through this function
    if (closure_obj.t == PRIMITIVE_PROC) {
        return closure_obj.c_primitive_proc(args_list_obj);
    }

    if (closure_obj.t != CLOSURE) {
        printf("ERROR: Attempted to apply non-closure object.\n");
        write_obj(closure_obj);
        puts(" is not a closure.");
        exit(1);
    }

    reg* formal_params_list = closure_obj.vars; // Could be list or symbol
    int variadic = closure_obj.variadic;
    reg* body_expr_ptr = closure_obj.body;     // This is a reg* pointing to the body expression
    reg* captured_env = closure_obj.env;     // This is a reg*
    if (captured_env == NULL) {
        captured_env = env; // Use current global environment if none captured
    }

    // Arity check (skip if variadic)
    if (!variadic) {
        int params_count = list_length(*formal_params_list);
        int args_count = list_length(args_list_obj);

        #ifdef DEBUG_VM
        printf("DEBUG: Arity check - Params: %d, Args: %d\n", params_count, args_count);
        #endif

        if (params_count != args_count) {
            printf("ERROR: Arity mismatch. Expected %d arguments, got %d.\n", params_count, args_count);
            exit(1);
        }
    }

    // Construct new environment frame: ( (param_symbols_list) (actual_args_values_list) )
    // The params list is closure_obj.vars (a reg* to a list)
    // The args list is args_list_obj (a reg, which is a list)
    
    // We need to make args_list_obj a reg* for cons.
    // A bit of a hack: allocate a temp reg to hold args_list_obj to pass its address to cons.
    // Proper memory management for these temp regs is important with GC.
    // Or, better, ensure cons can take (reg*, reg*) or adapt.
    // For now, let's assume args_list_obj (a reg) can be used if cons takes (reg*, reg*), where one is by value.
    // The current cons takes (reg*, reg*). So args_list_obj needs to be a pointer.
    
    reg* args_list_obj_ptr = alloc_reg(); // Allocate a reg on the heap
    memcpy(args_list_obj_ptr, &args_list_obj, sizeof(reg)); // Copy args_list_obj content to it

    reg* params_for_env = formal_params_list;
    reg* arg_values_for_env = args_list_obj_ptr;
    if (variadic) {
        reg* nil_for_var = alloc_reg();
        nil_for_var->t = NIL;
        params_for_env = cons(formal_params_list, nil_for_var);
        reg* nil_binding_tail = alloc_reg();
        nil_binding_tail->t = NIL;
        arg_values_for_env = cons_ptr(args_list_obj_ptr, nil_binding_tail);
    }

    reg* new_frame_bindings = cons(params_for_env, arg_values_for_env);
    
    // Extend captured environment
    // captured_env should now always be a valid pointer (to NIL or a PAIR)
    reg* eval_env = cons(new_frame_bindings, captured_env); 

    #ifdef DEBUG_VM
    printf("DEBUG: apply_closure - New evaluation environment: "); 
    if (eval_env) write_obj(*eval_env); else printf("(null eval_env!) "); 
    puts("");
    #endif

    reg result = eval_scheme_expr(*body_expr_ptr, eval_env);

    #ifdef DEBUG_VM
    printf("DEBUG: apply_closure returning result from eval_scheme_expr.\n");
    #endif
    return result;
}

// Helper function to get the length of a Scheme list (dotted pairs not fully handled)
int list_length(reg list_obj) {
    if (list_obj.t == NIL) return 0;
    if (list_obj.t != PAIR) {
        // This isn't a proper list for length calculation in this context
        // Or it's a dotted pair at the end. For arity, we care about proper list structure.
        printf("Warning: list_length called on non-pair/non-nil or improper list tail\n");
        return -1; // Indicate error or improper list
    }
    int count = 0;
    reg* current = &list_obj;
    while (current->t == PAIR) {
        count++;
        current = current->cdr;
    }
    if (current->t != NIL) {
        printf("Warning: Improper list for length calculation (dotted pair ending)\n");
        return -1; // Or count, depending on how dotted pairs should affect length here
    }
    return count;
}

// Helper function to check if a Scheme symbol reg matches a C string name
int is_symbol_eq(reg symbol_reg, const char* c_name) {
    if (symbol_reg.t != SYMBOL) {
        return 0; // Not a symbol
    }
    if (symbol_reg.s == NULL || c_name == NULL) {
        return 0; // Should not happen with valid symbols/names
    }
    return strncmp(symbol_reg.s, c_name, MAX_SYMBOL_LEN) == 0;
}

reg eval_scheme_expr(reg expr, reg* current_eval_env) {
    while (1) {
        #ifdef DEBUG_VM
        printf("DEBUG: eval_scheme_expr called.\n");
        printf("DEBUG: Expr to eval: "); write_obj(expr); puts("\n");
        printf("DEBUG: Eval Env (ptr %p): ", (void*)current_eval_env);
        if(current_eval_env) {
            write_obj(*current_eval_env);
        } else {
            printf("(null C env pointer - problem!)");
        }
        puts("");
        #endif

    // Self-evaluating types
    if (expr.t == FIXNUM || expr.t == BOOLEAN || expr.t == NIL || expr.t == CHAR || expr.t == STRING) {
        #ifdef DEBUG_VM
        printf("DEBUG: eval_scheme_expr - SELF-EVALUATING: "); write_obj(expr); puts("");
        #endif
        return expr;
    }

    // SYMBOL - Lookup in environment
    if (expr.t == SYMBOL) {
        #ifdef DEBUG_VM
        printf("DEBUG: eval_scheme_expr - SYMBOL: "); write_obj(expr); puts("");
        #endif
        reg preserved_eax = eax; // Preserve global eax state if lookup_in_env relies on it heavily
        eax = expr;              // lookup_in_env expects symbol in global eax
        lookup_in_env(current_eval_env);
        reg found_val = eax;     // lookup_in_env puts result in global eax
        eax = preserved_eax;     // Restore global eax
        #ifdef DEBUG_VM
        printf("DEBUG: eval_scheme_expr - SYMBOL LOOKUP RESULT: "); write_obj(found_val); 
        if (found_val.t == PRIMITIVE_PROC) {
            printf(", c_primitive_proc = %p", (void*)found_val.c_primitive_proc);
        }
        puts("");
        #endif
        return found_val;
    }

    // PAIR - Could be special form or application
    if (expr.t == PAIR) {
        // Ensure car is not null before trying to access its members
        if (expr.car == NULL) {
            printf("ERROR: Malformed pair in expression - car is NULL\n");
            exit(1); // Or return error object
        }
        reg first_elem = *(expr.car);

        // QUOTE: (quote <datum>)
        if (first_elem.t == SYMBOL && is_symbol_eq(first_elem, "quote")) {
            #ifdef DEBUG_VM
            printf("DEBUG: eval_scheme_expr - QUOTE: "); write_obj(expr); puts("");
            #endif
            // Basic structure check: (quote <datum>). <datum> is cadr.
            if (expr.cdr == NULL || expr.cdr->t != PAIR || expr.cdr->car == NULL) { 
                printf("ERROR: Malformed quote expression - expected (quote <datum>)\n");
                exit(1); 
            }
            reg datum = *(expr.cdr->car); // This is cadr(expr)
            #ifdef DEBUG_VM
            printf("DEBUG: eval_scheme_expr - QUOTE returning: "); write_obj(datum); puts("");
            #endif
            return datum;
        }

        // IF: (if test consequent optional-alternative)
        if (first_elem.t == SYMBOL && is_symbol_eq(first_elem, "if")) {
            #ifdef DEBUG_VM
            printf("DEBUG: eval_scheme_expr - IF: "); write_obj(expr); puts("");
            #endif

            // Structure check: (if <test> <consequent> ...)
            if (expr.cdr == NULL || expr.cdr->t != PAIR || expr.cdr->car == NULL || // Missing <test>
                expr.cdr->cdr == NULL || expr.cdr->cdr->t != PAIR || expr.cdr->cdr->car == NULL) { // Missing <consequent>
                printf("ERROR: Malformed if expression - expected (if <test> <consequent> ...)\n");
                exit(1);
            }

            reg test_expr = *(expr.cdr->car);           // cadr(expr)
            reg conseq_expr = *(expr.cdr->cdr->car);   // caddr(expr)
            
            reg alt_expr;
            int has_alternative = 0;
            // Check for <alternative> (cadddr(expr))
            if (expr.cdr->cdr->cdr != NULL && expr.cdr->cdr->cdr->t == PAIR && expr.cdr->cdr->cdr->car != NULL) {
                alt_expr = *(expr.cdr->cdr->cdr->car);
                has_alternative = 1;
                // Check if there are more expressions after alternative (malformed)
                if (expr.cdr->cdr->cdr->cdr != NULL && expr.cdr->cdr->cdr->cdr->t != NIL) {
                    printf("ERROR: Malformed if expression - too many forms after alternative\n");
                    exit(1);
                }
            } else if (expr.cdr->cdr->cdr != NULL && expr.cdr->cdr->cdr->t == NIL) {
                // This means the form was (if test conseq) and we are at the end of the list.
                has_alternative = 0;
            } else {
                // Anything else after conseq that isn't a proper list for alt or NIL ending means malformed.
                // This case could also be (if test conseq) if expr.cdr->cdr->cdr is NULL (not typical for parser output)
                #ifdef DEBUG_VM
                printf("DEBUG: if expression appears to be (if test conseq) or malformed end.");
                #endif
                // Assuming this means (if test conseq) and the list just ends.
                has_alternative = 0; 
            }

            reg test_result = eval_scheme_expr(test_expr, current_eval_env);
            #ifdef DEBUG_VM
            printf("DEBUG: eval_scheme_expr - IF test_result: "); write_obj(test_result); puts("");
            #endif

            int condition_is_true = 1; // In Scheme, only #f (boolean type, value 0) is false.
            if (test_result.t == BOOLEAN && test_result.b == 0) {
                condition_is_true = 0;
            }

            if (condition_is_true) {
                #ifdef DEBUG_VM
                printf("DEBUG: eval_scheme_expr - IF evaluating consequent\n");
                #endif
                expr = conseq_expr;
                continue;
            } else {
                if (has_alternative) {
                    #ifdef DEBUG_VM
                    printf("DEBUG: eval_scheme_expr - IF evaluating alternative\n");
                    #endif
                    expr = alt_expr;
                    continue;
                } else {
                    #ifdef DEBUG_VM
                    printf("DEBUG: eval_scheme_expr - IF no alternative, returning NIL (unspecified)\n");
                    #endif
                    reg nil_return_val;
                    nil_return_val.t = NIL;
                    return nil_return_val;
                }
            }
        }

        // LET: (let ((var1 init1) ... (varN initN)) body1 ... bodyM)
        // Implements parallel binding (all inits evaluated in current_eval_env first)
        if (first_elem.t == SYMBOL && is_symbol_eq(first_elem, "let")) {
            #ifdef DEBUG_VM
            printf("DEBUG: eval_scheme_expr - LET: "); write_obj(expr); puts("");
            #endif

            // Structure check: (let <bindings> <body1> ...)
            if (expr.cdr == NULL || expr.cdr->t != PAIR || expr.cdr->car == NULL ||        // Missing <bindings>
                expr.cdr->cdr == NULL || expr.cdr->cdr->t != PAIR || expr.cdr->cdr->car == NULL) { // Missing <body1>
                printf("ERROR: Malformed let expression - expected (let <bindings> <body1> ...)\\n");
                exit(1);
            }

            reg* bindings_list_ptr = expr.cdr->car;      // cadr(expr)
            reg* body_forms_ptr = expr.cdr->cdr;       // cddr(expr)

            if (bindings_list_ptr->t != PAIR && bindings_list_ptr->t != NIL) { // Bindings can be '()
                printf("ERROR: let bindings must be a list.\\n");
                exit(1);
            }

            reg* let_param_symbols_rev = alloc_reg(); let_param_symbols_rev->t = NIL;
            reg* let_arg_values_rev = alloc_reg();    let_arg_values_rev->t = NIL;

            // Step 1 & 2: Evaluate all init expressions in the *current* environment
            // and collect param symbols and evaluated arg values (in reverse order)
            reg* current_binding_node = bindings_list_ptr;
            while(current_binding_node != NULL && current_binding_node->t == PAIR) {
                if (current_binding_node->car == NULL || current_binding_node->car->t != PAIR) {
                    printf("ERROR: Malformed let binding - expected list of (var init) pairs.\\n"); exit(1);
                }
                reg* binding_pair_ptr = current_binding_node->car; // (var init)

                if (binding_pair_ptr->car == NULL || binding_pair_ptr->car->t != SYMBOL || // var must be a symbol
                    binding_pair_ptr->cdr == NULL || binding_pair_ptr->cdr->t != PAIR || // init must exist as a list element
                    binding_pair_ptr->cdr->car == NULL || // actual init expression
                    (binding_pair_ptr->cdr->cdr != NULL && binding_pair_ptr->cdr->cdr->t != NIL) // binding pair must be (var init) exactly
                    ) {
                    printf("ERROR: Malformed let binding pair - expected (var init-expr).\\n"); exit(1);
                }
                reg* var_symbol_ptr = binding_pair_ptr->car; // This is reg* to the symbol
                reg init_expr = *(binding_pair_ptr->cdr->car); // This is the init expression (reg)

                reg evaluated_init_val = eval_scheme_expr(init_expr, current_eval_env);
                reg* evaluated_init_val_ptr = alloc_reg(); // cons needs reg*
                memcpy(evaluated_init_val_ptr, &evaluated_init_val, sizeof(reg));

                let_param_symbols_rev = cons(var_symbol_ptr, let_param_symbols_rev);
                let_arg_values_rev = cons(evaluated_init_val_ptr, let_arg_values_rev);

                current_binding_node = current_binding_node->cdr;
            }
            if (current_binding_node != NULL && current_binding_node->t != NIL) { // Check for improper bindings list
                 printf("ERROR: Malformed let bindings list - improper list.\\n"); exit(1);
            }

            // Step 3: Reverse the collected lists to get correct order for the frame
            reg* final_let_param_symbols = alloc_reg(); final_let_param_symbols->t = NIL;
            reg* p_sym_iter = let_param_symbols_rev;
            while (p_sym_iter != NULL && p_sym_iter->t == PAIR) {
                final_let_param_symbols = cons(p_sym_iter->car, final_let_param_symbols);
                p_sym_iter = p_sym_iter->cdr;
            }

            reg* final_let_arg_values = alloc_reg(); final_let_arg_values->t = NIL;
            reg* p_val_iter = let_arg_values_rev;
            while (p_val_iter != NULL && p_val_iter->t == PAIR) {
                final_let_arg_values = cons(p_val_iter->car, final_let_arg_values);
                p_val_iter = p_val_iter->cdr;
            }
            
            // Step 4 & 5: Create new frame and extend environment
            reg* new_let_frame = cons(final_let_param_symbols, final_let_arg_values);
            reg* eval_env_for_let_body = cons(new_let_frame, current_eval_env);

            // Step 6: Evaluate body forms (like a 'begin' block)
            #ifdef DEBUG_VM
            printf("DEBUG: eval_scheme_expr - LET evaluating body forms. New env: "); 
            if(eval_env_for_let_body) write_obj(*eval_env_for_let_body); else printf("(null env for let body!)"); 
            puts("");
            #endif
            
            if (body_forms_ptr == NULL || body_forms_ptr->t == NIL) { // (let (...) ) -- no body forms
                 // R5RS says result is unspecified. Some Schemes return last val of bindings, some NIL.
                 // For (let () body), body is evaluated. If (let bindings) and bindings is non-empty,
                 // but body_forms_ptr is NIL, it's like (let ((a 1)) /*unspecified*/).
                 // Let's return NIL for an empty body list, consistent with an empty (begin).
                #ifdef DEBUG_VM
                printf("DEBUG: eval_scheme_expr - LET with no body forms, returning NIL\\n");
                #endif
                reg nil_val; nil_val.t = NIL;
                return nil_val;
            }

            reg* current_let_body_node = body_forms_ptr;
            while(current_let_body_node != NULL && current_let_body_node->t == PAIR &&
                  current_let_body_node->cdr != NULL && current_let_body_node->cdr->t == PAIR) {
                if (current_let_body_node->car == NULL) {
                    printf("ERROR: Malformed let body - null expression.\\n"); exit(1);
                }
                eval_scheme_expr(*(current_let_body_node->car), eval_env_for_let_body);
                current_let_body_node = current_let_body_node->cdr;
            }
            if (current_let_body_node == NULL || current_let_body_node->car == NULL) {
                printf("ERROR: Malformed let body - missing last expression.\\n"); exit(1);
            }
            if (current_let_body_node->cdr != NULL && current_let_body_node->cdr->t != NIL) {
                printf("ERROR: Malformed let body - improper list.\\n"); exit(1);
            }
            expr = *(current_let_body_node->car);
            current_eval_env = eval_env_for_let_body;
            continue;
        }

        // OR: (or expr1 expr2 ...)
        if (first_elem.t == SYMBOL && is_symbol_eq(first_elem, "or")) {
            #ifdef DEBUG_VM
            printf("DEBUG: eval_scheme_expr - OR: "); write_obj(expr); puts("");
            #endif

            reg* current_arg_node = expr.cdr; // List of argument expressions
            reg false_val; false_val.t = BOOLEAN; false_val.b = 0;

            if (current_arg_node == NULL || current_arg_node->t == NIL) { // (or) -> #f
                #ifdef DEBUG_VM
                printf("DEBUG: eval_scheme_expr - (or) with no arguments, returning #f\\n");
                #endif
                return false_val;
            }

            if (current_arg_node->t != PAIR) {
                 printf("ERROR: Malformed or expression - arguments not a proper list.\\n"); exit(1);
            }

            while(current_arg_node != NULL && current_arg_node->t == PAIR) {
                if (current_arg_node->car == NULL) {
                    printf("ERROR: Malformed or - null expression in arguments.\\n"); exit(1);
                }
                reg arg_val = eval_scheme_expr(*(current_arg_node->car), current_eval_env);
                
                int is_true = 1; // In Scheme, any value other than #f is true.
                if (arg_val.t == BOOLEAN && arg_val.b == 0) {
                    is_true = 0;
                }

                if (is_true) {
                    #ifdef DEBUG_VM
                    printf("DEBUG: eval_scheme_expr - OR found true value: "); write_obj(arg_val); puts("");
                    #endif
                    return arg_val; // Return the first true value
                }
                current_arg_node = current_arg_node->cdr;
            }
            // Check if the argument list was proper
            if (current_arg_node != NULL && current_arg_node->t != NIL) {
                printf("ERROR: Malformed or expression - improper list of argument expressions\\n");
                exit(1);
            }

            // All arguments evaluated to #f, or list was exhausted
            #ifdef DEBUG_VM
            printf("DEBUG: eval_scheme_expr - OR all arguments were false, returning #f\\n");
            #endif
            return false_val;
        }

        // LAMBDA: (lambda (param...) body-expr)
        // For now, assumes lambda has exactly one body expression.
        // R5RS allows multiple, which acts like an implicit (begin ...)
        if (first_elem.t == SYMBOL && is_symbol_eq(first_elem, "lambda")) {
            #ifdef DEBUG_VM
            printf("DEBUG: eval_scheme_expr - LAMBDA: "); write_obj(expr); puts("");
            #endif

            // Structure check: (lambda <params> <body>)
            // <params> is cadr(expr), <body> is caddr(expr)
            if (expr.cdr == NULL || expr.cdr->t != PAIR || expr.cdr->car == NULL ||        // Missing <params>
                expr.cdr->cdr == NULL || expr.cdr->cdr->t != PAIR || expr.cdr->cdr->car == NULL || // Missing <body>
                (expr.cdr->cdr->cdr != NULL && expr.cdr->cdr->cdr->t != NIL) ) { // Disallow extra forms after body for now
                printf("ERROR: Malformed lambda expression - expected (lambda <params> <body>)\n");
                exit(1);
            }

            reg* params_list_ptr = expr.cdr->car;  // cadr(expr)
            reg* body_expr_ptr = expr.cdr->cdr->car; // caddr(expr)

            int variadic_flag = 0;
            // Params can be a list of symbols (PAIR/NIL) or a single symbol for variadic
            if (params_list_ptr->t == SYMBOL) {
                 variadic_flag = 1;
            } else if (params_list_ptr->t != PAIR && params_list_ptr->t != NIL) {
                 printf("ERROR: Lambda parameters must be a list or symbol.\n");
                 exit(1);
            }
            // Further validation: check if all elements in params_list_ptr are symbols (can be added later)
            
            #ifdef DEBUG_VM
            printf("DEBUG: eval_scheme_expr - LAMBDA creating closure. Captured env: "); 
            if(current_eval_env && current_eval_env->t != NIL) { write_obj(*current_eval_env); } else { printf("(empty)"); }
            puts("");
            #endif

            reg* new_closure = make_closure(params_list_ptr, body_expr_ptr, current_eval_env, variadic_flag);
            return *new_closure; // make_closure returns reg*, eval_scheme_expr returns reg
        }

        // BEGIN: (begin expr1 expr2 ... exprN)
        if (first_elem.t == SYMBOL && is_symbol_eq(first_elem, "begin")) {
            #ifdef DEBUG_VM
            printf("DEBUG: eval_scheme_expr - BEGIN: "); write_obj(expr); puts("");
            #endif
            // If (begin) with no expressions, R5RS says result is unspecified. We return NIL.
            if (expr.cdr == NULL || expr.cdr->t == NIL) {
                #ifdef DEBUG_VM
                printf("DEBUG: eval_scheme_expr - (begin) with no expressions, returning NIL\n");
                #endif
                reg nil_val; nil_val.t = NIL;
                return nil_val;
            }

            if (expr.cdr->t != PAIR) {
                printf("ERROR: Malformed begin expression - body is not a list\n");
                exit(1);
            }

            reg* current_stmt_node = expr.cdr;
            while(current_stmt_node != NULL && current_stmt_node->t == PAIR &&
                  current_stmt_node->cdr != NULL && current_stmt_node->cdr->t == PAIR) {
                if (current_stmt_node->car == NULL) {
                    printf("ERROR: Malformed begin - null expression in body\n"); exit(1);
                }
                eval_scheme_expr(*(current_stmt_node->car), current_eval_env);
                current_stmt_node = current_stmt_node->cdr;
            }
            if (current_stmt_node == NULL || current_stmt_node->car == NULL) {
                printf("ERROR: Malformed begin - missing last expression\n"); exit(1);
            }
            if (current_stmt_node->cdr != NULL && current_stmt_node->cdr->t != NIL) {
                printf("ERROR: Malformed begin expression - improper list of statements\n");
                exit(1);
            }
            expr = *(current_stmt_node->car);
            continue;
        }

        // APPLICATION: (proc-expr arg1-expr ...)
        // This is the default case for a PAIR that is not a special form.
        #ifdef DEBUG_VM
        printf("DEBUG: eval_scheme_expr - APPLICATION: "); write_obj(expr); puts("");
        #endif

        reg proc_expr = first_elem; // Correct: first_elem was *(expr.car)
        reg* arg_exprs_list_ptr = expr.cdr; // This is the list of argument expressions (unevaluated)

        #ifdef DEBUG_VM
        printf("DEBUG: eval_scheme_expr - Evaluating procedure part\n");
        #endif
        reg proc_obj = eval_scheme_expr(proc_expr, current_eval_env);
        #ifdef DEBUG_VM
        printf("DEBUG: eval_scheme_expr - Evaluated procedure: "); write_obj(proc_obj); 
        if (proc_obj.t == PRIMITIVE_PROC) {
            printf(", c_primitive_proc before check = %p", (void*)proc_obj.c_primitive_proc);
        }
        puts("");
        #endif

        // Evaluate arguments and build a list of argument values
        reg evaluated_args_list_obj;
        evaluated_args_list_obj.t = NIL;
        reg* temp_reversed_args_head = alloc_reg(); // For building args list in reverse
        temp_reversed_args_head->t = NIL;

        reg* current_arg_expr_node = arg_exprs_list_ptr;
        #ifdef DEBUG_VM
        printf("DEBUG: eval_scheme_expr - Evaluating arguments...\n");
        #endif
        while(current_arg_expr_node != NULL && current_arg_expr_node->t == PAIR) {
            if (current_arg_expr_node->car == NULL) { 
                printf("ERROR: Malformed application - null argument expression\n"); exit(1); 
            }
            reg current_arg_val = eval_scheme_expr(*(current_arg_expr_node->car), current_eval_env);
            
            reg* current_arg_val_ptr = alloc_reg(); // cons needs reg*
            memcpy(current_arg_val_ptr, &current_arg_val, sizeof(reg));
            
            temp_reversed_args_head = cons(current_arg_val_ptr, temp_reversed_args_head);
            current_arg_expr_node = current_arg_expr_node->cdr;
        }
        // Check if the argument list was proper
        if (current_arg_expr_node != NULL && current_arg_expr_node->t != NIL) {
            printf("ERROR: Malformed application - improper list of argument expressions\n");
            exit(1);
        }

        // Reverse the temp_reversed_args_head to get args in correct order
        reg* final_args_list_head = alloc_reg();
        final_args_list_head->t = NIL;
        reg* p = temp_reversed_args_head;
        while (p != NULL && p->t == PAIR) {
            // p->car is already a reg* pointing to the evaluated argument value
            final_args_list_head = cons(p->car, final_args_list_head);
            p = p->cdr;
        }
        evaluated_args_list_obj = *final_args_list_head;
        #ifdef DEBUG_VM
        printf("DEBUG: eval_scheme_expr - Evaluated arguments list: "); write_obj(evaluated_args_list_obj); puts("");
        #endif

        // Now, apply the procedure
        if (proc_obj.t == CLOSURE) {
            #ifdef DEBUG_VM
            printf("DEBUG: eval_scheme_expr - Applying closure\n");
            #endif
            reg* formal_params_list = proc_obj.vars; // could be list or symbol
            int variadic = proc_obj.variadic;
            reg* body_expr_ptr = proc_obj.body;
            reg* captured_env = proc_obj.env;
            if (captured_env == NULL) {
                captured_env = env;
            }

            if (!variadic) {
                int params_count = list_length(*formal_params_list);
                int args_count = list_length(evaluated_args_list_obj);
                if (params_count != args_count) {
                    printf("ERROR: Arity mismatch. Expected %d arguments, got %d.\n", params_count, args_count);
                    exit(1);
                }
            }

            reg* args_list_obj_ptr = alloc_reg();
            memcpy(args_list_obj_ptr, &evaluated_args_list_obj, sizeof(reg));

            reg* params_for_env = formal_params_list;
            reg* arg_values_for_env = args_list_obj_ptr;
            if (variadic) {
                reg* nil_for_var = alloc_reg();
                nil_for_var->t = NIL;
                params_for_env = cons(formal_params_list, nil_for_var);
                reg* nil_binding_tail = alloc_reg();
                nil_binding_tail->t = NIL;
                arg_values_for_env = cons_ptr(args_list_obj_ptr, nil_binding_tail);
            }

            reg* new_frame_bindings = cons(params_for_env, arg_values_for_env);
            reg* eval_env = cons(new_frame_bindings, captured_env);

            expr = *body_expr_ptr;
            current_eval_env = eval_env;
            continue;
        } else if (proc_obj.t == PRIMITIVE_PROC) {
            #ifdef DEBUG_VM
            printf("DEBUG: eval_scheme_expr - Applying PRIMITIVE_PROC, proc_obj.c_primitive_proc = %p\n", (void*)proc_obj.c_primitive_proc);
            #endif
            if (proc_obj.c_primitive_proc == NULL) {
                printf("ERROR: Primitive procedure object has NULL function pointer!\n");
                exit(1);
            }
            return proc_obj.c_primitive_proc(evaluated_args_list_obj);
        }
        else {
            printf("ERROR: Attempt to apply non-procedure/non-primitive: "); write_obj(proc_obj); puts("");
            exit(1);
        }
    }

    printf("ERROR: eval_scheme_expr - Unhandled expression type or structure AFTER PAIR CHECKS: "); write_obj(expr); puts("");
    // Fallback or error for unhandled expressions
    reg error_val;
    error_val.t = NIL; // Or a specific error type
    return error_val;
    }
}

