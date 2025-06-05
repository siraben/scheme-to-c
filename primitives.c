#include "primitives.h"

// Helper functions used by primitive implementations
static const char* type_name(type t) {
    switch (t) {
        case FIXNUM: return "FIXNUM";
        case CHAR: return "CHAR";
        case BOOLEAN: return "BOOLEAN";
        case NIL: return "NIL";
        case PAIR: return "PAIR";
        case SYMBOL: return "SYMBOL";
        case STRING: return "STRING";
        case CLOSURE: return "CLOSURE";
        case PRIMITIVE_PROC: return "PRIMITIVE_PROC";
        default: return "UNKNOWN";
    }
}

static reg expect_single_arg(reg args, const char* prim, type t_expected) {
    if (args.t != PAIR || args.car == NULL ||
        (args.cdr != NULL && args.cdr->t != NIL)) {
        printf("ERROR: primitive '%s': requires exactly one argument\n", prim);
        exit(1);
    }
    reg arg = *(args.car);
    if (t_expected != TYPE_ANY && arg.t != t_expected) {
        printf("ERROR: primitive '%s': argument must be %s\n", prim, type_name(t_expected));
        exit(1);
    }
    return arg;
}

static void expect_two_args(reg args, const char* prim,
                            type t1, type t2,
                            reg* out1, reg* out2) {
    if (args.t != PAIR || args.car == NULL ||
        args.cdr == NULL || args.cdr->t != PAIR || args.cdr->car == NULL ||
        (args.cdr->cdr != NULL && args.cdr->cdr->t != NIL)) {
        printf("ERROR: primitive '%s': requires exactly two arguments\n", prim);
        exit(1);
    }
    *out1 = *(args.car);
    *out2 = *(args.cdr->car);
    if ((t1 != TYPE_ANY && out1->t != t1) ||
        (t2 != TYPE_ANY && out2->t != t2)) {
        printf("ERROR: primitive '%s': argument type mismatch\n", prim);
        exit(1);
    }
}

static void expect_three_args(reg args, const char* prim,
                              type t1, type t2, type t3,
                              reg* out1, reg* out2, reg* out3) {
    if (args.t != PAIR || args.car == NULL ||
        args.cdr == NULL || args.cdr->t != PAIR || args.cdr->car == NULL ||
        args.cdr->cdr == NULL || args.cdr->cdr->t != PAIR ||
        args.cdr->cdr->car == NULL ||
        (args.cdr->cdr->cdr != NULL && args.cdr->cdr->cdr->t != NIL)) {
        printf("ERROR: primitive '%s': requires exactly three arguments\n", prim);
        exit(1);
    }
    *out1 = *(args.car);
    *out2 = *(args.cdr->car);
    *out3 = *(args.cdr->cdr->car);
    if ((t1 != TYPE_ANY && out1->t != t1) ||
        (t2 != TYPE_ANY && out2->t != t2) ||
        (t3 != TYPE_ANY && out3->t != t3)) {
        printf("ERROR: primitive '%s': argument type mismatch\n", prim);
        exit(1);
    }
}

reg primitive_plus(reg args_list_obj) {
    #ifdef DEBUG_VM
    printf("DEBUG: primitive_plus called with args: "); write_obj(args_list_obj); puts("");
    #endif
    reg result; result.t = FIXNUM; result.n = 0;

    reg *cur = &args_list_obj;
    if (cur->t == NIL) {
        return result; // sum of zero numbers is 0
    }

    while (cur->t == PAIR) {
        if (cur->car == NULL || cur->car->t != FIXNUM) {
            printf("ERROR: primitive '+': arguments must be FIXNUMs\n");
            exit(1);
        }
        result.n += cur->car->n;
        cur = cur->cdr;
    }

    if (cur->t != NIL) {
        printf("ERROR: primitive '+': improper argument list\n");
        exit(1);
    }

    #ifdef DEBUG_VM
    printf("DEBUG: primitive_plus result: %lld\n", result.n);
    #endif
    return result;
}

reg primitive_zero_p(reg args_list_obj) {
    #ifdef DEBUG_VM
    printf("DEBUG: primitive_zero_p called with args: "); write_obj(args_list_obj); puts("");
    #endif
    reg result; result.t = BOOLEAN;
    reg arg = expect_single_arg(args_list_obj, "zero?", FIXNUM);
    result.b = (arg.n == 0);
    #ifdef DEBUG_VM
    printf("DEBUG: primitive_zero_p result: %s\n", result.b ? "#t" : "#f");
    #endif
    return result;
}

reg primitive_multiply(reg args_list_obj) {
    #ifdef DEBUG_VM
    printf("DEBUG: primitive_multiply called with args: "); write_obj(args_list_obj); puts("");
    #endif
    reg result; result.t = FIXNUM;
    reg a1, a2;
    expect_two_args(args_list_obj, "*", FIXNUM, FIXNUM, &a1, &a2);
    result.n = a1.n * a2.n;
    #ifdef DEBUG_VM
    printf("DEBUG: primitive_multiply result: %lld\n", result.n);
    #endif
    return result;
}

reg primitive_sub1(reg args_list_obj) {
    #ifdef DEBUG_VM
    printf("DEBUG: primitive_sub1 called with args: "); write_obj(args_list_obj); puts("");
    #endif
    reg result; result.t = FIXNUM;
    reg arg = expect_single_arg(args_list_obj, "sub1", FIXNUM);
    result.n = arg.n - 1;
    #ifdef DEBUG_VM
    printf("DEBUG: primitive_sub1 result: %lld\n", result.n);
    #endif
    return result;
}

reg primitive_null_p(reg args_list_obj) {
    #ifdef DEBUG_VM
    printf("DEBUG: primitive_null_p called with args: "); write_obj(args_list_obj); puts("");
    #endif
    reg result; result.t = BOOLEAN;
    reg arg = expect_single_arg(args_list_obj, "null?", TYPE_ANY);
    result.b = (arg.t == NIL);
    #ifdef DEBUG_VM
    printf("DEBUG: primitive_null_p result: %s\n", result.b ? "#t" : "#f");
    #endif
    return result;
}

reg primitive_car(reg args_list_obj) {
    #ifdef DEBUG_VM
    printf("DEBUG: primitive_car called with args: "); write_obj(args_list_obj); puts("");
    #endif
    reg pair_arg = expect_single_arg(args_list_obj, "car", PAIR);
    if (pair_arg.car == NULL) {
         printf("ERROR: primitive 'car': pair has NULL car field.\n");
         exit(1);
    }
    #ifdef DEBUG_VM
    printf("DEBUG: primitive_car result: "); write_obj(*(pair_arg.car)); puts("");
    #endif
    return *(pair_arg.car);
}

reg primitive_cdr(reg args_list_obj) {
    #ifdef DEBUG_VM
    printf("DEBUG: primitive_cdr called with args: "); write_obj(args_list_obj); puts("");
    #endif
    reg pair_arg = expect_single_arg(args_list_obj, "cdr", PAIR);
    if (pair_arg.cdr == NULL) {
         printf("ERROR: primitive 'cdr': pair has NULL cdr field.\n");
         exit(1);
    }
    #ifdef DEBUG_VM
    printf("DEBUG: primitive_cdr result: "); write_obj(*(pair_arg.cdr)); puts("");
    #endif
    return *(pair_arg.cdr);
}

reg primitive_cons(reg args_list_obj) {
    #ifdef DEBUG_VM
    printf("DEBUG: primitive_cons called with args: "); write_obj(args_list_obj); puts("");
    #endif
    reg a1, a2;
    expect_two_args(args_list_obj, "cons", TYPE_ANY, TYPE_ANY, &a1, &a2);
    reg* result_pair_ptr = cons(&a1, &a2);
    if (result_pair_ptr == NULL) { 
        printf("ERROR: internal cons call returned NULL in primitive_cons\n");
        exit(1);
    }
    #ifdef DEBUG_VM
    printf("DEBUG: primitive_cons result: "); write_obj(*result_pair_ptr); puts("");
    #endif
    return *result_pair_ptr; 
}

reg primitive_less_than(reg args_list_obj) {
    #ifdef DEBUG_VM
    printf("DEBUG: primitive_less_than called with args: "); write_obj(args_list_obj); puts("");
    #endif
    reg result;
    result.t = BOOLEAN;

    reg arg1, arg2;
    expect_two_args(args_list_obj, "<", FIXNUM, FIXNUM, &arg1, &arg2);

    result.b = (arg1.n < arg2.n);
    #ifdef DEBUG_VM
    printf("DEBUG: primitive_less_than result: %s\\n", result.b ? "#t" : "#f");
    #endif
    return result;
}

reg primitive_minus(reg args_list_obj) {
    reg result; result.t = FIXNUM;
    reg arg1, arg2;
    expect_two_args(args_list_obj, "-", FIXNUM, FIXNUM, &arg1, &arg2);
    result.n = arg1.n - arg2.n;
    return result;
}

reg primitive_numeric_equal(reg args_list_obj) {
    reg result; result.t = BOOLEAN;
    reg arg1, arg2;
    expect_two_args(args_list_obj, "=", FIXNUM, FIXNUM, &arg1, &arg2);
    result.b = (arg1.n == arg2.n);
    return result;
}

reg primitive_divide(reg args_list_obj) {
    reg result; result.t = FIXNUM;
    reg arg1, arg2;
    expect_two_args(args_list_obj, "/", FIXNUM, FIXNUM, &arg1, &arg2);
    if (arg2.n == 0) {
        printf("ERROR: primitive '/': divisor must not be 0\n");
        exit(1);
    }
    result.n = arg1.n / arg2.n;
    return result;
}

reg primitive_modulo(reg args_list_obj) {
    reg result; result.t = FIXNUM;
    reg arg1, arg2;
    expect_two_args(args_list_obj, "modulo", FIXNUM, FIXNUM, &arg1, &arg2);
    if (arg2.n == 0) {
        printf("ERROR: primitive 'modulo': divisor must not be 0\n");
        exit(1);
    }
    long long m = arg1.n % arg2.n;
    if (m < 0) m += llabs(arg2.n);
    result.n = m;
    return result;
}

reg primitive_greater_than(reg args_list_obj) {
    reg result; result.t = BOOLEAN;
    reg a1, a2;
    expect_two_args(args_list_obj, ">", FIXNUM, FIXNUM, &a1, &a2);
    result.b = (a1.n > a2.n);
    return result;
}

reg primitive_greater_equal(reg args_list_obj) {
    reg result; result.t = BOOLEAN;
    reg a1, a2;
    expect_two_args(args_list_obj, ">=", FIXNUM, FIXNUM, &a1, &a2);
    result.b = (a1.n >= a2.n);
    return result;
}

reg primitive_less_equal(reg args_list_obj) {
    reg result; result.t = BOOLEAN;
    reg a1, a2;
    expect_two_args(args_list_obj, "<=", FIXNUM, FIXNUM, &a1, &a2);
    result.b = (a1.n <= a2.n);
    return result;
}

reg primitive_eqv(reg args_list_obj) {
    reg result; result.t = BOOLEAN;
    reg a1, a2;
    expect_two_args(args_list_obj, "eqv?", TYPE_ANY, TYPE_ANY, &a1, &a2);
    result.b = reg_equal(a1, a2);
    return result;
}

reg primitive_boolean_p(reg args_list_obj) {
    reg result; result.t = BOOLEAN;
    reg arg = expect_single_arg(args_list_obj, "boolean?", TYPE_ANY);
    result.b = (arg.t == BOOLEAN);
    return result;
}

reg primitive_symbol_p(reg args_list_obj) {
    reg result; result.t = BOOLEAN;
    reg arg = expect_single_arg(args_list_obj, "symbol?", TYPE_ANY);
    result.b = (arg.t == SYMBOL);
    return result;
}

reg primitive_procedure_p(reg args_list_obj) {
    reg result; result.t = BOOLEAN;
    reg a = expect_single_arg(args_list_obj, "procedure?", TYPE_ANY);
    result.b = (a.t == CLOSURE || a.t == PRIMITIVE_PROC);
    return result;
}

reg primitive_pair_p(reg args_list_obj) {
    reg result; result.t = BOOLEAN;
    reg arg = expect_single_arg(args_list_obj, "pair?", TYPE_ANY);
    result.b = (arg.t == PAIR);
    return result;
}

reg primitive_number_p(reg args_list_obj) {
    reg result; result.t = BOOLEAN;
    reg arg = expect_single_arg(args_list_obj, "number?", TYPE_ANY);
    result.b = (arg.t == FIXNUM);
    return result;
}

reg primitive_set_car(reg args_list_obj) {
    reg pair_reg, val_reg;
    expect_two_args(args_list_obj, "set-car!", PAIR, TYPE_ANY, &pair_reg, &val_reg);
    reg *pairp = args_list_obj.car; // using original pointer for mutation
    memcpy(pairp->car, args_list_obj.cdr->car, sizeof(reg));
    reg r; r.t = NIL; return r;
}

reg primitive_set_cdr(reg args_list_obj) {
    reg pair_reg, val_reg;
    expect_two_args(args_list_obj, "set-cdr!", PAIR, TYPE_ANY, &pair_reg, &val_reg);
    reg *pairp = args_list_obj.car;
    memcpy(pairp->cdr, args_list_obj.cdr->car, sizeof(reg));
    reg r; r.t = NIL; return r;
}

reg primitive_list(reg args_list_obj) {
    // Since args are already a list, just return a copy
    reg *copy = alloc_reg();
    memcpy(copy, &args_list_obj, sizeof(reg));
    return *copy;
}

reg primitive_apply_proc(reg args_list_obj) {
    reg proc, arg_list;
    expect_two_args(args_list_obj, "apply", TYPE_ANY, TYPE_ANY, &proc, &arg_list);
    if (proc.t == PRIMITIVE_PROC) return proc.c_primitive_proc(arg_list);
    if (proc.t == CLOSURE) return apply_closure(proc, arg_list);
    printf("ERROR: apply: first argument not a procedure\n"); exit(1);
}

reg primitive_string_p(reg args_list_obj) {
    reg result; result.t = BOOLEAN;
    reg arg = expect_single_arg(args_list_obj, "string?", TYPE_ANY);
    result.b = (arg.t == STRING);
    return result;
}

reg primitive_symbol_to_string(reg args_list_obj) {
    reg arg = expect_single_arg(args_list_obj, "symbol->string", SYMBOL);
    reg* res = make_string(arg.s);
    return *res;
}

reg primitive_string_to_symbol(reg args_list_obj) {
    reg arg = expect_single_arg(args_list_obj, "string->symbol", STRING);
    reg* res = make_symbol(arg.s);
    return *res;
}

reg primitive_string_append(reg args_list_obj) {
    // concatenate all string args
    size_t total = 0;
    reg *cur = &args_list_obj;
    while (cur->t == PAIR) {
        if (cur->car->t != STRING) { printf("ERROR: string-append: all args must be strings\n"); exit(1); }
        total += strlen(cur->car->s);
        cur = cur->cdr;
    }
    char *buf = GC_MALLOC(total + 1);
    buf[0] = '\0';
    cur = &args_list_obj;
    while (cur->t == PAIR) {
        strcat(buf, cur->car->s);
        cur = cur->cdr;
    }
    reg* r = make_string(buf);
    return *r;
}

reg primitive_append(reg args_list_obj) {
    reg list1, list2;
    expect_two_args(args_list_obj, "append", TYPE_ANY, TYPE_ANY, &list1, &list2);
    reg *lst1 = args_list_obj.car;      // original pointer
    reg *lst2 = args_list_obj.cdr->car; // original pointer

    if (lst1->t == NIL) {
        reg *copy = alloc_reg();
        memcpy(copy, lst2, sizeof(reg));
        return *copy;
    }

    if (lst1->t != PAIR) {
        printf("ERROR: append: first argument not list\n");
        exit(1);
    }

    reg *head_copy = NULL;
    reg *tail_copy = NULL;
    reg *cur = lst1;

    while (cur->t == PAIR) {
        reg *new_car = alloc_reg();
        memcpy(new_car, cur->car, sizeof(reg));
        reg *new_pair = alloc_reg();
        new_pair->t = PAIR;
        new_pair->car = new_car;
        new_pair->cdr = alloc_reg();
        new_pair->cdr->t = NIL;

        if (!head_copy) {
            head_copy = new_pair;
        } else {
            tail_copy->cdr = new_pair;
        }
        tail_copy = new_pair;
        cur = cur->cdr;
    }

    if (cur->t != NIL) {
        printf("ERROR: append: improper list as first argument\n");
        exit(1);
    }

    tail_copy->cdr = lst2;
    return *head_copy;
}

reg primitive_number_to_string(reg args_list_obj) {
    reg arg = expect_single_arg(args_list_obj, "number->string", FIXNUM);
    char buf[32];
    snprintf(buf, sizeof(buf), "%lld", arg.n);
    reg* r = make_string(buf);
    return *r;
}

reg primitive_string_length(reg args_list_obj) {
    reg arg = expect_single_arg(args_list_obj, "string-length", STRING);
    reg* r = make_number(strlen(arg.s));
    return *r;
}

reg primitive_string_ref(reg args_list_obj) {
    reg str, idx;
    expect_two_args(args_list_obj, "string-ref", STRING, FIXNUM, &str, &idx);
    if (idx.n < 0 || idx.n >= (long long)strlen(str.s)) { printf("ERROR: string-ref: index out of range\n"); exit(1); }
    return *make_char(str.s[idx.n]);
}

reg primitive_string_set(reg args_list_obj) {
    reg str, idx, ch;
    expect_three_args(args_list_obj, "string-set!", STRING, FIXNUM, CHAR, &str, &idx, &ch);
    reg* strp = args_list_obj.car; // use original pointer for mutation
    if (idx.n < 0 || idx.n >= (long long)strlen(strp->s)) {
        printf("ERROR: string-set!: index out of range\n");
        exit(1);
    }
    strp->s[idx.n] = ch.c;
    reg r; r.t = NIL; return r;
}

reg primitive_list_p(reg args_list_obj) {
    reg lst = expect_single_arg(args_list_obj, "list?", TYPE_ANY);
    reg* cur = &lst;
    while (cur->t == PAIR) cur = cur->cdr;
    reg result; result.t = BOOLEAN; result.b = (cur->t == NIL);
    return result;
}

reg primitive_char_to_integer(reg args_list_obj) {
    reg arg = expect_single_arg(args_list_obj, "char->integer", CHAR);
    reg* r = make_number((unsigned char)arg.c);
    return *r;
}

reg primitive_integer_to_char(reg args_list_obj) {
    reg arg = expect_single_arg(args_list_obj, "integer->char", FIXNUM);
    return *make_char((char)arg.n);
}

reg primitive_char_p(reg args_list_obj) {
    reg result; result.t = BOOLEAN;
    reg arg = expect_single_arg(args_list_obj, "char?", TYPE_ANY);
    result.b = (arg.t == CHAR); return result;
}

static reg make_char_comparison_result(int cond) {
    reg r; r.t = BOOLEAN; r.b = cond; return r;
}

reg primitive_char_equal(reg args_list_obj) {
    reg a, b;
    expect_two_args(args_list_obj, "char=?", CHAR, CHAR, &a, &b);
    return make_char_comparison_result(a.c == b.c);
}

reg primitive_char_less(reg args_list_obj) {
    reg a, b;
    expect_two_args(args_list_obj, "char<?", CHAR, CHAR, &a, &b);
    return make_char_comparison_result((unsigned char)a.c < (unsigned char)b.c);
}

reg primitive_char_greater(reg args_list_obj) {
    reg a, b;
    expect_two_args(args_list_obj, "char>?", CHAR, CHAR, &a, &b);
    return make_char_comparison_result((unsigned char)a.c > (unsigned char)b.c);
}

reg primitive_char_less_equal(reg args_list_obj) {
    reg a, b;
    expect_two_args(args_list_obj, "char<=?", CHAR, CHAR, &a, &b);
    return make_char_comparison_result((unsigned char)a.c <= (unsigned char)b.c);
}

reg primitive_char_greater_equal(reg args_list_obj) {
    reg a, b;
    expect_two_args(args_list_obj, "char>=?", CHAR, CHAR, &a, &b);
    return make_char_comparison_result((unsigned char)a.c >= (unsigned char)b.c);
}

static reg make_string_comparison_result(int cond) { reg r; r.t = BOOLEAN; r.b = cond; return r; }

reg primitive_string_equal(reg args_list_obj) {
    reg a, b;
    expect_two_args(args_list_obj, "string=?", STRING, STRING, &a, &b);
    return make_string_comparison_result(strcmp(a.s, b.s) == 0);
}

reg primitive_string_less(reg args_list_obj) {
    reg a, b;
    expect_two_args(args_list_obj, "string<?", STRING, STRING, &a, &b);
    return make_string_comparison_result(strcmp(a.s, b.s) < 0);
}

reg primitive_string_greater(reg args_list_obj) {
    reg a, b;
    expect_two_args(args_list_obj, "string>?", STRING, STRING, &a, &b);
    return make_string_comparison_result(strcmp(a.s, b.s) > 0);
}

reg primitive_string_less_equal(reg args_list_obj) {
    reg a, b;
    expect_two_args(args_list_obj, "string<=?", STRING, STRING, &a, &b);
    return make_string_comparison_result(strcmp(a.s, b.s) <= 0);
}

reg primitive_string_greater_equal(reg args_list_obj) {
    reg a, b;
    expect_two_args(args_list_obj, "string>=?", STRING, STRING, &a, &b);
    return make_string_comparison_result(strcmp(a.s, b.s) >= 0);
}

reg primitive_length(reg args_list_obj) {
    reg arg = expect_single_arg(args_list_obj, "length", TYPE_ANY);
    if (arg.t != PAIR && arg.t != NIL) {
        printf("ERROR: length: argument not list\n");
        exit(1);
    }
    reg* cur = &arg;
    int n = 0;
    while (cur->t == PAIR) {
        n++;
        cur = cur->cdr;
    }
    if (cur->t != NIL) {
        printf("ERROR: length: improper list\n");
        exit(1);
    }
    return *make_number(n);
}

reg primitive_make_string_prim(reg args_list_obj) {
    if (args_list_obj.t != PAIR || args_list_obj.car == NULL) { printf("ERROR: make-string requires at least length\n"); exit(1); }
    reg len_reg = *(args_list_obj.car);
    if (len_reg.t != FIXNUM) { printf("ERROR: make-string: first arg not number\n"); exit(1); }
    char fill = ' ';
    if (args_list_obj.cdr != NULL && args_list_obj.cdr->t == PAIR) {
        reg ch = *(args_list_obj.cdr->car);
        if (ch.t != CHAR) { printf("ERROR: make-string: second arg not char\n"); exit(1); }
        fill = ch.c;
        if (args_list_obj.cdr->cdr != NULL && args_list_obj.cdr->cdr->t != NIL) { printf("ERROR: make-string: too many args\n"); exit(1); }
    } else if (args_list_obj.cdr != NULL && args_list_obj.cdr->t != NIL) { printf("ERROR: make-string: too many args\n"); exit(1); }
    char *buf = GC_MALLOC(len_reg.n + 1); for (long long i=0;i<len_reg.n;i++) buf[i] = fill; buf[len_reg.n] = '\0';
    return *make_string(buf);
}

reg primitive_string_to_list(reg args_list_obj) {
    reg str = expect_single_arg(args_list_obj, "string->list", STRING);
    reg* head = alloc_reg(); head->t = NIL; reg* tail = head; size_t len = strlen(str.s);
    for (size_t i=0;i<len;i++) {
        reg* cell = alloc_reg(); cell->t = PAIR; cell->car = make_char(str.s[i]); cell->cdr = alloc_reg(); cell->cdr->t = NIL;
        if (tail->t == NIL) { memcpy(tail, cell, sizeof(reg)); } else { tail->cdr = cell; tail = cell; }
    }
    return *head;
}

reg primitive_list_to_string(reg args_list_obj) {
    reg list_arg = expect_single_arg(args_list_obj, "list->string", TYPE_ANY);
    reg* cur = &list_arg;
    size_t count = 0; reg* tmp = cur; while (tmp->t == PAIR) { if (tmp->car->t != CHAR) { printf("ERROR: list->string: element not char\n"); exit(1); } count++; tmp = tmp->cdr; }
    if (tmp->t != NIL) { printf("ERROR: list->string: improper list\n"); exit(1); }
    char *buf = GC_MALLOC(count + 1); size_t i=0; while (cur->t == PAIR) { buf[i++] = cur->car->c; cur = cur->cdr; } buf[count] = '\0';
    return *make_string(buf);
}

reg primitive_substring(reg args_list_obj) {
    reg str, start, end;
    expect_three_args(args_list_obj, "substring", STRING, FIXNUM, FIXNUM, &str, &start, &end);
    if (start.n < 0 || end.n < start.n || end.n > (long long)strlen(str.s)) { printf("ERROR: substring: invalid indices\n"); exit(1); }
    char *buf = GC_MALLOC(end.n - start.n + 1); memcpy(buf, str.s + start.n, end.n - start.n); buf[end.n - start.n] = '\0';
    return *make_string(buf);
}

reg primitive_string_copy(reg args_list_obj) {
    reg str = expect_single_arg(args_list_obj, "string-copy", STRING);
    return *make_string(str.s);
}

reg primitive_string_fill(reg args_list_obj) {
    reg str, ch;
    expect_two_args(args_list_obj, "string-fill!", STRING, CHAR, &str, &ch);
    reg* strp = args_list_obj.car; // mutate original string
    size_t len = strlen(strp->s);
    for (size_t i=0; i<len; i++) strp->s[i] = ch.c;
    reg r; r.t = NIL; return r;
}


#define X(name, str) {str, primitive_##name},
const primitive_def primitive_table[] = {
PRIMITIVE_LIST
#undef X
};
const size_t primitive_table_count = sizeof(primitive_table)/sizeof(primitive_table[0]);

