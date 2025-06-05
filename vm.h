#ifndef VM_H
#define VM_H
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gc/gc.h>

#define malloc(n) GC_MALLOC(n)
#define calloc(m,n) GC_malloc((m)*(n))
#define strdup(a) GC_STRDUP(a)
#define strndup(a,b) GC_strndup(a,b)

#define MAX_SYMBOL_LEN 32

typedef enum type {
    FIXNUM,
    CHAR,
    BOOLEAN,
    NIL,
    PAIR,
    SYMBOL,
    STRING,
    CLOSURE,
    PRIMITIVE_PROC
} type;

#define TYPE_ANY ((type)-1)

typedef struct reg {
    type t;
    union {
        long long n;
        char c;
        char *s;
        int b;
        struct { // PAIR
            struct reg *car;
            struct reg *cdr;
        };
        struct { // CLOSURE
            struct reg *vars;
            struct reg *body;
            struct reg *env;
            int variadic;
        };
        struct reg (*c_primitive_proc)(struct reg args_list);
    };
} reg;

extern int al;
extern reg eax, ebx;
extern reg *env;

int reg_equal(reg a, reg b);
reg *car(reg *head);
reg *cdr(reg *head);
void write_obj(reg r);
void display_obj(reg r);
reg *cons(reg *a, reg *b);
reg *cons_ptr(reg *a, reg *b);
reg *alloc_reg();
reg *make_symbol(char *name);
reg *make_number(long long value);
reg *make_boolean(unsigned int value);
reg *make_string(char *name);
reg *make_char(char ch);
reg* make_closure(reg* params, reg* body_expr, reg* captured_env, int variadic);
reg apply_closure(reg closure_obj, reg args_list_obj);
reg eval_scheme_expr(reg expr, reg* current_eval_env);
void initialize_global_env();
void lookup_in_env(reg *env_ptr);
void set_var_in_env(reg *env_ptr);
int list_length(reg list_obj);

#endif // VM_H
