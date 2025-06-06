#ifndef PRIMITIVES_H
#define PRIMITIVES_H

#include "vm.h"

// Macro list of primitive procedures
#define PRIMITIVE_LIST \
    X(plus, "+") \
    X(zero_p, "zero?") \
    X(multiply, "*") \
    X(sub1, "sub1") \
    X(null_p, "null?") \
    X(car, "car") \
    X(cdr, "cdr") \
    X(cadr, "cadr") \
    X(caddr, "caddr") \
    X(cadddr, "cadddr") \
    X(cddr, "cddr") \
    X(cons, "cons") \
    X(less_than, "<") \
    X(numeric_equal, "=") \
    X(minus, "-") \
    X(divide, "/") \
    X(modulo, "modulo") \
    X(greater_than, ">") \
    X(greater_equal, ">=") \
    X(less_equal, "<=") \
    X(eqv, "eqv?") \
    X(eq, "eq?") \
    X(boolean_p, "boolean?") \
    X(not, "not") \
    X(symbol_p, "symbol?") \
    X(procedure_p, "procedure?") \
    X(pair_p, "pair?") \
    X(number_p, "number?") \
    X(integer_p, "integer?") \
    X(set_car, "set-car!") \
    X(set_cdr, "set-cdr!") \
    X(list, "list") \
    X(map, "map") \
    X(apply_proc, "apply") \
    X(string_p, "string?") \
    X(symbol_to_string, "symbol->string") \
    X(string_to_symbol, "string->symbol") \
    X(string_append, "string-append") \
    X(append, "append") \
    X(number_to_string, "number->string") \
    X(string_length, "string-length") \
    X(string_ref, "string-ref") \
    X(string_set, "string-set!") \
    X(list_p, "list?") \
    X(char_to_integer, "char->integer") \
    X(integer_to_char, "integer->char") \
    X(char_p, "char?") \
    X(char_equal, "char=?") \
    X(char_less, "char<?") \
    X(char_greater, "char>?") \
    X(char_less_equal, "char<=?") \
    X(char_greater_equal, "char>=?") \
    X(char_alphabetic_p, "char-alphabetic?") \
    X(char_numeric_p, "char-numeric?") \
    X(string_equal, "string=?") \
    X(string_less, "string<?") \
    X(string_greater, "string>?") \
    X(string_less_equal, "string<=?") \
    X(string_greater_equal, "string>=?") \
    X(length, "length") \
    X(make_string_prim, "make-string") \
    X(string_to_list, "string->list") \
    X(list_to_string, "list->string") \
    X(substring, "substring") \
    X(string_copy, "string-copy") \
    X(string_fill, "string-fill!") \
    X(newline, "newline") \
    X(for_each, "for-each") \
    X(and, "and") \
    X(error, "error")

// Prototype declarations for primitives
#define X(name, str) reg primitive_##name(reg args_list_obj);
PRIMITIVE_LIST
#undef X

typedef struct {
    const char *name;
    reg (*func)(reg);
} primitive_def;

extern const primitive_def primitive_table[];
extern const size_t primitive_table_count;

#endif // PRIMITIVES_H
