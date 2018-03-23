#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <assert.h>
#include <stdarg.h>
/* #include <gc/gc.h> */

/* #define malloc(n) GC_MALLOC(n) */
/* #define calloc(m,n) GC_malloc((m)*(n)) */
/* #define strdup(a) GC_STRDUP(a) */
/* #define strndup(a, b) GC_strndup(a, b) */
#define DEBUG 0

#define debug_print(fmt, ...) \
            do { if (DEBUG) fprintf(stderr, fmt, __VA_ARGS__); } while (0)
#define MAX_SYMBOL_LEN 32
typedef enum type {FIXNUM,
  CHAR,
  BOOLEAN,
  NIL,
  PAIR,
  SYMBOL,
  STRING,
  GOTO} type;


typedef struct reg {
  type t;
  union {
    long long n; 
    char c;
    char *s;
    int b;
    struct {
      struct reg *car;
      struct reg *cdr;
    };
    void *g;
  };
} reg;

int al;

void print();
  
typedef void (*test) (reg *a, ...);

typedef struct stack_s {
  reg r[1024];
  int stack_ptr;
} stack_s;

// Explicit stack structure
stack_s *stack;

// Save register r at the top of the stack
void save(reg *r) {
  unsigned int curr = stack->stack_ptr;
  debug_print("Save: %u\n", curr);
  memcpy(&stack->r[curr], r, sizeof(reg));
  stack->stack_ptr++;
  debug_print("Save: %u\n", curr);

}

// Restore the item at the top of the stack to the destination.
void restore(reg *dest) {
  unsigned int curr = stack->stack_ptr;
  debug_print("Restore: %u\n", curr);
  curr -= (curr == 0) ? 0 : 1;
  memcpy(dest, &stack->r[curr], sizeof(reg));
  debug_print("Restore: %u\n", curr);
  stack->stack_ptr = curr;
}

// Compare contents of eax with ebx
void equal_to_primop(reg *eax, reg *ebx)
{
  if (eax->t != ebx->t) {
    // type mismatch
    al = 0;
    return;
  } else {
    int t = eax->t;
    switch(t) {
    case BOOLEAN:
      al = (eax->b == ebx->b);
      break;
    case FIXNUM:
      al = (eax->n == ebx->n);
      break;
    case CHAR:
      al = (eax->c == ebx->c);
      break;
    case NIL:
      al = (eax->t == ebx->t);
      break;
    case SYMBOL:
      if (ebx->t == SYMBOL) {
        al = (strncmp(eax->s, ebx->s, MAX_SYMBOL_LEN - 1) == 0);
      }
      break;
    case STRING:
      if (ebx->t == STRING) {
        al = (strncmp(eax->s, ebx->s, MAX_SYMBOL_LEN - 1) == 0);
      }
      break;
    }
  }
}

void print();

reg *car(reg *head) {
  if(head->t == PAIR) {
    return head->car;
  }
  print(head);
  puts(" is not a pair!");
  exit(1);
}

reg *cdr(reg *head) {
  if(head->t == PAIR) {
    return head->cdr;
  }
  print(head);
  puts(" is not a pair!");
  exit(1);
}

void print(reg r) {
  int t = r.t;
  switch (t) {
  case BOOLEAN:
    printf("%s", r.b ? "#t" : "#f");
    break;
  case FIXNUM:
    printf("%lld", r.n);
    break;
  case CHAR:
    printf("#\%c",r.c);
    break;
  case NIL: 
    printf("()");
    break;
  case SYMBOL: 
    printf("%s", r.s);
    break;
  case STRING:
    printf("\"%s\"", r.s);
    break;
  case GOTO:
    printf("#<INTERNAL_GOTO>");
    break;
  }

  if (t == PAIR) {
    reg *head = calloc(1, sizeof(reg));
    *head = r;
    memcpy(head,&r,sizeof(reg));
    printf("(");
  print_pair:

    print(*car(head));
    if (cdr(head)->t != NIL) {
      printf(" ");
      head = cdr(head);
      if (head->t != PAIR) {
        printf(" . ");
        print(*head);
        goto print_done;
      }
      goto print_pair;
    }
  print_done:
    printf(")");
    free(head);
    return;
  }
}

reg *alloc_reg() {
  reg *res = 0;
  res = calloc(1, sizeof(reg));
  return res;
}

reg *cons(reg *a, reg *b) {
  reg *res, *ac, *bc;
  res = alloc_reg();
  ac = alloc_reg();
  bc = alloc_reg();
  res->t = PAIR;
  memcpy(ac, a, sizeof(reg));
  memcpy(bc, b, sizeof(reg));
  res->car = ac;
  res->cdr = bc;
  return res;
}

reg *make_symbol(const char *name) {
  reg *res;
  res = alloc_reg();
  res->t = SYMBOL;
  res->s = strndup(name, MAX_SYMBOL_LEN);
  return res;
}

reg *make_fixnum(long long value) {
  reg *res;
  res = alloc_reg();
  res->t = FIXNUM;
  res->n = value;
  return res;
}
/* reg *the_empty_list, *true, *false; */


/* reg *make_boolean(unsigned int value) { */
/*   return value ? true : false; */
/* } */

reg *make_string(const char *name) {
  reg *res;
  res = alloc_reg();
  res->t = STRING;
  res->s = strndup(name, MAX_SYMBOL_LEN);
  return res;
}

/* void setup_constants() { */
/*   the_empty_list = alloc_reg(); */
/*   true = alloc_reg(); */
/*   false = alloc_reg(); */
/*   the_empty_list->t = NIL; */
/*   true->t = BOOLEAN; */
/*   true->b = 1; */
/*   false->t = BOOLEAN; */
/*   false->b = 0; */
/* } */

void less_than_primop(reg *a, reg *b)
{
  printf("Comparing ");
  print(*a);
  printf(" < ");
  print(*b);
  assert(a->t == FIXNUM && b->t == FIXNUM);
  al = a->n < b->n;
  printf(" = %d\n", al);
}

void greater_than_primop(reg *a, reg *b)
{
  printf("Comparing ");
  print(*a);
  printf(" > ");
  print(*b);
  assert(a->t == FIXNUM && b->t == FIXNUM);
  al = a->n > b->n;
  printf(" = %d\n", al);
}

reg *sub_primop(reg *a, reg* b)
{
  reg *res;
  res = alloc_reg();
  res->t = FIXNUM;
  if (a->t != FIXNUM || b->t != FIXNUM) {
    printf("sub_primop: Can't subtract ");
    print(*a);
    printf(" and ");
    print(*b);
    exit(1);
  } else {
    res->n = a->n - b->n;
  }
  
  return res;
}

reg *add_primop(reg *a, reg* b)
{
  reg *res;
  res = alloc_reg();
  res->t = FIXNUM;
  if (a->t != FIXNUM || b->t != FIXNUM) {
    printf("add_primop: Can't add ");
    print(*a);
    printf(" and ");
    print(*b);
    exit(1);
  } else {
    res->n = a->n + b->n;
  }
  
  return res;
}

reg *mult_primop(reg *a, reg* b)
{
  reg *res;
  res = alloc_reg();
  res->t = FIXNUM;
  if (a->t != FIXNUM || b->t != FIXNUM) {
    printf("mult_primop: Can't multiply ");
    print(*a);
    printf(" and ");
    print(*b);
    exit(1);
  } else {
    res->n = a->n * b->n;
  }
  
  return res;
}
int main(void) {
stack = calloc(1, sizeof(stack_s));
void (*test)();
reg *msg = alloc_reg();
reg *val = alloc_reg();
reg *n = alloc_reg();
reg *cont;
cont = alloc_reg();
cont->t = GOTO;
cont->g = &&fact_done;
n = make_fixnum(5);
fact_loop:
test = &equal_to_primop;
(*test)((reg*)n,make_fixnum(1));
if (al) {goto base_case;}
save(cont);
save(n);
n = (**sub_primop)((reg*)n,make_fixnum(1));
cont->g = &&after_fact;
goto fact_loop;
after_fact:
restore(n);
restore(cont);
val = (**mult_primop)((reg*)n,(reg*)val);
goto *cont->g;
base_case:
val = make_fixnum(1);
goto *cont->g;
fact_done:
msg = make_string("Value: ");
print(*msg);
puts("");
print(*val);
puts("");
;
}
