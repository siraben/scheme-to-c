#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <gc/gc.h>

#define malloc(n) GC_MALLOC(n)
#define calloc(m,n) GC_malloc((m)*(n))
#define strdup(a) GC_STRDUP(a)
#define strndup(a, b) GC_strndup(a, b)
#define MAX_SYMBOL_LEN 32
typedef enum type {FIXNUM, CHAR, BOOLEAN, NIL, PAIR, SYMBOL, STRING, CLOSURE} type;
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
        // Closure
    struct {
      struct reg *vars;
      struct reg *body;
      struct reg *env;
    };
  };
} reg;
static int al;
reg eax, ebx;

typedef struct llist {
  reg curr;
  struct llist *next;
} llist;

llist *stack = 0;

// Push eax onto stack
void push() {
  llist *head, *prev = stack;
  head = malloc(sizeof(llist));
  head->curr = eax;
  head->next = prev;
  stack = head;
}

// Replace eax with top of stack
void pop() {
  llist *tail = stack->next;
  eax = stack->curr;
  stack = tail;
}

// Compare contents of eax with top of stack
void cmp() {
  ebx = eax;
  pop();
  if (eax.t != ebx.t) {
        // type mismatch
    al = 0;
  } else {
    if (eax.t == BOOLEAN) {
      al = (eax.b == ebx.b);
    } else if (eax.t == FIXNUM) {
      al = (eax.n == ebx.n);
    } else if (eax.t == CHAR) {
      al = (eax.c == ebx.c);
    } else if (eax.t == NIL) {
      al = (eax.t == ebx.t);
    } else if (eax.t == SYMBOL) {
      if (ebx.t == SYMBOL) {
        al = (strncmp(eax.s, ebx.s, MAX_SYMBOL_LEN - 1) == 0);
      }
    }
  }
  eax.t = BOOLEAN;
  eax.b = al;
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
  }

  if (r.t == PAIR) {
    reg *head = calloc(1, sizeof(reg));
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
        printf(")");
        return;
      }
      goto print_pair;
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
  memcpy(bc, b, sizeof(reg));
  res->car = ac;
  res->cdr = bc;
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
    eax = *car(var);
    push();
    eax = symbol;
    cmp();
    if (al == 1) {
      // We found the symbol, "return" the binding in eax.
      eax = *car(binding);
      return;
    }
    // Failed, reloop
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
    // Succeeded.
    if (al == 1) {
      return;
    }
  }
  printf("Unbound symbol: ");
  print(symbol);
  puts("");
  exit(1);
}

reg *env = 0;
int main(int argc, char const *argv[])
{
  GC_INIT();

eax = *make_symbol("g");
push();
eax = *make_symbol("b");
push();
eax = *make_symbol("c");
push();
eax = *make_symbol("d");
push();
eax.t = NIL;
ebx = eax;
pop();
eax = *cons(&eax, &ebx);
ebx = eax;
pop();
eax = *cons(&eax, &ebx);
ebx = eax;
pop();
eax = *cons(&eax, &ebx);
ebx = eax;
pop();
eax = *cons(&eax, &ebx);
push();
eax.t = FIXNUM;
eax.n = 1;
push();
eax.t = FIXNUM;
eax.n = 2;
push();
eax.t = FIXNUM;
eax.n = 3;
push();
eax.t = FIXNUM;
eax.n = 4;
push();
eax.t = NIL;
ebx = eax;
pop();
eax = *cons(&eax, &ebx);
ebx = eax;
pop();
eax = *cons(&eax, &ebx);
ebx = eax;
pop();
eax = *cons(&eax, &ebx);
ebx = eax;
pop();
eax = *cons(&eax, &ebx);
ebx = eax;
pop();
eax = *cons(&eax, &ebx);
push();
eax = *make_symbol("x");
push();
eax = *make_symbol("y");
push();
eax = *make_symbol("z");
push();
eax = *make_symbol("a");
push();
eax.t = NIL;
ebx = eax;
pop();
eax = *cons(&eax, &ebx);
ebx = eax;
pop();
eax = *cons(&eax, &ebx);
ebx = eax;
pop();
eax = *cons(&eax, &ebx);
ebx = eax;
pop();
eax = *cons(&eax, &ebx);
push();
eax.t = FIXNUM;
eax.n = 3;
push();
eax.t = FIXNUM;
eax.n = 4;
push();
eax.t = FIXNUM;
eax.n = 5;
push();
eax.t = FIXNUM;
eax.n = 3;
push();
eax.t = FIXNUM;
eax.n = 4;
push();
eax.t = FIXNUM;
eax.n = 5;
push();
eax.t = NIL;
ebx = eax;
pop();
eax = *cons(&eax, &ebx);
ebx = eax;
pop();
eax = *cons(&eax, &ebx);
ebx = eax;
pop();
eax = *cons(&eax, &ebx);
push();
eax.t = NIL;
ebx = eax;
pop();
eax = *cons(&eax, &ebx);
ebx = eax;
pop();
eax = *cons(&eax, &ebx);
ebx = eax;
pop();
eax = *cons(&eax, &ebx);
ebx = eax;
pop();
eax = *cons(&eax, &ebx);
ebx = eax;
pop();
eax = *cons(&eax, &ebx);
push();
eax.t = NIL;
ebx = eax;
pop();
eax = *cons(&eax, &ebx);
ebx = eax;
pop();
eax = *cons(&eax, &ebx);
 
  ebx.t = NIL;
  // Duplicate the contents of eax
  env = car(cons(&eax, &ebx));

  // eax contains
  // (((g b c d) 1 2 3 4) ((x y z a) 3 4 5 (3 4 5)))
  
  printf("Value of eax: ");
  print(eax);
  puts("");
  print(*env);
  eax = *make_symbol("a");
  puts("");
  print(eax);
  puts("");
  lookup_in_env(env);
  print(eax);
  puts("");
}

