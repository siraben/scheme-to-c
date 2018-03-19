#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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
static reg eax, ebx;

typedef struct llist {
  reg curr;
  struct llist *next;
} llist;

llist *stack = 0;

// Push eax onto stack
static inline void push() {
  llist *head, *prev = stack;
  head = malloc(sizeof(llist));
  head->curr = eax;
  head->next = prev;
  stack = head;
}

// Replace eax with top of stack
static inline void pop() {
  llist *tail = stack->next;
  eax = stack->curr;
  free(stack);
  stack = tail;
}

// Compare contents of eax with top of stack
static inline void cmp() {
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

reg *car(reg *head) {
  if(head->t == PAIR) {
    return head->car;
  }
  return 0;
}

reg *cdr(reg *head) {
  if(head->t == PAIR) {
    return head->cdr;
  }
  return 0;
}

static inline void print(reg r) {
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
        free(head);
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

// Collatz program
// (begin
//      (define a 27)
//      (label foo)
//      (display a)
//      (if (eq? 1 a)
//          (goto end)
//          (if (eq? 1 (remainder a 2))
//              (begin
//                (set! a (+ (* 3 a) 1))
//                (goto foo))
//              (begin
//                (set! a (/ a 2))
//                (goto foo))))
//      (label end))

int main(int argc, char const *argv[])
{
eax.t = FIXNUM;
eax.n = 27;
reg a = eax;
foo:
print((a));
puts("");
eax.t = FIXNUM;
eax.n = 1;
push();
eax = a;
cmp();
if (!eax.b){goto label2;};
goto end;
goto label1;
label2:;
eax.t = FIXNUM;
eax.n = 1;
push();
eax = a;
push();
eax.t = FIXNUM;
eax.n = 2;
al = eax.n;
pop();
eax.n %= al;
cmp();
if (!eax.b){goto label4;};
eax.t = FIXNUM;
eax.n = 3;
push();
eax = a;
al = eax.n;
pop();
eax.n *= al;
push();
eax.t = FIXNUM;
eax.n = 1;
al = eax.n;
pop();
eax.n += al;
a = eax;
goto foo;
goto label3;
label4:;
eax = a;
push();
eax.t = FIXNUM;
eax.n = 2;
al = eax.n;
pop();
eax.n /= al;
a = eax;
goto foo;
label3:;
;
label1:;
;
end:
printf("Value of eax: ");
print(eax);
puts("");
}