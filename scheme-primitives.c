#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_SYMBOL_LEN 32

typedef enum type {NUMBER, PAIR, SYMBOL, STRING, CLOSURE, NIL, BOOL} type;

typedef struct object {
  type type;
  union {
    long long number;
    unsigned int bool;
    char *symbol;
    struct {
      struct object *car;
      struct object *cdr;
    };
    struct {
      struct object *args;
      struct object *body;
      struct object *env;
    };
    char *string;
  };
} object;

object *alloc_object() {
  object *res = 0;
  res = calloc(1, sizeof(object));
  return res;
}

object *make_symbol(char *name) {
  object *res;
  res = alloc_object();
  res->type = SYMBOL;
  res->symbol = strndup(name, MAX_SYMBOL_LEN);
  return res;
}

object *make_number(long long value) {
  object *res;
  res = alloc_object();
  res->type = NUMBER;
  res->number = value;
  return res;
}

object *make_boolean(unsigned int value) {
  object *res;
  res = alloc_object();
  res->type = BOOL;
  res->bool = value;
  return res;
}

object *make_string(char *name) {
  object *res;
  res = alloc_object();
  res->type = STRING;
  res->symbol = strndup(name, MAX_SYMBOL_LEN);
  return res;
}

object *false_obj, *true_obj, *null_obj;

void init_objects(void) {
  false_obj = alloc_object();
  true_obj = alloc_object();
  null_obj = alloc_object();
  false_obj->type = BOOL;
  true_obj->type = BOOL;
  null_obj->type = NIL;
  false_obj->bool = 0;
  true_obj->bool = 1;
}

object *eqp(object *a, object *b) {
  if (a->type == SYMBOL) {
    if (b->type == SYMBOL) {
      if (strncmp(a->symbol, b->symbol, MAX_SYMBOL_LEN - 1) == 0) {
        return true_obj;
      }
    }
  }
  return false_obj;
}

object *cons(object *a, object *b) {
  object *res;
  res = alloc_object();
  res->type = PAIR;
  res->car = a;
  res->cdr = b;
  return res;
}

object *car(object *head) {
  if(head->type == PAIR) {
    return head->car;
  }
  return 0;
}

object *cdr(object *head) {
  if(head->type == PAIR) {
    return head->cdr;
  }
  return 0;
}

void display(object *obj) {
  if (obj->type == SYMBOL) {
    printf("%s", obj->symbol);
  }
  if (obj->type == BOOL) {
    printf("%s", obj == true_obj ? "#t" : "#f");
  }
  if (obj->type == NUMBER) {
    printf("%lld", obj->number);
  }
  if (obj->type == NIL) {
    printf("()");
  }
  if (obj->type == PAIR) {
    object *head = obj;
    printf("(");
print_pair:
    display(car(head));
    if (cdr(head) != null_obj) {
      printf(" ");
      head = cdr(head);
      if (head->type != PAIR) {
        printf(" . ");
        display(head);
        printf(")");
        return;
      }
      goto print_pair;
    }
    printf(")");
  }
  if (obj->type == STRING) {
    printf("\"%s\"", obj->string);
  }
  if (obj->type == CLOSURE) {
    printf("#<CLOSURE>");
  }
}

object *nullp(object *obj) {
  if (obj == null_obj) {
    return true_obj;
  }
  return false_obj;
}

int main(void) {

  doge.x = 4;
  doge.breed = cat;
  printf("%d, %d\n", doge.x, doge.breed);
  init_objects();
  object *foo, *bar, *baz;
  foo = make_symbol("apples");
  bar = make_symbol("oranges");
  baz = cons(foo, bar);
  display(car(baz));
  display(cdr(baz));
  display(eqp(foo,bar));

  object *a, *b, *c, *d, *list;
  a = make_symbol("a");
  b = make_symbol("b");
  c = make_symbol("c");
  d = make_symbol("d");
  list = cons(a, cons(b, cons(c, cons(d, null_obj))));
  puts("");
  display(list);
  display(cons(baz,list));
  return 0;

}
