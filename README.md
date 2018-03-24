scheme-to-c - The quest to write a Scheme->C compiler
======================================================

This is an experiment in compiling Scheme to C.

*Disclaimer*: I do not have any background in writing compilers. Pull
requests are welcome!

Also included in this repository:

- A SICP register machine language to C compiler
  - This is in the hopes that an existing Scheme to SICP register
    machine compiler and be used as an IR on the way to C.
- A meta-circular evaluator for Scheme
  - Now includes tests!
  - Includes lexical scoping and proper closures.
  - [ ] Add mutation and `define`


Grammar of input language
-------------------------

``` xml
<expr> := #t
          #f
          (-)<number>
          <sym>
          (eq? <expr> <expr>)
          (boolean? <expr>)
          (fixnum? <expr>)
          (add1 <expr>)
          (sub1 <expr>)
          (zero? <expr>)
          (define <sym> <expr>)
          (let ((<sym> <expr>) ...)
            <body>)
          (car <expr>)
          (cdr <expr>)
          (cons <expr> <expr>)
          (quote <expr>)
          (cond (<expr> <expr>) ...)
          (begin <expr> ...)
          (+ <expr> <expr>)
          (- <expr> <expr>)
          (* <expr> <expr>)
          (/ <expr> <expr>)
          (remainder <expr> <expr>)
          (if <expr> <expr> <expr>)
          (label <sym>)
          (goto <sym>)
```

Example use
-----------

``` scheme
(begin
  (define a (quote (a b c d e)))
  (define b (quote ()))
  (label start)
  (if (null? a)
      (goto end)
      (begin (set! b (cons (car a) b))
             (display a)
             (set! a (cdr a))
             (goto start)))
  (label end)
  (display b))
```

Output
------

``` c
// Skipping ~ 200 lines of helper code
int main(void)
{
GC_INIT();
eax = *make_symbol("a");
push();
eax = *make_symbol("b");
push();
eax = *make_symbol("c");
push();
eax = *make_symbol("d");
push();
eax = *make_symbol("e");
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
reg a = eax;
eax.t = NIL;
reg b = eax;
start:
eax = a;
al = (eax.t == NIL);
eax.t = BOOLEAN;
eax.b = al;
if (!eax.b){goto label2;};
goto end;
goto label1;
label2:;
eax = a;
eax = *car(&eax);
push();
eax = b;
ebx = eax;
pop();
eax = *cons(&eax, &ebx);
b = eax;
print((a));
puts("");
eax = a;
eax = *cdr(&eax);
a = eax;
goto start;
label1:;
;
end:
print((b));
puts("");
}
```

Running the code results in:

```
(e d c b a)
```
Project Goals
-------------

- [x] Be Turing complete!
- [ ] Implement IO
- [ ] Implement closures (i.e. `lambda`)
    -  [ ] Implement frames and environments
- [ ] Implement define (in the sense of functions)
- [ ] Implement strings, vectors and their respective operations
- [ ] Be self-hosting

Futamura Projections
--------------------

Program *a* := Scheme -> C, written in Scheme

Program *b* := (a a) yields a compiler, in this case a C program that
converts Scheme programs to C

(b b) yields b, a C program that converts Scheme programs to C, here
it ends up being a quine.

Then, given an interpreter *x* written in Scheme for a language (say,
Brainfuck), performing (a x) yields a C interpreter for language *x*.

Many more interesting semantic games to be played...
