scheme-to-c - The quest to write a Scheme->C compiler
======================================================

This is an experiment in compiling Scheme to C.

The project now uses the Python program `scheme_to_c.py` as the compiler.
It reads a Scheme source file and emits C. A typical invocation is:

```bash
python3 scheme_to_c.py input.scm output.c
```
Then compile the generated C file together with `vm.c`:

```bash
clang -O2 -Wall -Wextra -o program output.c vm.c -lgc
./program
```

*Disclaimer*: I do not have any background in writing compilers. Pull
requests are welcome!

Also included in this repository:

- `scheme_to_c.py` - the Python implementation of the compiler
- `vm.c` - the C runtime providing objects and primitive procedures

Compiler overview
-----------------
The compiler parses Scheme code and performs several passes:

1. **A-normal form** transformation simplifies nested expressions.
2. **Lambda analysis** collects free variables and converts lambdas
   into closures with environment structs.
3. C code is generated that links against `vm.c`, which provides the
   runtime system and uses the Boehm GC.

Running tests
-------------

The test suite lives in the `tests` directory. Each Scheme file has an
`.expected` file containing its output. To run all tests using the Python
compiler invoke:

```bash
python3 tests/run_tests.py
```


Grammar of input language
-------------------------

``` scheme
<expr> := #t | #f | <number> | <string> | <sym>
         (quote <expr>)
         (if <expr> <expr> <expr>)
         (begin <expr> ...)
         (lambda (<sym> ...) <expr> ...)
         (let ((<sym> <expr>) ...) <expr>)
         (let* ((<sym> <expr>) ...) <expr>)
         (letrec ((<sym> <expr>) ...) <expr>)
         (set! <sym> <expr>)
         (cond (<expr> <expr> ...) ...)
         (label <sym>)
         (goto <sym>)
         (<expr> <expr> ...)
```

Primitive procedures such as arithmetic operations, list manipulation,
comparators and string functions are provided by the runtime.

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
- [x] Implement closures (i.e. `lambda`)
    -  [x] Implement frames and environments
- [x] Implement define (in the sense of functions)
- [ ] Implement strings (vectors still TODO) and their respective operations
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
