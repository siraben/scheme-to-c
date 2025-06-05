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
  (define (factorial n)
    (if (= n 0)
        1
        (* n (factorial (- n 1)))))

  (display (factorial 6)))
```

Output
------

``` c
// -- END GENERATED C PREAMBLE --
int main(void)
{
initialize_global_env();
GC_INIT();
// Defining potentially recursive function factorial as ['if', ['=', 'n', 0], 1, ['*', 'n', ['factorial', ['-', 'n', 1]]]];
reg* v_0_storage = alloc_reg();;
reg quoted_params_val_for_v_0;;
reg quoted_body_val_for_v_0;;
eax = *make_symbol("n");;
reg tmp_label1 = eax;
eax.t = NIL;
ebx = eax;
eax = *cons(&tmp_label1, &ebx);
quoted_params_val_for_v_0 = eax;;
eax = *make_symbol("if");;
reg tmp_label2 = eax;
...
tmp_label18 = *cons(&ebx, &tmp_label18);
eax = apply_closure(tmp_label17, tmp_label18);
} // End APPLY scope
display_obj(eax);
fflush(stdout);
}
```

Running the code results in:

```
720
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
