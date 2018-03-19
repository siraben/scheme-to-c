scheme-to-c - The quest to write a Scheme->C compiler
======================================================

This is an experiment in compiling Scheme to C.

*Disclaimer*: I do not have any background in writing compilers. Pull
requests are welcome!

Here is the grammar that can be compiled.

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
     (define a 27)
     (label foo)
     (display a)
     (if (eq? 1 a)
         (goto end)
         (if (eq? 1 (remainder a 2))
             (begin
               (set! a (+ (* 3 a) 1))
               (goto foo))
             (begin
               (set! a (/ a 2))
               (goto foo))))
     (label end))
```

Output
------

``` c
// skipping ~ 200 lines of helper functions
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
```

Running the code results in:

```
Value of eax: 9999
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
