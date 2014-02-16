YASC
====

A lisp-to-posix-shell compiler, based on the scheme runtime from [Write yourself a scheme][1]

syntax
======

The language itself is very schemey. It's somewhat vaguely specified, and will be for a while since it's far easier to add syntax than remove it.

The current test suite looks something like:

```scheme
(intrinsic! echo (str)
            ("echo" str))
(define main (lambda ()
               (echo "hello world!")))
(export main)
```

and should compile to:

```bash
main() { echo "hello world!"; }
```

usage
=====

You can either compile files with:

```bash
yasc input.sc
```

and receive the compiled output to stdout, or start a repl with

```bash
yasc
```

[1]: http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
