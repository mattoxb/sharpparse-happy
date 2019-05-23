# sharpparse

To use on the command line:

```
stack build && stack exec parser < test-data/test1.k > src/Grammar2.y
stack build && stack exec subparser < test-data/test1.in
```

There is a script "sharpparse.sh" that takes two command line
arguments, the k file and the input file and does the threading for you.

## Details

The file `Grammar.y` is the grammar to parse K syntax declarations.
This should not need to be modified in day-to-day operations.

The executable `parser` will parse a given K syntax specification on
`STDIN` and output a `happy` compatible grammar file.  It is intended
that you copy this over `Grammar2.y` and run `stack build` again.

The nice thing is that if you parse the same grammar more than once
the build stage detects this and doesn't have to recompile anything.

## Test Data

The test data has three kinds of files.  The `.k` files are the syntax
declarations.  The `.in` files contain code in the given language.
The `.out` files are the KAST parses.
