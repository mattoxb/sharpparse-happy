# sharpparse

This is a Haskell parser meant to parse a grammar written in K and produce
a parser using `happy` that will return KAST.

It runs it two stages: the parser generator reads a K file and generates
a corresponding `happy` file.  Currently this resides in `src/Grammar2.y`.

The second stage involves compiling `Grammar2` into a `parser` executable
which then can parse a file written in the specified grammar.

## Running directly from `stack`

To run this directly from `stack`, issue the following two commands.

```
stack build && stack exec parser-generator < test-data/test1.k > src/Grammar2.y
stack build && stack exec parser < test-data/test1.in
```

If you want to specify a start symbol, you can do that with an argument to
the `parser-generator` executable.  For example, `test4.k` has this code:

```
syntax Nat ::= Nat "+" Nat     [klabel ( plus  )]
syntax Nat ::= "s" "(" Nat ")" [klabel ( succ  )]
syntax Nat ::= "."             [klabel (zero)]

syntax Foo ::= Foo "+" Foo     [klabel ( fooplus  )]
syntax Foo ::= "s" "(" Foo ")" [klabel ( foosucc  )]
syntax Foo ::= "."             [klabel (foozero)]
```

The test data `test4-1.in` has this:

```
s(s(.))  + s(s(.))
```

If you don't specify a start symbol, the first token defined will be taken
as start.  In this case, `Nat`.

```
% stack exec parser-generator < test-data/test4.k > src/Grammar2.y
% stack exec parser < test-data/test4-1.in
"plus { } (succ { } (succ { } (zero { } ())),succ { } (succ { } (zero { } ())))"
```

But if we specify `Foo` instead, we get this:

```
% stack exec parser-generator Foo < test-data/test4.k > src/Grammar2.y
% stack exec parser < test-data/test4-1.in
"fooplus { } (foosucc { } (foosucc { } (foozero { } ())),foosucc { } (foosucc { } (foozero { } ())))"
```

Unfortunately, you must rerun the parser-generator if you want to change
the start symbol.


## sharpparse.sh

There is a script "sharpparse.sh" that takes two or three command line
arguments, the k file, the input file, and optionally the start symbol.
It does all the building and checking for you.

# Details

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
