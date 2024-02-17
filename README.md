# Scheme 48

This is a simple implementation of a very strict subset of Scheme written in
Haskell. It is based on the book "Write Yourself a Scheme in 48 Hours" by
Jonathan Tang.

Because I like the way zig handles errors, I decided to implement the error
handling in a similar way. This means that the errors are values that can be
returned from a function, and the caller can decide what to do with it.

## Building

To build the project, you need to have `cabal` installed. Then you can run the
following commands:

```sh
cabal update
cabal install --only-dependencies
cabal build
```

## Running

To run the project, you can use the following command:

```sh
cabal run scheme-test <expression>
```

For example:

```sh
cabal run scheme-test "(+ 1 2)" # => 3
cabal run scheme-test "(if (> 1 2) 1 2)" # => 2
```
