
## A quick and dirty tree editor

Currently contains a mostly-incomplete parser for C, but the idea is
it should be easy enough to extend [tokenizer.rs](src/tokenizer.rs) and
[parser.rs](src/parser.rs) as needed.

Edit [`edit`](src/edit.rs) to describe the desired tree transformation
then build and run:

```
$ make
$ ./qadte input.c --dump-tokens
$ ./qadte input.c --dump-tree
$ ./qadte input.c -o output.c
```
