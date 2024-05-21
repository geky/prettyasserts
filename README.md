
## prettyasserts

A preprocessor that makes C asserts easier to debug

``` c
// test.c
int main() {
    assert(1 + 1 == 3);
}
```

``` bash
$ make
$ ./prettyasserts test.c -o test.a.c
$ gcc test.a.c -o test
$ ./test
test.c:2:assert: assert failed with 2, expected eq 3
Illegal instruction
```

prettyasserts works by parsing the C syntax tree, applying some
transformations, and then injecting the prettyassert macros found in
[src/prettyassert.c](src/prettyassert.c). The result is more info immediately
available when something goes wrong.

## Asserts

prettyasserts operates at the syntax-tree level and provides a number of
useful transformations:

- Comparison operations are rewritten to print both sides of the expression on
  failure. This work with any of the eq, ne, lt, gt, le, or ge operators:

  ``` c
  assert(1 + 1 > 3);
  ```

  ``` bash
  $ ./test
  test.c:2:assert: assert failed with 2, expected gt 3
  ```

- If prettyasserts sees a common `memcmp` pattern, it gets rewritten to inspect
  the actual contents of the memory:

  ``` c
  assert(memcmp("aaaa", "bbbb", 4) > 0);
  ```

  ``` bash
  $ ./test
  test.c:2:assert: assert failed with "aaaa", expected gt "bbbb"
  ```

- This is also extended to `strcmp`:

  ``` c
  assert(strcmp("aaaa", "aaaaaa") > 0);
  ```

  ``` bash
  $ ./test
  test.c:2:assert: assert failed with "aaaa", expected gt "aaaaaa"
  ```

- By default prettyasserts falls back to normal assert behavior:

  ``` c
  assert(3+3 == 9 && 3*3 == 9);
  ```

  ``` bash
  $ ./test
  test.c:2:assert: assert failed with false, expected eq true
  ```

  prettyasserts currently does not support complex logic. Consider breaking
  this up into multiple assert statements. Then you already know which branch
  is the issue even without prettyasserts.

## Unreachables

prettyasserts also rewrites any unreachable statements it sees:

``` c
__builtin_unreachable()
```

``` bash
$ ./test
test.c:2:unreachable: unreachable statement reached
```

## Arrows

prettyasserts also supports a non-standard operator, the arrow operator: `=>`

The arrow operator acts as an assert at the highest precedence level:

``` c
42 * 2 => 85
```

``` bash
$ ./test
test.c:2:assert: assert failed with 84, expected eq 85
```

This is especially useful for checking the return values of functions without
introducing a temporary variable or cluttering the statement:

``` c
open("missing", O_RDONLY) => 0;
```

``` bash
test.c:2:assert: assert failed with -1, expected eq 0
```

Though a word of caution. The arrow operator is non-standard and should only be
used if you know your code will always be compiled with prettyasserts.

The original motivation was to improve the readability of
[littlefs's tests][littlefs], where this erroring function pattern is extremely
common (14,087 arrows at the time of writing!).

## Debuggability

You may have noticed prettyasserts prints "Illegal instruction" after an
assert failure, which is a bit weird.

The reason is prettyasserts halts with `__builtin_trap()` on assert failure
instead of exiting normally or calling `abort()`. `abort()` may seem like the
correct thing to do, but it moves the assert breakpoint down into stdlib
functions, which can be annowing to navigate out of when debugging.

With `__builtin_trap()`, the debugger stops at exactly where the assert failed,
making debugging quick and easy (ok debugging is never really quick and easy,
but at least getting _to_ the bug is quick and easy, this way you can focus on
the bug and only the bug):

``` bash
$ make
$ ./prettyasserts test.c -o test.a.c
$ gcc -g test.a.c -o test
$ gdb -ex run ./test
Program received signal SIGILL, Illegal instruction.
0x000055555555547d in main () at test.c:2
2           assert(1 + 1 == 3);
(gdb)
```

## Prefixes

It's common for libraries to provide their own assert definitions, if only to
allow user-level configuration. To help with this, prettyasserts can also parse
custom assert/unreachable symbols:

``` c
MY_ASSERT(1 + 1 == 3);
```

```
$ ./prettyasserts -Pmy_ test.c -o test.a.c
$ gcc test.a.c -o test
$ ./test
test.c:2:assert: assert failed with 2, expected eq 3
```

`-P/--prefix-insensitive` matches both lower and upper case symbols,
while `-p/--prefix` only matches the exact case.

By default, prettyasserts looks for the following symbols:

- `assert`
- `__builtin_assert`
- `unreachable`
- `__builtin_unreachable`

Though these can be disabled with the `-n/--no-defaults` flag.

## Completeness

WARNING! prettyasserts is intentionally incomplete.

The original goal was to improve the debugging experience in
[littlefs][littlefs], and supporting all possible C programs was intentionally
left out of scope.

That being said, prettyasserts is built in [qadte][qadte], where the goal is
for the parser, [src/parser.rs](src/parser.rs), and the tokenizer,
[src/tokenizer.rs](src/tokenizer.rs), to be easy to extend when needed.

PRs are also welcome.

## _S p e e e e e d_

But wait, doesn't littlefs already provide this with the suspiciously similarly
named [prettyasserts.py][prettyasserts.py] script?

Yes, but the number of tests in littlefs is constantly increasing, and
prettyasserts.py just couldn't keep up. Minimizing the edit->compile->run
roud-trip-time is also incredibly important for debugging experience.

The main culprit is lfs.t.c, a file generated by littlefs's testing framework
that includes basically the entire littlefs codebase as well as any internal
tests (tests that need access to internal symbols). At the time of writing,
this file ends up with 42,231 lines of code, which unfortunately can't easily
be parallelized by virtue of being a single file.

``` bash
$ time ./scripts/prettyasserts.py -Plfs_ lfs.t.c -o lfs.t.a.c
real    0m16.187s
user    0m16.163s
sys     0m0.025s
```

This is ~6x as long as the actual compilation:

``` bash
$ time gcc -c -O0 -I. lfs.t.a.c -o lfs.o
real    0m2.466s
user    0m2.345s
sys     0m0.105s
```

To be clear, this isn't entirely Python's fault. prettyasserts.py was a quick
script and not intended to be performant. In particular, prettyasserts.py does
quite a number of string allocations. The Rust version does... basically one?
(and wait, that could be an mmap! hmmmmm...).

Actually, both tools really shine here. Python made it easy to build a
prototype, and Rust made it easy to make that prototype performant.

The result is satisfying:

``` bash
$ time ../prettyasserts/prettyasserts -Plfs_ lfs.t.c -o lfs.t.a.c
real    0m0.504s
user    0m0.464s
sys     0m0.040s
```

Some more intersting numbers:

| tool                          | real time |
|:------------------------------|----------:|
| prettyasserts.py (Python)     | 0m16.187s |
| prettyasserts (Rust, debug)   |  0m4.874s |
| prettyasserts (Rust, release) |  0m0.511s |
| gcc -O0                       |  0m2.466s |
| gcc -Os                       |  0m8.939s |


[qadte]: https://github.com/geky/qadte
[littlefs]: https://github.com/littlefs-project/littlefs
[prettyasserts.py]: https://github.com/littlefs-project/littlefs/blob/master/scripts/prettyasserts.py

