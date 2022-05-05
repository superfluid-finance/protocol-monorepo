Hot Fuzz
========

Testing Superfluid protocol and Super Apps like Simon Pegg.

![Hot Fuzz - Simon Pegg](hot-fuzz-simon.jpg)

How To Use
==========

## Setup

- Download echidna binary from: https://github.com/crytic/echidna

## Develop New Hot Fuzzer

1. Create a new hot fuzz contract inheriting `HotFuzzBase`.
2. Write its configuration `configs/NewHotFuzz.yaml`.
3. Add possible parameterized operations.
4. Add invariance public view functions starting with "echidna_" and returning a bool.

## Run The Fuzzer

```
$ ./hotfuzz NewHotFuzz|SuperHotFuzz
```
