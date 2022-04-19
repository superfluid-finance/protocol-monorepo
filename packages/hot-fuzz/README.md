Hot Fuzz
========

Testing Superfluid protocol and Super Apps like Simon Pegg.

![Hot Fuzz - Simon Pegg](hot-fuzz-simon.jpg)

Motivations
===========

- Test Superfluid Protocol:
  - Integration level
    - CFA
    - IDA
  - Unit Level:
    - Superfluid (host) as an unit
    - CFA as an unit
    - IDA as an unit
- Test Super Apps
  - Flow Lottery?

How To Use
==========

## Setup

- Download echidna binary from: https://github.com/crytic/echidna

## Run The Fuzzer

```
$ ./fuzz.sh CFAFuzzer
```

## Develop New Fuzzer

1. Write its configuration `contracts/CFAFuzzer.yaml`.
2. Inherit AbstractBaseFuzzer and write a new AbstractFuzzer contract.
3. Create the actual fuzzer contract using multiple inheritance e.g.:
    ```
    contract CFAFuzzer is AbstractCFAFuzzer, BaseFuzzer { }

    contract MultiFuzzer is
        AbstractCFAFuzzer,
        AbstractIDAFuzzer,
        BaseFuzzer { }
    ```

TODOs
=====
