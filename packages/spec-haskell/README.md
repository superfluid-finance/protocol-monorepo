The formal specification of the Superfluid protocol is written in Haskell, namely GHC2021 with additional extensions
such as TypeFamilies & FunctionalDependencies.

Development
===========

**Prerequisite**

- Ubuntu `build-essential` (or equivalents in other OSes)
- `node` (for `npx` command and TDD environment)
- `GHC` >= 9.0.0
- [Cabal-install](https://www.haskell.org/cabal/download.html) >= 3.6.0.0
- It is recommended to use [GHCup](https://www.haskell.org/ghcup/) to setup the `GHC` and `Cabal-install`

**TDD Environment**

```bash
$ # TDD with test suite only
$ make dev
$ # TDD with both test suite and a demo run
$ make dev DEV_TARGETS="test demo-expo"
```

Packages
========

This package is managed using [cabal](https://www.haskell.org/cabal/) to manage multiple haskell packages:

## superfluid-protocol-spec-core

The library where core concepts, agreements and other sub-systems of the Superfluid protocol specification are defined.

## superfluid-protocol-system-simple

The library where the type classes of the Superfluid protocol specifications are instantiated with simple data types for
testing purpose.

The _Simple Superfluid system_ where the Superfluid core type classes are instantiated with simple types mainly for
testing purpose:

-   `Value` is `Wad` (18 decimals Integer).
-   `Timestamp` is `Integer`.
-   `Address` in `String`.
-   `SimpenTokenT` is a `Token` monad transformer for building your monad stack with.

# superfluid-protocol-spec-validator

The executable for Superfluid protocol specification validation.

## Validator

The validator provides these major modes:

- execute a demo from the preset,
- [TODO] validate the test outputs of any Superfluid protocol implementation against the specification,
- [TODO] execute a scenario test script,
- [TODO] run a protocol simulation environment with REPL.

## Test Suites

The packages use test-framework, and its HUnit, QuickCheck2 support packages.
