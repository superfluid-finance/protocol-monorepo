The formal specification of the Superfluid protocol is written in Haskell, namely GHC2021 with GADTs and TypeFamilies.

This package is managed using [cabal](https://www.haskell.org/cabal/) to manage multiple haskell packages:

-   `superfluid-protocol-spec-core` - a library where core concepts, agreements and other sub-systems of the Superfluid
protocol specification are defined.
-   `superfluid-protocol-system-simple` - a library where the type classes of the Superfluid protocol specifications are
instantiated with simple data types for testing purpose.
-   `superfluid-protocol-spec-validator` - an executable for Superfluid protocol specification validation.

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

## Core Library

The core library exposes the _Concepts_, _Agreements_, _SubSystems_ and _System_ modules.

### Concepts Module

These are the core Superfluid protocol concepts expressed in multi-parameter type classes.

**Liquidity & TypedLiquidty**

These related type classes defines the integral numeric types where a _Superfluid system_ can attach different meaning
to, such as minbreated tokens, demand deposit, safety deposit, app loan & debt etc.

```
                                                    ┌──────────────┐
                                                    │TaggedTypeable│
                                                    └───────▲──────┘
                                                            │
                                                            │
                                                   ┌────────┴────────┐
                                               ┌───►TypedLiquidityTag◄─────┐
                                               │   └─────────────────┘     │
                                               │                           │
                                               │                           │
                                    ┌──────────┴─────────┐      ┌──────────┴────────┐
                                    │UntappedLiquidityTag│      │*TappedLiquidityTag│
                                    └────────────────────┘      └──────────@────────┘
                                                                           │
                                                                           │
                                                               ┌───────────┴─────────┐
                                                               │AnyTappedLiquidityTag│
                                                               └─────────────────────┘


           ┌───────────────────┐                     ┌──────────────┐
           │(Untyped) Liquidity│                 ┌───►TypedLiquidity◄────┐
           └───────────────────┘                 │   └──────────────┘    │
                                                 │                       │
                                                 │                       │
                                         ┌───────┴─────────┐    ┌────────┴──────┐
                                         │UntappedLiquidity│    │TappedLiquidity│
                                         └─────────────────┘    └───────────────┘
```

**Timestamp**

`Timestamp ts` type class defines the integral numeric types for representing time in a _Superfluid system_.

**RealtimeBalance**

`RealtimeBalance rtb lq` type class defines a vector of `Liquidity`, and itself is also an instance of `Num`.

**Address**

`Address addr` type class defines types which index the accounts in a _Superfluid system_.

**SuperfluidTypes**

`SuperfluidTypes sft` type family indexes the base types used in a _Superfluid system_.

**Agreement**

`AgreementAccountData aad sft` type class defines agreement account data, which provides a part of the real-time balance for the account.

`AgreementContractData acd sft` type class defines agreement contract data, which binds several `AgreementAccountData` for agreement operations to work with.

`AnyAgreementAccountData sft` a existential GADT all existing `AgreementAccountData`.

### Agreements Modules

These are instances of the agreement concept:

-   **TransferableBalanceAgreement**: the payment modality as we are all familiar with, i.e. instant settlement between
    two parties.
-   **ConstantFlowAgreement**: a streaming payment modality allowing sender to stream money to receiver in constant flow
    rate.
-   **DecayingFlowAgreement**: an alternative streaming payment modality allowing sender to stream money to receiver in
    decaying flow rate that has a half-life.
-   **GeneralDistributionAgreement**: a pubsub-like payment modality allowing publisher to pay its subscribers in
    predefined proportions instantly or in streams.

### System Modules

These encapsulate Superfluid concepts and agreements as `SuperfluidToken` monad for building any _Superfluid system_.

## System Simple Library

The _Simple Superfluid system_ where the Superfluid core type classes are instantiated with simple types mainly for
testing purpose:

-   `Liquidity` is `Wad` (18 decimals Integer).
-   `Timestamp` is `Integer`.
-   `Address` in `String`.
-   `SimpleTokenStateT` is a `SuperfluidToken` monad transformer for building your monad stack with.

## Validator

The validator provides these major modes:

- execute a demo from the preset,
- [TODO] validate the test outputs of any Superfluid protocol implementation against the specification,
- [TODO] execute a scenario test script,
- [TODO] run a protocol simulation environment with REPL.

## Test Suite

It uses test-framework, and its HUnit, QuickCheck2 support packages.

References
==========

- Liquidity ascii art:

https://asciiflow.com/#/share/eJyrVspLzE1VssorzcnRUcpJrEwtUrJSqo5RqohRsrK0NNSJUaoEsowsDICsktSKEiAnRkkBDTyasockFBOTh8WIkMT09NSUkMqC1MSknFScyrCjabuItQnJKFJlSfUnDmegORzk4xSfzMLSzJTMkkpgKGD1DIYRlLgHuxdhEkSGCDHKyXYhqlPJ9yk88IBkaF5JYgF6WCPZoBWCTZqevnAgOg3hA9RVTLnnibHCMa8SV%2BjTwW0QRLHx6GagSKNn9EfTWwhrJ8MROCL1Ec7cSq5isgOcTM1I9mLkZISpIRgSA%2BZepVqlWgCpQbl%2F)
