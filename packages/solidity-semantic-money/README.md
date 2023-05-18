Solidity Semantic Money Library
===============================


This is the first release of the solidity semantic money library, used for building GDA in SuperToken v1 and SuperToken
v2. The audience of this is the implementers of SuperTokens on any target system.

:warning: Note that many concepts are brand new without prio-art, and the time to provide the detailed documentation for
the new idea is limited. As a result, considering the fairly limited audience for this library, for ones actually
interested in using it, instead of asking for the documentation, one should contact the author and Superfluid team for
further assistance to your work.


# What's in the Bag

- A Solidity library for building the general payment primitives aspect of the semantic money is ready:
  - A pure functional solidity library: `SemanticMoney`.
    - Property based testing using foundry fuzz.
    - (Limited, WIP) Certora formal verification using CVL (Certora Verification Language) v1.
  - A token monad using abstract contract approach: `TokenMonad.
  - Toy models for `ToySuperToken` & `TokenSuperfluidPool`, as reference implementation for GDA & SuperToken v2.
    - Property based testing using foundry fuzz, notably also with a randomized steps generation testing in it (to be replaced by foundy invariance testing in the future).
  - Toy model for Aqueduct, a ZILMM (Zero Intermediate Liquidity  Money Market).
    - Property based testing using foundry fuzz.

# References

- A work-in-progress specification written in Haskell can be found in the same repo as package `spec-haskell`.
- A work-in-progress yellowpaper explaining the idea can be downloaded
  [here](https://semantic.money/assets/semantic-money-yellowpaper1.pdf)
