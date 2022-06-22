Tradable Cash Flow
==================

The Tradeable cashflow provides the deployer with an NFT. Whoever is the owner
of the NFT will receive a consolidated stream for all the streams the contract
receives. It is composed of two contracts: TradeableCashflow and RedirectAll.
The first contains the NFT logic (which is mainly inherited from OpenZeppelin)
and the second contains the logic which condenses the streams, and has a
function which changes the receiver of the condensed stream.

# Usage

## Run tests

```bash
yarn install
yarn build
yarn test
```

**Run property tests with Hot Fuzz**

This project includes property tests based on [Hot Fuzz](../../../packages/hot-fuzz/), which is itself an [Echidna](https://github.com/crytic/echidna) wrapper.  

You can run it with

```bash
npx hot-fuzz contracts/TradeableCashFlowHotFuzz.yaml
```
