# Data Integrity Test

## Usage

To run the data integrity tests, just run:

```bash
yarn integrity <NETWORK>
```

You can find a list of `<NETWORK>` in `hardhat.config.ts`. You will have to provide a `NETWORK_PROVIDER_URL` for the network you want to run the integrity tests on in the `.env` file.

> Note: You can use `.env.example` as a template for your `.env` file.

## Goals

- To ensure that the mapping functions of the Subgraph are correct and that the indexers are correctly capturing the events and creating the entities
- To double check protocol level invariants on production data, just in case anything is incorrect

## Issues

- Can only batch one call at a time because of fetch error (will probably need to do a pause in between)
- We are not comparing the sum of all realtime balances against the total supply (AUM) of a SuperToken. This ensures that the protocol is solvent and the shared liquidity is "safe"