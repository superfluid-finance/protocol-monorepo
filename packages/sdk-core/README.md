<h1 align="center">Welcome to @superfluid-finance/sdk-core 👋
</h1>
<div align="center">
<img  width="300" padding="0 0 10px" alt="Superfluid logo" src="https://github.com/superfluid-finance/protocol-monorepo/raw/dev/sf-logo.png" />
<p>
  <a href="https://www.npmjs.com/package/@superfluid-finance/sdk-core" target="_blank">
    <img alt="Version" src="https://img.shields.io/npm/v/@superfluid-finance/sdk-core.svg">
  </a>
  <a href="https://codecov.io/gh/superfluid-finance/protocol-monorepo/tree/dev/packages/sdk-core">
    <img src="https://codecov.io/gh/superfluid-finance/protocol-monorepo/branch/dev/graph/badge.svg?token=LJW5NDGEJ9&flag=sdk-core"/>
  </a>
  <a href="#" target="_blank">
    <img alt="License: MIT" src="https://img.shields.io/badge/License-MIT-yellow.svg" />
  </a>
  <a href="https://twitter.com/Superfluid_HQ/" target="blank">
    <img alt="Twitter: Superfluid_HQ" src="https://img.shields.io/twitter/follow/Superfluid_HQ.svg?style=social" />
  </a>
</p>
</div>

### 🏠 [Homepage](https://superfluid.finance)

### ✨ [Superfluid App](https://app.superfluid.finance/)

### 📖 [Docs](https://docs.superfluid.finance)

# Introduction
SDK-Core is an application framework for interacting with the Superfluid Protocol without Solidity knowledge.

More specifically, SDK-Core is a wrapper library around `@superfluid-finance/ethereum-contracts` which allows web developers to interact with the Superfluid contracts.
Under the hood, SDK-Core leverages TypeScript, ethers.js and The Graph and GraphQL.

## Important Disclaimer
SDK-Core is in early active development and can have breaking releases without warning and without consideration for semantic versioning.

# Features
* Minimal Framework initialization (`chainId` and `provider`)
* New Operation syntax for transactions
* Create/Update/Delete Agreement Operations (Constant Flow Agreement and Instant Distribution Agreement)
* SuperToken Operations
* Subgraph-powered GraphQL querying with ordering and pagination
* Event subscription
* CFA/IDA/SuperToken Web3 Getters
* Batch Call functionality for batching multiple Superfluid Operations in one transaction

# Notable Used Technologies
* TypeScript
* Ethers
* GraphQL

# Prerequisites

> NOTE: You need to have graphql and ethers installed as a dependency in order to use SDK-Core:

```bash
yarn install graphql ethers
```


To get the package up and running you'll need to install the necessary dependencies and build the project:

```bash
yarn install && yarn build
```

> NOTE: `yarn build` has a prerequisite that the contracts in `ethereum-contracts` have been built.

# Tutorial

For the best and most immersive experience of learning how to use the main sdk-core features, I would highly recommend heading over to our [interactive tutorials](https://docs.superfluid.finance/superfluid/protocol-developers/interactive-tutorials/sdk-initialization).
