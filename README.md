# Superfluid Protocol

The Superfluid Protocol is a framework that realizes the real-time finance vision
where user accounts are connected together, and transactions can happen between
user accounts instantaneously as a result.

This repository implements the superfluid protocol as Ethereum contracts. It also
contains a Javascript SDK for developing Web3 applications using the superfluid
protocol.

For technical document, references and tutorials, etc, please refer to the
[docs site](http://docs.superfluid.finance/).

# Integration

It is recommended that you use our JS SDK to interact with the protocol.

Please see [/packages/js-sdk](./packages/js-sdk) for documentation.

# Installation

To install, using its npm package is recommended.

```
$ npm install @superfluid-finance/ethereum-contracts
```

# Development

To develop the project, pull the repository from GitHub and install its
dependencies. You will need npm and jq installed.

```bash
sudo apt-get install jq

git clone https://github.com/superfluid-finance/ethereum-contracts
cd ethereum-contracts
npm ci
```

## Linting

Javascript is linted using [eslint](https://eslint.org/).

Solidity is linted using [solhint](https://protofire.github.io/solhint/)

## Testing

For any feature development, test driven development is recommended:

```
$ npm run dev
```

This will detect any code changes then run lint, build and test suite.

**NB!**: Since these tests take long time to execute, it is quite possible
that you want to use the [execlusive tests](https://mochajs.org/#exclusive-tests)
feature from MochaJS to speed up isolated feature development.

There are three major test suite:

-   Contracts (test/contracts.test.js)
-   Deployment (test/deployment.test.js)
-   SDK (test/sdk.test.js)

Each contracts test suite is named as `test/{Type}/{ContractName}.test.js`.

Deployment test is for testing the deployment script.

SDK test is to test the JS SDK.

## Code Coverage

To run the coverage tests please use:

```
$ truffle run test-coverage
```

This step is not integrated with the unit test because of the time it consumes to execute.
