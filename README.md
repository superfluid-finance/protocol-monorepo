Superfluid Protocol
===================

The Superfluid Protocol is a framework that realizes the real-time finance vision
where user accounts are connected together, and transactions can happen between
user accounts instantaneously as a result.

This repository implements the superfluid protocol as Ethereum contracts.

Installation
============

To install, using its npm package is recommended.

```
$ npm install @superfluid-finance/ethereum-contracts
```

Development
===========

To develop the project, pull the repository from GitHub and install its
dependencies. You will need npm installed.

```
$ git clone https://github.com/superfluid-finance/ethereum-contracts
$ cd ethereum-contracts
$ npm ci
```

Linting
-------

Javascripts are linted using [eslint](https://eslint.org/).

Solidity are linted using [solhint](https://protofire.github.io/solhint/)

Testing
-------

For any feature development, test driven development is recommended:

```
$ npm run dev
```

This will detect any code changes then run lint, build and test suite.

There are three major test suite:

- Contracts
- Package
- Deployment

Each contracts test suite is named as `test/ContractName.test.js`.

Package test is for other packages that use this package as a dependency.

Deployment test is for testing the deployment script.

Code Coverage
--------------

To run the coverage tests please use:

```
$ truffle run test-coverage
```

This step is not integraded with the unit test because of the time it consumes to execute.

Integration Steps
-----------------

Use the code similar to the one bellow for your web3 project:

```
const Superfluid = require("@superfluid-finance/ethereum-contracts");
const {
    IERC20,
    TestResolver,
    TestToken,
    IFlowAgreement,
    ISuperToken
}  = Superfluid.load(yourWeb3Provider);
```
