Superfluid Protocol
===================

The Superfluid Protocol is a collection of Ethereum smart Contracts for building
rails for realtime finance.

Contracts
=========

SuperTokenRegistry
------------------

A global registry of all super tokens that are created with trusted SuperToken
logic contract. It has a governance layer that decides what features are
included in the SuperToken contract. And it also as a permission system that
allows financial managers to manage your tokens.

SuperToken
----------

The token contract that can upgrade any ERC20 token with realtime finance
capabilities, while being ERC20 compatible itself.

FlowAgreement
-------------

A realtime finance primitive that facilitate payments in streams.

SuperMaster
-----------

An optional financial manager for your SuperTokens.

Installation
============

To install Superfluid Protocol, install its npm package is recommended.

```
$ npm install @superfluid/protocol
```

Development
===========

To run the Superfluid Protocol, pull the repository from GitHub and install its
dependencies. You will need npm installed.

```
$ git clone https://github.com/superfluid-finance/superfluid-protocol
$ cd superfluid-protocol
$ npm install
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

There are two major test suite:

- Contracts
- Package

Each contracts test suite is named as `test/ContractName.test.js`.

Package test is for other packages that use this package as a dependency.


Scenario Script
--------------

The is one folder inside the test directory that contains the scenario scripts.
This script are not unit testing, but provide some information about the system scope and interactions.

```
$ truffle test test/bbox/*.js
```

Time tests Script
--------------

Testing smart contract with the factor of time is not a linear and can introduce non deterministic error when running tests.
To give a more robust testing environment there is a script in the folder scripts that will run the tests x amount of times.

```bash
./consistency.sh 20
```

The argument of 20 tells to script to run 20x. The script will only output Errors.
You should run before trying to execute the script.

```bash
chmod u+x consistency.sh
```

Code Coverage
--------------

To run the coverage tests please use:

```
$ truffle run coverage
```

This step is not integraded with the unit test because of the time it consumes to execute.

Integration Steps
-----------------
