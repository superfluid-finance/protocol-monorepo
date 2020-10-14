Superfluid Protocol
===================

The Superfluid Protocol is a framework that realizes the real-time finance vision
where user accounts are connected together, and transactions can happen between
user accounts instantaneously as a result.

This repository implements the superfluid protocol as Ethereum contracts. It also
contains a Javascript SDK for developing Web3 applications using the superfluid
protocol.

For technical document, references and tutorials, etc, please refer to the
[docs site](http://docs.superfluid.finance/).

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

**NB!**: Since these tests take long time to execute, it is quite possible
that you want to use the [execlusive tests](https://mochajs.org/#exclusive-tests)
feature from MochaJS to speed up isolated feature development.

There are three major test suite:

- Contracts (test/contracts.test.js)
- Deployment (test/deployment.test.js)
- SDK (test/sdk.test.js)

Each contracts test suite is named as `test/{Type}/{ContractName}.test.js`.

Deployment test is for testing the deployment script.

SDK test is to test the JS SDK.

Code Coverage
--------------

To run the coverage tests please use:

```
$ truffle run test-coverage
```

This step is not integraded with the unit test because of the time it consumes to execute.

Integration
===========

It is recommended that you use our JS SDK to interact with the protocol.

**NB!** The SDK is still under development, its API and interfaces can change.

Initialize the SDK
------------------


```
const SuperfluidSDK = require("@superfluid-finance/ethereum-contracts");
const sf = new SuperfluidSDK.Framework({
    version: "0.1.2-preview-20201014", // This is for using different protocol release
    web3Provider: web3.currentProvider // your web3 provider
});

await sf.initialize();

const daiAddress = await sf.resolver.get("tokens.fDAI");
const dai = await sf.contracts.TestToken.at(daiAddress);
const daixWrapper = await sf.getERC20Wrapper(dai);
// assert(daixWrapper.created);
const daix = await sf.contracts.ISuperToken.at(daixWrapper.wrapperAddress);
```

What's In the Bag
--------------------

* `sf.host` : The [truffle contract instance](https://www.trufflesuite.com/docs/truffle/getting-started/interacting-with-your-contracts)
to interact with the host contract (Superfluid.sol).
* `sf.contracts` : The [truffle contract](https://www.trufflesuite.com/docs/truffle/reference/contract-abstractions) objects loaded by the SDK:
  - `IERC20` : The ERC20 Interface.
  - `TokenInfo` : A customary ERC20 token info interface (name/symbol/decimals).
  - `ERC20WithTokenInfo` : A combination of IERC20 and TokenInfo.
  - `TestToken` : A ERC20 Test token.
  - `IResolver` : A simple resolver interface to locate different versions of the contracts.
  - `ISuperfluid` : The Superfluid host contract interface.
  - `ISuperToken` : The Super token contract interface.
  - `IConstantFlowAgreementV1` : The constant flow agreement (v1) contract interface.
  - `IInstantDistributionAgreementV1` : The instant distribution agreement (v1) contract interface.
* Token wrapper convenient functions:
  - `sf.getERC20Wrapper`
  - `sf.createERC20Wrapper`
* `sf.resolver`: The resolver used by the SDK.
  - In test nets, there are some test tokens can be located with the resolver:
    - `fDAI` : The fake DAI. `sf.resolver.get("tokens.fDAI")`.
    - `fUSDC` : The fake USDC. `sf.resolver.get("tokens.fUSDC")`.
* `sf.agreements` :
  - `sf.agreements.cfa` : Constant flow agreement truffle contract instance.
  - `sf.agreements.ida` : Instant distribution agreement truffle contract instance.
