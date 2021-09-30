<h1 align="center">Welcome to @superfluid-finance/ethereum-contracts 👋
</h1>
<div align="center">
<img  width="300" padding="0 0 10px" alt="Superfluid logo" src="/sf-logo.png" />
<p>
  <a href="https://www.npmjs.com/package/@superfluid-finance/ethereum-contracts" target="_blank">
    <img alt="Version" src="https://img.shields.io/npm/v/@superfluid-finance/ethereum-contracts.svg">
  </a>
  <a href='https://coveralls.io/github/superfluid-finance/protocol-monorepo?branch=dev'>
    <img src='https://coveralls.io/repos/github/superfluid-finance/protocol-monorepo/badge.svg?branch=dev' alt='Coverage Status' />
  </a>
  <a href="#" target="_blank">
    <img alt="License: AGPLv3" src="https://img.shields.io/badge/License-AGPL%20v3-blue.svg" />
  </a>
  <a href="https://twitter.com/Superfluid_HQ/status/" target="_blank">
    <img alt="Twitter: Superfluid_HQ" src="https://img.shields.io/twitter/follow/Superfluid_HQ.svg?style=social" />
  </a>
</p>
</div>

> EVM contracts implementation for the Superfluid Protocol

### 🏠 [Homepage](https://superfluid.finance)

### ✨ [Superfluid App](https://app.superfluid.finance/)

### 📖 [Docs](https://docs.superfluid.finance)

## Usage

If you're building a dapp using existing protocol or Super Token contracts, then you should use [`@superfluid-finance/js-sdk`](/packages/js-sdk). [Here](https://docs.superfluid.finance/superfluid/networks/networks) you can find a list of networks where the Superfluid protocol is already deployed.

If you're building a smart contract that uses Superfluid protocol,
or even your own [SuperApp](https://docs.superfluid.finance/), then great! This is definitely the place to be.

### Installation

```sh
$ yarn add @superfluid-finance/ethereum-contracts
```

### Smart Contract

You can then import Superfluid interfaces or contracts into your contracts like this:

```js
import { IConstantFlowAgreementV1 } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";
```

### Writing Tests

For convenient testing, the package contains deployment scripts which allow you to set up and initialize the protocol and test tokens with a few lines of code.
Currently, this requires [web3.js](https://github.com/ChainSafe/web3.js), support for other frameworks is work in progress.

```js
const deployFramework = require("@superfluid-finance/ethereum-contracts/scripts/deploy-framework");
const deployTestToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-test-token");
const deploySuperToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-super-token");

contract("My Test", accounts => {
    const [admin, bob, carol, dan] = accounts;

    before(async () => {
        await deployFramework(errorHandler, {
            web3,
            from: admin
        });
    });

    beforeEach(async function() {
        await deployTestToken(errorHandler, [":", "fDAI"], {
            web3,
            from: admin
        });
        await deploySuperToken(errorHandler, [":", "fDAI"], {
            web3,
            from: admin
        });
    });
```
In order to write concise testing code, we further recommend the use of [`@superfluid-finance/js-sdk`](/packages/js-sdk) not only in your UI code, but also in JS contract tests.

### Examples

You may also want to check out same example dapps in the [examples folder](https://github.com/superfluid-finance/protocol-monorepo/tree/dev/examples) instead of starting from scratch.
Clone a project, modify and play!

### Deploying Superfluid Protocol

In the section [Writing Tests](#Writing-Tests) deploy scripts are invoked from within JS code.

In truffle projects, this deploy scripts can also be used on a CLI. E.g. in order to deploy to a local ganache dev chain:
```sh
NEW_TEST_RESOLVER=1 DISABLE_NATIVE_TRUFFLE=true truffle --network ganache exec "node_modules/@superfluid-finance/ethereum-contracts/scripts/deploy-test-environment.js"
```

In order to deploy to another network, set the `network` argument accordingly.

`NEW_TEST_RESOLVER=1` forces the script to deploy a new resolver even if there's one already deployed (this is the case on Ethereum testnets). That's useful because otherwise the script would try to use the pre-existing resolver and have failing transactions due to lacking permissions.

`DISABLE_NATIVE_TRUFFLE=true` tells the script to use the contract binaries bundled with the npm package.

After successful execution of this command, you should get something like this:
```
...
======== Super token deployed ========
=============== TEST ENVIRONMENT RESOLVER ======================
export TEST_RESOLVER_ADDRESS=0x43098b8d85Fe90eCE6B055e135759B558d2c0224
```

Run the export command to save TEST_RESOLVER_ADDRESS to your local environment.
This allows tests/scripts running later in the same environment to find and use the contracts you deployed.

## Contributing

If you want to not just interface with Superfluid protocol contracts, but contribute, this is what you need:

### Setup Development Environment

Prerequisites: You need node.js v12+ and yarn installed.

First, check out this repository and cd into it.
```sh
git clone https://github.com/superfluid-finance/protocol-monorepo.git
cd protocol-monorepo/
```

Then install dependencies:

```sh
yarn install
```

Now cd to the contracts directory:
```sh
cd packages/ethereum-contracts/
```

Then prepare an `.env` file (see `.env.template`).
The most important config items are RPC endpoint (`X_PROVIDER_URL`) and a sender account (`X_MNEMONIC`), _X_ being a network specific prefix - e.g. `GOERLI`.
If you provide an actual mnemonic, the key derived at `m/44'/60'/0'/0/0` will be used. You can instead also use private keys (hex format).

In order to get an overview of available config items, look for instances of `process.env` in `truffle-config.js` and in files in the `scripts` folder.


### Testing

We aim to have 100% test coverage. This requires test code to be modular, just like the contracts themselves.
The test file hierarchy in `test/contracts` thus reflects the contract file hierarchy in `contracts`.
Mock contracts reside in `contracts` (not in the `test` directory, as is often the case), because that way `truffle compile` will cache their artifacts in the `build` directory. This considerably speeds up test runs.

You can run either all tests, specific tests or test suites.

Run all tests:
```sh
yarn test
```

Run a specific test using the [execlusive tests](https://mochajs.org/#exclusive-tests) feature of MochaJS:
```sh
yarn pretest
npx truffle test test/contracts/agreements/ConstantFlowAgreementV1.test.js
yarn posttest
```

Run the test suite for core contracts:
```sh
yarn pretest
npx truffle test testsuites/superfluid-core.js
yarn posttest
```

The `pretest` script starts a ganache instance with deterministic accounts in the background, the `posttest` script stops it.
When running tests with `yarn test`, those get executed automatically ([reason](https://docs.npmjs.com/cli/v7/using-npm/scripts#pre--post-scripts)).

### TDD Sessions

When working on the contracts, a test driven approach is recommented.
In order to facilitate that, you can easily set up a test environment which uses ganache with snapshots for much faster test executions:

First, start ganache configured as needed in the background:
```sh
yarn pretest
```
Then deploy a test environment:
```sh
TESTENV_SNAPSHOT_VARS=testenv.ignore.vars npx truffle exec scripts/deploy-test-environment.js : TEST
```
This will deploy the framework and a Super Token named _TEST_ (which is needed by many test cases).
This command writes the address of the newly deployed resolver and snapshot information to a file `testenv.ignore.vars`.
Its contents will look something like this:
```sh
$ cat testenv.ignore.vars
TEST_RESOLVER_ADDRESS=0xF12b5dd4EAD5F743C6BaA640B0216200e89B60Da
TESTENV_EVM_SNAPSHOT_ID=0x1
```

Now you can run selective tests

Now



There are two major test suites:

-   Contracts (test/contracts.test.js) tests the contracts
    Each contracts test suite is named as `test/{Type}/{ContractName}.test.js`.
-   Deployment (test/deployment.test.js) tests the deployment script

```bash
yarn test
```

Since testing can take a long time to execute, you may want to use the [execlusive tests](https://mochajs.org/#exclusive-tests) feature from MochaJS to isolate only the test you want. For example:

```bash
# Only run deployment.test.js
nodemon -x npx truffle test ./test/contracts/superfluid/Superfluid.test.js
```

### Troubleshooting

Superfluid requires the [ERC-1820](https://eips.ethereum.org/EIPS/eip-1820) Registry contract to be deployed. That's because [ERC-777](https://eips.ethereum.org/EIPS/eip-777) - the basis for Super Tokens - depends on it.
If you use the deployment scripts as described above, that will be done automatically. If not, you may want to manually deploy ERC-1820 yourself. You can use `scripts/deploy-erc1820.js` to do so.

In the [scripts folder](/scripts) you can find several scripts for deploying/configuring/querying protocol contracts.


## Show your support

Give a ⭐️ if this project helped you!

---

_This README was generated with ❤️ by [readme-md-generator](https://github.com/kefranabg/readme-md-generator)_
