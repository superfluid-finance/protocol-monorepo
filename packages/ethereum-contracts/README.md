<h1 align="center">Welcome to @superfluid-finance/ethereum-contracts 👋
</h1>
<div align="center">
<img  width="300" padding="0 0 10px" alt="Superfluid logo" src="/sf-logo.png" />
<p>
  <a href="https://www.npmjs.com/package/@superfluid-finance/ethereum-contracts" target="_blank">
    <img alt="Version" src="https://img.shields.io/npm/v/@superfluid-finance/ethereum-contracts.svg">
  </a>
  <a href="https://codecov.io/gh/superfluid-finance/protocol-monorepo/tree/dev/packages/ethereum-contracts">
    <img src="https://codecov.io/gh/superfluid-finance/protocol-monorepo/branch/dev/graph/badge.svg?token=LJW5NDGEJ9&flag=ethereum-contracts"/>
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

If you're building a smart contract that uses Superfluid protocol, or even your own [SuperApp](https://docs.superfluid.finance/), then great! This is definitely the place to be.

### Installation

Prerequisites: You need node.js v12+ and yarn installed.

Once you have set up your project, cd into its base directory and add the npm package:

```sh
$ yarn add @superfluid-finance/ethereum-contracts
```

### Smart Contract

You can then import Superfluid interfaces or contracts into your contracts like this:

```js
import { IConstantFlowAgreementV1 } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";
```
The paths in the npm package are the same as in this repository.

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

You may also want to check out some example dapps in the [examples folder](https://github.com/superfluid-finance/protocol-monorepo/tree/dev/examples) instead of starting from scratch.
Clone a project, modify and play!

### Deploying Superfluid Protocol

In the example test code, you have seen how to deploy protocol contracts from JS code.
In truffle projects, this deploy scripts can also be used on a CLI. E.g. in order to deploy to a local ganache dev chain:
```sh
CREATE_NEW_RESOLVER=1 DISABLE_NATIVE_TRUFFLE=true truffle --network ganache exec "node_modules/@superfluid-finance/ethereum-contracts/scripts/deploy-test-environment.js"
```

In order to deploy to another network, set the `network` argument accordingly.

`CREATE_NEW_RESOLVER=1` forces the script to deploy a new resolver even if there's one already deployed (this is the case on Ethereum testnets). That's useful because otherwise the script would try to use the pre-existing resolver and have failing transactions due to lacking permissions.

`DISABLE_NATIVE_TRUFFLE=true` tells the script to use the contract binaries bundled with the npm package.

After successful execution of this command, you should get something like this:
```
...
======== Super token deployed ========
=============== TEST ENVIRONMENT RESOLVER ======================
export RESOLVER_ADDRESS=0x43098b8d85Fe90eCE6B055e135759B558d2c0224
```

Run the export command to save `RESOLVER_ADDRESS` to your local environment.
This allows tests/scripts running later in the same environment to find and use the contracts just deployed.

## Contributing

If you want contribute to Superfluid protocol contracts instead of just interfacing with them, the setup is a bit different.

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
The most important config items are RPC endpoint (`<X>_PROVIDER_URL`) and a sender account (`<X>_MNEMONIC`), _<X>_ being a network specific prefix - e.g. `GOERLI_PROVIDER_URL`.
If you provide an actual mnemonic, the key derived at `m/44'/60'/0'/0/0` will be used. You can instead also set private keys (hex format) for `<X>_MNEMONIC`.

In order to get an overview of available config items, look for instances of `process.env` in [truffle-config.js](truffle-config.js) and in files in the [scripts](scripts) folder.


### Testing

We aim to have 100% test coverage. This requires test code to be modular, just like the contracts themselves.
The test file hierarchy in [test/contracts](test/contracts) thus reflects the contract file hierarchy in [contracts](contracts).
Mock contracts reside in [contracts](contracts) (not in the [test](test) directory, as is often the case), because that way `yarn build` will cache their artifacts in the `build` directory. This considerably speeds up test runs.

You can run either all tests, specific tests or test suites.

Run all tests:
```sh
yarn test
```

Run a specific test using the [exclusive tests](https://mochajs.org/#exclusive-tests) feature of MochaJS:
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
When running tests with `yarn test`, those get executed automatically (see [npm docs](https://docs.npmjs.com/cli/v7/using-npm/scripts#pre--post-scripts)).

### TDD Session

When working on the contracts, a test driven approach is recommended.
In order to facilitate that, you can easily set up a test environment which uses ganache with snapshots for much faster test executions and thus iterations:

First, start ganache configured as needed in a terminal window:
```sh
yarn testenv:start
```

Then open another terminal window and continue there.
If you're not planning to modify core contracts, set an environment variable:
```sh
export TESTENV_SNAPSHOT_VARS=testenv.ignore.vars
```
This env variable will be used by the deploy script executed in the next step to persist a resolver address and a ganache snapshot id to the configured file.
Without this variable set, the protocol contracts will be re-deployed for every new test run, making the process considerably slower.
Note: if you want to modify core contracts, you **need** to have them re-deployed after every change, thus should **not** set this env variable.

Next, deploy a test environment:
```sh
yarn testenv:deploy
```
This will deploy the protocol contracts and a Super Token named _TEST_ (which is needed by many test cases).
If you have set the env `TESTENV_SNAPSHOT_VARS` before, the file it's set to should now exist and look something like this:
```sh
$ cat testenv.ignore.vars
RESOLVER_ADDRESS=0xF12b5dd4EAD5F743C6BaA640B0216200e89B60Da
TESTENV_EVM_SNAPSHOT_ID=0x1
```

Next, you should choose the tests relevant for what you're working on using the [only keyword](https://mochajs.org/#exclusive-tests).
You can put the `only` keyword at any level between whole test suites (`only` appended to a top level `describe`) and individual testcases (`it`).
With the testing scope defined, run:
```
yarn dev
```
This has [testsuites/all-contracts.js](testsuites/all-contracts.js) as its entrypoint, but if there's an `only` keyword at any nesting level in any of the tests traversed, only that selected subset of tests will be executed.
The selected test(s) will run once when starting the session and re-run everytime you save changes in a relevant file.

After finishing the session, you can stop the ganache instance you started in the first step (Ctrl-C).
Also, don't forget to remove `only` keywords from test files before making git commits.

### Troubleshooting

Superfluid requires the [ERC-1820](https://eips.ethereum.org/EIPS/eip-1820) Registry contract to be deployed. That's because [ERC-777](https://eips.ethereum.org/EIPS/eip-777) - the basis for Super Tokens - depends on it.
If you use the deployment scripts as described above, that will be done automatically. If not, you may want to manually deploy ERC-1820 yourself. You can use `scripts/deploy-erc1820.js` to do so.

In the [scripts folder](/scripts) you can find several scripts for deploying/configuring/querying protocol contracts.

## Show your support

Give a ⭐️ if this project helped you!

---

_This README was generated with ❤️ by [readme-md-generator](https://github.com/kefranabg/readme-md-generator)_
