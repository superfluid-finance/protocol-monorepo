<h1 align="center">Welcome to Superfluid EVM Contracts (v1) 👋
</h1>
<div align="center">
<img  width="300" padding="0 0 10px" alt="Superfluid logo" src="https://github.com/superfluid-finance/protocol-monorepo/raw/dev/sf-logo.png" />
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

If you're building a dapp using existing protocol or Super Token contracts, then you should use [`@superfluid-finance/sdk-core`](/packages/sdk-core). [Here](https://docs.superfluid.finance/superfluid/networks/networks) you can find a list of networks where the Superfluid protocol is already deployed.

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

For all future tests, we will be using Foundry as the main framework for writing property, integration and invariant tests. 

#### Types of Test

There are four types of tests that are written to ensure correctness: are property, integration and invariant tests.

- Property tests aim to ensure correct behavior in isolation in a single function.
- Integration tests aim to ensure correct behavior in the interaction between two or more contracts.
- Invariant tests aim to ensure that certain invariants always hold true, no matter the sequence of functions being called.

#### Test Contract File Naming

Tests should be named in the following format: `ContractName.(prop|t|invariant).sol` depending on the type of test you are writing.

#### Folder structure

Tests should be placed in the `test/foundry` folder. The folder structure must follow that of the contracts folder. For example, if you are writing property tests for the `ConstantFlowAgreementV1.sol` contract, the test file will be placed in `test/foundry/agreements/ConstantFlowAgreement.prop.sol`.

#### Test Contract Naming

The naming convention for test contracts is taken from: 
- The contract you are testing (e.g. `ConstantFlowAgreementV1.sol`)
- The type of test you are writing (e.g. `property, integration, invariant`)
- The name of the contract will follow the format: `ContractName.(Property|Integration|Invariant)Test`
e.g. the property testing contract in `ConstantFlowAgreementV1.prop.sol` should be titled:

```solidity
contract ConstantFlowAgreementV1PropertyTest {
    ...
}
```

The reason for this naming convention is for multiple reasons:
- allow for easy searching of test contracts using an IDE
- allow easy filtering of tests to run locally or in CI

#### Test Naming

The naming convention for test functions must use camelCase and should be descriptive enough so the reader can have a good idea of what the test is doing before even reading the actual test code.

There are two primary type of tests written for unit and integration tests: 
- tests that are expected to pass
- tests that are expected to revert

For passing tests, the naming convention for the function is: `testADescriptiveName()`. For example:

```solidity
function testIncreaseFlowRateAllowance() {
    ...
}
```

For tests that are expected to revert, the naming convention for the function is: `testRevertADescriptiveName()`. For example:

```solidity
function testRevertIfDecreaseFlowRateAllowanceAndACLCreateFlow() {
    ...
}
```

#### Internal Helper Functions

If you are writing a test that requires a helper function, the helper function is prefixed with `_` and uses camelCase and snake case to indicate that it is an internal helper function. For example:

```solidity
function _assert_Flow_Operator_Data(AssertFlowOperatorData memory data) internal {
    ...
}
```

It is also recommended to create an pass structs to the helper functions to make the test code more readable. For example:

```solidity
// this is easier to understand what is going on before reading the _assert_Flow_Operator_Data function
_assert_Flow_Operator_Data(
    AssertFlowOperatorData({
        superToken: superToken,
        flowOperatorId: oldFlowOperatorId,
        expectedPermissions: oldPermissions,
        expectedFlowRateAllowance: oldFlowRateAllowance +
            flowRateAllowanceIncreaseDelta -
            flowRateAllowanceDecreaseDelta
    })
);

// this is a bit more confusing and may require constant jumping back and forth to understand what is going on
_assert_Flow_Operator_Data(
    superToken,
    oldFlowOperatorId,
    oldPermissions,
    oldFlowRateAllowance +
        flowRateAllowanceIncreaseDelta -
        flowRateAllowanceDecreaseDelta
);
```

Additionally, we use foundry's assert functions to make debugging easier by providing a descriptive and unique error message. For example:
  
```solidity
assertEq(newFlowRateAllowance, expectedFlowRateAllowance, "CFAv1 ACL: unexpected flow rate allowance");
```

### Writing Tests (Deprecated)

For convenient testing, the package contains deployment scripts which allow you to set up and initialize the protocol and test tokens with a few lines of code.
Currently, this requires [web3.js](https://github.com/ChainSafe/web3.js), support for other frameworks is work in progress.

```js
const deployFramework = require("@superfluid-finance/ethereum-contracts/ops-scripts/deploy-framework");
const deployTestToken = require("@superfluid-finance/ethereum-contracts/ops-scripts/deploy-test-token");
const deploySuperToken = require("@superfluid-finance/ethereum-contracts/ops-scripts/deploy-super-token");

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
})
```
In order to write concise testing code, we further recommend the use of [`@superfluid-finance/sdk-core`](/packages/sdk-core) not only in your UI code, but also in JS contract tests.

### Examples

You may also want to check out some example dapps in the [examples repo](https://github.com/superfluid-finance/super-examples) instead of starting from scratch.
Clone a project, modify and play!

### Deploying Superfluid Protocol

In the example test code, you have seen how to deploy protocol contracts from JS code.
In truffle projects, this deploy scripts can also be used on a CLI. E.g. in order to deploy to a local ganache dev chain:
```sh
CREATE_NEW_RESOLVER=1 DISABLE_NATIVE_TRUFFLE=true truffle --network ganache exec "node_modules/@superfluid-finance/ethereum-contracts/ops-scripts/deploy-test-environment.js"
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

Then install dependencies and do an initial build:

```sh
yarn install
yarn build
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
yarn run-hardhat test test/contracts/agreements/ConstantFlowAgreementV1.test.js
```

Run the test suite for core contracts:
```sh
yarn run-hardhat test testsuites/superfluid-core.js
```

The `pretest` script starts a ganache instance with deterministic accounts in the background, the `posttest` script stops it.
When running tests with `yarn test`, those get executed automatically (see [npm docs](https://docs.npmjs.com/cli/v7/using-npm/scripts#pre--post-scripts)).
> NOTE: You don't need to run the `pretest` and `posttest` scripts when running hardhat tests, but you do when running tests with truffle.

### TDD Session

When working on the contracts, a test driven approach is recommended.
You should choose the tests relevant for what you're working on using the [only keyword](https://mochajs.org/#exclusive-tests).
You can put the `only` keyword at any level between whole test suites (`only` appended to a top level `describe`, e.g. `describe.only`) and individual testcases (`it`, e.g. `it.only`).
With the testing scope defined, run:
```
yarn dev
```
This has [testsuites/all-contracts.js](testsuites/all-contracts.js) as its entrypoint, but if there's an `only` keyword at any nesting level in any of the tests traversed, only that selected subset of tests will be executed.
The selected test(s) will run once when starting the session and re-run everytime you save changes in a relevant file.

You may also focus on a specific testsuite with yarn dev:
```
yarn dev test/contracts/libs/CallUtils.test.js
```

After finishing the session, you can stop the hardhat instance you started in the first step (Ctrl-C).
Also, don't forget to remove `only` keywords from test files before making git commits.

To generate jpeg image charts from the test output run first install these dependencies: numpy, pandas, plotly and kaleido by running:
```
python3 -m pip install -U libraryName
```

After that is done, just simply run the testDataToCharts script and put the folder containing the csv files as the first argument e.g
```
python3 testDataToCharts.py output
```

### Other Useful Commands

```
yarn run-hardhat # run hardhat
yarn run-truffle # run truffle
yarn run-forge # run foundry forge
yarn run-nodemon forge test # use nodemon to run forge test
```

### Troubleshooting

Superfluid requires the [ERC-1820](https://eips.ethereum.org/EIPS/eip-1820) Registry contract to be deployed. That's because [ERC-777](https://eips.ethereum.org/EIPS/eip-777) - the basis for Super Tokens - depends on it.
If you use the deployment scripts as described above, that will be done automatically. If not, you may want to manually deploy ERC-1820 yourself. You can use `ops-scripts/deploy-erc1820.js` to do so.

In the [scripts folder](/scripts) you can find several scripts for deploying/configuring/querying protocol contracts.

## Show your support

Give a ⭐️ if this project helped you!

---

_This README was generated with ❤️ by [readme-md-generator](https://github.com/kefranabg/readme-md-generator)_
