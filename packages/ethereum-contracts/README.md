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

> Ethereum contracts implementation for the Superfluid Protocol

### 🏠 [Homepage](https://superfluid.finance)

### ✨ [Superfluid App](https://app.superfluid.finance/)

### 📖 [Docs](https://docs.superfluid.finance)

## Usage

If you're building a dapp using the deployed contracts (goerli or mainnet) then you should instead use [`@superfluid-finance/js-sdk`](/packages/js-sdk).

If you're building a smart contract that uses Superfluid protocol,
or even your own [SuperApp](https://docs.superfluid.finance/), then great! This is definitely the place to be.

### Installation

```sh
$ yarn add @superfluid-finance/ethereum-contracts
```

### Smart Contract

The contracts can be imported into your `.sol` file like this:

```js
import { IConstantFlowAgreementV1 } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";
```

### Writing Test

For writing tests, you can use the the deployment scripts to deploy all the necessary contracts. Currently they only works with [web3.js](https://github.com/ChainSafe/web3.js),
we are working on to support to other frameworks in the future.

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

To interact with the protocol, you should consider to use the
[`@superfluid-finance/js-sdk`](/packages/js-sdk). Here is a quick-start example:

```js
const SuperfluidSDK = require("@superfluid-finance/js-sdk");

let sf;
let daix;

beforeEach(async () => {
    await deployTestToken(errorHandler, [":", "fDAI"], {
        web3,
        from: admin,
    });
    await deploySuperToken(errorHandler, [":", "fDAI"], {
        web3,
        from: admin,
    });

    sf = new SuperfluidSDK.Framework({
        web3,
        version: "test",
        tokens: ["fDAI"],
    });
    await sf.initialize();

    daix = sf.tokens.fDAIx;

    // Create user objects
    admin = sf.user({ address: adminAddress, token: daix.address });
    alice = sf.user({ address: aliceAddress, token: daix.address });
});
```

Awesome, now that you have the basics, check out the apps over in the [examples folder](https://github.com/superfluid-finance/protocol-monorepo/tree/dev/examples).

### Deploying Superfluid Protocol

**Local**

To deploy to your local ganache environment:

```sh
DISABLE_NATIVE_TRUFFLE=true truffle --network ganache exec "node_modules/@superfluid-finance/ethereum-contracts/scripts/deploy-test-environment.js"
```

**Public**

If you want to deploy to a public network:

```sh
NEW_TEST_RESOLVER=1 DISABLE_NATIVE_TRUFFLE=true truffle --network goerli exec "node_modules/@superfluid-finance/ethereum-contracts/scripts/deploy-test-environment.js"
```

Note `NEW_TEST_RESOLVER=1`, it is to avoid using the official resolver address. Doing so
after the command finishes, you should see:

```
...
======== Super token deployed ========
=============== TEST ENVIRONMENT RESOLVER ======================
export TEST_RESOLVER_ADDRESS=0x43098b8d85Fe90eCE6B055e135759B558d2c0224
```

Run the export command to save TEST_RESOLVER_ADDRESS to your local environment.
Whenever you run additional tests/scripts this will be the address used to find the SF Framework contracts.

### Examples

We created a few [examples here](/examples). So that you don't have to start everything
from the scratch. Clone a project, modify and play!

### Troubleshooting

One thing to keep in mind is that Superfluid relies on a persistent 1820 registry contract. This must be deployed before you can interact with the protocol. If you follow the examples using the deployment scripts, you don't need to worry about it.

If you want to see examples for manually deploying contracts, check out the [scripts folder](https://github.com/superfluid-finance/protocol-monorepo/tree/dev/packages/ethereum-contracts/scripts).

In case your curious, or really hacking away, you might want to deploy the registry manually. Here is an example for how to deploy the 1820 contract to a local Ganache. (read more about [EIP 1820 Pseudo-introspection Registry Contract](https://eips.ethereum.org/EIPS/eip-1820))

```bash
# Start Ganache on 127.0.0.1:8545
ganache-cli

# Build the contracts + prepare the SDK
yarn build

# Deploy the 1820 contract
cd packages/ethereum-contracts
npx truffle exec scripts/deploy-erc1820.js --network ganache

# Now you can run tests and interact with the protocol
yarn test
```

## Contributing

### Setup Development Environment

1. Install dependencies

```sh
yarn install
```

2. Setup your own `.env` file from `.env.template`

### Testing

There are two major test suite:

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

## Show your support

Give a ⭐️ if this project helped you!

---

_This README was generated with ❤️ by [readme-md-generator](https://github.com/kefranabg/readme-md-generator)_
