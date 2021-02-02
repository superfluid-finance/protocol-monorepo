<h1 align="center">Welcome to @superfluid-finance/ethereum-contracts 👋
</h1>
<div align="center">
<img  width="300" padding="0 0 10px" alt="Superfluid logo" src="/sf-logo.png" />
<p>
  <a href="https://www.npmjs.com/package/@superfluid-finance/ethereum-contracts" target="_blank">
    <img alt="Version" src="https://img.shields.io/npm/v/@superfluid-finance/ethereum-contracts.svg">
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

## Install

```sh
yarn install
```

## Usage

If you're building a dapp using the deployed contracts (goerli or mainnet) then head directly over to the `@superfluid-finance/js-sdk`, also located in this repo.

If you're building a Super App, then great! This is definitely the place to be. The contracts can be imported into your Solidity file like this:

```js
import { IConstantFlowAgreementV1 } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";
```

And in your tests, use the `TestEnvironment` helper to deploy the Superfluid Framework.

```js
const TestEnvironment = require("@superfluid-finance/ethereum-contracts/test/TestEnvironment");

contract("My Test", accounts => {
    const t = new TestEnvironment(accounts.slice(0, 4), { isTruffle: true });
    const {
        admin: adminAddress,
        alice: aliceAddress,
    } = t.aliases;
    //...
```

This also exposes the `SuperfluidSDK`, so you can easily deploy tokens and User objects.

```js
let sf;
let superToken;

before(async () => {
    // Initialize the SuperfluidSDK
    await t.reset();
    sf = t.sf;
});

beforeEach(async () => {
    // Deploy a test token
    await t.createNewToken({ doUpgrade: true });
    ({ superToken } = t.contracts);

    // Create user objects
    admin = sf.user({ address: adminAddress, token: superToken.address });
    alice = sf.user({ address: aliceAddress, token: superToken.address });
});
```

Awesome, now that have the basics, check out the apps over in [examples/](examples/).

## Troubleshooting

One thing to keep in mind is that Superfluid relies on a persistent 1820 registry contract. This must be deployed before you can interact with the protocol. If you follow the examples using the `TestEnvironment` helper, you don't need to worry about it.

There are also many scripts available to manually deploy things, or adapt to your needs.

In case your curious, or really hacking away, you might want to deploy the registry manually. Here is an example for deploying the 1820 contract to a local Ganache. (read more about [EIP 1820 Pseudo-introspection Registry Contract](https://eips.ethereum.org/EIPS/eip-1820))

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

### Testing

**NB!**: Since these tests take long time to execute, it is quite possible
that you want to use the [execlusive tests](https://mochajs.org/#exclusive-tests)
feature from MochaJS to speed up isolated feature development.

There are two major test suite:

-   Contracts (test/contracts.test.js) tests the contracts
    Each contracts test suite is named as `test/{Type}/{ContractName}.test.js`.
-   Deployment (test/deployment.test.js) tests the deployment script

```bash
yarn test
```

## Show your support

Give a ⭐️ if this project helped you!

---

_This README was generated with ❤️ by [readme-md-generator](https://github.com/kefranabg/readme-md-generator)_
