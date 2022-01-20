<h1 align="center">Welcome to @superfluid-finance/js-sdk 👋
</h1>
<div align="center">
<img  width="300" padding="0 0 10px" alt="Superfluid logo" src="/sf-logo.png" />
<p>
  <a href="https://www.npmjs.com/package/@superfluid-finance/js-sdk" target="_blank">
    <img alt="Version" src="https://img.shields.io/npm/v/@superfluid-finance/js-sdk.svg">
  </a>
  <a href="#" target="_blank">
    <img alt="License: MIT" src="https://img.shields.io/badge/License-MIT-yellow.svg" />
  </a>
  <a href="https://twitter.com/Superfluid_HQ/status/" target="blank">
    <img alt="Twitter: Superfluid_HQ" src="https://img.shields.io/twitter/follow/Superfluid_HQ.svg?style=social" />
  </a>
</p>
</div>

<>{`\_`}</>

> Javascript SDK for building with Superfluid Protocol

### 🏠 [Homepage](https://superfluid.finance)

### ✨ [Superfluid App](https://app.superfluid.finance/)

### 📖 [Docs](https://docs.superfluid.finance)

# Important Notice

We will be deprecating the JS-SDK and stop providing support/fixes in the near future, so please use either:
-  [sdk-core](https://www.npmjs.com/package/@superfluid-finance/sdk-core) which can be used for both front-end and back-end dApp development, this can be thought of more as a thin wrapper of the protocol contracts and is unconcerned with state.

or

- [sdk-redux](https://www.npmjs.com/package/@superfluid-finance/sdk-redux) for building production-grade front-ends for dApps with state management, caching logic, transaction tracking, react hooks and much more.

# Usage

Here is a quick look at using the SDK.

```js
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const { Web3Provider } = require("@ethersproject/providers");

const sf = new SuperfluidSDK.Framework({
    ethers: new Web3Provider(window.ethereum),
    tokens: ["fDAI"],
});
await sf.initialize();

const bob = sf.user({ address: "0xabc...", token: sf.tokens.fDAIx.address });

// Constant Flow Agreement
await bob.flow({
    recipient: "0x123...",
    flowRate: "38580246913580", // 100 tokens / mo
});

// Instant Distribution Agreement
// - More utilities coming soon. See the docs for now
```

## What's In the Bag

-   `sf.user`: A helpful abstraction that should handle most of your needs. See below for more details.
-   `sf.host` : The [truffle contract instance](https://www.trufflesuite.com/docs/truffle/getting-started/interacting-with-your-contracts)
    to interact with the host contract (Superfluid.sol).
-   `sf.contracts` : The [truffle contract](https://www.trufflesuite.com/docs/truffle/reference/contract-abstractions) objects loaded by the SDK:
    -   `IERC20` : The ERC20 Interface.
    -   `TokenInfo` : A customary ERC20 token info interface (name/symbol/decimals).
    -   `ERC20WithTokenInfo` : A combination of IERC20 and TokenInfo.
    -   `TestToken` : A ERC20 Test token.
    -   `IResolver` : A simple resolver interface to locate different versions of the contracts.
    -   `ISuperfluid` : The Superfluid host contract interface.
    -   `ISuperToken` : The Super token contract interface.
    -   `IConstantFlowAgreementV1` : The constant flow agreement (v1) contract interface.
    -   `IInstantDistributionAgreementV1` : The instant distribution agreement (v1) contract interface.
-   Token factory helper functions:
    -   `sf.createERC20Wrapper`
-   `sf.resolver`: The resolver used by the SDK.
    -   In test nets, there are some test tokens can be located with the resolver:
        -   `fDAI` : The fake DAI. `sf.resolver.get("tokens.fDAI")`.
        -   `fUSDC` : The fake USDC. `sf.resolver.get("tokens.fUSDC")`.
        -   `fTUSD` : The fake TUSD. `sf.resolver.get("tokens.fTUSD")`.
-   `sf.agreements`:
    -   `sf.agreements.cfa` : Constant flow agreement truffle contract instance.
    -   `sf.agreements.ida` : Instant distribution agreement truffle contract instance.
-   `sf.cfa`: The constant flow agreement helper class instance.
-   `sf.ida`: The instant distribution agreement helper class instance.

## Initialization

During initialization, the resolver will be used to fetch the correct set of contracts based on the `version` you provide

| Argument             | Type     | description                                            | default   |
| :------------------- | :------- | ------------------------------------------------------ | --------- |
| version              | String   | Release version of the deployed protocol               | v1        |
| isTruffle            | Boolean  | Use the Framework under the native truffle environment | false     |
| web3                 | Object   | Use the Framework with web3.js (1.3.x)                 | undefined |
| ethers               | Object   | Use the Framework with ethers.js (5.x.y)               | undefined |
| additionalContracts  | String[] | additional contracts to be loaded                      | []        |
| tokens               | String[] | List of token keys to load from the resolver           | []        |
| loadSuperNativeToken | Boolean  | Load super native token (e.g. ETHx) if possible        | false     |
| resolverAddress      | Address  | Force resolver address                                 | undefined |

You also need to choose what web3 framework you plan to use, currently we support three modes:

-   Truffle native environment (developing using `truffle test|exec|egc.`).
-   [Web3.js](https://web3js.readthedocs.io/en/v1.2.1/), currently the SDK has been tested with web3.js `1.3.x` versions.
-   [Ethers.js](https://github.com/ethers-io/ethers.js/), currently the SDK has been tested with ethers.js `5.x.y` versions.

**Example with Ethers.js**

```js
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const { Web3Provider } = require("@ethersproject/providers");

const sf = new SuperfluidSDK.Framework({
    ethers: new Web3Provider(window.ethereum),
    tokens: ["fDAI"],
});
await sf.initialize();

const bob = sf.user({ address: "0xabc...", token: sf.tokens.fDAIx.address });
```

The exposed contract objects are an adapted version that look like [`truffle-contract`](https://github.com/trufflesuite/truffle/tree/develop/packages/contract/) objects.

**Example with Web3.js**

```js
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const web3 = require("web3");

const sf = new SuperfluidSDK.Framework({
    web3: new Web3(window.ethereum),
    tokens: ["fDAI"],
});
await sf.initialize();

const bob = sf.user({ address: "0xabc...", token: sf.tokens.fDAIx.address });
```

The exposed contract objects are of [`truffle-contract`](https://github.com/trufflesuite/truffle/tree/develop/packages/contract/) type.

**Example with truffle native**

```js
const SuperfluidSDK = require("@superfluid-finance/js-sdk");

const sf = new SuperfluidSDK.Framework({
    isTruffle: true,
    tokens: ["fDAI"],
});
await sf.initialize();

const bob = sf.user({ address: "0xabc...", token: sf.tokens.fDAIx.address });
```

The exposed contract objects are of [`truffle-contract`](https://github.com/trufflesuite/truffle/tree/develop/packages/contract/) type loaded using truffle `artifacts` object.

## :bust_in_silhouette: User

Create a new User object to quickly create and modify agreements.

Example:

```js
const bob = sf.user({ address: "0xabc...", token: sf.tokens.fDAI });
const carol = sf.user({ address: "0x123...", token: sf.tokens.fDAI });
```

### `user.details()`

Returns details about an account.

| Argument | Type | default |
| :------- | :--- | ------- |
| n/a      |      |         |

Example:

```js
console.log(await bob.details());

> {
    cfa: {
        flows: {
            inFlows: [
                {
                    sender: "0xf17f52151EbEF6C7334FAD080c5704D77216b732",
                    receiver: "0xC5fdf4076b8F3A5357c5E395ab970B5B54098Fef",
                    flowRate: "38580246913580"
                }
            ],
            outFlows: [
                {
                    sender: "0xC5fdf4076b8F3A5357c5E395ab970B5B54098Fef",
                    receiver: "0x821aEa9a577a9b44299B9c15c88cf3087F3b5544",
                    flowRate: "19290123456790"
                }
            ]
        },
        netFlow: "19290123456790"
    },
    ida: {} // Available soon
}
```

### `user.flow({recipient, flowRate, [, onTransaction, ...]})`

Create / update / delete a flow to the recipient. Enter "0" to delete a flow.

| Argument      | Type                           | default  |
| :------------ | :----------------------------- | -------- |
| recipient     | Address or another User Object | required |
| flowRate      | String                         | required |
| onTransaction | Function                       | null     |

Example:

```js
const tx = await alice.flow({
    recipient: bob,
    flowRate: "38580246913580", // 100 / mo
    // OPTIONS: See ConstantFlowAgreementV1Helper for more
    onTransaction: (hash) => {
        txHash = hash;
    },
});
```

## Closer to the metal

You may also use the helper classes and functions instead of the `User` abstraction.

The helpers are usually considered thinner wrappers of the underlying contract calls. It is suitable if you want more control and customization of how to interact with the protocol.

The available helpers currently are:

-   `sf.cfa` - Constant flow agreement helper.
-   `sf.ida` - Instant distribution agreement helper.
-   `sf.createERC20Wrapper` - Create a new ERC20 wrapper.
-   `sf.batchCall` - (TBD) batchCall helper.
-   `sf.callAppAction` - (TBD) callAppAction helper.

For their documentations, please look into their code comments directly.

# Contributing

Contributions and suggestions welcome!

## Module packaging

Check the module size and dependencies:

```bash
yarn stats
```

## Testing

Since testing can take a long time to execute, you may want to use the [execlusive tests](https://mochajs.org/#exclusive-tests) feature from MochaJS to isolate only the test you want. For example:

```bash
# Only run User.test.js
nodemon -x npx truffle test ./test/User.test.js
```
