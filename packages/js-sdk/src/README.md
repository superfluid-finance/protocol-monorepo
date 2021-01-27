# @superfuid-finance/js-sdk

**NB!** The SDK is still under development, its API and interfaces can change.

# Usage

## Overview

```js
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const sf = new SuperfluidSDK.Framework({
    version: "v1", // Protocol release version
    web3Provider: web3.currentProvider, // your web3 provider
    tokens: ["fDAI"]
});

await sf.initialize();

const bob = sf.user({ address: "0xabc...", token: sf.tokens.fDAIx.address });

// Constant Flow Agreement
await bob.flow({
    recipient: "0x123...",
    flowRate: "38580246913580" // 100 tokens / mo
});

// Instant Distribution Agreement (see docs)
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
-   Token factory functions:
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

## Initialization

During initialization, the resolver will be used to fetch the correct set of contracts based on the `version` you provide

| Argument     | Type     | default                                  |
| :----------- | :------- | ---------------------------------------- |
| version      | String   | Version of the latest deployed contracts |
| web3Provider | Object   | required                                 |
| tokens       | String[] | null                                     |

Example:

```js
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const sf = new SuperfluidSDK.Framework({
    version: "v1", // Protocol release version
    web3Provider: web3.currentProvider, // your web3 provider
    tokens: ["fDAI"]
});

await sf.initialize();
```

## :bust_in_silhouette: User

Create a new User object to quickly create and modify agreements.

Example:

```js
// First initialize the SDK
const sf = new SuperfluidSDK.Framework({...})
await sf.initialize()

const bob = sf.user({ address: "0xabc...", token: sf.tokens.fDAI })
const carol = sf.user({ address: "0x123...", token: sf.tokens.fDAI })
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
    onTransaction: hash => {
        txHash = hash;
    }
});
```

# Development

Contributions and suggestions welcome!

Since testing takes a while, we recommend specifying a test-suite. For example:

```bash
# Only run tests in User.test.js
nodemon -x npx truffle test ./test/sdk/User.test.js
```
