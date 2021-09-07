The `@superluid-finance/js-sdk` includes a wrapper function for creating batch calls quickly. For example, to upgrade some DAI to DAIx and transfer it in the same transaction, you can use `sf.batchCall()` like this:

```js
await sf.batchCall([
    {
        type: "SUPERTOKEN_UPGRADE",
        data: {
            token: "0x111...",
            amount: "1000000000000000000",
        },
    },
    {
        type: "ERC20_TRANSFER_FROM",
        data: {
            token: "0x111...",
            amount: "1000000000000000000",
            sender: "0xaaa...",
            recipient: "0xbbb...",
        },
    },
]);
```

All the `sf.batchCall()` options and available arguments are shown in the table below.

| Action                      | Type (required string or number)   | Data (required)                                                                            |
| :-------------------------- | :--------------------------------- | :----------------------------------------------------------------------------------------- |
| ERC20 `approve`             | "ERC20_APPROVE" or 1               | `token` Address, `spender` Address, `amount` String                                        |
| ERC20 `transferFrom`        | "ERC20_TRANSFER_FROM" or 2         | `token` Address, `sender` Address, `recipient` Address, `amount` String                    |
| Super Token `upgrade`       | "SUPERTOKEN_UPGRADE" or 101        | `token` Address, `amount` String                                                           |
| Super Token `downgrade`     | "SUPERTOKEN_DOWNGRADE" or 102      | `token` Address, `amount` String                                                           |
| Super Fluid `callAgreement` | "SUPERFLUID_CALL_AGREEMENT" or 201 | `agreeementType` "CFA" or "IDA", `method` String, `arguments` array, `userData` (optional) |
| Super Fluid `callAppAction` | "CALL_APP_ACTION" or 202           | `superApp` Address, `callData` see below                                                   |

See the specific sections below for more details.

## ERC20_APPROVE & ERC20_TRANSFER_FROM

Note: you can only use `batchCall` ERC20 features with Super Tokens. For example, you cannot perform a regular DAI approval. All non-Super Token ERC20 approvals must occur in a separate transaction.

```js
await sf.batchCall([
    {
        type: "ERC20_APPROVE",
        data: {
            token: "0x111...", // Super Tokens only
            amount: "1000000000000000000",
            spender: "0xbbb...",
        },
    },
    {
        type: "ERC20_TRANSFER_FROM",
        data: {
            token: "0x111...", // Super Tokens only
            amount: "1000000000000000000",
            sender: "0xaaa...",
            recipient: "0xbbb...",
        },
    },
]);
```

## SUPERTOKEN_UPGRADE and SUPERTOKEN_DOWNGRADE

No tricks here, just provide the Super Token address as `token` and the `amount`.

```js
await sf.batchCall([
    {
        type: "SUPERTOKEN_UPGRADE",
        data: {
            token: "0x111...", // Super Token address
            amount: "1000000000000000000",
        },
    },
    {
        type: "SUPERTOKEN_DOWNGRADE",
        data: {
            token: "0x111...", // Super Token address
            amount: "1000000000000000000",
        },
    },
]);
```

## SUPERFLUID_CALL_AGREEMENT

To call an agreement, provide the `agreementType` ("CFA" or "IDA"), `method`, and `arguments`. Use the docs for Constant Flow Agreement (TODO LINK) and Instant Distribution Agreement (TODO LINK) to see the proper order for the arguments.

```js
await sf.batchCall([
    {
        type: "SUPERFLUID_CALL_AGREEMENT",
        data: {
            agreementType: "CFA",
            method: "createFlow",
            arguments: [
                sf.tokens.fDAIx.address, // Token address
                app.address, // Flow recipient
                MINIMUM_GAME_FLOW_RATE.toString(), // Flow rate
                "0x",
            ],
        },
    },
]);
```

## CALL_APP_ACTION

The `@superfluid-finance/js-sdk` isn't aware of your Super App, so you must encode the method arguments yourself. Here is an example using two different libraries:

```js
// Example Solidity function in your Super App
function deposit(uint256 amount) {}

// Ethers.js
myContract.interface.encodeFunctionData(
   "deposit",
   ["1000000000000000000"]
);

// Web3.js
myContract.methods.deposit("1000000000000000000").encodeABI()
```

Here is an example which approves a Super App to spend a user's Super Tokens, followed by a Super App function call:

```js
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const { Web3Provider } = require("@ethersproject/providers");

const sf = new SuperfluidSDK.Framework({
    ethers: new Web3Provider(window.ethereum),
    tokens: ["fDAI"],
});
await sf.initialize();

const bob = sf.user({ address: "0xbbb...", token: sf.tokens.fDAIx.address });

// TODO: Create contract

await sf.batchCall([
    {
        type: "ERC20_APPROVE",
        data: {
            token: sf.tokens.fDAIx.address,
            spender: bob.address,
            amount: "1000000000000000000",
        },
    },
    {
        type: "CALL_APP_ACTION",
        data: {
            superApp: myContract.address,
            callData: myContract.interface.encodeFunctionData(
               "deposit",
               ["1000000000000000000"]
            );
        },
    },
]);
```
