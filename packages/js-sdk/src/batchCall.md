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

callAgreement and callAppAction rely on encoded data. You'll need to add a library to encode this data, since the SDK doesn't know how to encode your data.

TODO

```
yarn add @ethersproject/abi
```

Here is a compete example which approves a Super App to spend a user's Super Tokens, followed by a Super App function call:

```js
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const { Web3Provider } = require("@ethersproject/providers");

const sf = new SuperfluidSDK.Framework({
    ethers: new Web3Provider(window.ethereum),
    tokens: ["fDAI"],
});
await sf.initialize();

const bob = sf.user({ address: "0xabc...", token: sf.tokens.fDAIx.address });

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
            agreementType: "CFA",
            method: "createFlow",
            arguments: [],
        },
    },
]);
```
