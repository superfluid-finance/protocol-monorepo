The `@superluid-finance/js-sdk` includes a wrapper function for creating batch calls quickly. For example, to upgrade some DAI to DAIx and transfer it in the same transaction, you can use `sf.batchCall()` like this:

```js
await sf.batchCall([
    {
        type: "SUPERTOKEN_UPGRADE",
        data: {
            tokenAddress: "0xfff...000",
            amount: "1000000000000000000",
        },
    },
    {
        type: "ERC20_TRANSFER_FROM",
        data: {
            tokenAddress: "0xfff...000",
            amount: "1000000000000000000",
            sender: "0xaaa...111",
            recipient: "0xbbb...222",
        },
    },
]);
```

All the `sf.batchCall()` options and available arguments are shown in the table below.

| Action                      | Type (required string or number)   | Data (required)                                 |
| :-------------------------- | :--------------------------------- | :---------------------------------------------- |
| ERC20 `approve`             | "ERC20_APPROVE" or 1               | `tokenAddress`, `spender`, `amount`             |
| ERC20 `transferFrom`        | "ERC20_TRANSFER_FROM" or 2         | `tokenAddress`, `sender`, `recipient`, `amount` |
| Super Token `upgrade`       | "SUPERTOKEN_UPGRADE" or 101        | `tokenAddress`, `amount`                        |
| Super Token `downgrade`     | "SUPERTOKEN_DOWNGRADE" or 102      | `tokenAddress`, `amount`                        |
| Super Fluid `callAgreement` | "SUPERFLUID_CALL_AGREEMENT" or 201 | `agreeementType`, `callData` (TODO)             |
| Super Fluid `callAppAction` | "CALL_APP_ACTION" or 202           | `superAppAddress`, `callData` (TODO)            |

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
            tokenAddress: sf.tokens.fDAIx.address,
            spender: bob.address,
            amount: "1000000000000000000",
        },
    },
    {
        type: "CALL_APP_ACTION",
        data: {
            // todo
        },
    },
]);
```
