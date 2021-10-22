<h1 align="center">sdk-core</h1>
<div align="center">
<img  width="300" padding="0 0 10px" alt="Superfluid logo" src="/sf-logo.png" />
<p>
  <a href="https://www.npmjs.com/package/@superfluid-finance/sdk-core" target="_blank">
    <img alt="Version" src="https://img.shields.io/npm/v/@superfluid-finance/sdk-core.svg">
  </a>
  <a href="#" target="_blank">
    <img alt="License: MIT" src="https://img.shields.io/badge/License-MIT-yellow.svg" />
  </a>
  <a href="https://twitter.com/Superfluid_HQ/" target="blank">
    <img alt="Twitter: Superfluid_HQ" src="https://img.shields.io/twitter/follow/Superfluid_HQ.svg?style=social" />
  </a>
</p>
</div>

### 🏠 [Homepage](https://superfluid.finance)

### ✨ [Superfluid App](https://app.superfluid.finance/)

### 📖 [Docs](https://docs.superfluid.finance)

# Usage

## Read-only Initialization

Here is a quick look at using the SDK in different environments:

TypeScript:

```ts
import SuperfluidSDK, {
    ChainId,
    NetworkName,
} from "@superfluid-finance/sdk-core";

const sf = new SuperfluidSDK.Framework({
    chainId: ChainId.MATIC,
    networkName: NetworkName.MATIC,
});
```

JavaScript (Module):

```js
import SuperfluidSDK from "@superfluid-finance/sdk-core";

const sf = new SuperfluidSDK.Framework({
    chainId: 137,
    networkName: "matic",
});
```

JavaScript (CommonJS) - usually a Node.js environment:

```js
const SuperfluidSDK = require("@superfluid-finance/sdk-core");

const sf = new SuperfluidSDK.Framework({
    chainId: 137,
	networkName: "matic"
});
```

> Note: You specify your project type in `package.json` - `"type": "module"` or `"type": "commonjs"`.

This is the absolute minimum you need to provide the constructor (`chainId` or `networkName`) if all you want to do is query data.