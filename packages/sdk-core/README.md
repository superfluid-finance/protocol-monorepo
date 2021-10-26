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
    networkName: NetworkName.MATIC
});
```

JavaScript (Module):

```js
import SuperfluidSDK from "@superfluid-finance/sdk-core";

const sf = new SuperfluidSDK.Framework({
    chainId: 137,
    networkName: "matic"
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

## Queries

Once you have initialized the sf framework, you can make queries using it easily.

### Pre-Defined Queries

A list of the pre-defined queries:
```ts
sf.query.
	listAllSuperTokens()
	listStreams({ sender?: string, receiver?: string, token?: string }, { first?: number, skip?: number })
	listUserInteractedSuperTokens({ account?: string })
```

### Pagination

Some of the pre-defined query functions will accept pagination options: `({ first?: number, skip?: number })`. You can also write custom queries which take these options

### Custom Queries

If you would like, you can also build your own custom queries and pass them into the following function:
```ts
sf.query.custom(query: string, variables?: { [key: string]: any })
```

You may want to query some data which isn't part of the list of the pre-built queries. If you are interested, you can check out our schema in our [docs](https://docs.superfluid.finance/superfluid/docs/subgraph). You can do the following with the custom query function:

```ts
import { gql } from "graphql-request";
let query = gql`
	{
		response: accountTokenSnapshots(where: { account: $account, token: $token }, first: $first, skip: $skip) {
			id
			totalNumberOfActiveStreams
			totalNumberOfClosedStreams
			token {
				id
				name
				symbol
			}
		}
	}
`;

// Note: it is important to convert all addresses to lower case when querying the graph using an address.
const id = {
	account: "0x001fbfe0e74adedea03e6ad3e2fc1eee1a2d5045",
	token: "0x3ad736904e9e65189c3000c7dd2c8ac8bb7cd4e3",
	first: 150,
	skip: 25
};

const accountResponse = await sf.query.custom(query, id);
console.log(accountResponse);

/**
Prints out:
{
	response: {
		id: "0x3ad736904e9e65189c3000c7dd2c8ac8bb7cd4e3",
		accountTokenSnapshots: [
			{
				id: "0x...",
				totalNumberOfStreams: 0,
				totalNumberOfClosedStreams: 0,
				token: {
					id: "0x...",
					name: "Super MATIC",
					symbol: "MATICx"
				}
			}
		]
	}	
}
*/
```

> Note: this example uses the `graphql-request` library, but you just need to provide a valid query which is a string.