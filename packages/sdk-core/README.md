<h1 align="center">sdk-core</h1>
<div align="center">
<img  width="300" padding="0 0 10px" alt="Superfluid logo" src="./sf-logo.png" />
<p>
  <a href="https://www.npmjs.com/package/@superfluid-finance/sdk-core" target="_blank">
    <img alt="Version" src="https://img.shields.io/npm/v/@superfluid-finance/sdk-core.svg">
  </a>
  <a href="https://codecov.io/gh/superfluid-finance/protocol-monorepo/tree/dev/packages/sdk-core">
    <img src="https://codecov.io/gh/superfluid-finance/protocol-monorepo/branch/dev/graph/badge.svg?token=LJW5NDGEJ9&flag=sdk-core"/>
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

# Introduction
SDK-Core is an application framework for interacting with the Superfluid Protocol without Solidity knowledge.

More specifically, SDK-Core is a wrapper library around `@superfluid-finance/ethereum-contracts` which allows web developers to interact with the Superfluid contracts.
Under the hood, SDK-Core leverages TypeScript, ethers.js and The Graph and GraphQL.

## Important Disclaimer
SDK-Core is in early active development and can have breaking releases without warning and without consideration for semantic versioning.

# Features
* Minimal Framework initialization (`networkName` or `chainId` and `provider`)
* New Operation syntax for transactions
* Create/Update/Delete Agreement Operations (Constant Flow Agreement and Instant Distribution Agreement)
* SuperToken Operations
* Subgraph-powered GraphQL querying with ordering and pagination
* Event subscription
* CFA/IDA/SuperToken Web3 Getters
* Batch Call functionality for batching multiple Superfluid Operations in one transaction

# Notable Used Technologies
* TypeScript
* Ethers
* GraphQL

# Prerequisites

To get the package up and running you'll need to install the necessary dependencies and build the project:

```bash
yarn install && yarn build
```

# Tutorial

For the best and most immersive experience of learning how to use the main sdk-core features, I would highly recommend heading over to our [interactive tutorials](https://docs.superfluid.finance/superfluid/protocol-tutorials/interactive-tutorials).
# Getting Started
## Framework Initialization

Here is a quick look at initializing the SDK in different environments:

TypeScript / JavaScript (Module) vs. JavaScript (CommonJS) - usually a Node.js environment

The primary difference between the two environments is the import/require of the sdk-core package, everything else is the same.

TS/ESModule
```ts
import { Framework } from "@superfluid-finance/sdk-core";
import { ethers } from "ethers";
```

CommonJS/Node.js
```js
const { Framework } = require("@superfluid-finance/sdk-core");
const { ethers } = require("ethers");
```

```ts
// infura provider initialization
const provider = new ethers.providers.InfuraProvider(
  "matic",
  "<INFURA_API_KEY>"
);
const sf = await Framework.create({
  networkName: "matic",
  provider
});

// web3.js + Hardhat provider initialization
const web3jsProvider = new ethers.providers.Web3Provider(
  (global as any).web3.currentProvider
);
const web3jsSf = await Framework.create({
  networkName: "matic",
  provider: web3jsProvider
});

// injected web3.js initialization (Hardhat) 
// most likely to be used on backend for testing
// NOTE: if you're using truffle, you should be able to
// omit the (global as any) as this should be
// exposed already (in JS at least)
const injectedWeb3jsSf = await Framework.create({
  networkName: "custom",
  provider: (global as any).web3,
  dataMode: "WEB3_ONLY",
  resolverAddress: <RESOLVER_ADDRESS>,
  protocolReleaseVersion: "test",
});

// injected hardhat ethers initialization
// most likely to be used on backend for testing
import hardhat from "hardhat";
const injectedHardhatEthersSf = await Framework.create({
  networkName: "custom",
  provider: hardhat.ethers,
  dataMode: "WEB3_ONLY",
  resolverAddress: <RESOLVER_ADDRESS>,
  protocolReleaseVersion: "test",
})

// ethers.js + hardhat provider initialization (in testing environment w/ hardhat-ethers)
import { ethers } from "hardhat";
const [deployer] = await ethers.getSigners();
const ethersProvider = deployer.provider;
const ethersjsSf = await Framework.create({
  networkName: "custom",
  dataMode: "WEB3_ONLY",
  resolverAddress: <RESOLVER_ADDRESS>,
  protocolReleaseVersion: "test",
  provider: ethersProvider
});

// metamask
const mmProvider = new ethers.providers.Web3Provider(window.ethereum);
const mmSf = await Framework.create({
  networkName: "matic",
  provider: mmProvider
});

// web3modal
import Web3Modal from "web3modal";
const web3Modal = new Web3Modal({
  cacheProvider: false,
  providerOptions: {}
});
const web3ModalRawProvider = await web3Modal.connect();
const web3ModalProvider = new ethers.providers.Web3Provider(web3ModalRawProvider);
const web3ModalSf = await Framework.create({
  networkName: "matic",
  provider: web3ModalProvider
});

//bnc-onboard
const onboard = Onboard({
    dappId: "<API_KEY>",
    networkId: 4,
    subscriptions: {
        wallet: wallet => {
            const web3Provider = new ethers.providers.Web3Provider(wallet.provider);
            (async () => {
                const framework = await Framework.create({ networkName: "matic", provider: web3Provider });
            })();
        }
    }
});
// this is triggered by:
await onboard.walletSelect();
```

> Note: You specify your project type in `package.json` - `"type": "module"` or `"type": "commonjs"`.

The absolute minimum you need to provide the constructor is `chainId` or `networkName` and a `provider` object if all you want to do are read operations. It is also important to note that the provider does not need to be an InfuraProvider - it just needs to satisfy the `SupportedProvider` interface: `ethers.providers.Provider | (typeof ethers & HardhatEthersHelpers) | Web3`.

## Helper Classes

The `Framework` includes a variety of helper classes which can be directly accessed upon initialization, but can also be initialized as standalone classes if desired.

### Query

Once you have initialized the `Framework` class using `Framework.create`, you can make queries using it easily.

#### Pre-Defined Queries

A list of the pre-defined queries:

```ts
const { Framework } = require("@superfluid-finance/sdk-core");
const { ethers } = require("ethers");

const provider = new ethers.providers.InfuraProvider(
	"matic",
	"<INFURA_API_KEY>"
);
const sf = await Framework.create({
  networkName: "matic",
	provider
});

type Paging = { take: number, skip?: number, lastId?: string };

const pageResult = await sf.query.
  // The different queries can take different order by properties 
  // given the properties that exist on the entity itself.
  listAllSuperTokens({ isListed?: boolean },
    paging: Paging,
    ordering: Ordering<Token_OrderBy>
  );

  listIndexes({ indexId?: string, publisher?: string, token?: string },
    paging: Paging,
    ordering: Ordering<Index_OrderBy>
  );

  listIndexSubscriptions({ subscriber?: string, approved?: boolean },
    paging: Paging,
    ordering: Ordering<IndexSubscription_OrderBy>
  );

  listStreams({ sender?: string, receiver?: string, token?: string },
    paging: Paging,
    ordering: Ordering<Stream_OrderBy>
  );

  listUserInteractedSuperTokens({ account?: string, token?: string },
    paging: Paging,
    ordering: Ordering<AccountTokenSnapshot_OrderBy>
  );

  listEvents({ account?: string, timestamp_gt?: number },
    paging: Paging,
    ordering: Ordering<Event_OrderBy>
  );

  // A subscription function which allows you to subscribe to events via polling.
  on(
        callback: (events: AllEvents[], unsubscribe: () => void) => void,
        ms: number,
        account?: string,
        timeout?: number
    )
```

#### Direct Initialization

If you'd like, you can also initialize the `Query` class as a standalone class like so:

```ts
import { Query } from "@superfluid-finance/sdk-core";
const query = new Query({
  customSubgraphQueriesEndpoint: "<A_CUSTOM_ENDPOINT>",
  dataMode: "SUBGRAPH_ONLY" | "SUBGRAPH_WEB3" | "WEB3_ONLY"
});
```

#### Pagination

All of the pre-defined query functions will accept pagination options: `({ skip: number, take: number })`, if you don't pass anything in, it will use a default of: `{ skip: 0, take: 100 }`. You can also paginate by `lastId`, this allows you to bypass the limitation of the max skip of 5000 entities.

> Note: this example uses the `graphql-request` library, but you just need to provide a valid query which is a string.

#### Ordering

You can also pass in an ordering object for the different queries, each query function will accept different ordering properties depending on the properties on the entity. We have different defaults for each so you don't need to actually pass anything in.

#### Example Usage

```ts
const { Framework } = require("@superfluid-finance/sdk-core");
const { ethers } = require("ethers");

const provider = new ethers.providers.InfuraProvider(
	"matic",
	"<INFURA_API_KEY>"
);
const sf = await Framework.create({
  networkName: "matic",
	provider
});
const results = await sf.query.listAllSuperTokens(
  { isListed: true },
  { skip: 5, take: 150 },
  {
    orderBy: "createdAtBlockNumber",
    orderDirection: "desc"
  });
```

### Creating a Signer

In order to execute a transaction on the blockchain, you need to have a signer. That is, you need to have access to an EOA (Externally Owned Account) to trigger any sort of change. You can do this through a contract, but an EOA still has to be the one which triggers the contract to interact with another contract. The signer that is returned will be passed when executing transactions.

#### Web3Provider Signer Example

Below is an example of using the `Web3Provider` object to create a signer. This will likely be the way that most client-side applications create a signer.

```ts
import { Framework } from "@superfluid-finance/sdk-core";
import Web3Modal from "web3modal";
import { Web3Provider } from "@ethersproject/providers";

// web3Modal example
const web3ModalRawProvider = await web3Modal.connect();
const web3ModalProvider = new Web3Provider(web3ModalRawProvider, "any");

const sf = await Framework.create({
  networkName: "matic",
  provider: web3ModalProvider,
});

const web3ModalSigner = sf.createSigner({ web3Provider: web3ModalProvider });

// MetaMask example
const metamaskProvider = new Web3Provider(window.ethereum);
const metaMaskSigner = sf.createSigner({ web3Provider: metamaskProvider });
```

#### Hardhat Signer Example

Below is an example of creating a signer in a `Hardhat` + `ethers.js` environment. This will likely be the way that the `sdk-core` is used in a testing environment.

```ts
import { Framework } from "@superfluid-finance/sdk-core";
import { ethers } from "hardhat";

const sf = await Framework.create({
  networkName: "matic",
  provider: ethers.provider,
});

const signer = sf.createSigner({
  privateKey: "<TEST_ACCOUNT_PRIVATE_KEY>",
  provider: ethers.provider,
});
```

#### Signer/Wallet Example

Below is an example of creating a signer passing in a signer object (this can be a wallet for example). This will likely be the way that the `sdk-core` is used in a Node.js environment (back-end) or a testing environment.

```ts
import { Framework } from "@superfluid-finance/sdk-core";
import { ethers } from "ethers";

const provider = new ethers.providers.InfuraProvider(
  "matic",
  "<INFURA_API_KEY>"
);

const wallet = new ethers.Wallet(
  "cf2bea4c6aad8dbc387d5dd68bf408999b0b1ee949e04ff1d96dd60bc3553a49",
  provider
);

const sf = await Framework.create({
  networkName: "matic",
  provider,
});

const signer = sf.createSigner({ signer: wallet });
```

### Operation

The `Operation` class is an object that is returned after you execute a contract call from this package - instead of immediately executing, we return the `Operation` class which can be either executed to broadcast the transaction or used to create and execute a `BatchCall`.

#### Usage

```ts
import { Framework } from "@superfluid-finance/sdk-core";
import { ethers } from "ethers";

const provider = new ethers.providers.InfuraProvider(
  "matic",
  "<INFURA_API_KEY>"
);

const sf = await Framework.create({
  networkName: "matic",
  provider
});

// create a signer
const signer = sf.createSigner({ privateKey: "<TEST_ACCOUNT_PRIVATE_KEY>", provider });

// load the usdcx SuperToken via the Framework (using the token address)
const usdcx = await sf.loadSuperToken("0xCAa7349CEA390F89641fe306D93591f87595dc1F");
// OR
// load the daix SuperToken via the Framework (using the token symbol)
const daix = await sf.loadSuperToken("DAIx");

// create an approve operation
const approveOperation = usdcx.approve({ receiver: "0xab...", amount: ethers.utils.parseUnits("100").toString() });

// execute the approve operation, passing in a signer
const txn = await approveOperation.exec(signer);

// wait for the transaction to be confirmed
const receipt = await txn.wait();

// or you can create and execute the transaction in a single line
const approveTxn = await usdcx.approve({ receiver: "0xab...", amount: ethers.utils.parseUnits("100").toString() }).exec(signer);
const approveTxnReceipt = await approveTxn.wait();
```

### ConstantFlowAgreementV1

The `ConstantFlowAgreementV1` helper class provides access to create/update/delete flows. You can access this via the `Framework` class (`sf.cfaV1`) or initialize this as a standalone class.

#### Direct Initialization

```ts
import { ConstantFlowAgreementV1 } from "@superfluid-finance/sdk-core";

const config = {
  hostAddress: "0x3E14dC1b13c488a8d5D310918780c983bD5982E7",
  cfaV1Address: "0x6EeE6060f715257b970700bc2656De21dEdF074C",
  idaV1Address: "0xB0aABBA4B2783A72C52956CDEF62d438ecA2d7a1"
};

const cfaV1 = new ConstantFlowAgreementV1({ options: config });
```

#### CFAV1 Functions
```ts
// Read functions
await sf.cfaV1.getFlow({
  superToken: string,
  sender: string,
  receiver: string,
  providerOrSigner: ethers.providers.Provider | ethers.Signer
});

await sf.cfaV1.getAccountFlowInfo({
  superToken: string,
  account: string,
  providerOrSigner: ethers.providers.Provider | ethers.Signer
});

await sf.cfaV1.getNetFlow({
  superToken: string,
  account: string,
  providerOrSigner: ethers.providers.Provider | ethers.Signer
});


// Write operations
sf.cfaV1.createFlow({
  sender: string,
  receiver: string,
  superToken: string,
  flowRate: string,
  userData?: string
});

sf.cfaV1.updateFlow({
  sender: string,
  receiver: string,
  superToken: string,
  flowRate: string,
  userData?: string
});

sf.cfaV1.deleteFlow({
  sender: string,
  receiver: string,
  superToken: string,
  userData?: string
});
```

#### Example Usage

```ts
import { Framework } from "@superfluid-finance/sdk-core";
import { ethers } from "ethers";

const provider = new ethers.providers.InfuraProvider(
  "matic",
  "<INFURA_API_KEY>"
);

const sf = await Framework.create({
  networkName: "matic",
  provider
});

// Read example
const flowInfo = await sf.cfaV1.getFlow({
  superToken: "0x...",
  sender: "0x...",
  receiver: "0x...",
  providerOrSigner: provider
});
console.log("flowInfo", flowInfo);

// Write operation example
const signer = sf.createSigner({ privateKey: "<TEST_ACCOUNT_PRIVATE_KEY>", provider });
const createFlowOperation = sf.cfaV1.createFlow({
  sender: "0x...",
  receiver: "0x...",
  superToken: "0x...",
  flowRate: "1000000000"
});
const txnResponse = await createFlowOperation.exec(signer);
const txnReceipt = await txnResponse.wait();
// Transaction Complete when code reaches here
```

### InstantDistributionAgreementV1

The `InstantDistributionAgreementV1` helper class provides access to a variety of IDA functions. You can access this via the `Framework` class (`sf.idaV1`) or initialize this as a standalone class.

#### Direct Initialization

```ts
import { InstantDistributionAgreementV1 } from "@superfluid-finance/sdk-core";

const config = {
  hostAddress: "0x3E14dC1b13c488a8d5D310918780c983bD5982E7",
  cfaV1Address: "0x6EeE6060f715257b970700bc2656De21dEdF074C",
  idaV1Address: "0xB0aABBA4B2783A72C52956CDEF62d438ecA2d7a1"
};

const idaV1 = new InstantDistributionAgreementV1({ options: config });
```

#### IDAV1 Functions
```ts
// Read functions
await sf.idaV1.getSubscription({
  superToken: string,
  publisher: string,
  indexId: string,
  subscriber: string,
  providerOrSigner: string
});

await sf.idaV1.getIndex({
  superToken: string,
  publisher: string,
  indexId: string,
  providerOrSigner: string
});


// Write operations
sf.idaV1.createIndex({
  indexId: string,
  superToken: string,
  userData?: string
});

sf.idaV1.distribute({
  indexId: string,
  superToken: string,
  amount: string,
  userData?: string
});

sf.idaV1.updateIndexValue({
  indexId: string,
  superToken: string,
  indexValue: string,
  userData?: string
});

sf.idaV1.updateSubscriptionUnits({
  indexId: string,
  superToken: string,
  subscriber: string,
  units: string,
  userData?: string
});

sf.idaV1.approveSubscription({
  indexId: string,
  superToken: string,
  publisher: string,
  userData?: string
});

sf.idaV1.revokeSubscription({
  indexId: string,
  superToken: string,
  publisher: string,
  userData?: string
});

sf.idaV1.deleteSubscription({
  indexId: string,
  superToken: string,
  subscriber: string,
  publisher: string,
  userData?: string
});

sf.idaV1.claim({
  indexId: string,
  superToken: string,
  subscriber: string,
  publisher: string,
  userData?: string
});
```

#### Example Usage

```ts
import { Framework } from "@superfluid-finance/sdk-core";
import { ethers } from "ethers";

const provider = new ethers.providers.InfuraProvider(
  "matic",
  "<INFURA_API_KEY>"
);

const sf = await Framework.create({
  networkName: "matic",
  provider
});

// Read example
const subscription = await sf.idaV1.getSubscription({ superToken: "0x...", publisher: "0x...", indexId: "1", subscriber: "0x...", providerOrSigner: provider });
console.log(subscription);


// Write operation example
const signer = sf.createSigner({ privateKey: "<TEST_ACCOUNT_PRIVATE_KEY>", provider });
const createIndexOperation = sf.idaV1.createIndex({ indexId: "0", userData: "0x" });
const txnResponse = await createIndexOperation.exec(signer);
const txnReceipt = await txnResponse.wait();
// Transaction Complete when code reaches here
```

### SuperToken

The `SuperToken` class can also be accessed via the `Framework` class and allows you read from/write to the blockchain. It also provides write functions for both the CFAV1 and IDAV1 contracts in the context of the token. That is, the token field for these different methods will be the token address specified during the creation of this class.

#### Framework based initialization

```ts
import { Framework } from "@superfluid-finance/sdk-core";
import { ethers } from "ethers";

const provider = new ethers.providers.InfuraProvider(
  "matic",
  "<INFURA_API_KEY>"
);

const sf = await Framework.create({
  networkName: "matic",
  provider
});

const usdcx = await sf.loadSuperToken("0xCAa7349CEA390F89641fe306D93591f87595dc1F");
```

#### Direct Initialization

```ts
import { SuperToken } from "@superfluid-finance/sdk-core";
import { ethers } from "ethers";

const provider = new ethers.providers.InfuraProvider(
  "matic",
  "<INFURA_API_KEY>"
);

const config = {
  hostAddress: "0x3E14dC1b13c488a8d5D310918780c983bD5982E7",
  cfaV1Address: "0x6EeE6060f715257b970700bc2656De21dEdF074C",
  idaV1Address: "0xB0aABBA4B2783A72C52956CDEF62d438ecA2d7a1"
};

const usdcx = await SuperToken.create({
  address: "0xCAa7349CEA390F89641fe306D93591f87595dc1F",
  config,
  networkName: "matic", // you can also pass in chainId instead (e.g. chainId: 137)
  provider
});
```

#### SuperToken Functions

```ts
const usdcx = await sf.loadSuperToken("0xCAa7349CEA390F89641fe306D93591f87595dc1F");

// ERC20 `Token`
// Read functions
await usdcx.balanceOf({
  account: string,
  providerOrSigner: ethers.providers.Provider | ethers.Signer
});

await usdcx.allowance({
  owner: string,
  spender: string,
  providerOrSigner: ethers.providers.Provider | ethers.Signer
});

await usdcx.name({
  providerOrSigner: ethers.providers.Provider | ethers.Signer
});

await usdcx.symbol({
  providerOrSigner: ethers.providers.Provider | ethers.Signer
});

await usdcx.totalSupply({
  providerOrSigner: ethers.providers.Provider | ethers.Signer
});


// Write operations
usdcx.approve({
  receiver: string,
  amount: string
});

usdcx.transfer({
  receiver: string,
  amount: string
});

usdcx.transferFrom({
  sender: string,
  receiver: string,
  amount: string
});

// `SuperToken` only function
await usdcx.realtimeBalanceOf({
  account: string,
  timestamp: string,
  providerOrSigner: ethers.providers.Provider | ethers.Signer
});

// Write Functions

// All write functions return Promise<Operation>

// SuperToken Write operations
usdcx.downgrade({ amount: string });

usdcx.upgrade({ amount: string });

// SuperToken CFAV1/IDAV1 Functions are the same as the
// ConstantFlowAgreementV1/InstantDistributionAgreementV1 class functions
// except instead of the sf.cfaV1/idaV1.function() signature, it is token.function()
// e.g. await usdcx.createIndex({ indexId: "0", userData: "0x" }).exec(signer);
// and you don't need to pass in a token as a parameter as it uses the token address
// of the instantiated class.
```

#### Example Usage

```ts
import { Framework } from "@superfluid-finance/sdk-core";
import { ethers } from "ethers";

const provider = new ethers.providers.InfuraProvider(
  "matic",
  "<INFURA_API_KEY>"
);

const sf = await Framework.create({
  networkName: "matic",
  provider
});

const usdcx = await sf.loadSuperToken("0xCAa7349CEA390F89641fe306D93591f87595dc1F");

// Read example
const name = await usdcx.name();
console.log(name);

// Write operation example
const signer = sf.createSigner({ privateKey: "<TEST_ACCOUNT_PRIVATE_KEY>", provider });
const transferOperation = usdcx.transfer({ receiver: "0x...", amount: "1000000" });
const txnResponse = await transferOperation.exec(signer);
const txnReceipt = await txnResponse.wait();
// Transaction Complete when code reaches here
```

> Note: you can also get the underlying Token object which only has ERC20 token read/write methods-this is useful for things like approving token spend to a SuperToken contract prior to upgrading for example.

```ts
const usdc = usdcx.underlyingToken;
const totalSupply = await usdc.totalSupply();
```

### Batch Call

The `BatchCall` class allows the user to batch multiple supported operations/transactions in one operation. Similar to the other helper classes, we can create this either through the `Framework` or directly initialize this.

#### Supported Operations

Not all operations are supported by the batch call feature, below is a list of the supported operations:

- `ERC20_APPROVE (SuperToken only)`
- `ERC20_TRANSFER_FROM`
- `SUPERTOKEN_UPGRADE`
- `SUPERTOKEN_DOWNGRADE`
- `SUPERFLUID_CALL_AGREEMENT`
- `CALL_APP_ACTION`

Most of the token methods are self explanatory, but some additional context for the last two operations is helpful.
`SUPERFLUID_CALL_AGREEMENT` refers to all operations related to the CFA or IDA (`createFlow`, `updateIndex`, `distribute`, etc.).
`CALL_APP_ACTION` refers to an operation which is created from calling a function that exists on a super app you have created. Refer to Usage below to see how you can create a `CALL_APP_ACTION` operation.

#### Framework based initialization

```ts
import { Framework } from "@superfluid-finance/sdk-core";
import { ethers } from "ethers";

const provider = new ethers.providers.InfuraProvider(
  "matic",
  "<INFURA_API_KEY>"
);

const sf = await Framework.create({
  networkName: "matic",
  provider
});

const signer = sf.createSigner({ privateKey: "<TEST_ACCOUNT_PRIVATE_KEY>", provider });
const batchCall = sf.batchCall([<OPERATION_A>, <OPERATION_B>, ...]);
```

#### Direct Initialization

```ts
import { SuperToken } from "@superfluid-finance/sdk-core";

const batchCall = new BatchCall({
  hostAddress: "0x3E14dC1b13c488a8d5D310918780c983bD5982E7",
  operations: [<OPERATION_A>, <OPERATION_B>, ...],
});
```

#### Usage

```ts
import { Framework } from "@superfluid-finance/sdk-core";
import { ethers } from "ethers";

const provider = new ethers.providers.InfuraProvider(
  "matic",
  "<INFURA_API_KEY>"
);

const sf = await Framework.create({
  networkName: "matic",
  provider
});
 
 // 0xabc is the signer on Rinkeby testnet
const signer = sf.createSigner({ privateKey: "<TEST_ACCOUNT_PRIVATE_KEY>", provider });
const daix = await sf.loadSuperToken("0x745861AeD1EEe363b4AaA5F1994Be40b1e05Ff90");
const fromAddress = "0xabc";
const paymentAddress = "0xdef";
const approveOp = daix.approve({ receiver: paymentAddress, amount: "10000" });
const transferFromOp = daix.transferFrom({
  sender: fromAddress,
  receiver: paymentAddress,
  amount: "10000",
});
const batchCall = sf.batchCall([approveOp, transferFromOp]);
const txn = await batchCall.exec(signer);

// creating an operation from a super app function
// initialize your super app contract
const superApp = new ethers.Contract("0x...", <SUPER_APP_ABI>);

// populate the transaction
const superAppTransactionPromise = superApp.populateTransaction.helloWorld("hello world");

// create the super app operation you can execute this operation directly or pass it in to a batch call
const superAppOperation = new Operation(superAppTransactionPromise, "CALL_APP_ACTION");
```
