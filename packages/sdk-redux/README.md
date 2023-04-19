<h1 align="center">Welcome to @superfluid-finance/sdk-redux 👋
</h1>
<div align="center">
<img  width="300" padding="0 0 10px" alt="Superfluid logo" src="https://github.com/superfluid-finance/protocol-monorepo/raw/dev/sf-logo.png" />
<p>
  <a href="https://www.npmjs.com/package/@superfluid-finance/sdk-redux" target="_blank">
    <img alt="Version" src="https://img.shields.io/npm/v/@superfluid-finance/sdk-redux.svg">
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
SDK-Redux is an application framework for building front-end applications that interact with the Superfluid Protocol.

More specifically, SDK-Redux is a wrapper library around `@superfluid-finance/sdk-core` which adds state management to Superfluid related queries and operations.
Under the hood, SDK-Redux leverages popular Redux libraries Redux Toolkit & RTK Query.

## Important Disclaimer
SDK-Redux is in early active development and can have breaking releases without warning and without consideration for semantic versioning.

# Features
* Tracking loading state in order to show UI spinners
* Avoiding duplicate requests for the same data
* Managing cache lifetimes as the user interacts with the UI
* Tracking blockchain transactions produced by user interactions

# Notable Used Technologies
* TypeScript
* Redux
* Redux Toolkit
* RTK Query
* Ethers

# Requirements
* SDK-Core
* Redux store & Redux Toolkit
* React* (The SDK-Redux generates React Hooks which are recommended but not strictly necessary to use. The SDK-Redux is UI-framework agnostic but we currently have example only for React)

# Getting Started
## Plugging Into Redux Store
Requirements:

A brand-new scaffolded Redux store configuration looks something like this:
```ts
import { configureStore, ThunkAction, Action } from '@reduxjs/toolkit';

export const store = configureStore({
  reducer: {
  },
});

export type AppDispatch = typeof store.dispatch;
export type RootState = ReturnType<typeof store.getState>;
export type AppThunk<ReturnType = void> = ThunkAction<
  ReturnType,
  RootState,
  unknown,
  Action<string>
>;
```

We need to plug in the Superfluid SDK-Redux parts.

Import the following function:
```ts
import {
    allRpcEndpoints,
    allSubgraphEndpoints,
    createApiWithReactHooks,
    initializeRpcApiSlice,
    initializeSubgraphApiSlice,
    initializeTransactionTrackerSlice
} from "@superfluid-finance/sdk-redux";
```

Create the Redux slices:
```ts
export const rpcApi = initializeRpcApiSlice(createApiWithReactHooks).injectEndpoints(allRpcEndpoints);
export const subgraphApi = initializeSubgraphApiSlice(createApiWithReactHooks).injectEndpoints(allSubgraphEndpoints);
export const transactionTracker = initializeTransactionTrackerSlice();

```

Plug in the slices to the Redux store:
```ts
export const store = configureStore({
    reducer: {
        [rpcApi.reducerPath]: rpcApi.reducer,
        [subgraphApi.reducerPath]: subgraphApi.reducer
        [transactionTracker.reducerPath]: transactionTracker.reducer,
    }
});
```

Add the middlewares (important to add for both `rpcApi` & `subgraphApi`):
```ts
export const store = configureStore({
    reducer: {
        [rpcApi.reducerPath]: rpcApi.reducer,
        [subgraphApi.reducerPath]: subgraphApi.reducer
        [transactionTracker.reducerPath]: transactionTracker.reducer,
    },
    middleware: (getDefaultMiddleware) =>
        getDefaultMiddleware().concat(rpcApi.middleware).concat(subgraphApi.middleware),
});
```

Somewhere in your code, give instructions to the `sdkReduxConfig` to locate `Framework`:
```ts
import { setFrameworkForSdkRedux } from "@superfluid-finance/sdk-redux";

setFrameworkForSdkRedux(chainId, sdkCoreFramework);
```

That should be it! You should now be able to dispatch messages to Superfluid reducers & use the React hooks.

## Using Queries (i.e. "read" operations)
Read about RTK-Query queries here: https://redux-toolkit.js.org/rtk-query/usage/queries

Example using React Hook:
```ts
const {
    data: pagedStreams,
    isUninitialized,
    isFetching,
    isLoading,
    isError,
    error,
    refetch,
} = sfSubgraph.useStreamsQuery({
    chainId: queryChainId,
    filter: {
      token: superTokenAddress,
      sender: senderAddress
    },
    pagination: {
        skip: (page - 1) * pageSize,
        take: pageSize
    },
    ordering: {
      orderBy: "currentFlowRate",
      orderDirection: "desc"
    }
}, {
    pollingInterval: 5000 // Not necessary to use but nice-to-have additional option by RTK-Query.
});
```

## Using Mutations (i.e. "write" operations)
Read about RTK-Query queries here: https://redux-toolkit.js.org/rtk-query/usage/mutations

Example using React Hook:
```ts
const tx = await rpcApi.createFlow({
    signer,
    chainId,
    senderAddress: senderAddress,
    receiverAddress: receiver,
    flowRateWei: flowRate,
    superTokenAddress: superToken
}).unwrap();
```

### Transaction Tracking
All mutations trigger tracking for transaction progress (stored in `transactionTrackerSlice`) and transaction monitoring for re-orgs (all cached data is re-fetched in case of a re-org).

# Examples
Check out the extensive demo here: `examples/sdk-redux-react-typescript`.
