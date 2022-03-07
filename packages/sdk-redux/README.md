<h1 align="center">sdk-redux</h1>
<div align="center">
<img  width="300" padding="0 0 10px" alt="Superfluid logo" src="./sf-logo.png" />
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
    allSubgraphSliceEndpoints,
    createApiWithReactHooks,
    initializeSfApiSlice,
    initializeSfSubgraphSlice,
    initializeSfTransactionSlice
} from "@superfluid-finance/sdk-redux";
```

Create the Redux slices:
```ts
export const { sfApi } = initializeSfApiSlice(createApiWithReactHooks);
export const { sfTransactions } = initializeSfTransactionSlice();
export const sfSubgraph = initializeSfSubgraphSlice(createApiWithReactHooks).injectEndpoints(allSubgraphSliceEndpoints);

```

Plug in the slices to the Redux store:
```ts
export const store = configureStore({
    reducer: {
        "sfApi": sfApi.reducer,
        "sfTransactions": sfTransactions.reducer,
        "sfSubgraph": sfSubgraph.reducer
    }
});
```

Add the middleware:
```ts
export const store = configureStore({
    reducer: {
        "sfApi": sfApi.reducer,
        "sfTransactions": sfTransactions.reducer,
    },
    middleware: (getDefaultMiddleware) =>
        getDefaultMiddleware().concat(sfApi.middleware).concat(sfSubgraph.middleware),
});
```

Somewhere in your code, give instructions to the `superfluidContext` to locate `Framework` and `Signer`:
```ts
import { setFrameworkForSdkRedux, setSignerForSdkRedux } from "@superfluid-finance/sdk-redux";

setFrameworkForSdkRedux(chainId, sdkCoreFramework);
setSignerForSdkRedux(chainId, ethersWeb3Provider.getSigner());
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
const tx = await sfApi.createFlow({
    senderAddress: signerAddress,
    receiverAddress: receiver,
    flowRateWei: flowRate,
    chainId,
    superTokenAddress: superToken,
    waitForConfirmation,
}).unwrap();
```

### Transaction Tracking
All mutations trigger tracking for transaction progress (stored in `transactionSlice`) and transaction monitoring for re-orgs (all cached data is re-fetched in case of a re-org).

# Examples
Check out the extensive demo here: `examples/sdk-redux-react-typescript`.
