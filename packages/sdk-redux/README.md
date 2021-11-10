<h1 align="center">sdk-redux</h1>
<div align="center">
<img  width="300" padding="0 0 10px" alt="Superfluid logo" src="/sf-logo.png" />
<p>
  <a href="https://www.npmjs.com/package/@superfluid-finance/sdk-redux" target="_blank">
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

# Introduction

SDK-Redux is a wrapper library around SDK-Core which adds state management to Superfluid related queries and operations.
Under the hood, SDK-Redux leverages popular Redux libraries Redux Toolkit & RTK Query.

# Plugging into Redux store

Requirements:
* Project with Redux store & Redux Toolkit

A brand-new scaffolded Redux store configuration looks something like this:
```
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

We need to plug in the Superfluid pieces.

Import this function: `import { createPieces } from "@superfluid-finance/sdk-redux";`

Then create the pieces:
```
const [
    superfluidFrameworkSource,
    superfluidApiSlice,
    superfluidTransactionSlice,
] = createPieces();
```

Plug in the reducer slices:
```
export const store = configureStore({
  reducer: {
    //
    "superfluidApi": superfluidApiSlice.reducer,
    "superfluidTransactions": superfluidTransactionSlice.reducer,
    //
  },
});
```

Add the middleware:
```
export const store = configureStore({
  reducer: {
    "superfluidApi": superfluidApiSlice.reducer,
    "superfluidTransactions": superfluidTransactionSlice.reducer,
  },
  //
  middleware: (getDefaultMiddleware) =>
    getDefaultMiddleware().concat(superfluidApiSlice.middleware)
  //
});
```

Export the SDK-Core source:
```
export { superfluidFrameworkSource };
```

Somewhere in your code, give instructions to the `superfluidFrameworkSource` to locate `Framework` and `Signer`:
```
superfluidFrameworkSource.setFramework(
    chainId,
    Promise.resolve(superfluidSdk)
);
superfluidFrameworkSource.setSigner(
    chainId,
    Promise.resolve(ethersWeb3Provider.getSigner())
);
```

That should be it! You should now be able to dispatch messages to Superfluid reducers & use the React hooks.

# Examples

Check out `examples/sdk-redux-react-typescript`.
