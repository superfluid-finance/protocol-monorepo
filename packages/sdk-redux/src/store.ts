import {configureStore} from '@reduxjs/toolkit';

import {consoleDataApi} from "./features/console/consoleDataApi";
import {normalizedDataSlice} from './features/normalized/normalizedDataSlice';

export const dappSdkStore = configureStore({
    reducer: {
        normalizedData: normalizedDataSlice.reducer,
        [consoleDataApi.reducerPath]: consoleDataApi.reducer
    },
    middleware: (getDefaultMiddleware) => getDefaultMiddleware().concat(consoleDataApi.middleware),
});

export type DAppSdkRootState = ReturnType<typeof dappSdkStore.getState>;
export type DAppSdkDispatch = typeof dappSdkStore.dispatch;
export type DAppSdkStoreType = typeof dappSdkStore;
