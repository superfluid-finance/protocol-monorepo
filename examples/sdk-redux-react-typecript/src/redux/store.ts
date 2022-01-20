import {configureStore, Dispatch} from "@reduxjs/toolkit";
import {
    initializeSfApiSlice,
    initializeSfTransactionSlice,
    initializeSubgraphSlice,
    createApiWithReactHooks
} from "@superfluid-finance/sdk-redux";
import { TypedUseSelectorHook, useDispatch, useSelector } from "react-redux";

export const { sfApi } = initializeSfApiSlice(createApiWithReactHooks);
export const { sfSubgraph } = initializeSubgraphSlice(createApiWithReactHooks);
export const { sfTransactions } = initializeSfTransactionSlice();

export const store = configureStore({
    reducer: {
        "sfApi": sfApi.reducer,
        "sfTransactions": sfTransactions.reducer,
        "sfSubgraph": sfSubgraph.reducer
    },
    middleware: (getDefaultMiddleware) =>
        getDefaultMiddleware().concat(sfApi.middleware),
});

export type AppDispatch = typeof store.dispatch;
export type RootState = ReturnType<typeof store.getState>;

// eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
export const useAppDispatch = () => useDispatch<Dispatch>();
export const useAppSelector: TypedUseSelectorHook<RootState> = useSelector;
