import { configureStore, Dispatch } from "@reduxjs/toolkit";
import {
    initializeRpcSlice,
    initializeTransactionSlice,
    initializeSubgraphSlice,
    createApiWithReactHooks,
    allSubgraphEndpoints,
    allRpcEndpoints
} from "@superfluid-finance/sdk-redux";
import { TypedUseSelectorHook, useDispatch, useSelector } from "react-redux";

export const sfApi = initializeRpcSlice(createApiWithReactHooks).injectEndpoints(allRpcEndpoints);

export const sfSubgraph = initializeSubgraphSlice(createApiWithReactHooks).injectEndpoints(
    allSubgraphEndpoints
);

export const sfTransactions = initializeTransactionSlice();gi

export const store = configureStore({
    reducer: {
        [sfApi.reducerPath]: sfApi.reducer,
        sfTransactions: sfTransactions.reducer,
        [sfSubgraph.reducerPath]: sfSubgraph.reducer,
    },
    middleware: (getDefaultMiddleware) =>
        getDefaultMiddleware()
            .concat(sfApi.middleware)
            .concat(sfSubgraph.middleware),
});

export type AppDispatch = typeof store.dispatch;
export type RootState = ReturnType<typeof store.getState>;

// eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
export const useAppDispatch = () => useDispatch<Dispatch>();
export const useAppSelector: TypedUseSelectorHook<RootState> = useSelector;
