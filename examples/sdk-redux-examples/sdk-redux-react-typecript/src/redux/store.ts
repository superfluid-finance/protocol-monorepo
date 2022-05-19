import { configureStore, Dispatch } from "@reduxjs/toolkit";
import {
    initializeRpcApiSlice,
    initializeTransactionTrackerSlice,
    initializeSubgraphApiSlice,
    createApiWithReactHooks,
    allSubgraphEndpoints,
    allRpcEndpoints
} from "@superfluid-finance/sdk-redux";
import { TypedUseSelectorHook, useDispatch, useSelector } from "react-redux";

export const sfApi = initializeRpcApiSlice(createApiWithReactHooks).injectEndpoints(allRpcEndpoints);

export const sfSubgraph = initializeSubgraphApiSlice(createApiWithReactHooks).injectEndpoints(
    allSubgraphEndpoints
);

export const sfTransactions = initializeTransactionTrackerSlice();

export const store = configureStore({
    reducer: {
        [sfApi.reducerPath]: sfApi.reducer,
        [sfTransactions.reducerPath]: sfTransactions.reducer,
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
