import {configureStore, Dispatch} from "@reduxjs/toolkit";
import { createSdkReduxParts } from "@superfluid-finance/sdk-redux";
import {TypedUseSelectorHook, useDispatch, useSelector} from "react-redux";

const {
    superfluidContext,
    apiSlice,
    transactionSlice,
} = createSdkReduxParts();

export const store = configureStore({
    reducer: {
        [apiSlice.reducerPath]: apiSlice.reducer,
        [transactionSlice.reducerPath]: transactionSlice.reducer,
    },
    middleware: (getDefaultMiddleware) =>
        getDefaultMiddleware().concat(apiSlice.middleware),
});

export type AppDispatch = typeof store.dispatch;
export type RootState = ReturnType<typeof store.getState>;
export { superfluidContext };

// eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
export const useAppDispatch = () => useDispatch<Dispatch>();
export const useAppSelector: TypedUseSelectorHook<RootState> = useSelector;
